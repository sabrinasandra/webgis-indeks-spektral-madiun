# =============================================================================
# WebGIS Indeks Spektral Kabupaten & Kota Madiun
# -----------------------------------------------------------------------------
# Dashboard berbasis R (shiny + shinydashboard + leaflet) yang menampilkan:
#   1. Peta interaktif indeks spektral Sentinel-2 (NDVI, NDBI, NDWI)
#   2. Tabel statistik zonal per kecamatan (sorting, filter, pagination)
#
# Prasyarat : folder Data/ (batas administrasi) dan output/ (raster + CSV)
# Menjalankan: shiny::runApp(".")  atau  Rscript app.R
# =============================================================================


# =============================================================================
# 1. MEMANGGIL LIBRARY
# =============================================================================

library(shiny)           # framework aplikasi web
library(shinydashboard)  # komponen dashboard (header, sidebar, body)
library(leaflet)         # peta interaktif
library(sf)              # data vektor (batas kecamatan)
library(raster)          # data raster (indeks spektral)
library(DT)              # tabel data interaktif


# =============================================================================
# 2. KONFIGURASI PATH & METADATA INDEKS
# -----------------------------------------------------------------------------
# Semua data dimuat SEKALI di level global (di luar server), sehingga tidak
# dibaca ulang setiap kali ada pengguna baru membuka dashboard.
#
# Catatan: file batas administrasi di folder Data/ bernama "batas madiun"
# (tanpa ekstensi) sehingga dicari otomatis dengan pola nama "batas".
# =============================================================================

folder_data   <- "Data"
folder_output <- "output"

# Fungsi bantu: mencari satu file di dalam folder berdasarkan pola nama.
# ignore.case = TRUE membuat pencarian tidak terpengaruh huruf besar/kecil
# (misalnya "AWAN_madiun.tif" tetap ketemu walaupun ditulis "AWAN_Madiun.tif").
cari_file <- function(folder, pola) {
  hasil <- list.files(folder, pattern = pola, full.names = TRUE,
                      ignore.case = TRUE)
  hasil <- hasil[!dir.exists(hasil)]          # abaikan nama folder
  if (length(hasil) == 0) return(NA_character_)
  hasil[1]
}

# Nama file batas administrasi & tabel statistik zonal
file_batas <- cari_file(folder_data, "^batas")
file_zonal <- cari_file(folder_output, "^Zonal_Statistics_Madiun\\.csv$")

# Metadata tiap indeks: nama file, label di legenda, dan palet warna
#   NDVI  -> hijau-kuning        (vegetasi)
#   NDBI  -> merah-coklat        (lahan terbangun)
#   NDWI  -> gradasi biru        (perairan)
# Bila nanti ada file NDCI_madiun.tif, cukup tambahkan satu baris di sini
# (misal NDCI = list(file = "NDCI_madiun.tif", label = "NDCI - Klorofil",
# palet = "YlGnBu")) dan layer itu otomatis muncul di peta maupun legenda.
meta_indeks <- list(
  NDVI = list(file = "NDVI_madiun.tif",
              label = "NDVI - Vegetasi",
              palet = "YlGn"),
  NDBI = list(file = "NDBI_madiun.tif",
              label = "NDBI - Lahan Terbangun",
              palet = "YlOrRd"),
  NDWI = list(file = "NDWI_madiun.tif",
              label = "NDWI - Perairan",
              palet = "Blues")
)

# Lengkapkan path raster (nama file di-escape agar titik .tif tidak dianggap
# sebagai wildcard pada pola pencarian)
file_indeks <- vapply(
  meta_indeks,
  function(m) cari_file(folder_output, paste0("^", gsub("\\.", "\\\\.", m$file), "$")),
  character(1)
)

# Pastikan semua file yang dibutuhkan tersedia sebelum aplikasi jalan
file_wajib <- c(batas = file_batas, zonal = file_zonal, file_indeks)
if (any(is.na(file_wajib)) || any(!file.exists(file_wajib))) {
  stop("File tidak ditemukan:\n  ",
       paste(file_wajib[is.na(file_wajib) | !file.exists(file_wajib)],
             collapse = "\n  "))
}

# Faktor agregasi raster saat ditampilkan di web:
#   2 -> resolusi tampilan 40 m (3 raster dikirim +/- 6 MB per sesi)
#   3 -> resolusi tampilan 60 m (lebih ringan lagi, +/- 3 MB per sesi)
faktor_agregasi <- 2


# =============================================================================
# 2b. KONFIGURASI BASEMAP CARTO
# -----------------------------------------------------------------------------
# Sejak 25 September 2026 tile raster CARTO (basemaps.cartocdn.com) hanya
# dilayani bila URL-nya menyertakan parameter ?key=... Tanpa key, yang muncul
# adalah gambar bertuliskan "API KEY REQUIRED". Karena itu basemap CARTO di
# bawah ini dibangun manual (tidak lagi memakai providers$CartoDB.* milik
# leaflet yang tanpa key).
#
# Key diambil dari environment variable CARTO_API_KEY bila ada; kalau tidak,
# dipakai key bawaan di bawah ini. Key basemap CARTO ini gratis (bukan kredensial
# akun) dan boleh dipakai di aplikasi non-komersial dengan atribusi tetap tampil.
# =============================================================================

carto_api_key <- Sys.getenv(
  "CARTO_API_KEY",
  unset = "cb1_4hsu_1_cbb8a8ccb279515c7a94faa8"
)

# Atribusi wajib: OpenStreetMap + CARTO (syarat pemakaian CARTO Basemaps)
atrib_carto <- paste0(
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, ',
  '&copy; <a href="https://carto.com/attributions">CARTO</a>'
)

# Daftar gaya basemap CARTO yang dipakai (nama tampilan = kunci, style = nilai).
# Nama style mengikuti dokumentasi https://github.com/CartoDB/basemap-styles
carto_style <- c(
  "CARTO Positron"                = "light_all",             # terang, cocok untuk overlay
  "CARTO Positron Tanpa Label"    = "light_nolabels",        # terang tanpa nama tempat
  "CARTO Dark Matter"             = "dark_all",              # gelap, kontras untuk data
  "CARTO Dark Matter Tanpa Label" = "dark_nolabels",         # gelap tanpa nama tempat
  "CARTO Voyager"                 = "rastertiles/voyager"    # berwarna, jalan berkelas
)

# Fungsi bantu pembuat URL tile CARTO (sudah termasuk API key).
#   {s} = subdomain a-d, {r} = "@2x" pada layar retina (detectRetina)
url_carto <- function(style) {
  paste0("https://{s}.basemaps.cartocdn.com/", style,
         "/{z}/{x}/{y}{r}.png?key=", carto_api_key)
}

# Daftar basemap untuk layer control. Urutan pertama = basemap yang aktif
# saat aplikasi dibuka (CARTO Positron dipilih karena paling nyaman untuk
# membaca overlay raster di atasnya).
grup_basemap <- c(names(carto_style), "OpenStreetMap", "Esri World Imagery")


# =============================================================================
# 3. PRA-PROSES RASTER INDEKS (sekali di awal, bukan per sesi)
# -----------------------------------------------------------------------------
# Raster asli berukuran 2216 x 2207 sel (resolusi 20 m, UTM 49S) = 4,9 juta
# piksel per indeks. Bila langsung dikirim ke Leaflet, proyeksi + encoding PNG
# akan lambat dan berisiko melebihi batas ukuran (4 MB) bawaan addRasterImage.
#
# Solusinya, sekali di awal aplikasi kami:
#   a. turunkan resolusi 2x (faktor agregasi 2 -> 1108 x 1104 sel), DAN
#   b. reproject ke Web Mercator (EPSG:3857) sesuai sistem koordinat Leaflet,
#   semuanya dalam SATU langkah resampling saja (lebih hemat waktu).
# Hasilnya disimpan dalam objek global dan dipakai bersama oleh semua sesi.
# =============================================================================

baca_dan_praproses <- function(path, faktor = faktor_agregasi) {

  # Membaca metadata (nilai piksel baru dibaca saat di-resample)
  r <- raster(path)

  # Buat grid tujuan: proyeksi ke EPSG:3857 lalu diperkecil faktor agregasi
  grid_web <- projectExtent(r, crs = "EPSG:3857")
  grid_web <- raster(
    ncols = max(1L, round(ncol(grid_web) / faktor)),
    nrows = max(1L, round(nrow(grid_web) / faktor)),
    xmn = xmin(grid_web), xmx = xmax(grid_web),
    ymn = ymin(grid_web), ymx = ymax(grid_web),
    crs = "EPSG:3857"
  )

  # Resampling bilinear (cocok untuk nilai indeks yang kontinu)
  projectRaster(r, to = grid_web, method = "bilinear")
}

# Muat semua raster indeks (jumlahnya mengikuti isi meta_indeks)
raster_indeks <- lapply(file_indeks, baca_dan_praproses)

# Rentang nilai tiap indeks -> dipakai sebagai domain palet warna & legenda
rentang_indeks <- lapply(raster_indeks, function(r) {
  range(values(r), na.rm = TRUE)
})

# Objek palet Leaflet (colorNumeric). Palet dibuat sebagai FUNGSI agar
# addRasterImage memakai domain ini, bukan domain otomatis dari data.
# Warna transparan (#00000000) dipakai untuk piksel NA (di luar batas wilayah).
palet_indeks <- Map(
  f = function(nama, rentang) {
    colorNumeric(
      palette  = meta_indeks[[nama]]$palet,
      domain   = rentang,
      na.color = "#00000000",
      alpha    = TRUE
    )
  },
  nama    = names(raster_indeks),
  rentang = rentang_indeks
)


# =============================================================================
# 4. MEMUAT BATAS ADMINISTRASI (vektor)
# -----------------------------------------------------------------------------
# GeoJSON sudah dalam EPSG:4326 (lon/lat) sehingga siap dipakai Leaflet
# tanpa perlu transformasi. Isinya 21 kecamatan: Kota Madiun, Kabupaten
# Madiun, serta sebagian Magetan dan Ponorogo yang masuk ke dalam citra.
# =============================================================================

batas_kec <- st_read(file_batas, quiet = TRUE)

# Bersihkan geometri agar tidak ada poligon kosong / tidak valid
batas_kec <- batas_kec[!st_is_empty(batas_kec), ]
batas_kec <- st_make_valid(batas_kec)

# st_make_valid bisa menghasilkan campuran POLYGON / MULTIPOLYGON /
# GEOMETRYCOLLECTION. Leaflet hanya mau poligon, jadi paksa semuanya
# menjadi MULTIPOLYGON agar addPolygons tidak error.
batas_kec <- st_cast(batas_kec, "MULTIPOLYGON")

# Samakan sistem koordinat ke WGS84 (lon/lat) bila berbeda
epsg_batas <- st_crs(batas_kec)$epsg
if (is.null(epsg_batas) || is.na(epsg_batas) || epsg_batas != 4326) {
  batas_kec <- st_transform(batas_kec, 4326)
}

# Kolom untuk tooltip: pakai kolom "label" bila ada (berisi nama kecamatan,
# kabupaten/kota, dan provinsi), kalau tidak ada pakai nama kecamatan saja.
if ("label" %in% names(batas_kec)) {
  batas_kec$label_peta <- batas_kec$label
} else if ("kecamatan" %in% names(batas_kec)) {
  batas_kec$label_peta <- batas_kec$kecamatan
} else {
  batas_kec$label_peta <- as.character(seq_len(nrow(batas_kec)))
}


# =============================================================================
# 5. MEMUAT TABEL STATISTIK ZONAL
# =============================================================================

zonal_stats <- read.csv(file_zonal, stringsAsFactors = FALSE)


# =============================================================================
# 5b. GABUNG STATISTIK ZONAL KE ATRIBUT POLIGON
# -----------------------------------------------------------------------------
# Statistik dari CSV digabungkan ke atribut batas kecamatan agar bisa
# ditampilkan pada tooltip/popup. Dua kecamatan yang hanya terpotong di tepi
# citra (Bendo - Magetan, Jenangan - Ponorogo) tidak tercantum di CSV sehingga
# nilai statistiknya NA dan ditampilkan sebagai "n.a.".
# =============================================================================

normalisasi_nama <- function(x) tolower(trimws(x))

kolom_stat <- c("NDVI_Mean", "NDVI_Minimum", "NDVI_Maximum", "NDVI_Median",
                "NDBI_Mean", "NDBI_Minimum", "NDBI_Maximum", "NDBI_Median",
                "NDWI_Mean", "NDWI_Minimum", "NDWI_Maximum", "NDWI_Median")

zonal_join <- zonal_stats[, c("Kecamatan", kolom_stat)]
zonal_join$kec_join <- normalisasi_nama(zonal_join$Kecamatan)
batas_kec$kec_join  <- normalisasi_nama(batas_kec$kecamatan)

# Gabungkan hanya bila nama kecamatan di CSV unik
if (anyDuplicated(zonal_join$kec_join) == 0) {
  batas_kec <- merge(
    batas_kec,
    zonal_join[, c("kec_join", kolom_stat)],
    by = "kec_join",
    all.x = TRUE
  )
}


# =============================================================================
# 5c. TEKS INFORMATIF UNTUK LABEL, TOOLTIP, DAN POPUP
# -----------------------------------------------------------------------------
#   label_pendek : teks permanen di peta (nama kecamatan saja agar ringkas)
#   tooltip_kec  : muncul saat kursor mendarat di poligon (nama + statistik)
#   popup_kec    : muncul saat poligon diklik (info lengkap per indeks)
#   titik_label  : titik pusat kecamatan sebagai penempel label permanen
# =============================================================================

# Dua kecamatan (Bendo - Magetan, Jenangan - Ponorogo) berupa irisan nol-luas
# di tepi citra: setelah st_make_valid geometrinya tidak menyimpan koordinat
# berarti sehingga st_point_on_surface kosong. Karena tidak bisa digambar
# maupun diberi label, keduanya dikeluarkan dari peta agar konsisten.
crs_planar <- 32649   # UTM zona 49S (wilayah Madiun)
pusat_geom <- st_geometry(batas_kec) |>
  st_transform(crs_planar) |>
  st_point_on_surface() |>
  st_transform(4326)

sehat <- !st_is_empty(pusat_geom)
batas_kec  <- batas_kec[sehat, ]
pusat_geom <- pusat_geom[sehat]

# Nama kabupaten/kota yang mudah dibaca
kab_label <- ifelse(
  batas_kec$kab_kota == "Kota Madiun",
  "Kota Madiun",
  paste0("Kabupaten ", batas_kec$kab_kota)
)

# Format angka 3 desimal; "n.a." bila data tidak ada
fmt3 <- function(x) ifelse(is.na(x), "n.a.", sprintf("%.3f", x))

# Baris teks ringkas satu indeks untuk tooltip (nilai mean saja)
baris_tooltip <- function(pr) {
  if (all(is.na(batas_kec[[paste0(pr, "_Mean")]]))) return(rep("", nrow(batas_kec)))
  paste0(meta_indeks[[pr]]$label, ": <b>", fmt3(batas_kec[[paste0(pr, "_Mean")]]), "</b>")
}

# Baris teks lengkap satu indeks untuk popup (mean, median, min, max, sd)
baris_popup <- function(pr) {
  if (all(is.na(batas_kec[[paste0(pr, "_Mean")]]))) return(rep("", nrow(batas_kec)))
  paste0(
    "<b>", meta_indeks[[pr]]$label, "</b>: mean ",
    fmt3(batas_kec[[paste0(pr, "_Mean")]]), "<br>",
    "&nbsp;&nbsp;median ", fmt3(batas_kec[[paste0(pr, "_Median")]]),
    " &nbsp;|&nbsp; sd ", fmt3(batas_kec[[paste0(pr, "_SD")]]), "<br>",
    "&nbsp;&nbsp;min ", fmt3(batas_kec[[paste0(pr, "_Minimum")]]),
    " | max ", fmt3(batas_kec[[paste0(pr, "_Maximum")]])
  )
}

prs_aktif <- intersect(c("NDVI", "NDBI", "NDWI"), names(meta_indeks))

label_pendek <- batas_kec$kecamatan
tooltip_kec   <- paste0("<b>", label_pendek, "</b><br>", kab_label)
popup_kec     <- paste0("<b>", label_pendek, "</b><br>", kab_label,
                        "<hr style='margin:5px 0'>")
for (pr in prs_aktif) {
  tooltip_kec <- paste0(tooltip_kec, "<br>", baris_tooltip(pr))
  popup_kec   <- paste0(popup_kec, "<br>", baris_popup(pr))
}

# PENTING: htmltools::HTML() MENGGABUNGKAN sebuah vektor menjadi SATU string
# (length 1) sehingga bila dipakai langsung, semua kecamatan mendapat teks yang
# sama. Karena itu setiap teks dibungkus per-elemen menjadi DAFTAR string HTML.
tooltip_html <- lapply(tooltip_kec, htmltools::HTML)
popup_html   <- lapply(popup_kec, htmltools::HTML)

# Titik pusat tiap kecamatan sebagai penempel label nama permanen
titik_label <- st_sf(
  label_pendek = label_pendek,
  geometry     = pusat_geom
)


# =============================================================================
# 6. UI (shinydashboard)
# =============================================================================

ui <- dashboardPage(

  # --- 6.1 Header dashboard ---
  dashboardHeader(
    title = "WebGIS Indeks Spektral Madiun",
    titleWidth = 340
  ),

  # --- 6.2 Sidebar berisi menu navigasi ---
  dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "menu_samping",
      menuItem("Peta Interaktif",
               tabName = "peta",
               icon = icon("map")),
      menuItem("Statistik Zonal",
               tabName = "statistik",
               icon = icon("table"))
    )
  ),

  # --- 6.3 Body dashboard ---
  dashboardBody(

    # CSS: peta Leaflet mengisi tinggi layar (tinggi layar - tinggi header - jarak)
    tags$head(
      tags$style(HTML("
        /* Hilangkan scroll ganda pada tab peta */
        .content-wrapper { overflow-y: auto; }

        /* Kontainer peta setinggi viewport */
        .wrapper .content .map-wrap {
          padding: 0;
          margin: 0;
          height: calc(100vh - 80px);
        }
        .map-wrap .shiny-leaflet-output {
          height: 100% !important;
          border-radius: 0;
        }

        /* Ruang untuk tab tabel */
        .tab-content { padding-top: 10px; }

        /* Label nama kecamatan yang SELALU tampil di peta (tanpa klik/hover) */
        /* Teks putih dengan bayangan gelap agar terbaca di atas raster maupun */
        /* basemap terang/gelap. */
        .label-kec-permanen {
          background: transparent !important;
          border: none !important;
          box-shadow: none !important;
          color: #ffffff !important;
          font-size: 12px;
          font-weight: 700;
          text-shadow: 0 1px 2px rgba(0, 0, 0, 0.95), 0 0 5px rgba(0, 0, 0, 0.9);
          white-space: nowrap;
        }
      "))
    ),

    tabItems(

      # ---------- TAB 1: PETA INTERAKTIF ----------
      tabItem(
        tabName = "peta",
        tags$div(
          class = "map-wrap",
          leafletOutput("peta", height = "calc(100vh - 80px)")
        )
      ),

      # ---------- TAB 2: STATISTIK ZONAL ----------
      tabItem(
        tabName = "statistik",
        box(
          title = "Statistik Nilai Indeks Spektral per Kecamatan",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          # Baris filter di bawah judul kolom untuk menyaring data
          DTOutput("tabel_zonal")
        )
      )
    )
  )
)


# =============================================================================
# 7. SERVER
# =============================================================================

server <- function(input, output, session) {

  # -------------------------------------------------------------------------
  # 7.1 Peta dasar (renderLeaflet) - dieksekusi SEKALI per sesi
  # -------------------------------------------------------------------------
  output$peta <- renderLeaflet({

    # Bangun peta dasar
    peta <- leaflet(options = leafletOptions(
      minZoom = 8,
      maxZoom = 18,
      zoomControl = TRUE
    ))

    # --- 7.1.0 Basemap ---
    # a) Basemap CARTO (perlu API key, lihat bagian 2b)
    for (grup_bm in names(carto_style)) {
      peta <- peta %>%
        addTiles(
          urlTemplate = url_carto(carto_style[[grup_bm]]),
          group       = grup_bm,
          attribution = atrib_carto,
          options     = tileOptions(
            subdomains   = "abcd",     # {s} -> a/b/c/d.basemaps.cartocdn.com
            detectRetina = TRUE,       # {r} -> "@2x" pada layar retina
            maxZoom      = 20
          )
        )
    }

    # b) Basemap tambahan yang tidak memerlukan API key
    peta <- peta %>%
      addTiles(
        group = "OpenStreetMap",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>'
      ) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery")

    # --- 7.1.1 Overlay raster indeks spektral ---
    # Ditambahkan lewat perulangan agar jumlah layer mengikuti meta_indeks.
    # project = FALSE karena raster sudah diproyeksikan ke EPSG:3857 saat
    # aplikasi dimulai (lihat bagian 3) -> jauh lebih ringan.
    for (nama in names(raster_indeks)) {
      peta <- peta %>%
        addRasterImage(
          x        = raster_indeks[[nama]],
          colors   = palet_indeks[[nama]],
          project  = FALSE,
          group    = nama,
          opacity  = 0.85,
          layerId  = nama,
          maxBytes = 12 * 1024^2   # batas 12 MB (bawaan Leaflet hanya 4 MB)
        )
    }

    # --- 7.1.2 Batas kecamatan, layer control, legenda, dan fokus wilayah ---
    peta <- peta %>%

      # Batas kecamatan: hanya garis tepi (tanpa isi).
      # label   = tooltip sebelum diklik (nama + statistik rata-rata)
      # popup   = info lengkap saat diklik
      addPolygons(
        data        = batas_kec,
        group       = "Batas Kecamatan",
        fill        = FALSE,          # tidak ada warna isi
        color       = "#FFFFFF",      # warna garis batas
        weight      = 2,
        opacity     = 1,
        dashArray   = "4 4",
        label       = tooltip_html,
        popup       = popup_html,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#2E86C1",
          bringToFront = TRUE
        )
      ) %>%

      # Label nama kecamatan yang SELALU tampil, tanpa perlu klik/hover.
      # Titik penempel = st_point_on_surface (pusat poligon).
      addLabelOnlyMarkers(
        data  = titik_label,
        label = ~label_pendek,
        labelOptions = labelOptions(
          noHide    = TRUE,
          textOnly  = TRUE,
          direction = "center",
          className = "label-kec-permanen",
          style     = list("pointer-events" = "none", "z-index" = "500")
        ),
        group = "Nama Kecamatan"
      ) %>%

      # Tampilkan NDVI saja di awal, indeks lain disembunyikan
      hideGroup(setdiff(names(meta_indeks), "NDVI")) %>%

      # Pengontrol basemap + toggle overlay
      addLayersControl(
        baseGroups = grup_basemap,
        overlayGroups = c("Nama Kecamatan", names(meta_indeks), "Batas Kecamatan"),
        options = layersControlOptions(collapsed = FALSE, position = "topright")
      ) %>%

      # Legenda awal (akan diganti otomatis saat layer berganti)
      addLegend(
        position  = "bottomright",
        pal       = palet_indeks$NDVI,
        values    = rentang_indeks$NDVI,
        title     = meta_indeks$NDVI$label,
        labFormat = labelFormat(digits = 2),
        opacity   = 0.9
      ) %>%

      # Skala jarak di pojok kiri bawah (informasi ukuran/luas wilayah)
      addScaleBar(position = "bottomleft") %>%

      # Pusatkan dan zoom otomatis ke wilayah Kabupaten & Kota Madiun
      fitBounds(lng1 = st_bbox(batas_kec)[["xmin"]],
                lat1 = st_bbox(batas_kec)[["ymin"]],
                lng2 = st_bbox(batas_kec)[["xmax"]],
                lat2 = st_bbox(batas_kec)[["ymax"]])

    peta
  })


  # -------------------------------------------------------------------------
  # 7.2 Legenda dinamis - menyesuaikan layer raster yang sedang aktif
  # -------------------------------------------------------------------------
  # input$peta_groups berisi daftar group yang sedang tampil di peta, sehingga
  # setiap kali user mencentang / membuka centang layer, server menerima
  # pemberitahuan dan legenda diganti dengan palet milik layer tersebut.
  #
  # Prioritas legenda:
  #   1. layer yang BARU dicentang     -> legenda langsung menunjuk layer itu
  #   2. bila layer legenda dimatikan  -> jatuh ke layer raster berikutnya
  #   3. bila tidak ada raster aktif   -> legenda disembunyikan
  # -------------------------------------------------------------------------

  state <- reactiveValues(
    grup_lama = character(0),   # daftar group pada pemberitahuan sebelumnya
    legenda   = "NDVI"          # layer yang sedang menjadi acuan legenda
  )

  observeEvent(input$peta_groups, {

    grup <- input$peta_groups
    if (is.null(grup)) grup <- character(0)

    raster_aktif <- intersect(names(meta_indeks), grup)

    # Layer raster yang baru saja dinyalakan pengguna
    grup_baru <- intersect(raster_aktif, setdiff(grup, state$grup_lama))

    acuan <- if (length(grup_baru) > 0) {
      grup_baru[1]                                  # ikuti layer terakhir aktif
    } else if (state$legenda %in% raster_aktif) {
      state$legenda                                 # legenda tetap relevan
    } else if (length(raster_aktif) > 0) {
      raster_aktif[1]                               # fallback ke raster lain
    } else {
      NULL                                          # tidak ada raster aktif
    }

    state$grup_lama <- grup

    # Hanya gambar ulang bila acuan legenda benar-benar berubah
    if (identical(acuan, state$legenda)) return()
    state$legenda <- acuan

    proxy <- leafletProxy("peta") %>% clearControls()   # hapus legenda lama

    if (!is.null(acuan)) {
      proxy %>%
        addLegend(
          position  = "bottomright",
          pal       = palet_indeks[[acuan]],
          values    = rentang_indeks[[acuan]],
          title     = meta_indeks[[acuan]]$label,
          labFormat = labelFormat(digits = 2),
          opacity   = 0.9
        )
    }

    # Catatan: clearControls() hanya menghapus legenda, sedangkan
    # layers control (basemap/overlay) TIDAK ikut terhapus, jadi
    # pengguna tetap bisa mencentang layer seperti biasa.

  }, ignoreNULL = FALSE)


  # -------------------------------------------------------------------------
  # 7.3 Tabel statistik zonal (DT)
  # -------------------------------------------------------------------------
  output$tabel_zonal <- renderDT({

    # Bulatkan kolom numerik agar rapi; kolom AWAN (0/1) dibiarkan utuh
    kolom_bulat <- sapply(zonal_stats, is.numeric) &
      !grepl("AWAN", names(zonal_stats), ignore.case = TRUE)

    data_tampil <- zonal_stats
    data_tampil[kolom_bulat] <- lapply(data_tampil[kolom_bulat], round, digits = 3)

    datatable(
      data_tampil,
      rownames = FALSE,
      filter = "top",                       # filter per kolom
      extensions = "Buttons",               # ekspor CSV/Excel
      options = list(
        pageLength = 10,                    # baris per halaman
        lengthMenu = c(5, 10, 25, 50),
        scrollX = TRUE,                     # tabel lebar tetap bisa digulir
        order = list(list(0, "asc")),       # urutkan berdasarkan nama kecamatan
        dom = "lBfrtip",
        buttons = list(
          list(extend = "csv", text = "CSV"),
          list(extend = "excel", text = "Excel")
        )
      )
    )
  })
}


# =============================================================================
# 8. JALANKAN APLIKASI
# =============================================================================

shinyApp(ui = ui, server = server)
