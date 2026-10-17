# ====================================
# 1. PENGATURAN PROJ
# ====================================
Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("PROJ_DATA")
Sys.unsetenv("GDAL_DATA")

# ====================================
# 2. MEMANGGIL LIBRARY
# ====================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(sf)
library(terra)
library(dplyr)
library(DT)

# ====================================
# 3. MENENTUKAN FOLDER
# ====================================

setwd("C:/SEMESTER 5/PEMSPA/TUBES ETS")

folder_citra <- "C:/SEMESTER 5/PEMSPA/TUBES ETS/Data"

# ====================================
# 4. MENCARI FILE CITRA
# ====================================

files <- list.files(
  folder_citra,
  pattern = "\\.jp2$",
  recursive = TRUE,
  full.names = TRUE
)

print(files)

# ====================================
# 5. MEMBACA BAND SENTINEL-2
# ====================================

ambil_band <- function(kode) {
  
  hasil <- grep(
    kode,
    files,
    value = TRUE
  )
  
  if (length(hasil) != 1) {
    stop(paste("Periksa file band:", kode))
  }
  
  rast(hasil)
}

B03 <- ambil_band("B03_10m")
B04 <- ambil_band("B04_10m")
B08 <- ambil_band("B08_10m")
B11 <- ambil_band("B11_20m")

# ====================================
# 6. MENAMPILKAN CITRA
# ====================================

plot(B04, main = "Sentinel-2 Band 4")

# Memeriksa sistem koordinat
crs(B04)

# ====================================
# 7. MEMBACA BATAS ADMINISTRASI
# ====================================

madiun <- st_read(
  "C:/SEMESTER 5/PEMSPA/TUBES ETS/Data/batas madiun"
)

# Memilih Kabupaten dan Kota Madiun
madiun <- madiun %>%
  filter(as.character(kode_kk) %in%
           c("35.19", "35.77"))

# Memperbaiki geometri
madiun <- st_make_valid(madiun)

# Memeriksa wilayah
print(table(madiun$kode_kk))

# Menampilkan batas wilayah
plot(
  st_geometry(madiun),
  main = "Kabupaten dan Kota Madiun"
)
#========================
# 8. MENYAMAKAN RESOLUSI
# ====================================

B03_new <- resample(B03, B11, method = "bilinear")
B04_new <- resample(B04, B11, method = "bilinear")
B08_new <- resample(B08, B11, method = "bilinear")

# Memeriksa kesamaan raster
print(compareGeom(B03_new, B11))
print(compareGeom(B04_new, B11))
print(compareGeom(B08_new, B11))

# ====================================
# 9. MENGGABUNGKAN BAND
# ====================================

citra <- c(
  B03_new,
  B04_new,
  B08_new,
  B11
)

names(citra) <- c(
  "B03", "B04", "B08", "B11"
)

print(citra)

# ====================================
# 10. MENYAMAKAN KOORDINAT BATAS
# ====================================

# Menetapkan CRS asli citra Sentinel-2
terra::crs(citra) <- "EPSG:32749"

# Memeriksa CRS citra
print(terra::crs(citra))

# Mengubah batas administrasi menjadi SpatVector
batas <- terra::vect(madiun)

# Menyamakan CRS batas dengan citra
batas <- terra::project(
  batas,
  terra::crs(citra)
)

# Memeriksa CRS batas
print(terra::crs(batas))

# Menampilkan citra
plot(
  citra$B04,
  main = "Citra Sentinel-2 Kabupaten Madiun"
)

# Menampilkan batas Kabupaten Madiun
plot(
  batas,
  add = TRUE,
  border = "red",
  col = NA
)

# ====================================
# 11. CROP DAN MASK
# ====================================

# Memotong citra menggunakan batas
citra_crop <- crop(
  citra,
  batas
)

# Menghilangkan area di luar batas
citra_mask <- mask(
  citra_crop,
  batas
)

# Menampilkan hasil crop
plot(
  citra_mask$B04,
  main = "Hasil Crop Kabupaten madiun"
)

# ====================================
# 12. MENYIMPAN HASIL
# ====================================

# Membuat folder output
dir.create(
  "output",
  showWarnings = FALSE
)

# Menyimpan raster hasil crop
writeRaster(
  citra_mask,
  "output/citra_madiun.tif",
  overwrite = TRUE
)

# ====================================
# 13. MEMERIKSA HASIL
# ====================================

print(citra_mask)

plot(citra_mask)

# ====================================
# 14. MENGHITUNG NDVI
# ====================================

NDVI <- (citra_mask$B08 - citra_mask$B04) /
  (citra_mask$B08 + citra_mask$B04)

# Memberikan nama raster
names(NDVI) <- "NDVI"

# Menampilkan hasil
plot(
  NDVI,
  col = hcl.colors(100, "YlGn"),
  main = "Peta NDVI Kabupaten madiun"
)

# Menyimpan hasil
writeRaster(
  NDVI,
  "output/NDVI_madiun.tif",
  overwrite = TRUE
)

# ====================================
# 15. MENGHITUNG NDBI
# ====================================

NDBI <- (citra_mask$B11 - citra_mask$B08) /
  (citra_mask$B11 + citra_mask$B08)

names(NDBI) <- "NDBI"

plot(
  NDBI,
  col = hcl.colors(100, "YlOrRd"),
  main = "Peta NDBI Kabupaten madiun"
)

writeRaster(
  NDBI,
  "output/NDBI_madiun.tif",
  overwrite = TRUE
)

# ====================================
# 16. MENGHITUNG NDWI
# ====================================

NDWI <- (citra_mask$B03 - citra_mask$B08) /
  (citra_mask$B03 + citra_mask$B08)

names(NDWI) <- "NDWI"

plot(
  NDWI,
  col = hcl.colors(100, "Blues 3"),
  main = "Peta NDWI Kabupaten madiun"
)

writeRaster(
  NDWI,
  "output/NDWI_madiun.tif",
  overwrite = TRUE
)

# ====================================
# 17. MENGHITUNG NDCI
# ====================================

# ====================================

# Mengambil band Red dan NIR
Red <- citra_mask$B04
NIR <- citra_mask$B08

# Menghitung indeks ternormalisasi
NDCI <- terra::ifel(
  (NIR + Red) == 0,
  NA,
  (Red-NIR) / (Red+NIR)
)

# Memberikan nama raster
names(NDCI) <- "NDCI"

# Menampilkan hasil
plot(
  NDCI,
  col = colorRampPalette(
    c("steelblue4", "lightblue", "white", "gray70")
  )(100),
  main = "Indeks Red-NIR Kabupaten Madiun"
)

# Menyimpan hasil
writeRaster(
  NDCI,
  "output/NDCI_Cloud_Madiun.tif",
  overwrite = TRUE
)
# ====================================
# 18. SIMPAN 4 PETA
# ====================================

png(
  "output/4_Peta_Madiun.png",
  width = 2400,
  height = 1800,
  res = 200
)

par(mfrow = c(2, 2))

plot(NDVI,
     col = hcl.colors(100, "YlGn"),
     main = "NDVI - Vegetasi")

plot(NDBI,
     col = hcl.colors(100, "YlOrRd"),
     main = "NDBI - Terbangun")

plot(NDWI,
     col = hcl.colors(100, "Blues 3"),
     main = "NDWI - Perairan")

plot(
  NDCI,
  col = colorRampPalette(
    c("steelblue4", "lightblue", "white", "gray70")
  )(100),
  main = "NDCI Cloud Index Kabupaten Madiun"
)

dev.off()
par(mfrow = c(1, 1))

# ====================================
# 19. MENAMPILKAN 4 PETA
# ====================================

# Mengatur tampilan 2 baris dan 2 kolom
par(mfrow = c(2, 2))

# Peta NDVI
plot(
  NDVI,
  col = hcl.colors(100, "YlGn"),
  main = "NDVI - Vegetasi"
)

# Peta NDBI
plot(
  NDBI,
  col = hcl.colors(100, "YlOrRd"),
  main = "NDBI - Bangunan"
)

# Peta NDWI
plot(
  NDWI,
  col = hcl.colors(100, "Blues 3"),
  main = "NDWI - Perairan"
)

# Peta 4: NDCI - Awan
plot(
  NDCI,
  col = colorRampPalette(
    c("steelblue4", "lightblue", "white", "gray70")
  )(100),
  main = "NDCI - Cloud Index"
)

# Mengembalikan tampilan normal
par(mfrow = c(1, 1))

# ====================================
# 20. MEMBACA BATAS KECAMATAN MADIUN
# ====================================

kecamatan <- st_read(
  "C:/SEMESTER 5/PEMSPA/TUBES ETS/Data/batas madiun"
)

# Memilih Kabupaten dan Kota Madiun
kecamatan <- kecamatan %>%
  filter(as.character(kode_kk) %in%
           c("35.19", "35.77"))

# Menghapus geometri kosong
kecamatan <- kecamatan[
  !st_is_empty(kecamatan), ,
  drop = FALSE
]

# Memperbaiki geometri
kecamatan <- st_make_valid(kecamatan)

# ====================================
# 21. MENGGABUNGKAN POLIGON KECAMATAN
# ====================================

kecamatan <- kecamatan %>%
  group_by(kode_kk, kecamatan) %>%
  summarise(.groups = "drop")

# Memeriksa nama kecamatan
print(kecamatan$kecamatan)

# Memeriksa jumlah kecamatan
print(nrow(kecamatan))

# Memeriksa jumlah berdasarkan wilayah
print(table(kecamatan$kode_kk))
# ====================================
# 22. MENYAMAKAN CRS
# ====================================

kecamatan <- st_transform(
  kecamatan,
  crs(NDVI)
)

kec <- terra::vect(kecamatan)

plot(
  kec,
  main = "Batas Kecamatan Kabupaten Madiun"
)
# ====================================
# 23. MENYIAPKAN RASTER ZONAL STATISTICS
# ====================================

# Memastikan NDCI memiliki grid yang sama dengan NDVI
if (!same.crs(NDCI, NDVI)) {
  NDCI_fix <- project(
    NDCI,
    NDVI,
    method = "bilinear"
  )
} else {
  NDCI_fix <- resample(
    NDCI,
    NDVI,
    method = "bilinear"
  )
}

# Menggabungkan empat indeks
indeks_zonal <- c(
  NDVI,
  NDBI,
  NDWI,
  NDCI_fix
)

# Memberikan nama setiap indeks
names(indeks_zonal) <- c(
  "NDVI",
  "NDBI",
  "NDWI",
  "NDCI"
)

# Memeriksa hasil
print(indeks_zonal)

# Menampilkan keempat indeks
plot(indeks_zonal)
# ====================================
# 24. MENGHITUNG ZONAL STATISTICS
# ====================================

zonal <- data.frame(
  Kecamatan = kec$kecamatan
)

statistik <- list(
  Mean = mean,
  Minimum = min,
  Maximum = max,
  Median = median,
  SD = sd
)

for (nama in names(indeks_zonal)) {
  
  for (stat in names(statistik)) {
    
    hasil <- terra::extract(
      indeks_zonal[[nama]],
      kec,
      fun = statistik[[stat]],
      na.rm = TRUE
    )
    
    zonal[[paste(nama, stat, sep = "_")]] <- hasil[[2]]
  }
}

# Menampilkan hasil
View(zonal)
# ====================================
# 25. MENYIMPAN ZONAL STATISTICS
# ====================================

# Membuat folder output
dir.create("output", showWarnings = FALSE)

# Menyimpan hasil ke CSV
write.csv(
  zonal,
  "output/Zonal_Statistics_Madiun.csv",
  row.names = FALSE
)

