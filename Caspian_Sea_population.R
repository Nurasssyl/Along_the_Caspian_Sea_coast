library(sf)
library(tidyverse)
library(ggplot2)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)
library(glue)

map <- "cas"

# -------------- Геометрия Каспия через coastline + bbox --------------
coastline <- st_read("C:/Users/Admin/Рабочий стол/kaspi_bath/Caspian_Sea_boundaries/ne_10m_coastline.shp")

caspian_bbox <- st_as_sfc(st_bbox(c(
  xmin = 46.5, xmax = 55.5,
  ymin = 36.5, ymax = 47.2
), crs = 4326))

Caspian_Sea_geom <- coastline |>
  st_crop(caspian_bbox) |>
  st_transform(crs = 3857)  # будем подгонять под population data позже

# Проверка
plot(Caspian_Sea_geom)

# -------------- Страны по населению --------------
country_codes <- c("AZ", "IR", "KZ", "RU", "TM")

gz_files <- list.files("C:/Users/Admin/Рабочий стол/kaspi_bath", pattern = "\\.gpkg\\.gz$", full.names = TRUE)
lapply(gz_files, function(file) {
  out <- sub("\\.gz$", "", file)
  R.utils::gunzip(file, destname = out, overwrite = TRUE)
})

data <- map_df(country_codes, function(i) {
  st_read(glue("C:/Users/Admin/Рабочий стол/kaspi_bath/kontur_population_{i}_20220630.gpkg"))
})

# -------------- Буфер вокруг Каспия --------------
casp <- Caspian_Sea_geom |>
  st_union() |>
  st_transform(crs = st_crs(data)) |>
  st_buffer(25000)

# -------------- Фильтрация данных по пересечению --------------
mask <- lengths(st_intersects(data, casp)) > 0
st_d <- data[mask, ]

# -------------- Пропорции карты --------------
bb <- st_bbox(st_d)
crs_val <- st_crs(st_d)

pt_bl <- st_sfc(st_point(c(bb[["xmin"]], bb[["ymin"]])), crs = crs_val)
pt_tl <- st_sfc(st_point(c(bb[["xmin"]], bb[["ymax"]])), crs = crs_val)
pt_br <- st_sfc(st_point(c(bb[["xmax"]], bb[["ymin"]])), crs = crs_val)

yind <- as.numeric(st_distance(pt_bl, pt_tl))
xind <- as.numeric(st_distance(pt_bl, pt_br))

if (yind > xind) {
  y_rat <- 1
  x_rat <- xind / yind
} else {
  x_rat <- 1
  y_rat <- yind / xind
}

# -------------- Растр и визуализация --------------
size <- 6000

casp_rast <- st_rasterize(
  st_d |> select(population, geom),
  nx = floor(size * x_rat),
  ny = floor(size * y_rat)
)

mat <- matrix(casp_rast$population, nrow = floor(size * x_rat), ncol = floor(size * y_rat))

color <- MetBrewer::met.brewer(name = "Troy")
tx <- grDevices::colorRampPalette(color, bias = 2)(256)



mat |>
  height_shade(texture = tx) |>
  plot_3d(heightmap = mat,
          zscale = 15,
          solid = FALSE,
          windowsize = c(200, 200),
          shadowdepth = 0)

render_camera(theta = -8, phi = 50, zoom = .82)

testfile <- "caspian_test_plot.png"
outfile <- "caspian_final_plot.png"

render_highquality(
  filename = testfile,
  interactive = FALSE,
  lightdirection = 75,
  lightaltitude = c(20, 80),
  lightcolor = c(color[4], "white"),
  lightintensity = c(900, 200),
  height = 500,
  width = 500
)

# -------------- Финальный рендер --------------
{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality(
    filename = "caspian_preview.png",
    interactive = FALSE,
    lightdirection = 75,
    lightaltitude = c(20, 80),
    lightcolor = c(color[4], "white"),
    lightintensity = c(900, 200),
    samples = 100,
    height = 1500,
    width = 1500
  )
  
  
  end_time <- Sys.time()
  cat(crayon::cyan(end_time - start_time), "\n")
}
