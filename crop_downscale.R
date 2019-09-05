crop_downscale <- function(anomaly, current, extent) {
  x <- strsplit(anomaly, "\\.")
  x <- strsplit(x[[1]][1], "_")[[1]]
  filename <- paste0(x[1], "_", x[2], "_", x[4], "_hires.grd")
  filename <- file.path(filename)
  aa <- raster::stack(file.path(anomaly))
  aa <- raster::crop(aa, extent)
  cc  <- raster::stack(current)
  output_names <- c()
  for (i in seq_along(names(aa))) {
    an <- strsplit(anomaly, "\\.")[[1]][1]
    tmp_file <- paste0(".tmp_file_bio_", 
      stringr::str_pad(i, 2, pad = "0"), "_", an, ".grd")
    output_names[i] <- tmp_file
    a_h <- raster::resample(aa[[i]], cc[[i]], method = "bilinear")
    a_h <- a_h + cc[[i]]
    raster::writeRaster(a_h, file = tmp_file, format = "raster",
      overwrite = TRUE)
    print(tmp_file)
    rm(a_h)
    gc()
  }
  print(list_files())
  a_hires <- raster::stack(output_names)
  print(a_hires)
  raster::writeRaster(a_hires, file = filename, format = "raster")
  print(filename)
  output_names <- c(output_names,
    paste0(unlist(strsplit(output_names, ".grd")), ".gri"))
  file.remove(output_names)
}
