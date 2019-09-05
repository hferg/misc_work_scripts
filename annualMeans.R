annualMeans <- function(files, year, rcp) {
  # files will be a vector of filenames that will be loaded in to take 
  # the means of.
  files <- files[grep(".grd$", files)]
  x <- vector(mode = "list", length = length(files))
  for (i in seq_along(files)) {
    x[[i]] <- raster::stack(files[i])
  }
  n <- length(names(x[[1]]))
  mns <- lapply(1:n, function(z) {
    tmp <- raster::stack(
      x[[1]][[z]],
      x[[2]][[z]],
      x[[3]][[z]],
      x[[4]][[z]]
    )
    mn <- raster::calc(tmp, fun = mean)
    rm(tmp)
    gc()
    return(mn)
  })
  rm(tmp)
  xm <- raster::stack(mns)
  names(xm) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7",
    "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15",
    "bio16", "bio17", "bio18", "bio19")
  rm(mns)
  gc()
  # somehow make the filename from the files that are input.
  filename <- paste0("bioclim_", year, "_", rcp, ".grd")
  raster::writeRaster(xm, file = filename, format = "raster", overwrite = TRUE)
}