### testing the speed of projectRaster vs gdalwarp in gdalUtils

# set the working directory for the test data.

setwd("/home/hfg/Documents/hfg_soft/misc_work_scripts/test_data/speed_testing")

load("megachile_willughbiella_data.RData")
pnts <- dat$uk_data

ref <- raster::raster("leep_grid_raster_reference.grd")
dmap <- raster::stack("GFDL-ESM2M_1979-2013_baseline.nc")
# raster::writeRaster(dmap, file = "GFDL-ESM2M_1979-2013_baseline.grd", 
#   format = "raster")
dmap_ras <- raster::stack("GFDL-ESM2M_1979-2013_baseline.grd")

# first test how to use it.
x_rs <- raster::projectRaster(dmap, crs = sp::proj4string(ref))

gdalUtils::gdalwarp(
  srcfile = "GFDL-ESM2M_1979-2013_baseline.grd",
  dstfile = "reprojected.grd",
  s_srs = sp::proj4string(dmap_ras),
  t_srs = sp::CRS('+init=epsg:3035'),
  overwrite = TRUE
)
x_gd <- raster::stack("reprojected.grd")

par(mfrow = c(1, 2))
raster::plot(x_rs[[1]]);points(pnts)
raster::plot(x_gd[[1]]);points(pnts)

# check that the data match (i.e. the points plot...)

# test raster reproject.
ras <- system.time(lapply(1:10, function(x) {
  raster::projectRaster(dmap, crs = sp::proj4string(ref))
}))
