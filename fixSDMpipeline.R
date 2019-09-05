### Something odd is going on here. i shouldn't have to reproject the climate
# data at all - it's just to generate a mask before even lookin at the Uk
# level - as long as the points and the data are all in the same projection.

# Try going through the pipline, but leaving out the projection - I THINk the 
# problem is my previous bioclim data was in a funky european projection whereas
# currently it's just latlong like normal.

setwd("/home/hfg/rds_mount/advent-rds/henry/advent/modeling/poll_future_test")

# move through the pipeline.
source("pipeline_v6_testing.R")

# input data setup.
load('andrena_bicolor_data.RData')
now <- raster::stack('chelsa_current_europe.grd')
rcp60_21t30 <- raster::stack('bioclim_2021-2030_rcp60.grd')
rcp60_31t40 <- raster::stack('bioclim_2031-2040_rcp60.grd')
rcp60_41t50 <- raster::stack('bioclim_2041-2050_rcp60.grd')
rcp60_51t60 <- raster::stack('bioclim_2051-2060_rcp60.grd')
rcp85_21t30 <- raster::stack('bioclim_2021-2030_rcp85.grd')
rcp85_31t40 <- raster::stack('bioclim_2031-2040_rcp85.grd')
rcp85_41t50 <- raster::stack('bioclim_2041-2050_rcp85.grd')
rcp85_51t60 <- raster::stack('bioclim_2051-2060_rcp85.grd')
bioclim <- list(now = now,
      rcp60_21t30 = rcp60_21t30,
      rcp60_31t40 = rcp60_31t40,
      rcp60_41t50 = rcp60_41t50,
      rcp60_51t60 = rcp60_51t60,
      rcp85_21t30 = rcp85_21t30,
      rcp85_31t40 = rcp85_31t40,
      rcp85_41t50 = rcp85_41t50,
      rcp85_51t60 = rcp85_51t60
      )
uk_ref <- raster::raster('leep_grid_raster_reference.grd')
uk_landuse <- raster::stack('landuse_leep_raster.grd')

# just test the master function in it's entirity now...
system.time({
masterFunction(
  uk_data = dat$uk_data,
  europe_data = dat$europe_data,
  models = c('bc', 'glm', 'rf'),
  # I think this is the suspect line - change to standard latlon
  prjctn = sp::CRS('+init=epsg:3035'),
  predictors = bioclim,
  buffer_clim = NULL,
  sample_density_clim = 500,
  buffer_lu = 12,
  predictor_names = c('bio3',
                      'bio10',
                      'bio12',
                      'bio13',
                      'bio18'),
  uk_ref = uk_ref,
  landuse_predictors = uk_landuse,
  method = 'majority_pa',
  filename = 'andrena_bicolor',
  threshold = 'tss',
  remove_files = TRUE,
  overwrite = TRUE,
  maps = TRUE,
  threads = 8,
  new_landuse = NULL
)
})

# master function parameter setups.
uk_data = dat$uk_data
europe_data = dat$europe_data
models = c('bc', 'glm', 'rf')
# I think this is the suspect line - change to standard latlon
prjctn = sp::CRS('+init=epsg:3035')
predictors = bioclim
buffer_clim = NULL
sample_density_clim = 500
buffer_lu = 12
predictor_names = c('bio3',
                    'bio10',
                    'bio12',
                    'bio13',
                    'bio18')
uk_ref = uk_ref
landuse_predictors = uk_landuse
method = 'majority_pa'
filename = 'andrena_bicolor'
threshold = 'tss'
remove_files = TRUE
overwrite = TRUE
maps = TRUE
threads = 8
new_landuse = NULL


# europeanEnvelope parameters:
  uk_data = uk_data
  europe_data = europe_data
  models = models
  prjctn = prjctn
  predictors = predictors
  buffer = buffer_clim
  sample_density = sample_density_clim
  predictor_names = predictor_names
  method = method
  filename = paste0(filename, "_envelope")
  threshold = threshold
  remove_files = remove_files
  overwrite = overwrite

  # and within - paRaster parameters.
      mask = mask
      uk_points = uk_data
      europe_data = europe_data
      prjctn = prjctn
      shapefile = shapefile
      absence = TRUE
      sample_density = sample_density
      buffer_size = buffer

  # extract data arguments
      dat = predictors
      pa_raster = pa_raster

      # timings.
      # European envelope
      # making the mask - 201 seconds.
      # rarefy UK data - 9 seconds (but variable)
      # rarefy europe data - 9 seconds
      # paRaster - 5172 seconds. 82 minutes!!!
        # the slow parts are the raster functions - I don't think 
        # this can be sped up in a realistic way.
        # BUT it is REALLY slow for bigger datasets - check that it can't be 
        # sped up, and also make sure that it all has to be done via a raster
        # - all it's really doing is generating absences, right? Must be
        # able to be sped up. Make sure that the number of absences being 
        # generated isn't either a) useless or b) excessive.
      # extractData - 484 seconds - not bad.
      # Model fitting - 
        
  # the fit models step ought to be OK.

  # ensemble building - I THINK this is where the "invalid layer names"
  # is coming from... 
  preds = preds
  evals = evals 
  predictors = predictors
  method = method
  threshold = threshold





  # run landuseModel - fix the buffering here in light of being a latlon
    # projection

    uk_data = uk_data
    uk_ref = uk_ref
    euro_envelope = envelope$envelope
    landuse_predictors = landuse_predictors
    buffer = buffer_lu
    filen = filename


  landuse_model <- landuseModel(
    uk_data = uk_data,
    uk_ref = uk_ref,
    euro_envelope = envelope$envelope,
    landuse_predictors = landuse_predictors,
    buffer = buffer_lu,
    filen = filename
  )




# Notes.
# europeanEnvelope timing:
# size of one model + one prediction: 496Mb.
  # SO 4 models with 8 predictions: 16Gb + 0.5Mb for the envelope.
# timing of one model + one prediction:



# full test...
setwd("/home/hfg/rds_mount/advent-rds/henry/advent/modeling/poll_future_test")

# move through the pipeline.
source("../pipeline_v7.R")

# input data setup.
load('sphecodes_longulus_data.RData')
now <- raster::stack('chelsa_current_europe.grd')
rcp60_21t30 <- raster::stack('bioclim_2021-2030_rcp60.grd')
rcp60_31t40 <- raster::stack('bioclim_2031-2040_rcp60.grd')
rcp60_41t50 <- raster::stack('bioclim_2041-2050_rcp60.grd')
rcp60_51t60 <- raster::stack('bioclim_2051-2060_rcp60.grd')
rcp85_21t30 <- raster::stack('bioclim_2021-2030_rcp85.grd')
rcp85_31t40 <- raster::stack('bioclim_2031-2040_rcp85.grd')
rcp85_41t50 <- raster::stack('bioclim_2041-2050_rcp85.grd')
rcp85_51t60 <- raster::stack('bioclim_2051-2060_rcp85.grd')
bioclim <- list(now = now,
      rcp60_21t30 = rcp60_21t30,
      rcp60_31t40 = rcp60_31t40
      )
uk_ref <- raster::raster('leep_grid_raster_reference.grd')
uk_landuse <- raster::stack('landuse_leep_raster.grd')

# just test the master function in it's entirity now...
system.time({
masterFunction(
  uk_data = dat$uk_data,
  europe_data = dat$europe_data,
  models = c('bc', 'glm'),
  # I think this is the suspect line - change to standard latlon
  prjctn = sp::CRS('+init=epsg:3035'),
  predictors = bioclim,
  buffer_clim = NULL,
  sample_density_clim = 500,
  buffer_lu = 12,
  predictor_names = c('bio3',
                      'bio10',
                      'bio12',
                      'bio13',
                      'bio18'),
  uk_ref = uk_ref,
  landuse_predictors = uk_landuse,
  method = 'majority_pa',
  filename = 'sphecodes_longulus',
  threshold = 'tss',
  remove_files = TRUE,
  overwrite = TRUE,
  maps = TRUE,
  threads = 8,
  new_landuse = NULL
)
})



# and now set up a version to test the interactive legion - to see why legion 
# doesn't work and myriad does.
source("pipeline_v7.R")

# input data setup.
load('../pollinator_futures/andrena_bicolor_data.RData')
now <- raster::stack('../pollinator_futures/chelsa_current_europe.grd')
rcp60_21t30 <- raster::stack('../pollinator_futures/bioclim_2021-2030_rcp60.grd')
rcp60_31t40 <- raster::stack('../pollinator_futures/bioclim_2031-2040_rcp60.grd')
rcp60_41t50 <- raster::stack('../pollinator_futures/bioclim_2041-2050_rcp60.grd')
rcp60_51t60 <- raster::stack('../pollinator_futures/bioclim_2051-2060_rcp60.grd')
rcp85_21t30 <- raster::stack('../pollinator_futures/bioclim_2021-2030_rcp85.grd')
rcp85_31t40 <- raster::stack('../pollinator_futures/bioclim_2031-2040_rcp85.grd')
rcp85_41t50 <- raster::stack('../pollinator_futures/bioclim_2041-2050_rcp85.grd')
rcp85_51t60 <- raster::stack('../pollinator_futures/bioclim_2051-2060_rcp85.grd')
bioclim <- list(now = now,
      rcp60_21t30 = rcp60_21t30,
      rcp60_31t40 = rcp60_31t40,
      rcp60_41t50 = rcp60_41t50,
      rcp60_51t60 = rcp60_51t60,
      rcp85_21t30 = rcp85_21t30,
      rcp85_31t40 = rcp85_31t40,
      rcp85_41t50 = rcp85_41t50,
      rcp85_51t60 = rcp85_51t60
      )
uk_ref <- raster::raster('../pollinator_futures/leep_grid_raster_reference.grd')
uk_landuse <- raster::stack('../pollinator_futures/landuse_leep_raster.grd')

# just test the master function in it's entirity now...

# master function parameter setups.
uk_data = dat$uk_data
europe_data = dat$europe_data
models = c('bc', 'glm', 'rf')
# I think this is the suspect line - change to standard latlon
prjctn = sp::CRS('+init=epsg:3035')
predictors = bioclim
buffer_clim = NULL
sample_density_clim = 500
buffer_lu = 12
predictor_names = c('bio3',
                    'bio10',
                    'bio12',
                    'bio13',
                    'bio18')
uk_ref = uk_ref
landuse_predictors = uk_landuse
method = 'majority_pa'
filename = 'andrena_bicolor'
threshold = 'tss'
remove_files = TRUE
overwrite = TRUE
maps = TRUE
threads = 8
new_landuse = NULL


# europeanEnvelope parameters:
  uk_data = uk_data
  europe_data = europe_data
  models = models
  prjctn = prjctn
  predictors = predictors
  buffer = buffer_clim
  sample_density = sample_density_clim
  predictor_names = predictor_names
  method = method
  filename = paste0(filename, "_envelope")
  threshold = threshold
  remove_files = remove_files
  overwrite = overwrite

  # and within - paRaster parameters.
      mask = mask
      uk_points = uk_data
      europe_data = europe_data
      prjctn = prjctn
      shapefile = shapefile
      absence = TRUE
      sample_density = sample_density
      buffer_size = buffer

  # extract data arguments
      dat = predictors
      pa_raster = pa_raster

# load objects up to here. it is fitbioclimt hat's failing.


      # timings.
      # European envelope
      # making the mask - 201 seconds.
      # rarefy UK data - 9 seconds (but variable)
      # rarefy europe data - 9 seconds
      # paRaster - 5172 seconds. 82 minutes!!!
        # the slow parts are the raster functions - I don't think 
        # this can be sped up in a realistic way.
        # BUT it is REALLY slow for bigger datasets - check that it can't be 
        # sped up, and also make sure that it all has to be done via a raster
        # - all it's really doing is generating absences, right? Must be
        # able to be sped up. Make sure that the number of absences being 
        # generated isn't either a) useless or b) excessive.
      # extractData - 484 seconds - not bad.
      # Model fitting - 
        
  # the fit models step ought to be OK.

  # ensemble building - I THINK this is where the "invalid layer names"
  # is coming from... 
  preds = preds
  evals = evals 
  predictors = predictors
  method = method
  threshold = threshold
  # run landuseModel - fix the buffering here in light of being a latlon
    # projection

    uk_data = uk_data
    uk_ref = uk_ref
    euro_envelope = envelope$envelope
    landuse_predictors = landuse_predictors
    buffer = buffer_lu
    filen = filename


  landuse_model <- landuseModel(
    uk_data = uk_data,
    uk_ref = uk_ref,
    euro_envelope = envelope$envelope,
    landuse_predictors = landuse_predictors,
    buffer = buffer_lu,
    filen = filename
  )

