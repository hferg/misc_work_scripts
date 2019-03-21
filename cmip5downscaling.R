#### ISIMIP data.
# the ISIMIP data is bias-corrected, and should provide across-model averages
# directly, meaning I can skip the step of averaging across models.
# data are available here:
#  https://esg.pik-potsdam.de/search/isimip/
# openID is https://esgf-data.dkrz.de/esgf-idp/openid/henryfgow
# username is henryfgow

## Series of functions to downscale CMIP5 data to the UK OS grid used in the 
# ADVENT/NEVO work. This has two functions: 1) to put the data onto the correct
# grid and 2) to allow us to generate decadal averages going into the future
# over the same time periods that NEVO is intersted in.

# WAIT! This general algorithm is incorrect (or at least, it's incorrect
# compared to what vivienne has been doing).

# Vivenne algorithm.
#   1) Calculate climatogolies of all bioclim variables and GCMS in the original
#     low-resolution for the same period as the high-resolution climatologies
#     from CHELSA (1979-2005) from the 2006-2014 rcp85 historical experiments
#    2) calculate annual time-series (2006-2099) of bioclimatic variables for
#     future scenarios (whatever rcp, basically) in low-resolution (i.e., from
#     the GCMs).
#    3) calculate GCM climate anomalies relative to the GCM current (historical)
#      climatology (effectively future - present, per cell).
#    4) Add the low-resolution anomalies to the high resolution CHELSA data for
#     current climates.
#    5) bilinear interpolation used to reduce discontinuities at the gcm grid
#     boundaries.

# this is done for each gcm, and then the total output averaged.


# henry verbal version.

# Start with a GCM.

# Establish the baseline in low-res. This means taking the historical climate 
# from the same models for the years that match the years that CHELSA is taken
# from - 1979-2013. The PROBLEM here is that the historical experiments from
# CMIP5 are up to 2005. To rectify this I can take the future rcp85 predictions
# from 2005 to 2013 to use. At this point the downscaling ought to fork - one
# version that calculates anomalies from a 1979-2005 baseline (i.e., shorter
# time frame than CHELSA) and one that includes the first few years of the
# predictions. There will be a baseline for each of tmax, tmin, tas and pr
# Do this for each of the bands in each of the gcm outputs. these should be
# days I think.

# the GCM-now then needs to become bioclim - so calculate into bioclim
# variables.

# repeat this for each of the GCM modlels and take a mean - this then gives the 
# mean "present" gcm output for bioclim.

# then repeat for the future - average across decades. So:
#   1) calculate annual bioclim for each year in a decade per model
#     1a) the first thing is to turn the daily data into monthly means.
#   2) average across models for each timestep (year).
#   3) average across the ensemble years to generate the decadal data.
#   4) use the ensemble mean to get the anomaly from the gcm-now ensemble
#   5) add the anomaly to the chelsa data.

# NOTE ON AVERAGING
#   I want to average each model at each timestep, and then average across time.
#   So in short, make an ensemble for each timestep, then an average across 
#     those.
#  AVERAGING ALGORITHM
#   1) WITHIN a GCM calculate the monthly averages.
#   2) Use this to calculate ANNUAL bioclim for THAT GCM.
#   3) Average annual bioclim ACROSS GCMS to produce an ensemble model for each
#     year.
#   3) Generate decadal predictions from the annual ensemble.
#   4) Use the decadal ensemble average to calculate anomaly from baseline.
#   5) Add this anomaly back to the high-res data to create a decadal high-res
#     bioclim prediction.

# DO THIS WITH VELUX!!!

# files required to test.
# Low res historical - 4 GCMs
# Low res future for one variable - 4 GCMS
# High-res current climatology - CHELSA

# required info for the function.
#   gcm name
#   variable name
#   


# NOTES FROM OLLIE
#   if they have precip I highly recommend using ISIMIP2b
#   https://www.isimip.org/gettingstarted/availability-input-data-isimip2b/
#   "it's already regridded and downscaled
#   like they adjust the temperature to be more realistic
#   by comparing it against observations
#   and making adjustments"
#   I'd suggest getting an ensemble average of daily data 
#   and then doing the monmean


# first just test this for a single model and single input type.
# there could be multiple input types, due to different models. that means i
# i need to check them and make the function work accordingly. to get the
# general pipeline working, though, just work on one of them.


########
##  UNITS
# ISIMIP
#   pr  kg m-2 s-1 
#   t K
# CHELSA
#   pr mm for annual, mm per month for precip of wettest month, mm per quarter
#     for quarterly.
#   pr converts to daily percip by taking kg/m2s and 24 * 60 * 60
#   t C * 10 (some confusion here - the unit is tenths of C, written as 
#     C/10 but calculated by multiplying by 10. SO convert K to C, then compare
#     the result.)
#   To convert, use K - 273.15.


########
# FAILED FILES.
# pr_day_MIROC5_rcp26_r1i1p1_EWEMBI_landonly_22110101-22201231.nc
# tas_day_GFDL-ESM2M_rcp85_r1i1p1_EWEMBI_landonly_20510101-20601231.nc

setwd("/home/hfg/Documents/projects/advent/isimip_data/pr")

library(raster)

# Initial approach.
# Make the annual bioclim layers for each model and year, and then make them
# hi-res from the CHELSA climatology. Don't do any averaging at this stage, 
# but retain all steps - the averaging can be done later if needs be, but the
# per-model bioclims will be useful for comparing the SDMs fitted over each 
# model.

models <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")
time_periods <- c("historical", "rcp26", "rcp60", "rcp85")

return_files <- function(model, time_period) {
  files <- list.files()
  returns <- files[grepl(model, files)]
  if (time_period != "all") {
    returns <- returns[grepl(time_period, returns)]
  }
  return(returns)
}

#######
#######
#   Establishing a baseline.

# This function needs to take a historical file, and trim it down to only the
# years that are important for the baseline.
# This function should produce two outputs - one global and one cropped to my
# study area - that will save a lot of time downstream.

#' cdo_momeans
#' Takes a series of files and uses cdos to compute the montly means for those
#' files.
#' Naming convention will be the variable, the model name, and the yearly range
#' followed by whatever is specified in outname
#' @param files A vector of GCM output filenames.
#' @param outpath The path for the output (NULL of the same directory as input
#' is desired)

cdo_momeans <- function(files, outpath) {
  dir.create(outpath, showWarnings = FALSE)
  info <- strsplit(files, "_")
  # parallelise here...
  cl <- parallel::makeForkCluster(parallel::detectCores() - 4)

  parallel::parLapply(cl, 1:length(files), function(x) {
    inf <- info[[x]]
    years <- strsplit(strsplit(inf[8], "\\.")[[1]], "-")[[1]]
    years <- paste(substr(years, start = 1, stop = 4), collapse = "-")

    filename <- paste(
      c(inf[1], inf[3], inf[4], "monmeans", years), 
      collapse = "_")
    outfile <- paste0(outpath, "/", filename, ".nc")
    system(paste("cdo monmean", files[x], outfile))
    }
  )
  parallel::stopCluster(cl)
}

# establishing the baseline.
# 1) generate annual bioclims for each year in the historical time period.
# 2) turn these into a mean for each of the GCMs - this is then the baseline
#   from which future anomalies can be calculated.

# generate annual bioclims.
# Axes of variation:
#   Variable
#   Model
#   Time period
#   Scenario (historic vs rcp scenarios)

# QUESTION. Once I have the monthly means, can I just use biovars in dismo
# to generate the bioclim data?

# biovars takes 12 layers - the months of the year. So that means I can load
# the three monthly mean files (pr, tmin, tmax) and then split them into
# annual stacks (12 months each) and then generate from biovars?

makeBioclim <- function(model, scenario, pr_path, tmin_path, tmax_path,
  outpath) {
  dir.create(outpath, showWarnings = FALSE)
  pr_files <- list.files(pr_path)
    pr_files <- pr_files[grep(model, pr_files)]
    pr_files <- pr_files[grep(scenario, pr_files)]
    pr_files <- paste0(pr_path, "/", pr_files)

  tmin_files <- list.files(tmin_path)
    tmin_files <- tmin_files[grep(model, tmin_files)] 
    tmin_files <- tmin_files[grep(scenario, tmin_files)]  
    tmin_files <- paste0(tmin_path, "/", tmin_files)

  tmax_files <- list.files(tmax_path)
    tmax_files <- tmax_files[grep(model, tmax_files)]
    tmax_files <- tmax_files[grep(scenario, tmax_files)]
    tmax_files <- paste0(tmax_path, "/", tmax_files)
  
  single_file <- function(pr_file, tmin_file, tmax_file,
    model, scenario, outpath) {
    print("reading pr")
    pr_r <- raster::stack(pr_file)
    print("reading tmin")
    tmin_r <- raster::stack(tmin_file)
    print("reading tmax")
    tmax_r <- raster::stack(tmax_file)
    print("calculating indices")
    ind <- seq(1, length(names(pr_r)), 12)
    print(paste0(model, " ", scenario))
    # make an raster stack object to assign bioclims to.
    bc_temp <- vector(mode = "list", length = 19)
    for (i in seq_along(bc_temp)) {
      bc_temp[[i]] <- pr_r[[1]][[1]]
    }
    bc_temp <- stack(bc_temp)

    # calculate bioclim - this is pretty slow to be honest! Can we velox this?
    print("setting up cluster")
    cl <- parallel::makeForkCluster(parallel::detectCores() - 4, 
      outfile = "parallel_output.out")
      print("calculating bioclim")
      bcx <- parallel::parLapply(cl, seq_along(ind), function(x) {
        .ind <- c(ind[x]:(ind[x] + 11))
        n <- names(pr_r)[x]
        n <- strsplit(n, "\\.")[[1]][1]
        yr <- strsplit(n, "X")[[1]][2]
        
        # This is faster and safer - just need to put the output back into
        # raster format.
        pr <- as.matrix(pr_r[[.ind]])
        tmn <- as.matrix(tmin_r[[.ind]])
        tmx <- as.matrix(tmax_r[[.ind]])
        print(paste0("x = ", x))
        bc <- dismo::biovars(
          prec = pr,
          tmin = tmn,
          tmax = tmx
        )
        print("setting values")
        bc <- raster::setValues(bc_temp, bc)
        print("writing")
        filename <- paste0(
          outpath, "/bioclim_", model, "_", scenario, "_", yr, ".nc")
        raster::writeRaster(bc, file = filename, format = "CDF", 
          overwrite = TRUE)
        rm(bc)
      })
    print("stopping cluster")
    parallel::stopCluster(cl)
    # print("getting years")
    # years <- sapply(ind, function(x) {
    #   n <- names(pr_r)[x]
    #   n <- strsplit(n, "\\.")[[1]][1]
    #   strsplit(n, "X")[[1]][2]
    # })
    # print("writing")
    # for (i in seq_along(bc)) {
    #   filename <- paste0(
    #     outpath, "/bioclim_", model, "_", scenario, "_", years[i], ".nc")
    #   raster::writeRaster(bc[[i]], file = filename, format = "CDF", 
    #     overwrite = TRUE)
    # }
    gc()
  }

  x <- lapply(1:length(pr_files), function(x){
    single_file(pr_files[x], tmin_files[x], tmax_files[x],
      model, scenario, outpath)
  })
}

downscale_bioclim <- function(reference, baseline, future) {
  # calculate anomaly

  # run single downscaling for that anomaly (seperate function?)

  # save output

}

downscale_single <- function() {
  # this is the basic line for the downscaling.
  hires <- resample(lowres_anomaly, hires_current, method="bilinear")
}

bioclim_averages <- function(models, window_size, scenario) {

}

# generate the bioclims across the range of models and scnearios.
setwd("/home/hfg/rds_mount/advent-rds/henry/advent/climate_data/isimip_data")
models <- c("HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5", "GFDL-ESM2M")
scenarios <- c("historical", "rcp26", "rcp60", "rcp85")
combos <- expand.grid(models, scenarios)

# take the model and scenario and expand into grid to loop across - generate 
# annual bioclims per scenario, model, year combination.

for (i in 1:nrow(combos)) {
  makeBioclim(
    model = as.character(combos$Var1[i]),
    scenario = as.character(combos$Var2[i]),
    pr_path = "./pr/pr_momean",
    tmin_path = "./tasmin/tasmin_momean",
    tmax_path = "./tasmax/tasmax_momean",
    outpath = "bioclim_annual"
  )
}



# NOTE: I am pretty sure that I will need to generate annual bioclims for ALL 
# years regardless of the situation. Then it is a matter of combining historical
# ones and using the future ones for other things. Thus, this function might as
# well generate annual bioclims from all files.
# The upshot of this is that it can be flexible to file type. 
# Main input:
#   Model
#   Scenario
# METHOD:
#   Go into the folders and find all files with the model and scenario combo.
#   From these files, find all the date ranges and order.
#   Take first date range, and load the four variables for that specific model,
#     scenario and date range.
#   Identify key quarters.
#     Wettest quarter
#     Driest quarter
#     Hottest quarter
#     Coolest quarter
#     

# TODO Understand the quarterly calculations in the bioclim workflow.

#' get_historical_years
#' This function takes a set of files from a single GCM and returns the mean
#' climatology across all of those files for the specified time period.

get_historical_years <- function(files, minyear = 1970, maxyear = 2013) {
  x <- raster::stack(file)
  nms <- names(x)

  # IMPORTANT:
  # This needs to be done over the daily data, which means that... it means
  # that the

  # the names of each layer in the stack are the dates (year, month, day).
  # check that they are all within the year range.

  # If they are not, remove any bands/layers that are not included in the date
  # range.

  # Save out the file with the right date ranges, and include the name.

  # load in the saved files, and make into a historical baseline mean.

  # question: so I want to save the files to disk intermediately? Or do 
  # something else like build up a massive raster stack and then mean on that?
  # what is best for memory management here?


}

# this function makes monthly means of a file. cdo takes ~26 seconds to do this
# globally for a single file, so that's the thing to try to get near to. If it's
# close we can perhaps to the whole thing in R. 17 seconds seems right.
# the file that generate the 26 second time is this one:
# pr_day_HadGEM2-ES_historical_r1i1p1_EWEMBI_landonly_20010101-20051231.nc
make_monthly_means <- function(file) {
  x <- raster::stack(file)
}


################################################################################
#                                                                              #
#                               PIPELINE                                       #
#                                                                              #
################################################################################

# first process monthly means for all files.

cdo_momean(list.files(), outpath = "pr_momeans", )


# first build current climatology. refresher for steps.
# 1 load the data
# 2 generate per-year bioclims
# 3 take the first few years of the futures at rcp85 for the same model.
# 4 combine output of 2 and 3
# 5 save this for the model and the year and the bioclim variable
# 6 repeat 1-5 for models


# this is daily precipitation for 2005-2015.
cmip_f <- raster("pr_day_HadGEM2-ES_rcp26_r1i1p1_20051201-20151130.nc")








# General algorithm (verbal).
# 1) Take the CMIP5 layer, which will be on a larger grid than the UK data.
#   1a) In order to be able to compare to the CHELSA climatology for now the
#     cmip5 data needs to be converted to monthly maps, and then the temperature
#     needs to split into three new maps - mean for the month, max for the month
#     and min for the month.
# 2) Get the European extent of the area of interest, and crop the CMIP5 layer
#   to that same extent.
# 3) Make sure the grid lines are matched over the two extents, and then 
#   generate an intermediate layer for the CURRENT europe climate (taken from
#   CHELSA) where the cells are aggregated to be on the same resolution and grid
#   as the CMIP5 data.
# 4) Ensure that each of the cells on the standard Europe grid are associated
#   with one of the larger cells on the intermediate layer (n -> 1 relationship)
# 5) For each of the cells on the intermediate grid calculate the mean of the 
#   smaller cells that are within it. This gives the intermediate layer one
#   value per larger cell, comprised of the smaller cells that make it up.
# 6) Take that intermediate grid, and then calculate the anomlay between the 
#   current cliamte (intermediate cells) and the CMIP5 future (the CMIP5 layer)
# 7) For each of the larger cells in the intermediate layer, add the anomaly 
#   value to each of the smaller cells that it is compriesd of in the current
#   european layer. This gives the downscaled future.
# 8) Make an ensemble from the range of models used (i.e., the selection of 
#   climate models) and then generate bioclim for those decadal averages from
#   that.

# Missing/situational steps, and outstanding questions.
#   1) When the CMIP5 data is daily or monthly it needs to become annual. This 
#     can be done by averaging across the time period, either 12 bands if 
#     monthly, or 365 bands if daily. The files SHOULD have month and year 
#     specified, but if not be mindful of the number of days in each month.
#   2) The output needs to be averaged to decadal data. Two options: Either 
#     make the CMIP5 input decadal, OR generate daily/monthly/annual anomaly
#     maps and then make them decadal.
#     NB. Minimum computing time will be first make the CMIP5 data decadal, and 
#       then generating anomalies. i.e., just one anomlay calculation per decade
#       vs. 3650, 120, 10 etc.


# testing directory (where the data is...)
setwd("/home/hfg/Documents/hfg_soft/test_data/cmip5")

library(raster)

cmip <- raster("pr_day_HadGEM2-ES_rcp26_r1i1p1_20051201-20151130.nc")

## Notes.
#   1. CMIP5 data are 0-360 degrees, whereas everything I have been working with
#     is -180 - 180 degrees. The latitudes are the same. To deal with this use
#     the raster::rotate function.
#   2. Of note - the grid doesn't actually have to match the UK grid since this
#     is just envelope generation - it gets transformed during the envelope 
#     analysis. It just has to be the same resolution as the CHELSA data that 
#     I plan to use.