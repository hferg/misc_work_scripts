#### ISIMIP data.
# the ISIMIP data is bias-corrected, and should provide across-model averages
# directly, meaning I can skip the step of averaging across models.
# data are available here:
#  https://esg.pik-potsdam.de/search/isimip/
# openID is https://esgf-data.dkrz.de/esgf-idp/openid/henryfgow
# username is henryfgow

setwd("/home/hfg/rds_mount/advent-rds/henry/advent/climate_data/isimip_data")

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

# ERROR!
# Here, when I have calculated the monthly means, I have made precipitation into
# the mean daily per month, but bioclim wants the total per month... that means
# that the precipitation files need to be redone! That ALSO means that I need
# to re-download the precipitation data...

cdo_momeans <- function(files, outpath) {
  dir.create(outpath, showWarnings = FALSE)
  info <- strsplit(files, "_")
  # parallelise here...
  cl <- parallel::makeForkCluster(parallel::detectCores() - 4)

  parallel::parLapply(cl, 1:length(files), function(x) {
    inf <- info[[x]]
    years <- strsplit(strsplit(inf[8], "\\.")[[1]], "-")[[1]]
    years <- paste(substr(years, start = 1, stop = 4), collapse = "-")

    if (grepl("pr", inf[1])) {
      
      filename <- paste(
        c(strsplit(inf[[1]], "/")[[1]][2], inf[3], inf[4], "monsum", years), 
        collapse = "_")
      outfile <- paste0(outpath, "/", filename, ".nc")
      command <- "cdo mulc,86400 -monsum"
    } else {
      filename <- paste(
        c(strsplit(inf[[1]], "/")[[1]][2], inf[3], inf[4], "monmeans", years), 
        collapse = "_")
      outfile <- paste0(outpath, "/", filename, ".nc")
      command <- "cdo mulc,10 -addc,-273.15 -monmean"
    }   
    system(paste(command, files[x], outfile))
    }
  )
  parallel::stopCluster(cl)
}

#' make_bioclim
#' Generates annual bioclims from the precipitation, tmin and tmax from gcm
#' outputs. The outputs need to already be in monthly means (see cdo_momeans).
#' The model and scenario arguments are used as lookups for filenames, so
#' the gcm input files need to be named with the model name and scenario name 
#' with underscores to seperate. 
#' e.g. pr_GFDL-ESM2M_historical_monmeans_1861-1870.nc
#' @param model The name of the GCM model to use
#' @param scenario The GCM scenario (e.g. historical, rcp26 etc)
#' @param pr_path The filepath to where the precipitation files are
#' @param tmin_path The filepath to where the tmin files are
#' @param tmax_path The filepath to where the tmax files are
#' @param outpath The filepath to save the outputs.

make_bioclim <- function(model, scenario, pr_path, tmin_path, tmax_path,
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
    bc_temp <- raster::stack(bc_temp)

    # calculate bioclim - this is pretty slow to be honest! Can we velox this?
    print("setting up cluster")
    cl <- parallel::makeForkCluster(parallel::detectCores() - 4, 
      outfile = "parallel_output.out")
      print("calculating bioclim")
      bcx <- parallel::parLapply(cl, seq_along(ind), function(x) {
        .ind <- c(ind[x]:(ind[x] + 11))
        n <- names(pr_r)[.ind[1]]
        n <- strsplit(n, "\\.")[[1]][1]
        yr <- strsplit(n, "X")[[1]][2]
        
        # This is faster and safer - just need to put the output back into
        # raster format.
        pr <- raster::as.matrix(pr_r[[.ind]])
        tmn <- raster::as.matrix(tmin_r[[.ind]])
        tmx <- raster::as.matrix(tmax_r[[.ind]])
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
    gc()
  }

  x <- lapply(1:length(pr_files), function(x){
   t <- single_file(pr_files[x], tmin_files[x], tmax_files[x],
      model, scenario, outpath)
  })
}

#' make_baseline
#' @name make_baseline
#' @param model The name of the GCM model to use the model will be identified
#' by searching for files in the file path, so files must be named in the right
#' way. Can specify multiple models.
#' @param start The starting year for the baseline
#' @param end The ending year for the baseline
#' @param future_scenario If the end date is after the extent of the historical
#' files then we need to take from the future to make up the years - this is the
#' rcp scenario to use for the future. rcp85 is recommended.
#' @param file_path The file path to where the annual bioclims are. Defaults to
#' NULL (i.e., run in the folder where the files are)
#' @param outpath Where to save the baseline output to. Defaults to NULL.

make_baseline <- function(model, start = 1979, end = 2013, future_scenario, 
  file_path = NULL, outpath = NULL) {

  # get the historical files
  files <- list.files(file_path)
  files <- files[grep(model, files)]
  historical <- files[grep("historical", files)]
  years <- sapply(historical, function(x) {
    a <- strsplit(x, "_")[[1]]
    strsplit(a[4], "\\.")[[1]][1]
  })
  historical <- historical[as.numeric(years) >= start]

  # now add extension, if needed.
  if (end > max(as.numeric(years))) {
    ext <- files[grep(future_scenario, files)]
    years <- sapply(ext, function(x) {
      a <- strsplit(x, "_")[[1]]
      strsplit(a[4], "\\.")[[1]][1]
    })
    ext <- ext[as.numeric(years) <= end]
    dat <- c(historical, ext)
  } else {
    dat <- historical
  }

  # now read these in and make a mean.
  st <- lapply(dat, function(x) {
    raster::stack(file.path(file_path, x))
  })
  stx <- Reduce("+", st)
  baseline <- stx / length(dat)
  names(baseline) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7",
    "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", 
    "bio16", "bio17", "bio18", "bio19")

  filename <- file.path(outpath, 
    paste0(model, "_", start, "-", end, "_baseline.nc")
  )
  raster::writeRaster(baseline, file = filename, format = "CDF",
    overwrite = TRUE)
}

#' calculate_anomalies
#' Calculates the anomalies between future climate scenarios and a baseline
#' and saves the output to disk. The function calls cdo so all files need to be
#' netCDF
#' @param model The GCM to process data for
#' @param scenario The scenario to process for (e.g. rcp26, rcp85)
#' @param start The year to start at (needed because the baseline may use some
#' years from the early part of the future simulations)
#' @param baseline_path The file path where the baseline is saved
#' @param future_path The file path where the futures are saved
#' @param outpath Where to save the outputs to.

calculate_anomalies <- function(model, scenario, start, baseline_path, 
  future_path, outpath) {
  # first load the baseline
  base_files <- list.files(file.path(baseline_path))
  base_file <- base_files[grep(model, base_files)]
  # baseline <- raster::stack(
  #   file.path(baseline_path, base_file)
  # )

  # now get the filenames for the model, scenario, years in the future
  future_files <- list.files(file.path(future_path))
  future_files <- future_files[grep(model, future_files)]
  future_files <- future_files[grep(scenario, future_files)]

  # now get the years.
  years <- sapply(future_files, function(x) {
    a <- strsplit(x, "_")[[1]]
    strsplit(a[4], "\\.")[[1]][1]
  })

  future_files <- future_files[as.numeric(years) >= start]

  # now calculate the anomalies.
  cl <- parallel::makeForkCluster(10)
    parallel::parLapply(cl, future_files, function(x) {
      year <- strsplit(x, "_")[[1]]
      year <- strsplit(year[4], "\\.")[[1]][1]
      b_path <- file.path(baseline_path, base_file)
      f_path <- file.path(future_path, x)
      o_path <- file.path(outpath, 
        paste0(
          paste(c(model, scenario, "anomaly", year), collapse = "_"), ".nc")
      )
    system(paste("cdo sub", f_path, b_path, o_path))
    })
  cl <- parallel::stopCluster(cl)
}

downscale_bioclim <- function(reference, baseline, future) {
  # calculate anomaly

  # run single downscaling for that anomaly (seperate function?)

  # save output

}

crop_downscale <- function(anomaly, anomaly_path, current, extent, outpath) {
  # anomaly is the file name anomaly stack.
  # anomaly path is the path to the anomalies
  # baseline path is the path to where the baselines are (to be matched with 
  # the anomaly name).
  # extent is the extent of the european analysis.

    x <- strsplit(anomaly, "\\.")
    x <- strsplit(x[[1]][1], "_")[[1]]

    filename <- paste0(x[1], "_", x[2], "_", x[4], "_hires.grd")
    filename <- file.path(outpath, filename)

  # load the anomaly
  aa <- raster::stack(file.path(anomaly_path, anomaly))
  
  # crop the anomaly to the extent
  aa <- raster::crop(aa, extent)

  # load the current
  cc <- raster::stack(current)
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
    rm(a_h)
    gc()
  }

  # make a filename...
  a_hires <- raster::stack(output_names)
  raster::writeRaster(a_hires, file = filename, format = "raster")
  # remove the temporary files.
  # add .gri files to the vector.
  output_names <- c(output_names,
    paste0(unlist(strsplit(output_names, ".grd")), ".gri"))
  file.remove(output_names)
}

# this function is going to average across each model in a year to make annual
# averages, which will then be used to calculate decadal averages.

hires_path <- "./hires_bioclim_annual"
files <- list.files(hires_path)
models <- unique(sapply(files, function(x) strsplit(x, "_")[[1]][1]))
years <- unique(sapply(files, function(x) strsplit(x, "_")[[1]][3]))
rcps <- c("rcp26", "rcp60", "rcp85")
mode <- "by_year"
outpath <- "hires_annual_means"

annual_averages <- function(hires_path, models, years, rcps, mode = "by_year",
  outpath) {
  files <- list.files(hires_path)
  dir.create(file.path(outpath), showWarnings = FALSE)
  # identify all files of a particular year/model
  if (mode == "by_year") {
    all_denom <- years
  } else if (mode == "by_model") {
    all_denom <- models
  }

  # add RCPS
  combos <- expand.grid(all_denom, c("rcp26", "rcp60", "rcp85"))

  # parallelise the work through these combos... keep an eye on RAM and disk
  # space... 

  one_average <- function(files, denom, rcp) {
    gp <- files[grep(denom, files)]
    gp <- gp[grep(rcp, gp)]
    gp <- gp[grep(".grd$", gp)]
    x <- vector(mode = "list", length = length(gp))
    for (i in seq_along(gp)) {
      x[[i]] <- raster::stack(file.path(hires_path, gp[i]))
    }
    n <- length(names(x[[1]]))
    cl <- parallel::makeCluster(10)
    mns <- parallel::parLapply(cl, 1:n, function(z) {
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
    parallel::stopCluster(cl)

    rm(tmp)
    xm <- raster::stack(mns)
    names(xm) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7",
      "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15",
      "bio16", "bio17", "bio18", "bio19")
    rm(mns)
    gc()
    return(xm)
  }

  # parellisation check - this takes about 2000 seconds so these jobs are going
  # on the cluster... See new script for function, and second script for job
  # creation.
  # the script will need to scp the data over from RDS, then remove it after the
  # analysis finishes. There isn't enough space to store it on Myriad at the
  # moment. If each job takes half an hour in parallel, then give it 24 hours
  # for each job in series. This means we can move to $TMPDIR which means we 
  # can import the data properly without counting against scratch (as we would
  # if working parallel).
  system.time(bioc_2014 <- one_average(files, 
    denom = combos[1,1], rcp = combos[1,2]))



  # write out.

}


# test something...
x <- c(1:10)
xx <- 0
for (i in seq_along(x)) {
  xx <- xx + x[i]
  print(xx / i)
}



# downscale_single <- function(current_path, future_anomaly) {
#   # this is the basic line for the downscaling.
#   # resample the anomalies to the same resolution as the hires
#   hires <- resample(lowres_anomaly, hires_current, method="bilinear")
#   # then simply add...
#   # NOTE there might need to be a recalculation here (i.e. convert to same 
#   # units.)
#   # This is SUPER slow - I can use cdo again here, I think... test.
#   hires_future <- hirescurrent + hires

#   # time test the raster method.
#   current_path <- 

#   future_anomaly <- 

# }

# bioclim_averages <- function(models, window_size, scenario) {

# }

# start-to-finish wrapper function.

# generate the bioclims across the range of models and scnearios.
setwd("/home/hfg/rds_mount/advent-rds/henry/advent/climate_data/isimip_data")
models <- c("HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5", "GFDL-ESM2M")
scenarios <- c("historical", "rcp26", "rcp60", "rcp85")
combos <- expand.grid(models, scenarios)

# use CDO to generate the means/sums and rescale the units.
files <- c(
  file.path("pr", list.files("./pr")),
  file.path("tasmax", list.files("./tasmax")),
  file.path("tasmin", list.files("./tasmin"))
)

keeps <- c(
  grep(models[1], files),
  grep(models[2], files),
  grep(models[3], files),
  grep(models[4], files)
)
files <- files[keeps]

prf <- files[grep("^pr", files)]
tminf <- files[grep("^tasmin", files)]
tmaxf <- files[grep("^tasmax", files)]
cdo_momeans(prf, outpath = "./pr/pr_mosums")
cdo_momeans(tminf, outpath = "./tasmin/tasmin_momeans")
cdo_momeans(tmaxf, outpath = "./tasmax/tasmax_momeans")

# rescale the units and make monthly means (or, in the case of precip, sums).
# list all the files - take the prefixes to make this somewhat easier...
# should I use return files here?!

# take the model and scenario and expand into grid to loop across - generate 
# annual bioclims per scenario, model, year combination.

# generate annual bioclims
for (i in 1:nrow(combos)) {
  make_bioclim(
    model = as.character(combos$Var1[i]),
    scenario = as.character(combos$Var2[i]),
    pr_path = "./pr/pr_mosums",
    tmin_path = "./tasmin/tasmin_momeans",
    tmax_path = "./tasmax/tasmax_momeans",
    outpath = "lores_bioclim_annual"
  )
}

# make baselines
cl <- parallel::makeForkCluster(4)
  parallel::parLapply(cl, models, function(x) make_baseline(model = x, 
    start = 1979, end = 2013, 
    future_scenario = "rcp85",
    file_path = "lores_bioclim_annual", outpath = "baselines"))
parallel::stopCluster(cl)

# calculate anomalies
futures <- c("rcp26", "rcp60", "rcp85")
future_combos <- expand.grid(models, futures)
for (i in 1:nrow(future_combos)) {
  calculate_anomalies(
    model = as.character(future_combos$Var1[i]),
    scenario = as.character(future_combos$Var2[i]),
    start = 2014,
    baseline_path = "baselines",
    future_path = "lores_bioclim_annual",
    outpath = "anomalies"
  )
}

# test a single instance of the resample to europe function
# load extent
# OK - this takes 10 hours, and requires 68.6*2 Gb of temp space MIN. That's
# 137Gb - so I think make up to 150 just to make sure there's space for temp 
# files

extent <- readRDS("../euro_extent.rds")
system.time(
crop_downscale(
  anomaly = "GFDL-ESM2M_rcp26_anomaly_2014.nc",
  anomaly_path = "./anomalies",
  current = "../chelsa_climatologies/chelsa_all_bioclim.grd",
  extent = extent,
  outpath = "hires_bioclim_annual"
  )
)

# Full global downscaling is going to a) take FOREVER and b) generate one 
# million gigabytes (approx.) of data - so for the time being I think it's best
# to chop down to the european scale and downscale that for the sake of getting
# the ADVENT work going sooner rather than later (i.e. get plugged into NEVO
# with the pollinators).

# the crop_downscale function will do this. It's still slow though - I need to
# get this on the cluster for sure. It's using 10 cores at the moment - has been
# going for over an hour - I am sure it's still working?

# the first thing to do is to get the extent of the study area - this means
# load in one of the examples from the advent project. It MIGHT be possible,
# actually, to resample everything into the correct resolution for ADVENT as
# well... This will, of course, replace the current bioclim data with the 
# CHELSA data - but that's fine... probably better in fact?

# time the crop_downscale function and then compare that to the same operation
# using cdos.

# load in european extent from pollinators model.
euro <- raster::stack(
  "/home/hfg/rds_mount/advent-rds/henry/advent/modeling/pollinators/jobs/bioclim_now_cropped.grd"
  )
euro_w <- raster::projectRaster(euro, 
  crs = sp::CRS(sp::proj4string(raster::raster(
    file.path("./anomalies", anomalies[[1]])))))


# then take just one of those layers... 
e_1 <- euro[[1]]
# transform to match the other projections
e_p <- raster::projectRaster(e_1, 
  crs = sp::CRS(sp::proj4string(raster::raster(anomalies[[1]]))))

# then take the extent
euro_e <- raster::extent(e_p)

# save for the future...
saveRDS(euro, file = "../../euro_extent.rds")
saveRDS(e_1, file = "../../euro_extent_map.rds")

# NOW - see if cropping to the new object will do both the rescale AND the 
# crop...

anom <- raster::stack(
  "./anomalies/GFDL-ESM2M_rcp26_anomaly_2020.nc"
)

a_crop <- raster::crop(anom[[1]], euro)
system.time({
  a_crop <- raster::crop(anom, euro)
  a1 <- raster::resample(a_crop, e_p, method = "bilinear")
  x <- euro + a1
  })

# now we need the hi-res bioclim... unfortunately it's currently all seperate
# layers - can we read it all in and make one object?!

setwd("/home/hfg/rds_mount/advent-rds/henry/advent/climate_data/chelsa_climatologies")
files <- list.files()
files <- files[grep("CHELSA", files)]
xx <- raster::stack(files)

# now read the anomaly
anom <- raster::stack(
  "../isimip_data/anomalies/GFDL-ESM2M_rcp26_anomaly_2020.nc"
)

system.time({
  hires_anom <- raster::resample(anom, xx, method = "bilinear")
})

# How long does a single resample take? - answer! 8 minutes.
system.time({
  hires_anom <- raster::resample(anom, xx[[1]], method = "bilinear")
  raster::writeRaster(hires_anom, file = "test_hires.grd", format = "raster")
})

# How long does it take to make anomalies for a full stack in parallel?
# This is a NO - RAM caps out almost immediately. Perhaps take to the cluster?
# I could do a seperate job (and thus ram) for each layer - the problem here
# is that will be like... 99*19*4 jobs or something.


# use gdalwarp to a) see what it does and b) see if it's faster.

# if gdalwarp isn't any faster, or rather considerably faster, then make a
# script for the cluster - it's going to be 1032 jobs to do each stack
# altogether (8 hours ish per job) - that's 1 full queue and a tiny bit.

# this seems slow, and I'm not even sure if it will work...

chel <- 

gdalUtils::gdalwarp()

# problem 1 - the layers/names are in the wrong order.
# problem 2 - the names are different (probably fine?)
# problem 3 - 



# FROM MYRIAD
# Run is complete - check that it's worked and produced the right data...
# I don't think it has thouygh - it's 3.6GB instead of the expected 68Gb...
# I am not sure what's happened here? Got it! I did raster::raster not
# raster::stack for the anomaly...

#####
#####

# Take the 
