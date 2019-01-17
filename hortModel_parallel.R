################################################################################
#                                                                              #
#                         FUNCTIONS                                            #
#                                                                              #
################################################################################

#' land_classes
#' A simple function that generates the classes of land use (i.e., classifies
#' the landuse classes in the LEEP data into immutable, agri_crop, agri_noncrop
#' and natural).
#' No parameters.
#' @name land_classes
land_classes <- function() {
  immutable <- c(
                "COAST_07", 
                "FWATER_07", 
                "MARINE_07",
                "URBAN_07",
                "OCEAN",
                "OTHFRM_10"
                )  
  agri_crop <- c(
                "WHEAT_10",
                "WBARLEY_10",
                "SBARLEY_10",
                "OTHCER_10",
                "POTS_10",
                "WOSR_10",
                "SOSR_10",
                "MAIZE_10",
                "TBARLEY_10",
                "TOSR_10",
                "SUGARBEET_10",
                "OTHCRPS_10"
                )
  agri_noncrop <- c(
                    "PERMG_10",
                    "TEMPG_10",
                    "RGRAZ_10",
                    "FWOOD_10"
                    )
  non_agri <- c(
              "GRSNFRM_10",
              "NFWOOD_10"
              )
  lu_classes <- list(
    immutable = immutable,
    agri_crop = agri_crop,
    agri_noncrop = agri_noncrop,
    non_agri = non_agri
  )
  return(lu_classes)
}

#' case functions
#' Each of these functions replaces existing land use with horticulture
#' according to a particular filling algorithm, and priority of existing land
#' use classes.
#' case_1 - This is the algorithm for when a single land class of the first 
#'  priority group is enough to take all of the proposed horticulture. It will
#'  take all of the land use classes from the priority group that are large 
#'  enough, randomly select one and then replace part of it with the target
#'  quantity of horticulture (or all of it, if it is the exact size of the
#'  target quantity of horticulture).
#' case_2 - This algorithm is used when no single existing land use class from
#'  the priority group is big enough for the horticulture placement, but the
#'  total area of land use classes from the priority group is sufficient. It
#'  works by fully replacing the largest land use class in the priority group 
#'  with some of the horticulture, and the moving seqeuntially through the 
#'  priority group until the horticulture is all cited (of course, the last land
#'  use class is almost always only partially replaced).
#' case_3 - Case three is used when there is not enough space in the priority
#'  group for all of the horticulture. First of all all of the land in the
#'  priority group is converted into horticulture, and from there one of 4 cases
#'  are used:
#'    a) In case 3a there is enough space in the second priority land class to
#'      take up all the remaining horticulture - the algorithm proceeds in the 
#'      same way as case_2, but with one modification. If there is enough space
#'      in the secondary group EXCLUDING farm woodland, then farm woodland is 
#'      spared.
#'    b) This is used when there isn't enough space in the primary or secondary
#'      priotiy classes, but they can take some of the horticulture, so the 
#'      third priority (typically natural land, but it depends) is used.
#'    c) This is used in cases where there is no land in the secondary class to
#'      take the horticulture, so the third priority class is used.
#'    urban) This is used when there just isn't enough space in land use classes
#'      to accommodate the horticulture (i.e., most of the land is immutable),
#'      so the left over horticulture just isn't placed.
#' These functions will typically not need to be called directly and are,
#' instead, implemented and called in the wrapper function hortChange.
#' @param cl_dat This is the dataframe of data extracted from the original
#'  landuse raster.
#' @param aa The first priotity land use group (one of the groups fromt the
#'  land_classes function).
#' @param bb The second priority group
#' @param cc The third priority group
#' @remaining The amount of horticulture to be placed.

case_1 <- function(cl_dat, aa, remaining) {
  candidates <- names(which(cl_dat[names(cl_dat) %in% aa] >= remaining))
  chosen <- sample(candidates, 1)
  cl_dat[chosen] <- cl_dat[chosen] - remaining
  cl_dat["HORT_10"] <- cl_dat["HORT_10"] + remaining
  return(cl_dat)
}

case_2 <- function(cl_dat, aa, remaining) {
  while (remaining > 0) {
    cl_max <- max(cl_dat[names(cl_dat) %in% aa])
    chosen <- names(which(cl_dat[names(cl_dat) %in% aa] == cl_max))
    if (length(chosen) > 1) {
      chosen <- sample(chosen, 1)
    }
    if (cl_dat[chosen] < remaining) {
      cl_dat["HORT_10"] <- cl_dat["HORT_10"] + cl_dat[chosen]
      remaining <- remaining - cl_dat[chosen]
      cl_dat[chosen] <- 0
    } else {
      cl_dat["HORT_10"] <- cl_dat["HORT_10"] + remaining
      cl_dat[chosen] <- cl_dat[chosen] - remaining
      remaining <- remaining - remaining
    }
  }
  return(cl_dat)
}

case_3 <- function(cl_dat, aa, bb, cc, remaining, subcase) {
  for (j in seq_along(aa)) {
    lnd <- aa[j]
    if (cl_dat[lnd] > 0) {
      cl_dat["HORT_10"] <- cl_dat["HORT_10"] + cl_dat[lnd]
      remaining <- remaining - cl_dat[lnd]
      cl_dat[lnd] <- 0
    }
  }

  if (subcase == "3a") {
    cl_dat <- case_3a(cl_dat, bb, remaining)
  } else if (subcase == "3b") {
    cl_dat <- case_3b(cl_dat, bb, remaining)
  } else if (subcase == "3c") {
    if (length(
      names(which(cl_dat[names(cl_dat) %in% cc] >= remaining))
      ) == 0) {
      cl_dat <- case_3urban(cl_dat, aa, bb, cc, remaining)
    } else {
      cl_dat <- case_3c(cl_dat, cc, remaining)  
    }
  }
  return(cl_dat)
}

case_3a <- function(cl_dat, bb, remaining) {
  candidates <- names(which(cl_dat[names(cl_dat) %in% bb] >= remaining))
  # if there are candidates other than FWOOD, drop FWOOD.
  if (length(candidates) > 1 && any(candidates == "FWOOD_10")) {
    candidates <- candidates[candidates != "FWOOD_10"]
  }
  chosen <- sample(candidates, 1)
  cl_dat[chosen] <- cl_dat[chosen] - remaining
  cl_dat["HORT_10"] <- cl_dat["HORT_10"] + remaining
  return(cl_dat)
}

case_3b <- function(cl_dat, bb, remaining) {
  while (remaining > 0) {
    cl_max <- max(cl_dat[names(cl_dat) %in% bb])
    chosen <- names(which(cl_dat[names(cl_dat) %in% bb] == cl_max))
    if (length(chosen) > 1) {
      chosen <- sample(chosen, 1)
    }
    if (cl_dat[chosen] < remaining) {
      cl_dat["HORT_10"] <- cl_dat["HORT_10"] + cl_dat[chosen]
      remaining <- remaining - cl_dat[chosen]
      cl_dat[chosen] <- 0
    } else {
      cl_dat["HORT_10"] <- cl_dat["HORT_10"] + remaining
      cl_dat[chosen] <- cl_dat[chosen] - remaining
      remaining <- remaining - remaining
    }
  }
  return(cl_dat)
}

case_3c <- function(cl_dat, cc, remaining) {
  candidates <- names(which(cl_dat[names(cl_dat) %in% cc] >= remaining))
  chosen <- sample(candidates, 1)
  cl_dat[chosen] <- cl_dat[chosen] - remaining
  cl_dat["HORT_10"] <- cl_dat["HORT_10"] + remaining
  return(cl_dat)
}

case_3urban <- function(cl_dat, aa, bb, cc, remaining) {
  farm_noncrop <- names(which(cl_dat[names(cl_dat) %in% bb] > 0))
  for (i in seq_along(farm_noncrop)) {
    lnd <- farm_noncrop[i]
    cl_dat["HORT_10"] <- cl_dat["HORT_10"] + cl_dat[lnd]
    remaining <- remaining - cl_dat[lnd]
    cl_dat[lnd] <- 0
  }
  return(list(cl_dat = cl_dat, subcase = "3urban")) 
}

#' hortChange
#' This is the function that implements the horticulture placement, and uses the
#' case functions above. It has a range of options that modify the scenario.
#' These are basically determined by the arguments of the function - details
#' follow.
#' @param landuse This is the original landuse (i.e., the landuse now).
#' @param target The target percentage increase in horticulture sought.
#' @param mode This has three options, "crop_first", "noncrop_first" and 
#'  "nature_first". This argument determines the priority order of land use 
#'  classes being replaced:
#'    crop_first - crop agriculture > non-crop agriculture > natural land
#'    noncrop_first - non-crop_agriculture > crop agriculture > natural land
#'    nature_first - natural land > crop agriculture + non-crop agriculture (in
#'      this instance immutable forms the cc group, which is probably wrong but
#'      it hasn't come up yet - need to change).
#' @param method This takes two possible arguments. If "even" then the new
#'  horticulture is evenly divided between all cells that currently contain
#'  horticulture. If "weighted" then the new horticulture is divided into a
#'  number of parcels equal to the number of cells that already contain
#'  horticulture, and then these parcels are randomly assigned to cells that
#'  already contain horticulture, but weighting the random choice towards cells
#'  according to the amount of horticulture already in them.
#' @scenario The name of the scenario - all this does it provide a prefix for 
#'  the written outputs of the function. If left out then the output gets a 
#'  prefix of a random number between 1 and 10000000.

hortChange <- function(landuse, target = 40, mode = "crop_first",
  method = "even", scenario = NULL) {
  
  if (is.null(scenario)) {
    scenario <- as.character(round(runif(1, 1, 10000000)))
  }

  landuse_dat <- raster::getValues(landuse)
  total_hort_km <- sum(getValues((landuse$HORT_10 / 100) * 4), na.rm = TRUE)
  total_hort_m <- total_hort_km * 1000000
  target_hort_km <- total_hort_km * (1 + target / 100)
  new_hort <- target_hort_km - total_hort_km
  
  cls <- which(landuse_dat[ , "HORT_10"] > 0)
  
  ncells <- length(cls)
  nhort_per_cell <- new_hort / ncells 
  increase_per_cell <- (nhort_per_cell / 4) * 100
  
  if (method == "weighted") {
    # choose cells. Sample from all cells with replacement, weighted by
    # the horticulture already there.
    cls <- sample(cls, 
      length(cls), prob = landuse_dat[cls, "HORT_10"], 
      replace = TRUE)
  }    

  landuse_change_record <- rep(NA, length(cls))
  lu_cls <- land_classes()

  # build cell weightings.

  if (mode == "crop_first") {
    aa <- lu_cls$agri_crop
    bb <- lu_cls$agri_noncrop
    cc <- lu_cls$non_agri
  } else if (mode == "noncrop_first") {
    aa <- lu_cls$agri_noncrop
    bb <- lu_cls$agri_crop
    cc <- lu_cls$non_agri
  } else if (mode == "nature_first") {
    aa <- lu_cls$non_agri
    bb <- c(lu_cls$agri_crop, lu_cls$agri_noncrop)
    cc <- lu_cls$immutable
  }

  for (i in seq_along(cls)) {
    cell <- cls[i]
    cl_dat <- landuse_dat[cell, ]
    remaining <- increase_per_cell
    # check what case this cell is.
    if(any(cl_dat[names(cl_dat) %in% aa] >= increase_per_cell)) {
      cl_dat <- case_1(cl_dat, aa, remaining)
      landuse_change_record[i] <- 1
    } else if (sum(cl_dat[names(cl_dat) %in% aa]) >= remaining) {
      cl_dat <- case_2(cl_dat, aa, remaining)
      landuse_change_record[i] <- 2
    } else {
      # identify subclass.
      if(any(cl_dat[names(cl_dat) %in% bb] >= remaining)) {
        cl_dat <- case_3(cl_dat, aa, bb, cc,
          remaining, subcase = "3a")
        landuse_change_record[i] <- "3a"
      } else if (sum(cl_dat[names(cl_dat) %in% bb]) >= remaining) {
        cl_dat <- case_3(cl_dat, aa, bb, cc,
          remaining, subcase = "3b")
        landuse_change_record[i] <- "3b"
      } else {
        cl_dat <- case_3(cl_dat, aa, bb, cc, 
          remaining, subcase = "3c")
        if (class(cl_dat) == "numeric") {
          landuse_change_record[i] <- "3c"  
        } else {
          landuse_change_record[i] <- cl_dat$subcase
          cl_dat <- cl_dat$cl_dat
        }
        
      }
    }
    landuse_dat[cell, ] <- cl_dat
  }
  landuse <- raster::setValues(landuse, landuse_dat)
  raster::writeRaster(landuse, file = paste0(scenario, "_raster.grd"),
    overwrite = TRUE, format = "raster")
  write.table(table(landuse_change_record), 
    file = paste0(scenario, "_change_record.txt"))
  rets <- list(landuse = landuse, landuse_change_record = landuse_change_record)
  return(rets)
}

#' projectSpecies
#' 

projectSpecies <- function(species, resdir, new_landuse, scenario,
  outdir) {
  modfile <- paste0(species, "_model_outputs.RDS")
  mod <- readRDS(file.path(resdir, modfile))

  new_proj <- sdmpl::newLanduse(
    mod,
    new_landuse
    )
  prefix <- paste0(species, "_", scenario, "_presence.gri")
  raster::writeRaster(new_proj$new_occurence,
    file = file.path(outdir, prefix),
    format = "raster",
    overwrite = TRUE)
  gc()
  return(new_proj)
}

#' addHorti
#' 

addHorti <- function(species, mod_dir, horti_dir) {
  modfile <- paste0(species, "_model_outputs.RDS")
  mod <- readRDS(file.path(mod_dir, modfile))

  horti <- paste0(horti_dir, "/", species)
  mod$new_landuse_presence[["horti_1a"]] <- raster::raster(
    paste0(horti, "_hort_1a_presence.grd"))
  mod$new_landuse_presence[["horti_1b"]] <- raster::raster(
    paste0(horti, "_hort_1b_presence.grd"))
  mod$new_landuse_presence[["horti_1c"]] <- raster::raster(
    paste0(horti, "_hort_1c_presence.grd"))
  mod$new_landuse_presence[["horti_2a"]] <- raster::raster(
    paste0(horti, "_hort_2a_presence.grd"))
  mod$new_landuse_presence[["horti_2b"]] <- raster::raster(
    paste0(horti, "_hort_2b_presence.grd"))
  mod$new_landuse_presence[["horti_2c"]] <- raster::raster(
    paste0(horti, "_hort_2c_presence.grd"))
  
  out <- paste0(species, "_model_outputs.RDS")
  saveRDS(mod, file = paste0(mod_dir, "/", out))
}

### A simple proof of concpept eat-well scenario for the food bill for
# anna taylor.

# Fulfilling eat-well and maintaining the same import/export ratio requires
# a 40% increase in horitultural production.

# Here I will take the cells that currently have horticulture in them and 
# expand the horticultural component to explore the eat-well scenario.

# libraries
library(raster)

# First calculate the required increase.
# load the data from the ADVENT folders to avoid doubling up.
analysis_dir <-
  "/home/hfg/Documents/projects/shefs/analysis/uk/eat_well_scenario"
data_dir <- "/home/hfg/Documents/projects/advent/data/exeter_landcover_data"
landuse <- stack(
  file.path(data_dir, "landuse_leep_raster.gri")
  )
landuse_og <- landuse

# functions.



#####
###
#   SCENARIOS.

# 1a)
#   Even spread across UK, taking up existing agricultural crop land.

system.time(
  hort_1a <- hortChange(landuse,
                        target = 40,
                        mode = "crop_first",
                        method = "even",
                        scenario = "hort_1a")
  )

# this gives, for each cell, all of the data of each layer.

# 2a) 
#   weighted taking crop land first.
system.time(
hort_2a <- hortChange(landuse,
                      target = 40,
                      mode = "crop_first",
                      method = "weighted",
                        scenario = "hort_2a")
)
# 1b)
#   even spread across the UK taking natural land first.
system.time(
hort_1b <- hortChange(landuse,
                      target = 40,
                      mode = "nature_first",
                      method = "even",
                        scenario = "hort_1b")
)
# 2b)
#   weighted taking natural land first.
system.time(
hort_2b <- hortChange(landuse,
                      target = 40,
                      mode = "nature_first",
                      method = "weighted",
                        scenario = "hort_2b")
)
# 1c)
#   even spread across the UK taking non-crop land first,
system.time(
hort_1c <- hortChange(landuse,
                      target = 40,
                      mode = "noncrop_first",
                      method = "even",
                        scenario = "hort_1c")
)
# 2c) 
#   weighted taking non-crop land first.
system.time(
hort_2c <- hortChange(landuse,
                      target = 40,
                      mode = "noncrop_first",
                      method = "weighted",
                        scenario = "hort_2c")
)

################################################################################
################################################################################
# Models sent to cluster.
#
# ANALYSIS - projections under new landuse and map making.

setwd(file.path(analysis_dir, "results"))

resdir <- "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results"
analysis_dir <-
  "/home/hfg/Documents/projects/shefs/analysis/uk/eat_well_scenario"
# Get the species names (prefixes)
files <- list.files(resdir)

# get just the RDS files.
res <- files[grep("*.RDS$", files)]
species <- sapply(res, function(x) {
  .tmp <- strsplit(x, "_")[[1]]
  sp <- paste(.tmp[1:((length(.tmp)) - 2)], collapse = "_")
  sp
})


# Load the horticulture scenarios.
hort_1a <- raster::stack(file.path(analysis_dir, "hort_1a_raster.grd"))
hort_1b <- raster::stack(file.path(analysis_dir, "hort_1b_raster.grd"))
hort_1c <- raster::stack(file.path(analysis_dir, "hort_1c_raster.grd"))
hort_2a <- raster::stack(file.path(analysis_dir, "hort_2a_raster.grd"))
hort_2b <- raster::stack(file.path(analysis_dir, "hort_2b_raster.grd"))
hort_2c <- raster::stack(file.path(analysis_dir, "hort_2c_raster.grd"))

# For each species, reproject the model using each of the scenarios and save
# the output rasters into a new folder.

projectSpecies <- function(species, resdir, new_landuse, scenario,
  outdir) {
  modfile <- paste0(species, "_model_outputs.RDS")
  mod <- readRDS(file.path(resdir, modfile))

  new_proj <- sdmpl::newLanduse(
    mod,
    new_landuse
    )
  prefix <- paste0(species, "_", scenario, "_presence.gri")
  raster::writeRaster(new_proj$new_occurence,
    file = file.path(outdir, prefix),
    format = "raster",
    overwrite = TRUE)
  gc()
  return(new_proj)
}

system.time(projectSpecies(species[1], resdir, hort_1a, "hort_1a", outdir))

resdir <- "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results"
outdir <- "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results/horticulture_projections"

library(foreach)
cl <- parallel::makeCluster(14)
doParallel::registerDoParallel(cl)
res <- foreach(i = seq_along(species)) %dopar% {
  projectSpecies(species[i],
        resdir = "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results", 
    hort_1a, 
    "hort_1a", 
    outdir = "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results/horticulture_projections"
    )  
}

res <- foreach(i = seq_along(species)) %dopar% {
  projectSpecies(species[i],
        resdir = "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results", 
    hort_1b, 
    "hort_1b", 
    outdir = "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results/horticulture_projections"
    )  
}

res <- foreach(i = seq_along(species)) %dopar% {
  projectSpecies(species[i],
        resdir = "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results", 
    hort_1c, 
    "hort_1c", 
    outdir = "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results/horticulture_projections"
    )  
}

res <- foreach(i = seq_along(species)) %dopar% {
  projectSpecies(species[i],
        resdir = "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results", 
    hort_2a, 
    "hort_2a", 
    outdir = "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results/horticulture_projections"
    )  
}

res <- foreach(i = seq_along(species)) %dopar% {
  projectSpecies(species[i],
        resdir = "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results", 
    hort_2b, 
    "hort_2b", 
    outdir = "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results/horticulture_projections"
    )  
}

res <- foreach(i = seq_along(species)) %dopar% {
  projectSpecies(species[i],
        resdir = "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results", 
    hort_2c, 
    "hort_2c", 
    outdir = "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results/horticulture_projections"
    )  
}

parallel::stopCluster(cl)

# and then combine the outputs for each of the six scenarios into maps.

# The normal combine outputs function doesn't work well here since these outputs
# are outside of the normal run of things. Fairly simple, though, to load up 
# the previous model, and add in the new scenarios and then get it done that 
# way...


# combineOutputs takes file prefixes. I need to add a new argument so that it
# adds in the new landuse - or just do it in this function...

mod_dir <- "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results"
horti_dir <- "/media/hfg/DATAPART1/postdoc/projects/shefs/analysis/uk/horticulture_scenario/results/horticulture_projections"

addHorti <- function(species, mod_dir, horti_dir) {
  modfile <- paste0(species, "_model_outputs.RDS")
  mod <- readRDS(file.path(mod_dir, modfile))

  horti <- paste0(horti_dir, "/", species)
  mod$new_landuse_presence[["horti_1a"]] <- raster::raster(
    paste0(horti, "_hort_1a_presence.grd"))
  mod$new_landuse_presence[["horti_1b"]] <- raster::raster(
    paste0(horti, "_hort_1b_presence.grd"))
  mod$new_landuse_presence[["horti_1c"]] <- raster::raster(
    paste0(horti, "_hort_1c_presence.grd"))
  mod$new_landuse_presence[["horti_2a"]] <- raster::raster(
    paste0(horti, "_hort_2a_presence.grd"))
  mod$new_landuse_presence[["horti_2b"]] <- raster::raster(
    paste0(horti, "_hort_2b_presence.grd"))
  mod$new_landuse_presence[["horti_2c"]] <- raster::raster(
    paste0(horti, "_hort_2c_presence.grd"))
  
  out <- paste0(species, "_model_outputs.RDS")
  saveRDS(mod, file = paste0(mod_dir, "/", out))
}

cl <- parallel::makeCluster(14)
doParallel::registerDoParallel(cl)
x <- foreach(i = seq_along(species)) %dopar% {
  addHorti(species[i], mod_dir, horti_dir)
}
parallel::stopCluster(cl)

# then run the old comine outputs with the file prefixes.

# pdf version.
combineOutputs(
  file_prefixes = species,
  outfile = "figures/pollinators",
  method = "count",
  maps = TRUE,
  pal = "viridis",
  format = "pdf",
  parallel = TRUE)

# png version
combineOutputs(
  file_prefixes = species,
  outfile = "figures/pollinators",
  method = "count",
  maps = TRUE,
  pal = "viridis",
  format = "png",
  parallel = TRUE)




##### briefing doc fig - rough and ready!

  colfunc <- grDevices::colorRampPalette(c("#e62e00", "#e6e6e6", "#005ce6"))
  x_t <- hort2a - current
  x_p <- ((hort2a - current) / current) * 100 
  breaks <- getBreaks(x_t, neg = TRUE)
  cols <- colfunc(length(breaks) - 1)
  file <- "total_change_scenario_2a"
  map_t <- rasterVis::levelplot(x_t, at = breaks, col.regions = cols,
        margin = FALSE,
        main = "Changes in pollinator habitat suitability
        with 30% increase in horticultural land across the UK.",
        scales = list(draw = FALSE))
saveMap(map_t, "png", file)


  breaks <- getBreaks(x_p, neg = TRUE)
  cols <- colfunc(length(breaks) - 1)
  file <- "percentage_change_scenario_2a"
  map_p <- rasterVis::levelplot(x_p, at = breaks, col.regions = cols,
        margin = FALSE,
        main = "Percentage change in pollinator habitat suitability
        with 30% increase in horticultural land.",
        scales = list(draw = FALSE))
saveMap(map_p, "png", file)

# Now some insets - East Anglia, and Easter Scotland.

x_t_ea <- crop(x_t, extent(350000, 656000, 180000, 500000))
x_p_ea <- crop(x_p, extent(350000, 656000, 180000, 500000))

  breaks <- getBreaks(x_t_ea, neg = TRUE)
  cols <- colfunc(length(breaks) - 1)
  file <- "total_change_scenario_2a_eastanglia"
  map_t_ea <- rasterVis::levelplot(x_t_ea, at = breaks, col.regions = cols,
        margin = FALSE,
        main = "Changes in pollinator habitat suitability
        with 30% increase in horticultural land
        in the East of England.",
        scales = list(draw = FALSE))
  map_t_ea
saveMap(map_t_ea, "png", file)

  breaks <- getBreaks(x_p_ea, neg = TRUE)
  cols <- colfunc(length(breaks) - 1)
  file <- "percentage_change_scenario_2a_eastanglia"
  map_p_ea <- rasterVis::levelplot(x_p_ea, at = breaks, col.regions = cols,
        margin = FALSE,
        main = "Percentage change in pollinator habitat suitability
        with 30% increase in horticultural land
        in the East of England.",
        scales = list(draw = FALSE))
  map_p_ea
saveMap(map_p_ea, "png", file)

x_t_sc <- crop(x_t, extent(200000, 450000, 600000, 900000))
x_p_sc <- crop(x_p, extent(200000, 450000, 600000, 900000))

  breaks <- getBreaks(x_p_sc, neg = TRUE)
  cols <- colfunc(length(breaks) - 1)
  file <- "percentage_change_scenario_2a_eastscotland"
  map_p_sc <- rasterVis::levelplot(x_p_sc, at = breaks, col.regions = cols,
        margin = FALSE,
        main = "Percentage change in pollinator habitat suitability
        with 30% increase in horticultural land
        in the East of Scotland.",
        scales = list(draw = FALSE))
  map_p_sc
saveMap(map_p_sc, "png", file)


saveMap <- function(map, format, file, ...) {
  if (format == "pdf") {
    file <- paste0(file, ".pdf")
    pdf(file = file, ...)
      print(map)
    dev.off()      
  } else if (format == "png") {
    file <- paste0(file, ".png")
    png(file = file, 
      width = 8,
      height = 11,
      unit = "in",
      res = 300, ...)
      print(map)
    dev.off()      
  }
}
