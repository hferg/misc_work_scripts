# list all the files.

# get the combos of year and RCP.

# build the shell script.

# 1) copy the correct R script to TMPDIR

# 2) move to TMPDIR

# 3) scp the right files over.

# 4) execute the script with the named files.

files <- list.files("/home/hfg/rds_mount/advent-rds/henry/advent/climate_data/isimip_data/hires_bioclim_annual/upto2060")
models <- unique(sapply(files, function(x) strsplit(x, "_")[[1]][1]))
years <- unique(sapply(files, function(x) strsplit(x, "_")[[1]][3]))
rcps <- c("rcp26", "rcp60", "rcp85")
combos <- expand.grid(years, c("rcp26", "rcp60", "rcp85"))
colnames(combos) <- c("year", "rcp")

for (i in seq_along(1:nrow(combos))) {
  com <- combos[i, ]
  year <- com$year
  rcp <- com$rcp

  fls <- files[grep(year, files)]
  fls <- fls[grep(rcp, fls)]
  job_files <- fls[grep("*.grd$", fls)]
  # make a jobname
  jname <- paste0("job_", year, "_", rcp)
  jscript <- paste0(jname, "_analysis.R")
  rds_path <- "ssh.rd.ucl.ac.uk:/rd/live/ritd-ag-project-rd00nc-hferg62/henry/advent/climate_data/isimip_data/hires_bioclim_annual/upto2060"
  rds_dest <- "/rd/live/ritd-ag-project-rd00nd-hferg62/henry/hires_annual_means"
  # write the job info.
  # header
  sink(jscript)
    cat("source('annualMeans.R')\n")
    cat(paste0("annualMeans(files = c('",
      job_files[1], "', '",
      job_files[2], "', '",
      job_files[3], "', '",
      job_files[4],
      "'), year = ", year, ", rcp = '", rcp, "')\n")
    )
  sink()

  sink(paste0(jname, ".sh"))
    cat("#!/bin/bash -l\n")
    cat("#$ -S /bin/bash\n")
    cat("#$ -l h_rt=36:0:0\n")
    cat("#$ -l mem=32G\n")
    cat("#$ -l tmpfs=30G\n")
    cat(paste0("#$ -N ", jname, "\n"))
    cat("#$ -wd /home/ucbthfe/Scratch/advent/bioclim_annual_means\n")
    cat("module unload compilers mpi\n")
    cat("module load r/recommended\n")
    cat(paste0("cp annualMeans.R ", jscript, " $TMPDIR\n"))
    cat("cd $TMPDIR\n")
    for (i in seq_along(fls)) {
      cat(paste0(
        "scp ", file.path(rds_path, fls[i]), " $TMPDIR\n"
      ))
    }
    cat(paste0("Rscript ", jscript, "\n"))
    for (i in seq_along(fls)) {
      cat(paste0(
        "rm ", fls[i], "\n"
      ))
    }
    cat(paste0(
      "tar zcvf ", jname, "_output.tar.gz .\n"
    ))
    cat(paste0(
      "scp ", jname, "_output.tar.gz ssh.rd.ucl.ac.uk:", rds_dest, "\n"
    ))
  sink()
}

# here is a second script to re-run the failed runs - I have no idea what went
# on there but hopefully we can get them run in time. These are the same, except
# the data is ALSO being transferred back to myriad, which ought to be safer.

files <- list.files("/home/hfg/rds_mount/advent-rds/henry/advent/climate_data/isimip_data/hires_bioclim_annual/upto2060")
models <- unique(sapply(files, function(x) strsplit(x, "_")[[1]][1]))
broken_years <- c(2035:2060)
broken_combos <- expand.grid(broken_years, c("rcp26", "rcp60", "rcp85"))
broken_extras <- data.frame(Var1 = c(2028, 2033, 2034, 2034),
  Var2 = c("rcp26", "rcp60", "rcp26", "rcp85")
  )
broken_combos <- rbind(broken_extras, broken_combos)


colnames(broken_combos) <- c("year", "rcp")
for (i in seq_along(1:nrow(broken_combos))) {
  com <- broken_combos[i, ]
  year <- com$year
  rcp <- com$rcp

  fls <- files[grep(year, files)]
  fls <- fls[grep(rcp, fls)]
  job_files <- fls[grep("*.grd$", fls)]
  # make a jobname
  jname <- paste0("bjob_", year, "_", rcp)
  jscript <- paste0(jname, "_analysis.R")
  rds_path <- "ssh.rd.ucl.ac.uk:/rd/live/ritd-ag-project-rd00nc-hferg62/henry/advent/climate_data/isimip_data/hires_bioclim_annual/upto2060"
  rds_dest <- "/rd/live/ritd-ag-project-rd00nd-hferg62/henry/hires_annual_means"
  # write the job info.
  # header
  sink(jscript)
    cat("source('annualMeans.R')\n")
    cat(paste0("annualMeans(files = c('",
      job_files[1], "', '",
      job_files[2], "', '",
      job_files[3], "', '",
      job_files[4],
      "'), year = ", year, ", rcp = '", rcp, "')\n")
    )
  sink()

  sink(paste0(jname, ".sh"))
    cat("#!/bin/bash -l\n")
    cat("#$ -S /bin/bash\n")
    cat("#$ -l h_rt=36:0:0\n")
    cat("#$ -l mem=32G\n")
    cat("#$ -l tmpfs=30G\n")
    cat(paste0("#$ -N ", jname, "\n"))
    cat("#$ -wd /home/ucbthfe/Scratch/advent/bioclim_annual_means/broken_jobs\n")
    cat("module unload compilers mpi\n")
    cat("module load r/recommended\n")
    cat(paste0("cp annualMeans.R ", jscript, " $TMPDIR\n"))
    cat("cd $TMPDIR\n")
    for (i in seq_along(fls)) {
      cat(paste0(
        "scp ", file.path(rds_path, fls[i]), " $TMPDIR\n"
      ))
    }
    cat(paste0("Rscript ", jscript, "\n"))
    for (i in seq_along(fls)) {
      cat(paste0(
        "rm ", fls[i], "\n"
      ))
    }
    cat(paste0(
      "cp bioclim_", year, "_", rcp, ".grd $HOME/Scratch/advent/bioclim_annual_means/broken_jobs/\n"
      ))
    cat(paste0(
      "cp bioclim_", year, "_", rcp, ".gri $HOME/Scratch/advent/bioclim_annual_means/broken_jobs/\n"
      ))
    cat(paste0(
      "tar zcvf ", jname, "_output.tar.gz .\n"
    ))
    cat(paste0(
      "scp ", jname, "_output.tar.gz ssh.rd.ucl.ac.uk:", rds_dest, "\n"
    ))
  sink()
}

#### Now a script to calculate the decadal means for each of the RCP scenarios.

# Process.
# 1) scp the relevant tarballs over from shefs RDS to Myriad (10 * 2Gb).
# 2) Untar those tarballs, and remove the original tarballs (10 * 4.7Gb).
# 3) Use the previous method to calculate the mean of each layer in those files.
#   Do this in parallel?! Will be faster but might need some false starts.
# 4) CP the resulting grid file back onto Myriad, and also SCP it onto the 
#   shefs RDS.

setwd("/home/hfg/rds_mount/shefs-rds/henry/hires_annual_means")
files <- list.files()
files <- files[files != "decadal_means_jobs"]
years <- sapply(files, function(x) strsplit(x, "_")[[1]][2])
setwd("./decadal_means_jobs")

shefs_rds <- "ssh.rd.ucl.ac.uk:/rd/live/ritd-ag-project-rd00nd-hferg62/henry/hires_annual_means/"
rds_dest <- "ssh.rd.ucl.ac.uk:/rd/live/ritd-ag-project-rd00nd-hferg62/henry/hires_decadal_means/"
rcp <- c("rcp26", "rcp60", "rcp85")
ranges <- list(c(2021, 2030), c(2031, 2040), c(2041, 2050), c(2051, 2060))
decades <- expand.grid(ranges, rcp)
colnames(decades) <- c("year_range", "rcp")

for (i in 1:nrow(decades)) {
  com <- decades[i, ]
  yr_range <- com$year_range
  range <- paste0(yr_range[[1]], collapse = "-")
  rcp <- com$rcp
  outfiles <- c(
    paste0("bioclim_", range, "_", rcp, ".grd"),
    paste0("bioclim_", range, "_", rcp, ".gri")
  )

  # find the right files.
  # find the index of years that fit the description.
  yrs <- years >= yr_range[[1]][1] & years <= yr_range[[1]][2]
  fls <- files[yrs]
  fls <- fls[grep(rcp, fls)]

  # make a list of the expected files...
  f_years <- sapply(fls, function(x) strsplit(x, "_")[[1]][2])
  input_files <- paste0("bioclim_", f_years, "_", rcp, ".grd")
  all_analysis_files <- c(input_files, 
    paste0("bioclim_", f_years, "_", rcp, ".gri"))

  # make jobname.
  jname <- paste0("job_", range, "_", rcp)
  jscript <- paste0("sc_", range, "_", rcp, ".R")
  sink(jscript)
    cat("source('decadalMeans.R')\n")
    cat(paste0("decadalMeans(files = c('",
      input_files[1], "', '",
      input_files[2], "', '",
      input_files[3], "', '",
      input_files[4], "', '",
      input_files[5], "', '",
      input_files[6], "', '",
      input_files[7], "', '",
      input_files[8], "', '",
      input_files[9], "', '",
      input_files[10],
      "'), range = '", range, "', rcp = '", rcp, "')\n")
    )
  sink()

  # start script.
  sink(paste0(jname, ".sh"))
    cat("#!/bin/bash -l\n")
    cat("#$ -S /bin/bash\n")
    cat("#$ -l h_rt=36:0:0\n")
    cat("#$ -l mem=32G\n")
    cat("#$ -l tmpfs=500G\n")
    cat(paste0("#$ -N ", jname, "\n"))
    cat("#$ -wd /home/ucbthfe/Scratch/advent/bioclim_decadal_means\n")
    cat("module unload compilers mpi\n")
    cat("module load r/recommended\n")
    cat(paste0("cp decadalMeans.R ", jscript, " $TMPDIR\n"))
    cat("cd $TMPDIR\n")
    for (i in seq_along(fls)) {
      # copy tarball over
      cat(paste0(
        "scp ", file.path(shefs_rds, fls[i]), " $TMPDIR\n"
      ))
      # untar
      cat(paste0(
        "tar xvf ", fls[i], "\n"
      ))
      # remove
      cat(paste0(
        "rm ", fls[i], "\n"
      ))
    }
    # run script
    cat(paste0("Rscript ", jscript, "\n"))
    # remove input files.
    for (i in seq_along(all_analysis_files)) {
      cat(paste0(
        "rm ", all_analysis_files[i], "\n"
      ))
    }
    # scp the results to SHEFS rds - gri and grd file.
    for (i in seq_along(outfiles)) {
      cat(paste0(
        "scp ", outfiles[i], " ", rds_dest, "\n"
      ))
    }
    # cp the results back to MYRIAD - gri and grd file.
    for (i in seq_along(outfiles)) {
      cat(paste0(
        "cp ", outfiles[i], " $HOME/Scratch/advent/bioclim_decadal_means/\n"
      ))
    }
  sink()
}
