load.WAH <- function(experiment="fredi",
                     umid,
                     var,
                     years = "all",
                     months = "all",
                     lon.range,
                     lat.range,
                     temporal = "m") {

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function to load WAH runs
    ## 
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
  require(ncdf4)
  require(ncdf4.helpers)

  if (FALSE) {
      source("/ouce-home/staff/cenv0433/scripts/general_funcs/1_loadData/load.WAH.R")
      experiment <- "fredi"
      var <- "sm"
      rcm <- F
      years <- 1963
      umid <- "2jqi"

      ## test <- experiment
      require(fields)
      rcm.pwp <- load.fixedfld.WAH(experiment, var, rcm)
      source("/ouce-home/staff/cenv0433/scripts/general_funcs/4_mapFuncs/my.map.plot.R")
      my.map.plot(rcm.pwp, type="abs", axes=F)

      ## plot all three variables for regional and global model
      vars <- c("THETA_PWP", "THETA_FC", "THETA_SAT")
      rcm.soils <- gcm.soils <- list()
      for (i in 1:length(vars)) {
          rcm.soils[[i]] <- load.fixedfld.WAH(experiment, vars[i], rcm=T)
          gcm.soils[[i]] <- load.fixedfld.WAH(experiment, vars[i], rcm=F)
      }

      par(mfrow=c(3,2))
      for (i in 1:length(vars)) my.map.plot(rcm.soils[[i]], type="abs", breaks=seq(0,0.46, by=0.046))
      range(rcm.soils[[3]], na.rm=T)
      range(gcm.soils[[3]], na.rm=T)

  }


  ## which file?
  if (experiment == "Dann") {
      stop(paste("** ERROR ** unexpected experiment name", experiment))
#      file.in <- "/ouce-home/staff/cenv0433/data/MaRIUS/compare_start_dumps/start_dump_DANN/lsm_dann.nc"
  } else if (experiment == "fredi") {
      ##      if (rcm) stop("** ERROR ** only GCM available for 'fredi' *****")
      path.in <- "/ouce-home/staff/cenv0256/EXCLUDEFROMBACKUP"
      data.list <- data.frame(year=numeric(), umid=character(), file=character())
      umid.orig <- umid
      for (y in years) {
          if (umid.orig == "all") umid <- substr(list.files(file.path(path.in, y), pattern="hadam3p_eu_"), 12, 15)
          for (uid in umid) {
              folders.in <- list.files(file.path(path.in, y), pattern=paste("hadam3p_eu_", uid, "_", sep=""))
              file.in <- file.path(path.in, y, folders.in)
              if (length(file.in) == 0) next
              for (i in 1:length(file.in)) data.list <- rbind(data.list, data.frame(year=y, umid=uid, file=file.in[i]))
#              print(file.in)
          }
      }
      
#      file.in <- paste("/ouce-home/staff/cenv0433/data/WAH-fredi-russianHW/start_dumps/", ifelse(rcm, "region", "atmos"), "_restart_2owx_1989.nc", sep="")
  } else if (experiment == "UKCP09") {
      if (!rcm) stop("** ERROR ** only RCM available for UKCP09 *****")
    file.in <- "/ouce-home/staff/cenv0433/data/compare_start_dumps/start_dump_MO/lsm_MO.nc"
  } else {
    stop("** ERROR ** unexpected value for 'experiment' *****")
}

  ## which variable?
  variable <- switch(var,
                     "THETA_PWP"="field329",
                     "THETA_FC"="field331",
                     "THETA_SAT"="field332")
  
  ## load data and dimensions
  nc <- nc_open(file.in)
  lon <- ncvar_get(nc, ifelse(rcm, "x", "longitude"))
  lat <- ncvar_get(nc, ifelse(rcm, "y", "latitude"))
  dat <- ncvar_get(nc, variable)
  nc_close(nc)

  attr(dat, "lon") <- lon
  attr(dat, "lat") <- lat

  return(dat)
}




