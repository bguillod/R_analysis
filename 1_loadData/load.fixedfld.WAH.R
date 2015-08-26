load.fixedfld.WAH <- function(experiment="fredi",
                              var, # 'THETA_PWP', 'THETA_FC' 'THETA_SAT'
                              rcm=T) {

  ## ---------------------------------------------------------------------
  ## ---------------------------------------------------------------------
  ## function to load WAH runs
  ## 
  ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

  
  require(ncdf4)
  require(ncdf4.helpers)

  if (FALSE) {
      source("/ouce-home/staff/cenv0433/scripts/general_funcs/1_loadData/load.fixedfld.WAH.R")
      experiment <- "fredi"
      var <- "THETA_PWP"
      rcm <- F

      ## test <- experiment
      require(fields)
      rcm.pwp <- load.fixedfld.WAH(experiment, var, rcm)
      source("/ouce-home/staff/cenv0433/scripts/general_funcs/4_mapFuncs/my.map.plot.R")
      my.map.plot(rcm.pwp, type="abs", axes=F)

      ## plot all three variables for regional and global model
      vars <- c("THETA_PWP", "THETA_CRIT", "THETA_FC", "THETA_SAT")
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
      ##      stop(paste("** ERROR ** unexpected experiment name", experiment))
      if (!rcm) stop("** ERROR ** for 'Dann' only RCM is ready *****")
      file.in <- "/ouce-home/staff/cenv0433/data/MaRIUS/compare_start_dumps/start_dump_DANN/start_dump_dann.nc"
  } else if (experiment == "fredi") {
#      if (rcm) stop("** ERROR ** only GCM available for 'fredi' *****")
      file.in <- paste("/ouce-home/staff/cenv0433/data/WAH-fredi-russianHW/start_dumps/", ifelse(rcm, "region", "atmos"), "_restart_2owx_1989.nc", sep="")
  } else if (experiment == "UKCP09") {
      if (!rcm) stop("** ERROR ** only RCM available for UKCP09 *****")
    file.in <- "/ouce-home/staff/cenv0433/data/MaRIUS/compare_start_dumps/start_dump_MO/lsm_MO.nc"
  } else {
    stop("** ERROR ** unexpected value for 'experiment' *****")
}

  ## which variable?
  variable <- switch(var,
                     "THETA_PWP"="field329",
                     "THETA_CRIT" = "field330",
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

