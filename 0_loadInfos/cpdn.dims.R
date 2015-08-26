cpdn.dims <- function(model) {

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function to get the lon and lat dimensions of a CPDN/WAH model
    ## 
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

    if (FALSE) {
        source(file.path(r.infos.path, "cpdn.dims.R"))
    }

    if (model == "HadAM3P") {
        lon <- seq(0, 359, by=1.875)
        lat <- seq(90, -90, by=-1.25)
        grid.type <- "lonlat"
        grid.infos <- list(lon=lon, lat=lat, grid.type=grid.type)
    } else if (model == "HadRM3P_eu_50km") {
        rlon <- seq(-31.73, 21.51, by=0.44)
        rlat <- seq(25.19, -26.73, by=-0.44)
        plat <- 39.25
        plon <- 198
        grid.type <- "rotpol"
        ## compute rotated coordinates
        require(geocors)
        rlon.mat <- matrix(rep(rlon, length(rlat)), nrow=length(rlon))
        rlat.mat <- matrix(rep(rlat, each=length(rlon)), nrow=length(rlon))
        lonlat <- geocors.trafo(x=as.vector(rlon.mat), y=as.vector(rlat.mat), from.type="rotpol", from.pars=list(plon=plon, plat=plat), to.type="lonlat")
        lon <- matrix(lonlat$lon, nrow=length(rlon))
        lat <- matrix(lonlat$lat, nrow=length(rlon))
        grid.infos <- list(rlon=rlon, rlat=rlat, plon=plon, plat=plat, grid.type=grid.type, lon=lon, lat=lat)
    } else {
        stop("** ERROR ** unexpected 'model'. Should be one of 'HadAM3P', 'HadRM3P_eu_50km', 'HadRM3P_eu_25km', ... *****")
    }

    return(grid.infos)
}
