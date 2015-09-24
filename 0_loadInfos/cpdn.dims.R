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

    ## rlonlat2gridinfos
    source(file.path(r.infos.path, "rlonlat2gridinfos.R"))

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
        grid.infos <- rlonlat2gridinfos(rlon=rlon, rlat=rlat, plon=plon, plat=plat)
    } else if (model == "HadRM3P_pnw_25km") {
        rlon <- seq(-12.32, 11.66, by=0.22)
        rlat <- seq(13.42, -12.10, by=-0.22)
        plat <- 49.03
        plon <- 58.4
        grid.infos <- rlonlat2gridinfos(rlon=rlon, rlat=rlat, plon=plon, plat=plat)
        grid.infos$lon <- grid.infos$lon-180
    } else {
        stop("** ERROR ** unexpected 'model'. Should be one of 'HadAM3P', 'HadRM3P_eu_50km', 'HadRM3P_eu_25km', ... *****")
    }

    return(grid.infos)
}
