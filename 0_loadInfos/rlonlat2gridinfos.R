rlonlat2gridinfos <- function(rlon, rlat, plon, plat) {
    ## function to get all grid information, including longitude and latitude fields, from rotated coordinates
    
    if (FALSE) {
        rlon <- seq(-31.73, 21.51, by=0.44)
        rlat <- seq(25.19, -26.73, by=-0.44)
        plat <- 39.25
        plon <- 198
    }

    grid.type <- "rotpol"
    ## compute rotated coordinates
    require(geocors)
    rlon.mat <- matrix(rep(rlon, length(rlat)), nrow=length(rlon))
    rlat.mat <- matrix(rep(rlat, each=length(rlon)), nrow=length(rlon))
    lonlat <- geocors.trafo(x=as.vector(rlon.mat), y=as.vector(rlat.mat), from.type="rotpol", from.pars=list(plon=plon, plat=plat), to.type="lonlat")
    lon <- matrix(lonlat$lon, nrow=length(rlon))
    lat <- matrix(lonlat$lat, nrow=length(rlon))
    grid.infos <- list(rlon=rlon, rlat=rlat, plon=plon, plat=plat, grid.type=grid.type, lon=lon, lat=lat)

    return(grid.infos)
}
