map.add <- function(grid.atts, database="world", interior=TRUE, add=TRUE, ...) {

    ## get grid pars
    library(plotmap)
    library(maps)
    if (!is.list(grid.atts)) grid.atts <- get.grid.atts(grid.atts)
    if (is.null(grid.atts)) stop("** ERROR ** grid.atts not recognized *****")

    ## function to get the right range of longitude
    get.lonrange <- function(lon) {
        lonlim <- range(lon, na.rm=TRUE)
        panel.map <- findInterval(lonlim+c(1,-1), seq(-180, 540, by=180))
        if (any(panel.map < 1) | any(panel.map > 4) | (diff(panel.map) > 1)) {
            stop("** lon range cannot be determined *****")
        } else if (any(panel.map == 4)) {
            lon.range <- c(180, 540)
        } else if (any(panel.map == 3)) {
            lon.range <- c(0, 360)
        } else {
            lon.range <- c(-180, 180)
        }
        return(lon.range)
    }

    if (grid.atts$grid.type == "lonlat") {
        x <- grid.atts$lon
        y <- grid.atts$lat
        lonlim <- range(x)
        latlim <- range(y)
        lonlat.range <- get.lonrange(lonlim)
        x.range <- lonlat.range
    } else if (grid.atts$grid.type == "rotpol") {
        source(file.path(mapFuncs.path, "my.rotpol2lonlat.R"))
        plon <- grid.atts$plon
        plat <- grid.atts$plat
        x <- grid.atts$rlon
        y <- grid.atts$rlat
        xlim <- range(x)
        ylim <- range(y)
        lims <- my.rotpol2lonlat(rep(xlim, each=2), rep(ylim, times=2), plon=plon, plat=plat)
        lonlim <- range(lims$lon)
        latlim <- range(lims$lat)
        lonlat.range <- get.lonrange(lonlim)
#        x.range <- lonlat.range
        x.range <- get.lonrange(x)
    } else {
        stop("** ERROR ** unexpected value in 'grid.atts$grid.type' *****")
    }

    if (database == "world") {
        if (all(lonlat.range == c(0, 360))) database <- "world2"
    }

    map.data <- maps::map(plot=F, xlim=lonlim, ylim=latlim, database=database, interior=interior)
    if (grid.atts$grid.type == "rotpol") {
            map.data.na <- is.na(map.data$x)
            map.data.temp <- lonlat2rotpol(map.data$x[!map.data.na], map.data$y[!map.data.na], plon, plat)
            map.data.rot <- map.data
            map.data.rot$x[!map.data.na] <- map.data.temp[,1]
            map.data.rot$y[!map.data.na] <- map.data.temp[,2]
            map.data <- map.data.rot
    }
    source(file.path(r.infos.path, "degree.adjustRange.R"))
    #print(x.range)
    map.data$x <- degree.adjustRange(map.data$x, x.range)
    if (add) {
        lines(map.data, ...)
    }
    return(invisible(map.data))
}
