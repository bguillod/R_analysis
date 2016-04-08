
area.mean <- function(z,
                      area.in,
                      grid.atts=get.grid.atts(z),
                      fun=mean) {
    ## -------------------------------------------------------------------------------------
    ## area.mean(z, area.in, grid.atts, fun) 
    ##         -----> function to compute the average of z over an area defined by 'area.in'
    ##         -----> 'area.in' : either an array of dim equal to dim(z)[1:2]
    ##         ----->             or a list containing 'lon.range', 'lat.range'
    ##         ----->             or 'rlon.range', 'rlat.range' for grid.atts$grid.type='rotpol'
    ##         ----->             or if empty: average over the whole domain in 'z'
    ##         -----> To compute anything else than the mean, another function can specified via argument 'fun')
    ## -------------------------------------------------------------------------------------
    
    # configure 'area.in'
    if (missing(area.in)) {
        area.in <- array(data=TRUE, dim=dim(z)[1:2])
    } else if (any(class(area.in) == c("matrix", "array"))) {
        if (dim(area.in) != dim(z)[1:2]) {
            stop("** ERROR ** dimensions of 'area.in' do not match 'dim(z)[1:2]' *****")
        }
    } else if (is.list(area.in)) {
        x <- grid.atts[[grid.att.names(grid.atts$grid.type, what="grid.cors")[1]]]
        y <- grid.atts[[grid.att.names(grid.atts$grid.type, what="grid.cors")[2]]]
        area.names.should <- paste0(grid.att.names(grid.atts$grid.type)[2:3], ".range")
        match.area.names <- match(area.names.should, names(area.in))
        if (any(is.na(match.area.names))) stop("** ERROR ** 'area.in' not valid *****")
        x.in <- findInterval(x, area.in[[match.area.names[1]]])==1
        y.in <- findInterval(y, area.in[[match.area.names[2]]])==1
        area.in.new <- array(data=FALSE, dim=dim(z)[1:2])
        area.in.new[x.in, y.in] <- TRUE
        area.in <- area.in.new
    } else {
        stop("** ERROR ** unexpected format for 'area.in' *****")
    }
    
    # now compute the weighted average
    source(file.path(compute.path, "regional/subset.region.R"))
    z.sub <- z[x.in, y.in, ]
    wts <- grid.box.weight()
    
    output <- apply(z[x.in, y.in, ], 3, fun)
}

grid.box.weight <- function(grid.atts=get.grid.atts(z), z,
                            method="simple") {
    
    if (FALSE) {
        var <- "tas"
        years <- 1950:2014
        source('~/Documents/Scripts/R_general/local_funcs/1_load/marius/load.UK.20CR.R')
        data.20cr <- load.UK.20CR(var, temporal = "m", years=years)
        test <- grid.box.weight(z=data.20cr)
        library(fields)
        str(test)
        image.plot(test)
        #     
        #     test2 <- grid.box.weight(z=data.20cr, method="neil")
        #     image.plot(test2)
        
    }
    
    # function to give weights to grid cells for the computation of areal averages
    
    grid.type <- grid.atts$grid.type
    if (grid.type == "lonlat") {
        # just get weights from the cosine of the latitude
        lat <- grid.atts$lat
        lon <- grid.atts$lon
        dlat <- unique(diff(lat))
        dlon <- unique(diff(lon))
        if (length(dlat) >1) stop("** ERROR ** latitude not regular *****")
        if (length(dlon) >1) stop("** ERROR ** longitude not regular *****")
        weights <- lon*dlat*cos(lat/180*pi)
        weights.grid <- matrix(nrow=length(lon), ncol=length(lat), rep(weights, each=length(lon)))
        return(weights.grid)
    } else if (grid.type == "rotpol") {
        # approximate the area by assuming a square
        rlat <- as.vector(grid.atts$rlat)
        rlon <- as.vector(grid.atts$rlon)
        drlat <- unique(diff(rlat))
        drlon <- unique(diff(rlon))
        if (length(drlat) >1) stop("** ERROR ** rlat not regular *****")
        if (length(drlon) >1) stop("** ERROR ** rlon not regular *****")
        if (drlon < 0) stop("** ERROR ** drlon is negative *****")
        if (method == "simple") {
            weights <- drlon*drlat*cos(rlat/180*pi)
            weights.grid <- matrix(nrow=length(rlon), ncol=length(rlat), rep(weights, each=length(rlon)))
        } else if (method == "neil") {
            source(file.path(mapFuncs.path, "my.rotpol2lonlat.R"))
            bl.coords <- my.rotpol2lonlat(rlon-drlon[1]/2, rlat-abs(drlat[1]/2),
                                          plon=grid.atts$plon, plat=grid.atts$plat)
            ur.coords <- my.rotpol2lonlat(rlon+drlon[1]/2, rlat+abs(drlat[1]/2),
                                          plon=grid.atts$plon, plat=grid.atts$plat)
            weights.grid <- pi/180*abs(ur.coords$lon-bl.coords$lon)*abs(sin(pi/180*ur.coords$lat) - sin(pi/180*bl.coords$lat))
            
        } else {
            stop("** ERROR ** unexpected value for 'method' *****")
        }
    } else {
        stop("** ERROR ** unexpected grid.type *****")
    }
    return(weights.grid)
}

