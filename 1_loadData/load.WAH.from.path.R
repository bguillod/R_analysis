load.WAH.from.path <- function(paths.in,
                               var,
                               months = "all",
                               lon.range,
                               lat.range,
                               rlon.range,
                               rlat.range,
                               daily = F,
                               rcm=F,
                               region=NA) {# e.g. region="eu_50km"

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function to load WAH runs from a path variable obtained from
    ## get.list.files.from.path
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

    require(ncdf4)
    require(ncdf4.helpers)
    
    if (FALSE) {
        
        source(file.path(loadData.path, "get.list.files.from.path.R"))
        files.data.path <- "/data/ouce-cpdn/nmassey/wah_data/OSTIA_global/2011"
        test.neil <- get.list.files.from.path(files.data.path)
        test.raw <- get.list.files.from.path("/data/ouce-cpdn/nmassey/wah_data/OSTIA_natural/2011")

        paths.in <- test.raw
        var <- "field16"
        months <- "all"
        daily <- FALSE
        rcm <- FALSE
        region <- NA
        
    }

    cpdn.data.type <- attr(paths.in, "cpdn.data.type")
    if (is.na(cpdn.data.type) | is.null(cpdn.data.type)) {
        stop("** ERROR ** unavailable attributes in 'paths.in' : 'cpdn.data.type' *****")
    }
        
    ## which months?
    if (any(months == "all")) months <- c(12, 1:11)
    
    ## get useful values for file name
    get.files.names <- function(run.path, var, months, daily, rcm, cpdn.data.type) {
        source(file.path(r.infos.path, "decade.letter.R"))
        ## get basic info
        if (!rcm) {
            fil <- "ma.pc"
        } else if (daily) {
            fil <- "ga.pd"
        } else {
            fil <- "ga.pe"
        }
        file.list <- list()
        for (i in 1:length(run.path)) {
            file.list[[i]] <- vector(mode="character", length=length(months))
            for (m in 1:length(months)) {
                year.m <- as.numeric(run.path$year[i])+ifelse(months[m]==12, 0, 1)
                d <- decade.letter(year.m)
                dy <- paste0(d, substr(year.m, 4, 4))
                if (cpdn.data.type == "raw") {
                    file.list[[i]][m] <- paste0(run.path$dirs[i], "/", run.path$fnames[i], "/", run.path$umid[i], fil, dy, tolower(month.abb[months[m]]), ".nc")
                } else if (cpdn.data.type == "neil") {
                    file.list[[i]][m] <- paste0(run.path$dirs[i], "/", run.path$fnames[i], "/", fil, "/", var, "/", run.path$umid[i], fil, dy, tolower(month.abb[months[m]]), "_", var, ".nc")
                } else {
                    stop("** ERROR ** unexpected value for 'cpdn.data.type' *****")
                }
            }
        }
        return(file.list)
    }
    
    files.names <- get.files.names(paths.in, var=var, months=months, daily=daily, rcm=rcm, cpdn.data.type=cpdn.data.type)


    get.data.str <- function(file.in, var) {
        ## load a data sample grid information
        nc <- nc_open(file.in)
        ## data size
        dat <- ncvar_get(nc, var)
        ## dimensions:
        x <- nc.get.dim.for.axis(nc, var, "X")
        y <- nc.get.dim.for.axis(nc, var, "Y")
        z <- nc.get.dim.for.axis(nc, var, "Z")
        t <- nc.get.dim.for.axis(nc, var, "T")
        if (length(dim(dat)) != ifelse(is.na(z[1]), 3, 4)) stop("** ERROR ** no 'z' coordinate but more than 3 dimensions *****")
        ## rotated grid?
        grid.mapping <- ncatt_get(nc, var, "grid_mapping")
        if (grid.mapping$hasatt) {
            if (ncatt_get(nc,grid.mapping$value, "grid_mapping_name")$value != "rotated_latitude_longitude") stop("** ERROR ** 'grid_mapping' attr exists but not 'rotated_latitude_longitude' *****")
            grid.type <- "rotpol"
            plat <- ncatt_get(nc,grid.mapping$value, "grid_north_pole_latitude")$value
            plon <- ncatt_get(nc,grid.mapping$value, "grid_north_pole_longitude")$value
            dat <- nc.get.coordinate.axes(nc, var)
            lon <- ncvar_get(nc, names(dat)[1])
            lat <- ncvar_get(nc, names(dat)[2])
            rlon <- x$vals
            rlat <- y$vals
            grid.args <- list(grid.type=grid.type, plon=plon, plat=plat,
                              rlon=rlon, rlat=rlat, lon=lon, lat=lat)
            rm(dat, grid.mapping)
        } else {
            grid.type <- "lonlat"
            grid.args <- list(grid.type=grid.type, lon=x$vals, lat=y$vals)
        }
        nc_close(nc)
        if (!is.na(z[1])) {
            grid.args <- c(grid.args, z=z$vals)
            output <- list(dim=dim(dat), x=x$vals, y=y$vals, z=z$vals, grid.args=grid.args)
        } else {
            output <- list(dim=dim(dat), x=x$vals, y=y$vals, grid.args=grid.args)
        }
        return(output)
    }
    data.str <- get.data.str(files.names[[1]][1], var)
    if (rcm & data.str$grid.args$grid.type!="rotpol") stop("** ERROR ** region but grid type is not rotpol *****")













    
    

    ## load generic dimension and grid information and check
    source(file.path(r.infos.path, "cpdn.dims.R"))
    source(file.path(r.infos.path, "degree.adjustRange.R"))
    if (!rcm) {
        dims.gcm <- cpdn.dims("HadAM3P")
        lat.model <- dims.gcm$lat
        lon.model <- dims.gcm$lon
        if (any(abs(x$vals-lon.model) > abs(mean(diff(lon.model))/10))) stop("** ERROR ** unexpected 'lon' dimension *****")
        if (any(abs(y$vals-lat.model) > abs(mean(diff(lat.model))/10))) stop("** ERROR ** unexpected 'lat' dimension *****")
        
        if (missing(lon.range) | missing(lat.range)) {
            if (!(missing(rlon.range) & missing(rlat.range))) {
                stop("** ERROR ** RCM=F but only rotated coordinates range is specified *****")
            }
            lon.in <- 1:length(lon.model)
            lat.in <- 1:length(lat.model)
        } else {
            lon.in <- (findInterval(lon.model, lon.range) == 1)
            lat.in <- (findInterval(lat.model, lat.range) == 1)
        }
        lon.out <- lon.model[lon.in]
        if (!all(lon.out == cummax(lon.out))) {
            stop("** only works for continuous longitude in GCM **")
        }
        lat.out <- lat.model[lat.in]
        invlat <- !all(lat.out == cummax(lat.out))
        templat <- if (invlat) rev(lat.out) else lat.out
        nlon <- sum(lon.in)
        nlat <- sum(lat.in)
    } else if (is.na(region)) {
        warning("** no specific region information **")
    } else {
        dims.rcm <- cpdn.dims(paste("HadRM3P", region, sep="_"))
        lon.model <- dims.rcm$rlon
        lat.model <- dims.rcm$rlat
        if (missing(rlon.range) | missing(rlat.range)) {
            if (!(missing(lon.range) & missing(lat.range))) {
                stop("** ERROR ** RCM=T but only non-rotated coordinates range specified *****")
            }
            lon.in <- rep(TRUE, length(lon.model))
            lat.in <- rep(TRUE, length(lat.model))
        } else {
            lon.in <- (findInterval(lon.model, rlon.range) == 1)
            lat.in <- (findInterval(lat.model, rlat.range) == 1)
        }
        lon.out <- lon.model[lon.in]
        lat.out <- lat.model[lat.in]
        invlat <- !all(lat.out == cummax(lat.out))
        rlat <- if (invlat) rev(lat.out) else lat.out
        nlon <- sum(lon.in)
        nlat <- sum(lat.in)
        



















    
    ## get dimensions and file name
    source(file.path(r.infos.path, "cpdn.dims.R"))
    source(file.path(r.infos.path, "degree.adjustRange.R"))
    if (!rcm) {
        fil <- "ma.pc"
        dims.gcm <- cpdn.dims("HadAM3P")
        lat.model <- dims.gcm$lat
        if (missing(lon.range) | missing(lat.range)) {
            if (!(missing(rlon.range) & missing(rlat.range))) {
                stop("** ERROR ** RCM=F but only rotated coordinates range is specified *****")
            }
            lon.in <- 1:length(lon.model)
            lat.in <- 1:length(lat.model)
        } else {
            lon.in <- (findInterval(lon.model, lon.range) == 1)
            lat.in <- (findInterval(lat.model, lat.range) == 1)
        }
        lon.out <- lon.model[lon.in]
        if (!all(lon.out == cummax(lon.out))) {
            stop("** only works for continuous longitude in GCM **")
        }
        lat.out <- lat.model[lat.in]
        invlat <- !all(lat.out == cummax(lat.out))
        templat <- if (invlat) rev(lat.out) else lat.out
        nlon <- sum(lon.in)
        nlat <- sum(lat.in)
        data.out <- array(dim=c(nlon, nlat, length(months)))
        data.out <- put.atts(to=data.out,
                             atts=list(lon=lon.out,
                                 lat=templat,
                                 grid.type=dims.gcm$grid.type))
    } else if (daily) {
        stop("** ERROR ** only for GCM or monthly in this script *****")
        fil <- "ga.pd"
    } else {
        fil <- "ga.pe"
        dims.rcm <- cpdn.dims(paste("HadRM3P", region, sep="_"))
        lon.model <- dims.rcm$rlon
        lat.model <- dims.rcm$rlat
        if (missing(rlon.range) | missing(rlat.range)) {
            if (!(missing(lon.range) & missing(lat.range))) {
                stop("** ERROR ** RCM=T but only non-rotated coordinates range specified *****")
            }
            lon.in <- rep(TRUE, length(lon.model))
            lat.in <- rep(TRUE, length(lat.model))
        } else {
            lon.in <- (findInterval(lon.model, rlon.range) == 1)
            lat.in <- (findInterval(lat.model, rlat.range) == 1)
        }
        lon.out <- lon.model[lon.in]
        lat.out <- lat.model[lat.in]
        invlat <- !all(lat.out == cummax(lat.out))
        rlat <- if (invlat) rev(lat.out) else lat.out
        nlon <- sum(lon.in)
        nlat <- sum(lat.in)
        
        data.out <- array(dim=c(nlon, nlat, length(months)))
        data.out <- put.atts(to=data.out,
                             atts=list(rlon=lon.out,
                                 rlat=rlat,
                                 plon=dims.rcm$plon,
                                 plat=dims.rcm$plat,
                                 lon = dims.rcm$lon[lon.in, rev(which(lat.in))],
                                 lat = dims.rcm$lat[lon.in, rev(which(lat.in))],
                                 grid.type=dims.rcm$grid.type))
    }

    ## loop on months
    for (m in 1:length(months)) {
        ii <- ifelse(months[m]==12, 1, 2)
        ## file name
        file.name <- paste0(umid, fil, dec[ii], un[ii], tolower(month.abb[months[m]]), ".nc")
        file.in <- file.path(path.in, file.name)
        
        ## load data and dimensions
        if (!file.exists(file.in)) next
        nc <- nc_open(file.in)
#        lon <- ncvar_get(nc, ifelse(rcm, "x", "longitude0"))
        lon <- ncvar_get(nc,  "longitude0")
        if (any(abs(lon-lon.model) > abs(mean(diff(lon.model))/10))) stop("** ERROR ** unexpected 'lon' dimension *****")
        lat <- ncvar_get(nc, "latitude0")
        if (any(abs(lat-lat.model) > abs(mean(diff(lat.model))/10))) stop("** ERROR ** unexpected 'lat' dimension *****")
        dat <- ncvar_get(nc, var,
                         start=c(lon.in[1], lat.in[1],1,1),
                         count=c(length(lon.in), length(lat.in),-1, -1))
        nc_close(nc)
        if (invlat) {
            data.out[,nlat:1,m] <- dat
        } else {
            data.out[,,m] <- dat
        }
    }
    
    attr(data.out, "month") <- months

    return(data.out)
}




