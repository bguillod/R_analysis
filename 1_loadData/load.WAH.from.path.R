load.WAH.from.path <- function(paths.in,
                               var,
                               months = "all",
                               daily = F,
                               rcm=F,
                               lon.range,
                               lat.range,
                               rlon.range,
                               rlat.range,
                               lonlat.range.filename) {# e.g. region="eu_50km"

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function to load WAH runs from a path variable obtained from
    ## get.list.files.from.path
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

    require(ncdf4)
    require(ncdf4.helpers)
    library(plotmap)
    
    if (FALSE) {
        source(file.path(loadData.path, "load.WAH.from.path.R"))
        
        source(file.path(loadData.path, "get.list.files.from.path.R"))
        files.data.path <- "/data/ouce-cpdn/nmassey/wah_data/OSTIA_global/2011"
        test.neil <- get.list.files.from.path(files.data.path)
        test.raw <- get.list.files.from.path("/data/ouce-cpdn/nmassey/wah_data/OSTIA_natural/2011")

        paths.in <- test.raw
        var <- "field16"
        var <- "field8_1"
        months <- "all"
        daily <- FALSE
        rcm <- FALSE
        region <- NA

        data.neil <- load.WAH.from.path(test.neil[1:5,],var,months,daily,rcm)
        
        data.raw <- load.WAH.from.path(test.raw[1:5,],var,months,daily,rcm)
        raw
    }

    cpdn.data.type <- attr(paths.in, "cpdn.data.type")
    if (is.na(cpdn.data.type) | is.null(cpdn.data.type)) {
        stop("** ERROR ** unavailable attributes in 'paths.in' : 'cpdn.data.type' *****")
    }
        
    ## which months?
    if (any(months == "all")) months <- c(12, 1:11)
    
    ## get useful values for file name
    get.files.names <- function(run.path, var, months, daily, rcm, cpdn.data.type,
                                lonlat.range.filename) {
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
        for (i in 1:nrow(run.path)) {
            file.list[[i]] <- vector(mode="character", length=length(months))
            for (m in 1:length(months)) {
                year.m <- run.path$year[i]
                year.m <- as.numeric(substr(year.m, 1, 4))+ifelse(months[m]==12, 0, 1)
                d <- decade.letter(year.m)
                dy <- paste0(d, substr(year.m, 4, 4))
                if (cpdn.data.type == "raw") {
                    file.list[[i]][m] <- paste0(run.path$dirs[i], "/", run.path$fnames[i], "/", run.path$umid[i], fil, dy, tolower(month.abb[months[m]]), ".nc")
                } else if (cpdn.data.type == "neil") {
                    if (!missing(lonlat.range.filename)) {
                        var.fol <- paste0(var, "_W", lonlat.range.filename[1], "_N", lonlat.range.filename[2], "_W", lonlat.range.filename[3], "_N", lonlat.range.filename[4])
                    } else {
                        var.fol <- var
                    }
                    file.list[[i]][m] <- paste0(run.path$dirs[i], "/", run.path$fnames[i], "/", fil, "/", var.fol, "/", run.path$umid[i], fil, dy, tolower(month.abb[months[m]]), "_", var.fol, ".nc")
                } else {
                    stop("** ERROR ** unexpected value for 'cpdn.data.type' *****")
                }
            }
        }
        return(file.list)
    }
    
    files.names <- get.files.names(paths.in, var=var, months=months, daily=daily, rcm=rcm, cpdn.data.type=cpdn.data.type, lonlat.range.filename=lonlat.range.filename)


    get.data.file.str <- function(files.in,
                             var) {
        ## load a data sample grid information
        file.in <- files.in[which(file.exists(files.in))[1]]
        nc <- nc_open(file.in)
        ## data size
        dat <- ncvar_get(nc, var)
        ## dimensions:
        x <- nc.get.dim.for.axis(nc, var, "X")
        y <- nc.get.dim.for.axis(nc, var, "Y")
        z <- nc.get.dim.for.axis(nc, var, "Z")
        t <- nc.get.dim.for.axis(nc, var, "T")
        ndims.expected <- ifelse(is.na(z[1]), 2+!(t$len==1), 2+!(z$len==1)+!(t$len==1))
        if (length(dim(dat)) == ndims.expected) {
            dim.names <- c("X", "Y", "Z", "T")[c(TRUE, TRUE, !is.na(z[1]), !(t$len==1))]
        } else {
            warning("** ERROR ** no 'z' coordinate but more than 3 dimensions - the additional dimension is pretended to be 'z' *****")
            z <- list(TRUE, vals=NA)
            dim.names <- c("X", "Y", "Z", "T")[c(TRUE, TRUE, TRUE, !(t$len==1))]
        }
        ## rotated grid?
        grid.mapping <- ncatt_get(nc, var, "grid_mapping")
        if (grid.mapping$hasatt) {
            ## rotated grid
            if (ncatt_get(nc,grid.mapping$value, "grid_mapping_name")$value != "rotated_latitude_longitude") stop("** ERROR ** 'grid_mapping' attr exists but not 'rotated_latitude_longitude' *****")
            grid.type <- "rotpol"
            plat <- ncatt_get(nc,grid.mapping$value, "grid_north_pole_latitude")$value
            plon <- ncatt_get(nc,grid.mapping$value, "grid_north_pole_longitude")$value
            datc <- nc.get.coordinate.axes(nc, var)
            lon <- ncvar_get(nc, names(datc)[1])
            lat <- ncvar_get(nc, names(datc)[2])
            rlon <- x$vals
            rlat <- y$vals
            rm(datc, grid.mapping)
            grid.args <- list(grid.type=grid.type, rlon=x$vals, rlat=y$vals, plon=plon, plat=plat, lon=lon, lat=lat)
        } else {
            ## non-rotated grid
            grid.type <- "lonlat"
            grid.args <- list(grid.type=grid.type, lon=x$vals, lat=y$vals)
        }
        nc_close(nc)
        if (!is.na(z[1])) {
            grid.args <- c(grid.args)
            output <- list(dim=dim(dat), dim.names=dim.names, x=x$vals, y=y$vals, z=z$vals, t=t$vals, grid.args=grid.args)
        } else {
            output <- list(dim=dim(dat), dim.names=dim.names, x=x$vals, y=y$vals, t=t$vals, grid.args=grid.args)
        }
        return(output)
    }
    ## data.str <- get.data.file.str(files.names[[1]][1], var)
    ## if (rcm & data.str$grid.args$grid.type!="rotpol") stop("** ERROR ** region but grid type is not rotpol *****")


    get.loadArgs <- function(files.names,
                             lon.range,
                             lat.range,
                             rlon.range,
                             rlat.range) {

        ## data shape in files
        in.data.str <- get.data.file.str(files.names[[1]][1], var)
        ## success <- FALSE
        ## while(!success) {
        ##     tryCatch(
        ##     in.data.str <- get.data.file.str(files.names[[1]], var)
        ## }
        if (rcm & in.data.str$grid.args$grid.type!="rotpol") stop("** ERROR ** region but grid type is not rotpol *****")
        ## needed functions
        source(file.path(r.infos.path, "degree.adjustRange.R"))
        ## rotated coords?
        if (in.data.str$grid.args$grid.type == "rotpol") {
            if (missing(rlon.range) | missing(rlat.range)) {
                if (!(missing(lon.range) & missing(lat.range))) {
                    stop("** ERROR ** RCM=T but only non-rotated coordinates range specified *****")
                }
                x.in <- rep(TRUE, length(in.data.str$x))
                y.in <- rep(TRUE, length(in.data.str$y))
            } else {
                x.in <- (findInterval(in.data.str$x, rlon.range) == 1)
                y.in <- (findInterval(in.data.str$y, rlat.range) == 1)
            }
            rlon.out <- in.data.str$x[x.in]
            rlat.out <- in.data.str$y[y.in]
            x.order <- order(degree.adjustRange(rlon.out,range.out=c(-180,180)))
            y.order <- order(rlat.out)
            rlon.out <- rlon.out[x.order]
            rlat.out <- rlat.out[y.order]
            lon.out <- in.data.str$grid.args$lon[which(x.in)[x.order], which(y.in)[y.order]]
            lat.out <- in.data.str$grid.args$lat[which(x.in)[x.order], which(y.in)[y.order]]
            grid.args <- list(grid.type=in.data.str$grid.args$grid.type,
                              plon=in.data.str$grid.args$plon, plat=in.data.str$grid.args$plat,
                              rlon=rlon.out, rlat=rlat.out,
                              lon=lon.out, lat=lat.out)
            dims <- c(length(rlon.out), length(rlat.out), in.data.str$dim[-(1:2)])
            load.args <- list(dims=dims, dim.names=in.data.str$dim.names, x.in=x.in, y.in=y.in,
                              x.order=x.order, y.order=y.order,
                              grid.args=grid.args)
        } else if (in.data.str$grid.args$grid.type == "lonlat") {
            if (missing(lon.range) | missing(lat.range)) {
                if (!(missing(rlon.range) & missing(rlat.range))) {
                    stop("** ERROR ** RCM=F but only rotated coordinates range specified *****")
                }
                x.in <- rep(TRUE, length(in.data.str$x))
                y.in <- rep(TRUE, length(in.data.str$y))
            } else {
                x.in <- (findInterval(in.data.str$x, lon.range) == 1)
                y.in <- (findInterval(in.data.str$y, lat.range) == 1)
            }
            lon.out <- degree.adjustRange(in.data.str$x[x.in],range.out=c(-180,180))
            lat.out <- in.data.str$y[y.in]
            x.order <- order(lon.out)
            y.order <- order(lat.out)
            lon.out <- lon.out[x.order]
            lat.out <- lat.out[y.order]
            grid.args <- list(grid.type=in.data.str$grid.args$grid.type,
                              lon=lon.out, lat=lat.out)
            dims <- c(length(lon.out), length(lat.out), in.data.str$dim[-(1:2)])
            load.args <- list(dims=dims, dim.names=in.data.str$dim.names, x.in=x.in, y.in=y.in,
                              x.order=x.order, y.order=y.order,
                              grid.args=grid.args)
        } else {
            stop("** ERROR ** unexpected value in in.data.str$grid.args$grid.type *****")
        }
        return(load.args)
    }
    load.args <- get.loadArgs(files.names,
                              lon.range,
                              lat.range,
                              rlon.range,
                              rlat.range)

    ## Output array: get dimension, create
    nruns <- length(files.names)
    ntimesteps.file <- ifelse(any(load.args$dim.names == "T"),
                              load.args$dims[load.args$dim.names == "T"],
                              1)
    nfiles.per.run <- unique(sapply(files.names, length))
    if (length(nfiles.per.run) > 1) stop("** ERROR ** not always the same number of files *****")
    ntimesteps <- ntimesteps.file*nfiles.per.run
    dim.out <- c(load.args$dims[load.args$dim.names != "T"], ntimesteps, nruns)
    ts.starts <- seq(1, by=ntimesteps.file, length.out=nfiles.per.run)
    ts.inds <- lapply(ts.starts, seq, length.out=ntimesteps.file)
    data.out <- array(dim=dim.out)
    
    ## load data
    for (r in 1:nruns) {
        for (m in 1:nfiles.per.run) {
            if (!file.exists(files.names[[r]][m])) next
            ## load data
            nc <- nc_open(files.names[[r]][m])
            ## data size
            if (!all(diff(which(load.args$x.in)) ==1)) {
                stop("** ERROR ** lon or rlon not continuousl")
            }
            dat <- ncvar_get(nc, var,
                             start=c(which(load.args$x.in)[1], which(load.args$y.in)[1],1,1),
                             count=c(sum(load.args$x.in), sum(load.args$y.in),-1, -1))
            if (length(dim.out) == 4) {
                if (length(ts.inds[[m]]) == 1) {
                    data.out[,,ts.inds[[m]], r] <- dat[load.args$x.order,
                                                       load.args$y.order]
                } else {
                    data.out[,,ts.inds[[m]], r] <- dat[load.args$x.order,
                                                       load.args$y.order, ]
                }
            } else if (length(dim.out) == 5) {
                if (length(ts.inds[[m]]) == 1) {
                    data.out[,,,ts.inds[[m]], r] <- dat[load.args$x.order,
                                                       load.args$y.order, ]
                } else {
                    data.out[,,,ts.inds[[m]], r] <- dat[load.args$x.order,
                                                       load.args$y.order, , ]
                }
            } else {
                stop("** ERROR ** dimension of output data unexpected *****")
            }
            nc_close(nc)
        }
    }


    ## Put attributes
    data.out <- put.atts(to=data.out, atts=load.args$grid.args)
    attr(data.out, "umid") <- paths.in$umid

    ## done
    return(data.out)
}




