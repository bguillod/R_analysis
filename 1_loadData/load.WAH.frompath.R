load.WAH.frompath <- function(path.in,
                              var,
                              months = "all",
                              lon.range,
                              lat.range,
                              rlon.range,
                              rlat.range,
                              daily = F,
                              rcm=F,
                              region=NA,
                              subfolders="",
                              filename="UmidFi.leDYMon.nc") {# e.g. region="eu_50km"

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function to load WAH runs from a path
    ## 
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

    require(ncdf4)
    require(ncdf4.helpers)
    
    if (FALSE) {
        source(file.path(loadData.path, "get.list.files.R"))
        experiment <- "fredi"
        years <- "all"
        umid <- "all"
        
        list.of.paths <- get.list.files(experiment, umid, years=1960)
        dim(list.of.paths)
        list.of.paths[1,]
        
        path.in <- as.character(list.of.paths[1,3])
        var <- "field106"
        rcm <- FALSE
        months <- 6:8
        lon.range <- c(35, 55)
        lat.range <- c(50, 60)
        daily <- F

        paths.in <- as.character(list.of.paths[1:120,3])
        
        source(file.path(r.infos.path, "cpdn.dims.R"))
        dims.gcm <- cpdn.dims("HadAM3P")
        lon.gcm <- dims.gcm$lon
        lat.gcm <- dims.gcm$lat
        nlon <- sum(lon.gcm>=lon.range[1] & lon.gcm<=lon.range[2])
        nlat <- sum(lat.gcm>=lat.range[1] & lat.gcm<=lat.range[2])
        data.out <- array(dim=c(nlon, nlat, length(months), length(paths.in)))
        for (i in 1:length(paths.in)) {
            data.out[,,,i] <- load.WAH.frompath(paths.in[i],
                                                var=var, months=months,
                                                lon.range=lon.range, lat.range=lat.range,
                                                rcm=F, daily=F)
        }

        map()
        rect(lon.range[1], lat.range[1], lon.range[2], lat.range[2], col=2)
        par(mfrow=c(5,3))
        for (i in 1:length(paths.in)) for (j in 1:3) if (all(is.na(data.out[,,j,i]))) plot(1) else image.plot(data.out[,,j,i])
        for (i in 1:length(paths.in)) for (j in 1:3) if (all(is.na(data.out[,,j,i]))) print(paste(i,j))

    }

    ## which months?
    if (any(months == "all")) months <- c(12, 1:11)
    
    ## get useful values for file name
    region.short <- strsplit(region, "_")[[1]][1]
    nchar.reg <- nchar(region.short)
    folder.name <- basename(path.in)
    umid <- substr(folder.name, 10+nchar.reg, 13+nchar.reg)
    year <- substr(folder.name, 15+nchar.reg, 18+nchar.reg)
    source(file.path(r.infos.path, "decade.letter.R"))
    dec <- sapply(as.numeric(year)+c(0,1), decade.letter)
    un <- (as.numeric(year)+c(0,1))%%10

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




