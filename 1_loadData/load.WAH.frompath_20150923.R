load.WAH.frompath <- function(path.in,
                              var,
                              months = "all",
                              lon.range,
                              lat.range,
                              daily = F,
                              rcm=F) {

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

    ## get useful values for file name
    folder.name <- tail(strsplit(path.in, "/")[[1]], 1)
    umid <- substr(folder.name, 12, 15)
    year <- substr(folder.name, 17, 20)
    source(file.path(r.infos.path, "decade.letter.R"))
    dec <- sapply(as.numeric(year)+c(0,1), decade.letter)
    un <- (as.numeric(year)+c(0,1))%%10
    source(file.path(r.infos.path, "cpdn.dims.R"))
    if (!rcm) {
        fil <- "ma.pc"
        dims.gcm <- cpdn.dims("HadAM3P")
        lon.gcm <- dims.gcm$lon
        lat.gcm <- dims.gcm$lat
    } else if (daily) {
        stop("** ERROR ** only for GCM in this script *****")
        fil <- "ga.pd"
    } else {
        stop("** ERROR ** only for GCM in this script *****")
       fil <- "ga.pe"
    }

    ## loop on months
    if (any(months == "all")) months <- 1:12
    nlon <- sum(lon.gcm>=lon.range[1] & lon.gcm<=lon.range[2])
    nlat <- sum(lat.gcm>=lat.range[1] & lat.gcm<=lat.range[2])
    data.out <- array(dim=c(nlon, nlat, length(months)))
    for (m in 1:length(months)) {
        ii <- ifelse(months[m]==12, 1, 2)
        ## file name
        file.name <- paste0(umid, fil, dec[ii], un[ii], tolower(month.abb[months[m]]), ".nc")
        file.in <- file.path(path.in, file.name)
        
        ## load data and dimensions
        if (!file.exists(file.in)) next
        nc <- nc_open(file.in)
        lon <- ncvar_get(nc, ifelse(rcm, "x", "longitude0"))
        if (any(lon != lon.gcm)) stop("** ERROR ** unexpected 'lon' dimension *****")
        lat <- ncvar_get(nc, ifelse(rcm, "y", "latitude0"))
        if (any(lat != lat.gcm)) stop("** ERROR ** unexpected 'lat' dimension *****")
        lon.in <- which(lon>=lon.range[1] & lon<=lon.range[2])
        lat.in <- which(lat>=lat.range[1] & lat<=lat.range[2])
        dat <- ncvar_get(nc, var,
                         start=c(lon.in[1], lat.in[1],1,1),
                         count=c(length(lon.in), length(lat.in),-1, -1))
        nc_close(nc)
        data.out[,,m] <- dat
    }
    
    attr(data.out, "lon") <- lon
    attr(data.out, "lat") <- lat
    attr(data.out, "month") <- months

    return(data.out)
}




