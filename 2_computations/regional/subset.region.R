
subset.region <- function(obj, to) {

    ## -------------------------------------------------------------------------------------
    ## function to subset the domain of an array configured following the geocors conventions
    ## to is a list of the domain to subset on, i.e.:
    ##          either list(rlon.range=c(....), rlat.range=c(....)) for grid.type="rotpol"
    ##          or list(lon.range=c(....), lat.range=c(....)) for grid.type="lonlat"
    ## -------------------------------------------------------------------------------------
    
    if (FALSE) {
        source(file.path(compute.path, "regional/subset.region.R"))
    }

    if (length(to) != 2) stop("** ERROR ** 'to' should be of length 2 *****")
    atts.obj <- get.atts(obj)
    ndims <- length(dim(obj))
    if (atts.obj$grid.type == "rotpol") {
        if (any(names(to) != paste(c("rlon", "rlat"), "range", sep="."))) stop("** ERROR ** 'to' is not a rlon and rlat range but grid.type is 'rotpol' *****")
        if (!all(diff(atts.obj$rlon) > 0)) stop("** ERROR ** rlon is not increasing *****")
        if (!all(diff(atts.obj$rlat) > 0)) stop("** ERROR ** rlat is not increasing *****")
        prec.dec <- min(abs(diff(atts.obj$rlon)))/10
        rlon.in <- (atts.obj$rlon < to$rlon.range[2]+prec.dec) & (atts.obj$rlon > to$rlon.range[1]-prec.dec)
        rlat.in <- (atts.obj$rlat < to$rlat.range[2]+prec.dec) & (atts.obj$rlat > to$rlat.range[1]-prec.dec)
        atts.obj$rlon <- atts.obj$rlon[rlon.in]
        atts.obj$rlat <- atts.obj$rlat[rlat.in]
        atts.obj$lon <- atts.obj$lon[rlon.in, rlat.in]
        atts.obj$lat <- atts.obj$lat[rlon.in, rlat.in]
        if (ndims == 3) {
            obj.sub <- obj[rlon.in, rlat.in,]
        } else if (ndims == 4) {
            obj.sub <- obj[rlon.in, rlat.in,,]
        } else {
            stop("** ERROR ** ndim(obj)>4 (could be easily adapted in script) *****")
        }
        ## this does not work: obj.sub <- apply(obj, 3:length(dim(obj)), function(a) a[rlon.in, rlat.in, drop=FALSE])
        obj.sub <- put.atts(obj.sub, atts=atts.obj)
    } else if (atts.obj$grid.type == "lonlat") {
        if (any(names(to) != paste(c("lon", "lat"), "range", sep="."))) stop("** ERROR ** 'to' is not a lon and lat range but grid.type is 'lonlat' *****")
        if (!all(diff(atts.obj$lon) > 0)) stop("** ERROR ** lon is not increasing *****")
        if (!all(diff(atts.obj$lat) > 0)) stop("** ERROR ** lat is not increasing *****")
        prec.dec <- min(abs(diff(atts.obj$lon)))/10
        lon.in <- (atts.obj$lon < to$lon.range[2]+prec.dec) & (atts.obj$lon > to$lon.range[1]-prec.dec)
        lat.in <- (atts.obj$lat < to$lat.range[2]+prec.dec) & (atts.obj$lat > to$lat.range[1]-prec.dec)
        atts.obj$lon <- atts.obj$lon[lon.in]
        atts.obj$lat <- atts.obj$lat[lat.in]
        if (ndims == 3) {
            obj.sub <- obj[lon.in, lat.in,]
        } else if (ndims == 4) {
            obj.sub <- obj[lon.in, lat.in,,]
        } else {
            stop("** ERROR ** ndim(obj)>4 (could be easily adapted in script) *****")
        }
        ## this does not work: obj.sub <- apply(obj, 3:length(dim(obj)), function(a) a[lon.in, lat.in, drop=FALSE])
        obj.sub <- put.atts(obj.sub, atts=atts.obj)
    } else {
        stop("** ERROR ** unexpected value in attribute 'grid.type' *****")
    }
    return(obj.sub)
}
