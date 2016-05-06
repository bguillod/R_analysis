make.ncdf.UK <- function(data.out,
                         times,
                         var.name,
                         file.out,
                         units,
                         missval=-1.073742e+09,
                         atts,
                         atts.glob,
                         daily=FALSE,
                         times.bnds,
                         calendar,
                         ref.file=file.path(marius.20CR.data.path, 1990, "monthly", paste0("hadrm3p20CRv2c_pepm_monthly_1990.nc"))) {
    
    
    if (FALSE) {
        
        source(file.path(r.generics.path, "6_exportFuncs/make.ncdf.UK.R"))
        
    }
    
    # load required packages
    library(ncdf4)
    library(ncdf4.helpers)
    
    # create correct time dimension
    if (missing(times)) {
        times <- attr(data.out, "times")
        if (is.null(times)) stop("** ERROR ** 'times' not available *****")
    }
    if (missing(times.bnds)) {
        if (!daily) {
            time.bnds <- as.POSIXct(paste(times, "1 00:00:00", sep="-"), format="%Y-%m-%d %H:%M:%S", tz = "UTC")
            time.bnds <- c(time.bnds, seq.POSIXt(rev(time.bnds)[1], by="1 month", length.out=2)[2])
        } else {
            time.bnds <- as.POSIXct(paste(times, "00:00:00"), format="%Y-%m-%d %H:%M:%S", tz = "UTC")
            time.bnds <- c(time.bnds, seq.POSIXt(rev(time.bnds)[1], by="1 day", length.out=2, tz = "UTC")[2])
        }
    }
    # check arguments
    if (missing(units)) stop("** ERROR ** units has to be given as an input ('' for no unit) *****")
    if (missing(missval)) warning("** no missval given - will use -1.073742e+09 *****")
    if (!missing(calendar)) stop("** argument calendar not implemented yet - now only for standard calendar *****")
    
    # read in template netcdf file
    nc.ref <- nc_open(ref.file)
    
    # prepare new dimensions
    old_dims <- nc.get.dim.names(nc.ref)
    new_dims <- list()
    for (d in 1:length(old_dims)) {
        if (old_dims[d] == "time") {
            dt.bnds <- difftime(time.bnds[-1], time.bnds[-length(time.bnds)], units="secs")
            time.mid <- time.bnds[-1]-dt.bnds/2
            year.orig <- format(time.mid[1], "%Y")
            time.orig <- as.POSIXct(paste0(year.orig, "-01-01 00:00:00"), format="%Y-%m-%d %H:%M:%S", tz = "UTC")
            time.diffs <- difftime(time.mid, time.orig, units="days")
            new_dims[[d]] <- ncdim_def(old_dims[d], paste0("days since ", year.orig, "-01-01 00:00:00"), as.numeric(time.diffs), unlim=TRUE)
            time.bnds.new <- difftime(time.bnds, time.orig, units="days")
            time.bnds.new <- rbind(rev(rev(time.bnds.new)[-1]),
                                   time.bnds.new[-1])
        } else {
            new_dims[[d]] <- ncdim_def(old_dims[d], nc.ref$dim[[old_dims[d]]]$units, nc.ref$dim[[old_dims[d]]]$vals)
        }
    }
    
    # prepare new variables and files
    new.dims.names <- sapply(new_dims, function(l) l$name)
    old_vars <- names(nc.ref$var)
    new_vars <- list()
    for (i in 1:length(old_vars)) {
        if (old_vars[i] == "pepm") {
            new_vars[[i]] <- ncvar_def(var.name, units, new_dims[c(2,1,3)], missval=missval)
        } else if (old_vars[i] == "time_bnds") {
            new_vars[[i]] <- ncvar_def(old_vars[i], nc.ref$var[[old_vars[i]]]$units, new_dims[match(sapply(nc.ref$var[[old_vars[i]]]$dim, function(l) l$name), new.dims.names)], prec=nc.ref$var[[old_vars[i]]]$prec)
        } else if (old_vars[i] == "rotated_latitude_longitude") {
            new_vars[[i]] <- ncvar_def(old_vars[i], nc.ref$var[[old_vars[i]]]$units, new_dims[match(sapply(nc.ref$var[[old_vars[i]]]$dim, function(l) l$name), new.dims.names)], prec=nc.ref$var[[old_vars[i]]]$prec)
        } else {
            new_vars[[i]] <- ncvar_def(old_vars[i], nc.ref$var[[old_vars[i]]]$units, new_dims[match(sapply(nc.ref$var[[old_vars[i]]]$dim, function(l) l$name), new.dims.names)], nc.ref$var[[old_vars[i]]]$missval, nc.ref$var[[old_vars[i]]]$longname, nc.ref$var[[old_vars[i]]]$prec)
        }
    }
    
    
    nc.new <- nc_create(file.out, new_vars)
    
    #         ncvar_put(nc.new, new_vars[[i]]$name, spei.20cr.data[[j]])
    # put data
    for (i in 1:length(new_vars)) { 
        if (new_vars[[i]]$name == var.name) {
            ncvar_put(nc.new, var.name, data.out[,dim(data.out)[2]:1,])
            nc.copy.atts(nc.ref, old_vars[i], nc.new, new_vars[[i]]$name, exception.list = c("long_name", "units", "_FillValue"))
            if (!missing(atts)) {
                for (j in 1:nrow(atts)) {
                    ncatt_put(nc.new, var.name, attname = atts$name[j], attval = atts$val[j])
                }
            }
        } else if (new_vars[[i]]$name == "time_bnds") {
            ncvar_put(nc.new, new_vars[[i]]$name, time.bnds.new)
            nc.copy.atts(nc.ref, old_vars[i], nc.new, new_vars[[i]]$name)
        } else if (new_vars[[i]]$name == "rotated_latitude_longitude") {
            nc.copy.atts(nc.ref, old_vars[i], nc.new, new_vars[[i]]$name)
        } else if (new_vars[[i]]$name == "time") {
#             nc.copy.atts(nc.ref, old_vars[i], nc.new, new_vars[[i]]$name, exception.list = c("long_name", "units"))
        } else {
            ncvar_put(nc.new, new_vars[[i]]$name, ncvar_get(nc.ref, new_vars[[i]]$name))
            nc.copy.atts(nc.ref, old_vars[i], nc.new, new_vars[[i]]$name)
        }
    }
    
    for (i in 1:length(new_dims)) { 
        if (new_dims[[i]]$name == "time") {
            nc.copy.atts(nc.ref, old_dims[i], nc.new, new_dims[[i]]$name, exception.list = c("units", "long_name"))
        } else {
        nc.copy.atts(nc.ref, old_dims[i], nc.new, new_dims[[i]]$name)
        }
    }
    if (!missing(atts.glob)) {
        for (j in 1:nrow(atts.glob)) {
            ncatt_put(nc.new, 0, atts.glob$name[j], atts.glob$val[j])
        }
    }
    
    # done - now close the file 
    nc_close(nc.new)
    
}