multi.map.plot <- function(z, x, y,
                           plot.inds,
                           titles=rep("", length(plot.inds)), nrows, col=tim.colors, breaks,
                           file.out=NULL, width=7, height=7,
                           map.database="world", map.interior=TRUE, legend.name="",
                           axis.args,
                           ...) {
    ## -------------------------------------------------------------------------------------
    ## multi.plot.map(mats, titles, nrows, col, breaks, file.out, width=7, height=7, ...) 
    ##         -----> function to make multiple maps and print in 'file.out'
    ##         -----> 'breaks' and 'col' must be the same
    ##         -----> the i-th map is the map of mats[,,i] or mats[[i]]
    ##         -----> maps are plotted by row
    ## -------------------------------------------------------------------------------------

    ## load required functions and packages
    source(file.path(r.generics.path, "4_mapFuncs/my.image.plot.R"))
    library(geocors)
    library(fields)

    if (missing(plot.inds)) {
        plot.inds <- if (is.list(z)) 1:length(z) else 1:(dim(z)[3])
    }


    ## convert z to a list
    if (!is.list(z)) {
        if (length(dim(z))==3) {
            z.old <- z
            temp <- get.atts(z)
            z <- list()
            for (i in 1:length(plot.inds)) {
                z[[i]] <- z.old[,,plot.inds[i]]
#                temp$dim <- dim(z[[i]])
                z[[i]] <- put.atts(to=z[[i]], temp)
            }
            z[[i]] <- put.atts(to=z, temp)
            nmaps <- i
        } else {
            stop("** ERROR ** 'z' should be a list of matrices to plot *****")
        }
    } else {
        nmaps <- min(c(length(z), length(plot.inds)))
    }

    ## get X and Y dimensions
    if (missing(x)) {
        x <- get.grid.atts(z, what="grid.cors")[[1]]
        if (is.null(x)) x <- 1:dim(z)[1]
        
    }
    if (missing(y)) {
        y <- get.grid.atts(z, what="grid.cors")[[2]]
        if (is.null(y)) y <- 1:dim(z)[2]
    }

    ## defines layout parameters
    if (missing(nrows)) {
        if (nmaps == 1 | is.na(nmaps)) {
            nrows <- ncols <- 1
            mats <- 1:2
        } else if (nmaps == 30) {
            nrows <- 5
            ncols <- 6
            mats <- c(1:30, rep(31, ncols))
        } else if (nmaps == 28) {
            nrows <- 5
            ncols <- 6
            mats <- c(1:28, nmaps+2, nmaps+2, rep(nmaps+1, ncols))
        } else {
            ncols <- ceiling(sqrt(nmaps))
            nrows <- ceiling(nmaps/ncols)
            miss.pan <- nrows*ncols-nmaps
            mats <- c(1:nmaps, rep(nmaps+2, miss.pan), rep(nmaps+1, ncols))
        }
    } else {
        ncols <- ceiling(nmaps/nrows)
        miss.pan <- nrows*ncols-nmaps
        mats <- c(1:nmaps, rep(nmaps+2, miss.pan), rep(nmaps+1, ncols))
    }

    ## define figure dimensions
    if (missing(width)) {
        if (missing(height)) {
            maxdim <- 10
            if (maxdim/(ncols+0.3) > maxdim/nrows) {
                width <- maxdim
                figsize <- width/(ncols+0.3)
                height <- figsize*nrows
            } else {
                height <- maxdim
                figsize <- height/nrows
                width=(ncols+0.3)*figsize
            }
        } else {
            figsize <- height/nrows
            width=(ncols+0.3)*figsize
        }
    } else if (missing(height)) {
        figsize <- width/(ncols+0.3)
        height=figsize*nrows
    }

    ## define breaks
    if (missing(breaks)) {
        breaks <- range(sapply(z, range, na.rm=TRUE))
        breaks <- pretty(breaks, 15)
        breaks <- breaks[-c(1, length(breaks))]
    }

    if (!is.null(file.out)) ps.print(file=file.out, height=height, width=width)
    ## do plot
    tryCatch({
        layout.args <- list(mat=matrix(mats, ncol=ncols, nrow=nrows+1, byrow=TRUE), heights=c(rep(3, nrows), 1))
        legend.args <- list(col=col, breaks=breaks)
        plot.args <- c(legend.args, list(x=x, y=y, use.plt=FALSE, add.legend=FALSE), list(...))
        grid.atts <- get.grid.atts(z)
        source(file.path(mapFuncs.path, "map.add.R"))
        map.data <- list()
        for (i in 1:length(map.database)) {
            map.data[[i]] <- map.add(grid.atts, database=map.database[[i]], add=FALSE, interior=map.interior)
        }
        do.call(layout, layout.args)
        par(mar=c(0.5,0.5,2,0.5))
        for (i in 1:nmaps) {
            do.call(my.image.plot, c(list(z=z[[i]], main=titles[i]), plot.args))
            for (j in 1:length(map.database)) {
                lines(map.data[[j]])
            }
        }
        if (missing(axis.args)) {
            axis.args <- list(mgp=c(3,0.5,0))
        }
        my.image.plot(z=z[[i]], x=x, y=y, breaks=breaks, col=col, use.plt=FALSE, legend.only=TRUE,
                      smallplot=c(0.1,0.9,0.5,0.7), axis.args=axis.args,
                      legend.args=list(text=legend.name, line=0.5), horizontal=TRUE)
        par(mar=c(3,0,2,4))
        if (!is.null(file.out)) dev.off()
    }, error = function(e) {
        if (!is.null(file.out)) dev.off()
        print("plotting error is caught")
        stop(e)
    })
}
