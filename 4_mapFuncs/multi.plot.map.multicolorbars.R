multi.plot.map.multicolorbars <- function(mats, titles=names(mats), nrow, col, breaks,
                                          bigplots=c(0,0.83, 0,0.9), smallplots=c(0.85,0.88,0.1,0.8),
                                          file.out, width=7, height=7, ...) {
    ## -------------------------------------------------------------------------------------
    ## multi.plot.map.multicolorbars(mats, titles, nrow, col, breaks, file.out, width=7, height=7, ...) 
    ##         -----> function to make multiple maps and print in 'file.out'
    ##         -----> 'breaks' and 'col' can be lists if different ranges
    ##         -----> the i-th map is the map of mats[,,i] or mats[[i]]
    ##         -----> maps are plotted by row
    ## -------------------------------------------------------------------------------------


    require(plotmap)
    source(file.path(mapFuncs.path, "my.plot.map.R"))
    if (!is.list(mats)) {
        if (length(dim(mats))==3) {
            mats.old <- mats
            temp <- attributes(mats)
            mats <- list()
            for (i in 1:(dim(mats.old)[3])) {
                mats[[i]] <- mats.old[,,i]
                temp$dim <- dim(mats[[i]])
                attributes(mats[[i]]) <- temp
                
            }
            nmaps <- i
        } else {
            stop("** ERROR ** 'mats' should be a list of matrices to plot *****")
        }
    } else {
        nmaps <- length(mats)
    }
    if (missing(col)) col <- cool2warm.colors
    if (missing(breaks)) {
        temp <- sapply(mats, range, na.rm=TRUE)
        temp <- range(temp, na.rm=T)
        if (is.vector(col) | is.list(col)) {
            if (is.vector(col)) {
                temp <- seq(temp[1], temp[2], length.out=length(col))
                for (i in 1:nmaps) breaks[[i]] <- temp
                rm(temp)
            } else {
                for (i in 1:nmaps) breaks[[i]] <- seq(temp[1], temp[2], length.out=length(col[[i]]))
            }
        } else {
            if (is.function(col)) {
                nbrk <- rep(11, nmaps)
            } else if (is.list(col)) {
                nbrk <- sapply(col, length)
            } else {
                stop("** ERROR ** unexpected format of col for breaks definition *****")
            }
            breaks <- lapply(nbrk, function(s) seq(temp[1], temp[2], length.out=s))
        }
    } else if (!is.list(breaks)) {
        temp <- breaks
        breaks <- list()
        for (i in 1:nmaps) breaks[[i]] <- temp
        rm(temp)
    }
    if (is.function(col)) {
        temp <- col
        col <- list()
        for (i in 1:length(breaks)) col[[i]] <- temp(length(breaks[[i]])+1)
    } else if (!is.list(col)) {
        temp <- col
        col <- list()
        for (i in 1:nmaps) col[[i]] <- temp
        rm(temp)
    }     
    ncol <- ceiling(nmaps/nrow)
    if (missing(width)) {
        if (missing(height)) {
            maxdim <- 10
            if (maxdim/(ncol+0.3) > maxdim/nrow) {
                width <- maxdim
                figsize <- width/(ncol+0.3)
                height <- figsize*nrow
            } else {
                height <- maxdim
                figsize <- height/nrow
                width=(ncol+0.3)*figsize
            }
        } else {
            figsize <- height/nrow
            width=(ncol+0.3)*figsize
        }
    } else if (missing(height)) {
        figsize <- width/(ncol+0.3)
        height=figsize*nrow
    }
    width.plt <- 1/ncol
    height.plt <- 1/nrow
#    width.plt.map <- width.plt/(1+leg.space)
    width.plt.map.offsets <- bigplots[1:2]*width.plt
#    width.plt.col <- width.plt.map*leg.space
    width.plt.col.offsets <- smallplots[1:2]*width.plt
    height.plt.map.offsets <- bigplots[3:4]*height.plt
    height.plt.col.offsets <- smallplots[3:4]*height.plt
    bigplots.plts <- smallplots.plts <- list()
    for (i in 1:nmaps) {
        icol <- ((i-1)%%ncol)+1
        irow <- ceiling(i/ncol)
        bigplots.plts[[i]] <- c(rep((icol-1)*width.plt, 2)+width.plt.map.offsets, rep(1-(irow)*height.plt, 2)+height.plt.map.offsets)
        smallplots.plts[[i]] <- c(rep((icol-1)*width.plt, 2)+width.plt.col.offsets, rep(1-(irow)*height.plt, 2)+height.plt.col.offsets)
    }
    ps.print(file=file.out, height=height, width=width)
    tryCatch({
        ##        layout(cbind(matrix(c(1:nmaps), nrow=nrow, byrow=T), rep(nmaps+1, nrow)), width=c(rep(1, ceiling(nmaps/nrow)), 0.2))
        for (i in 1:nmaps) {
            legend.args <- list(col=col[[i]], breaks=breaks[[i]])
            plot.args <- c(legend.args, list(legend=TRUE, projection="stereographic", orientation=c(45,0,7.5)), list(...), new=ifelse(i==1, FALSE, TRUE))
            do.call(my.plot.map, c(list(fld=mats[[i]], bigplot=bigplots.plts[[i]], smallplot=smallplots.plts[[i]]), plot.args))
            title(titles[i])
        }
        ## par(mar=c(3,0,2,4))
        ## do.call(plot.legend, legend.args)
        dev.off()
    }, error = function(e) {
        dev.off()
        print("plotting error is caught")
        stop(e)
    })
}
