multi.plot.map <- function(mats, titles, nrow, col, breaks, file.out, width=7, height=7, ...) {
    ## -------------------------------------------------------------------------------------
    ## multi.plot.map(mats, titles, nrow, col, breaks, file.out, width=7, height=7, ...) 
    ##         -----> function to make multiple maps and print in 'file.out'
    ##         -----> 'breaks' and 'col' must be the same
    ##         -----> the i-th map is the map of mats[,,i] or mats[[i]]
    ##         -----> maps are plotted by row
    ## -------------------------------------------------------------------------------------


    require(plotmap)
    if (missing(col)) col <- cool2warm.colors(length(breaks)+1)
    if (is.function(col)) col <- col(length(breaks)+1)
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
    ps.print(file=file.out, height=height, width=width)
    tryCatch({
        layout(cbind(matrix(c(1:nmaps), nrow=nrow, byrow=T), rep(nmaps+1, nrow)), width=c(rep(1, ceiling(nmaps/nrow)), 0.2))
        legend.args <- list(col=col, breaks=breaks)
        plot.args <- c(legend.args, list(legend=FALSE, projection="stereographic", orientation=c(45,0,7.5)), list(...))
        for (i in 1:nmaps) {
            do.call(plot.map, c(list(fld=mats[[i]]), plot.args))
            title(titles[i])
        }
        par(mar=c(3,0,2,4))
        do.call(plot.legend, legend.args)
        dev.off()
    }, error = function(e) {
        dev.off()
        print("plotting error is caught")
        stop(e)
    })
}
