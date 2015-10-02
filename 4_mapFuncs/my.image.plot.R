my.image.plot <- function(z,x,y, add=FALSE, breaks=NULL, nlevel = 64, col = tim.colors,
                          horizontal = FALSE, grey.is=is.na(z),
                          white.is=array(F, dim=dim(z)),
                          grey.col="grey",
                          add.legend=TRUE,
                          axis.args=list(),
                          legend.only=FALSE,
                          legend.args=list(col=colbar, text=""),
                          smallplot = NULL,
                          ...) {

    if (FALSE) {
        source(file.path(r.generics.path, "4_mapFuncs/my.image.plot.R"))
    }

    
    old.par <- par(no.readonly = TRUE)
    if (missing(x)) {
        x <- attr(z, "lon")
        if (is.null(x)) x <- 1:dim(z)[1]
    }
    if (missing(y)) {
        y <- attr(z, "lat")
        if (is.null(y)) y <- 1:dim(z)[2]
    }
    info <- imagePlotInfo(..., z=z, x=x, y=y, breaks = breaks, nlevel=nlevel)
    if (missing(breaks)) {
        breaks <- info$breaks
    }
    if (add) {
        big.plot <- old.par$plt
    }
    if (legend.only) {
        graphics.reset <- TRUE
    }
    imageplot.setup.args <-match(c("add", "legend.shrink", "legend.width", "legend.mar",
                                   "bigplot"), names(list(...)))
    imageplot.setup.args <- imageplot.setup.args[!is.na(imageplot.setup.args)]
    if (length(imageplot.setup.args) > 0) {
        imageplot.setup.args <- c(imageplot.setup.args, list(horizontal=horizontal, smallplot=smallplot))
    } else {
        imageplot.setup.args <- list(horizontal=horizontal, smallplot=smallplot)
    }
    temp <- do.call(imageplot.setup, imageplot.setup.args)
    smallplot <- temp$smallplot
    bigplot <- temp$bigplot
    zval <- z
    colbar <- col(length(breaks)+1)
    zval <- array(findInterval(z, breaks), dim=dim(z))
    zval[grey.is] <- length(breaks)+1
    zval[white.is] <- NA
    brk.leg <- (-0.5:(length(breaks)+0.5))
    brk.image <- seq(-0.5, length(breaks)+1.5, by=1)
    if (!legend.only) {
        if (!add) {
            par(plt = bigplot)
        }
        if (!info$poly.grid) {
            image(..., z=zval, x=x, y=y, breaks = brk.image, add = add, col = c(colbar, grey.col), 
                       axes=FALSE, xlab="", ylab="")
        }
        else {
            poly.image(..., z=zval, x=x, y=y, add = add, breaks=brk.image, col = c(colbar, grey.col), 
                       axes=FALSE, xlab="", ylab="")
        }
        big.par <- par(no.readonly = TRUE)
        box()
    }
    if (add.legend) {
        if ((smallplot[2] < smallplot[1]) | (smallplot[4] < smallplot[3])) {
            par(old.par)
            stop("plot region too small to add legend\n")
        }
        if (legend.only) {
            plot(1, type="n", axes=F, xlab="", ylab="")
        }
        if (is.null(legend.args$text)) legend.args$text <- ""
        axat <- 1:length(breaks)-0.5
        axlabs <- as.character(breaks)
        if (!is.null(axis.args$at)) {
            which.at <- match.numeric(axis.args$at, breaks)
            if (any(is.na(which.at))) {
                stop("** ERROR ** 'axis.args$at' should only contain values in breaks *****")
            } else {
                axat <- axat[which.at]
                axlabs <- axlabs[which.at]
            }
        }
        axis.args$at <- axat
        axis.args$labels <- axlabs
        image.plot(..., z=zval, legend.only=TRUE,
                   col=colbar, breaks=brk.leg,
                   legend.args=legend.args,
                   axis.args=axis.args, zlim=range(brk.leg)+c(+0.5,-0.5), smallplot=smallplot, graphics.reset=TRUE)
        mfg.save <- par()$mfg
        graphics.reset <- list(...)$graphics.reset
        if (is.null(graphics.reset)) graphics.reset <- FALSE
        if (graphics.reset | add) {
            par(old.par)
            par(mfg = mfg.save, new = FALSE)
            invisible()
        }
        else {
            par(big.par)
            par(plt = big.par$plt, xpd = FALSE)
            par(mfg = mfg.save, new = FALSE)
            invisible()
    }

    }

}
