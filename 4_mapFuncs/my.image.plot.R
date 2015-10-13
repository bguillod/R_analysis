my.image.plot <- function(z,x,y, add=FALSE, breaks=NULL, nlevel = 64, col = tim.colors,
                          horizontal = FALSE,legend.shrink = 0.9, legend.width = 1.2, 
                          legend.mar = ifelse(horizontal, 3.1, 5.1), legend.lab = NULL, 
                          legend.line = 2, graphics.reset = FALSE, bigplot = NULL, 
                          smallplot = NULL, legend.only = FALSE, lab.breaks = NULL, 
                          axis.args = list(), legend.args = list(col=colbar, text=""), legend.cex = 1, midpoint = FALSE, 
                          border = NA, lwd = 1, verbose = FALSE,
                          add.legend=TRUE,
                          grey.is=is.na(z),
                          white.is=array(F, dim=dim(z)),
                          grey.col="grey",
                          use.plt = TRUE,
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
    info <- imagePlotInfo(..., x=x, y=y, z=z, breaks = breaks, nlevel=nlevel)
    ## if (missing(breaks)) {
    breaks <- info$breaks
    ## }
    if (verbose) {
        print(info)
    }
    if (add) {
        big.plot <- old.par$plt
    }
    if (legend.only) {
        graphics.reset <- TRUE
    }
    ## imageplot.setup.args <-match(c("add", "legend.shrink", "legend.width", "legend.mar",
    ##                                "bigplot"), names(list(...)))
    ## imageplot.setup.args <- imageplot.setup.args[!is.na(imageplot.setup.args)]
    ## if (length(imageplot.setup.args) > 0) {
    ##     imageplot.setup.args <- c(imageplot.setup.args, list(horizontal=horizontal, smallplot=smallplot))
    ## } else {
    ##     imageplot.setup.args <- list(horizontal=horizontal, smallplot=smallplot)
    ## }
    ## temp <- do.call(imageplot.setup, imageplot.setup.args)
    temp <- imageplot.setup(add = add, legend.shrink = legend.shrink, 
        legend.width = legend.width, legend.mar = legend.mar, 
                            horizontal = horizontal, bigplot = bigplot, smallplot = smallplot)
    if (use.plt) {
        smallplot <- temp$smallplot
        bigplot <- temp$bigplot
    } else {
        if (!legend.only & add.legend) {
            stop("** ERROR ** 'use.plt'=FALSE but both legend and plot required *****")
        }
        bigplot <- NULL
        smallplot <- temp$smallplot
    }
    ## START: CONVERT VALUES TO FIT
    zval <- z
    colbar <- col(length(breaks)+1)
    zval <- array(findInterval(z, breaks), dim=dim(z))
    zval[grey.is] <- length(breaks)+1
    zval[white.is] <- NA
    brk.leg <- (-0.5:(length(breaks)+0.5))
    brk.image <- seq(-0.5, length(breaks)+1.5, by=1)
    ## END: CONVERT VALUES TO FIT
    if (!legend.only) {
        if (add.legend) {
            if (!add) {
                par(plt = bigplot)
            }
        }
        if (!info$poly.grid) {
            image(..., z=zval, x=x, y=y, breaks = brk.image, add = add, col = c(colbar, grey.col), 
                       axes=FALSE, xlab="", ylab="")
        }
        else {
            poly.image(..., z=zval, x=x, y=y, add = add, breaks=brk.image, col = c(colbar, grey.col), 
                       axes=FALSE, xlab="", ylab="", midpoint=midpoint,
                       border = border, lwd.poly = lwd)
        }
        big.par <- par(no.readonly = TRUE)
        box()
    }
    if (add.legend) {
        if ((smallplot[2] < smallplot[1]) | (smallplot[4] < smallplot[3])) {
            if (use.plt) {
                par(old.par)
            }
            stop("plot region too small to add legend\n")
        }
        if (legend.only & !add) {
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
        image.plot(..., legend.only=TRUE, z=zval,
                   add=add, breaks=brk.leg, nlevel=nlevel, col=colbar,
                   horizontal=horizontal, legend.shrink=legend.shrink, legend.width=legend.width,
                   legend.mar=legend.mar, legend.lab=legend.lab,
                   legend.line=legend.line, graphics.reset=FALSE, bigplot=bigplot,
                   smallplot=smallplot, lab.breaks = lab.breaks,
                   axis.args=axis.args, legend.args=legend.args,
                   zlim=range(brk.leg)+c(+0.5,-0.5))
    }
    mfg.save <- par()$mfg
    ##    graphics.reset <- ifelse(legend.only, TRUE, list(...)$graphics.reset)
    ## if (add.legend & !legend.only) graphics.reset <- FALSE
    ## if (is.null(graphics.reset)) graphics.reset <- FALSE
    ## print(graphics.reset)
    ## print(add)
    if (use.plt) {
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
