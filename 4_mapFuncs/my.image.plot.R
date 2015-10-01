my.image.plot <- function(z,
                          breaks=NULL,
                          col,
                          na.col="grey",
                          grey.is,
                          white.is=array(F, dim=dim(z)),
                          grey.col="grey",
                          add.legend=TRUE,
                          axis.args=list(),
                          ...) {

    info <- imagePlotInfo(..., z=z, breaks = breaks)
    if (missing(breaks)) {
        breaks <- info$breaks
    }
    if (missing(grey.is)) {
        grey.is <- is.na(z)
    }
    zval <- z
    colbar <- col(length(breaks)+1)
    zval <- array(findInterval(z, breaks), dim=dim(z))
    zval[grey.is] <- length(breaks)+1
    zval[white.is] <- NA
    axat <- 1:length(breaks)-0.5
    axlabs <- as.character(breaks)
    brk.leg <- (-0.5:(length(breaks)+0.5))
    brk.image <- seq(-0.5, length(breaks)+1.5, by=1)
    if (!info$poly.grid) {
        image(..., z=zval, breaks = breaks, add = add, col = col)
    }
    else {
        poly.image(..., z=zval, breaks=brk.image, col = c(colbar, grey.col), 
                   axes=FALSE)
    }
    if (add.legend) {
        axis.args$at <- axat
        axis.args$labels <- axlabs
        image.plot(..., z=zval, legend.only=TRUE,
                   col=colbar, breaks=brk.leg,
                   legend.args=list(col=colbar, text=""),
                   axis.args=axis.args, zlim=range(brk.leg)+c(+0.5,-0.5))

    }

}
