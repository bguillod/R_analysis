vals2colvals <- function(z, breaks, nlevel=64, col=tim.colors,
                         na.col="grey",
                         white.is=array(F, dim=dim(z))) {

    
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function 'vals2colvals'
    ## convert values of z and breaks and col to values to be used by
    ## 'image', 'poly.image'
    ## to have non-capped breaks
    ## 
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------


    ## define breaks if missing
    if (missing(breaks)) {
        zlim <- range(z, na.rm=TRUE)
        midpoints <- seq(zlim[1], zlim[2], len=nlevel)
        delta <- (midpoints[2] - midpoints[1])/2
        breaks <- c(midpoints[1] - delta, midpoints + delta)
    }

    na.is <- is.na(z)
    ## START: CONVERT VALUES TO FIT
    zval <- z
    colbar.leg <- col(length(breaks)+1)
    colbar.image <- c(colbar.leg, na.col)
    zval <- array(findInterval(z, breaks), dim=dim(z))
    zval[na.is] <- length(breaks)+1
    zval[white.is] <- NA
    brk.leg <- (-0.5:(length(breaks)+0.5))
    brk.image <- seq(-0.5, length(breaks)+1.5, by=1)
    axat <- 1:length(breaks)-0.5
    axlabs <- as.character(breaks)
    ## END: CONVERT VALUES TO FIT

    return(list(zval=zval, breaks=breaks,
                breaks.image=brk.image, col.image=colbar.image,
                breaks.leg=brk.leg, col.leg=colbar.leg,
                axis.args=list(at=axat, labels=axlabs)))

}
