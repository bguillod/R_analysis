pretty0 <- function(x, n=12, midval=0, outer=FALSE, midval.keep=TRUE, ...) {
    ## outer:  TRUE-> range(x) fully included ; FALSE-> there will be values outside of the breaks
    ## midval.keep: TRUE-> midval can be one of the breaks ; FALSE-> if one of the breaks is midval, it is removed
    xr <- range(x, na.rm=TRUE)
    maxval <- max(abs(xr-midval))
    x[x==xr[1]] <- midval-maxval
    x[x==xr[2]] <- midval+maxval
    temp <- pretty(x, ifelse(outer, n+2, n), ...)
    if (outer) temp <- temp[-c(1, length(temp))]
    if (!midval.keep & any(temp==midval)) temp <- temp[temp!=midval]
    return(temp)
}
