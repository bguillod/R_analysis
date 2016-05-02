subset.mask.UK <- function(fld) {

    ## -------------------------------------------------------------------------
    ## function to subset the UK subdomain and to mask points over France
    ## (i.e., keeping only the grid points over the British Isles)
    ## -------------------------------------------------------------------------

    if (FALSE) {
        
        source(file.path(compute.path, "regional", "subset.mask.UK.R"))

    }

    if (any(findInterval(attr(fld, "rlon"), c(-17.6, -9.1)) != 1) | length(attr(fld, "rlon"))!=39) {
        stop("** ERROR ** unexpected input domain - attribute rlon *****")
    }
    if (any(findInterval(attr(fld, "rlat"), c(-0.1, 11.3)) != 1) | length(attr(fld, "rlat"))!=52) {
        stop("** ERROR ** unexpected input domain - attribute rlat *****")
    }

    nlon <- dim(fld)[1]
    ilon.rem <- list(seq(nlon, by=-1, length.out=17),
                     14:18)
    ilat.rem <- list(1:7,
                     1)

    for (s in 1:length(ilon.rem)) {
        for (i in ilon.rem[[s]]) {
            for (j in ilat.rem[[s]]) {
                fld[i, j, ] <- NA
            }
        }
    }
    return(fld)
}
