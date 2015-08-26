fieldNdmat <- function (field) #, years = "all", months = "all", xlim = "all", 
#    ylim = "all") 
{
    ## FUNCTION equivalent to function 'field2dmat' from C. Frei's package pcaXcca,
    ## but to convert multi-dimensional array (rather than just 3d arrays) to a 2d matrice
    ## This can be useful, e.g., when doing an EOF on several levels at the same time
    ##         -----> convert a multivariable field to a 2d matrice for PCA and MCA
    
    gtype <- get.grid.atts(field, "grid.type")$grid.type
    gcornams <- grid.att.names(gtype, "grid.cors")
    gcors <- get.grid.atts(field, "grid.cors")
    gpars <- get.grid.atts(field, "grid.pars")
    xs <- gcors[[gcornams[1]]]
    ys <- gcors[[gcornams[2]]]
    ## tims <- attr(field, "times")
    ## mons <- as.numeric(sapply(tims, FUN = substr, start = 6, 
    ##     stop = 7))
    ## yeas <- as.numeric(sapply(tims, FUN = substr, start = 1, 
    ##     stop = 4))
    ## if (months[1] == "all") {
    ##     i.mons <- rep(TRUE, length = length(mons))
    ## }
    ## else {
    ##     i.mons <- sapply(mons, FUN = function(a) {
    ##         a %in% months
    ##     })
    ## }
    ## if (years[1] == "all") {
    ##     i.yeas <- rep(TRUE, length = length(yeas))
    ## }
    ## else {
    ##     i.yeas <- sapply(yeas, FUN = function(a) {
    ##         a %in% years
    ##     })
    ## }
    ## i.tims <- i.mons & i.yeas
    ## if (xlim[1] == "all") {
    i.xs <- rep(TRUE, length = length(xs))
    ## }
    ## else {
    ##     i.xs <- (xs <= xlim[2]) & (xs >= xlim[1])
    ## }
    ## if (ylim[1] == "all") {
    i.ys <- rep(TRUE, length = length(ys))
    ## }
    ## else {
    ##     i.ys <- (ys <= ylim[2]) & (ys >= ylim[1])
    ## }
    ## field <- field[i.xs, i.ys, i.tims]
    gcors[[gcornams[1]]] <- xs[i.xs]
    gcors[[gcornams[2]]] <- ys[i.ys]
    field <- put.grid.atts(field, grid.type = gtype, grid.cors = gcors, 
        grid.pars = gpars)
    ## attr(field, "times") <- tims[i.tims]
    ## dmat <- t(array(field, dim = c(dim(field)[1] * dim(field)[2], 
    ##     dim(field)[3])))
    ndims <- length(dim(field))
    dmat <- t(array(field, dim = c(prod(dim(field)[-ndims]), 
        dim(field)[ndims])))
    isna <- apply(is.na(dmat), FUN = function(x) (sum(x) > 0), 
        MARGIN = c(2))
    dmat <- dmat[, !isna]
    row.names(dmat) <- attr(field, "times")
    dmat <- copy.grid.atts(field, dmat)
    gcors <- get.grid.atts(field, "grid.cors")
    xs <- gcors[[gcornams[1]]]
    ys <- gcors[[gcornams[2]]]
    ixs <- 1:length(xs)
    iys <- 1:length(ys)
    ixs.arr <- rep(ixs, times = length(ys))
    iys.arr <- as.vector(sapply(iys, FUN = rep, times = length(ixs)))
    attr(dmat, "ixs") <- ixs.arr[!isna]
    attr(dmat, "iys") <- iys.arr[!isna]
    dmat
}
