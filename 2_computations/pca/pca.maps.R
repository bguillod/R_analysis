pca.maps <- function(pca.data, n.pca, mat.data, varname="", ...) {

    ## ----------------------------------------------------------------
    ## ----------------------------------------------------------------
    ## function 'pca.maps': make maps of n.pca leading EOFs
    ## 
    ## ----------------------------------------------------------------
    ## ----------------------------------------------------------------

    if (FALSE) {
        source(file.path(compute.path, "pca/pca.maps.R"))
    }

    ## load required packages and functions
    library(pcaXcca)
    source(file.path(r.generics.path, "4_mapFuncs/multi.map.plot.R"))
    source(file.path(r.generics.path, "5_cols/red2blue.colors.R"))
    source(file.path(compute.path, "pca/get.explv.R"))
    source(file.path(colFuncs.path, "pretty0.R"))
    
    ## plot maps of the 'n.pca' first PCs of object 'pca.data'
    grid.cors <- get.grid.atts(mat.data, what="grid.cors")
    nx <- length(grid.cors[[1]])
    ny <- length(grid.cors[[2]])
    dat <- array(dim=c(nx, ny, n.pca))
    for (i in 1:n.pca) {
        temp <- pca.data$rotation[,i]
        temp <- copy.atts(to=temp, from=mat.data)
        dat[,,i] <- vector2field(temp)
    }
    dat <- copy.grid.atts(to=dat, from=mat.data)
    titles <- paste0(varname, " PC ", 1:n.pca, " (", round(100*get.explv(pca.data, n=n.pca)), "%)")
    multi.map.plot(dat, plot.inds=1:n.pca, breaks=pretty0(dat, outer=TRUE, midval.keep=FALSE), col=red2blue.colors, titles=titles, legend.name=varname, ...)
}
