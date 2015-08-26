splitLevels.pca <- function(hh, nlevs) {
    ## -------------------------------------------------------------------------------------
    ## function to convert multi-layer EOF to individual single-layer lists
    ##         -----> nlevs is optional (will be retrived in the function)
    ## -------------------------------------------------------------------------------------

    atts.hh <- get.atts(hh)
    atts.hh.dat <- get.atts(hh$dat)
    if (missing(nlevs)) nlevs <- dim(hh$dat)[2]/sum(!is.na(atts.hh$ixs))
    npts <- dim(hh$dat)[2]/nlevs

    pca.list <- list()
    atts.hh$ixs <- atts.hh$ixs[1:npts]
    atts.hh$iys <- atts.hh$iys[1:npts]
    atts.hh.dat$ixs <- atts.hh.dat$ixs[1:npts]
    atts.hh.dat$iys <- atts.hh.dat$iys[1:npts]
    for (i in 1:nlevs) {
        inds <- seq(((i-1)*npts+1), by=1, length.out=npts)
        dat <- hh$dat[, inds]
        dat <- put.atts(atts=atts.hh.dat, to=dat)
        sdev <- hh$sdev
        center <- hh$center[inds]
        scale <- hh$scale[inds]
        loadings <- hh$loadings[inds, ]
        scores <- hh$scores
        homcor <- hh$homcor[inds, ]
        n.obs <- hh$n.obs
        EVF <- hh$EVF
        CEVF <- hh$CEVF
        pca.list[[i]] <- list(dat=dat, sdev=sdev, center=center, scale=scale, loadings=loadings,
                              scores=scores, homcor=homcor, n.obs=n.obs, EVF=EVF, CEVF=CEVF)
        pca.list[[i]] <- put.atts(atts=atts.hh, to=pca.list[[i]])
    }
    names(pca.list) <- paste0("lev", 1:nlevs)
    return(pca.list)
}
