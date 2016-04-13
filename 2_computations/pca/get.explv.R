get.explv <- function(obj, cumul=FALSE, n=length(obj$sdev)) {
    ## ------------------------------
    ## Get the fraction of explained variance from a prcomp object
    ## ------------------------------

    if (FALSE) {
        source(file.path(compute.path, "pca/get.explv.R"))
    }

    if (cumul) {
        output <- cumsum((obj$sdev[1:n])^2)/sum(obj$sdev^2)
    } else {
        output <- (obj$sdev[1:n])^2/sum(obj$sdev^2)
    }
    return(output)
}
