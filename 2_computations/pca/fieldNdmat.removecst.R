fieldNdmat.removecst <- function(fld, ...) {
    require(plotmap)
    source(file.path(compute.path, "pca", "fieldNdmat.R"))
    ## a few functions
    fieldNdmat.remove.vals <- function(fld, which.rem) {
        require(plotmap)
        mat <- fieldNdmat(fld)
        temp.atts <- get.atts(mat)
        temp.atts$ixs <- temp.atts$ixs[!which.rem]
        temp.atts$iys <- temp.atts$iys[!which.rem]
        mat <- mat[, !which.rem]
        mat <- put.atts(to=mat, atts=temp.atts)
        return(mat)
    }
    fld.allnas <- !apply(fld, 3, function(m) any(!is.na(m)))
    fld.new <- fld[,,!fld.allnas]
    fld.new <- copy.atts(to=fld.new, from=fld)
    ntim <- dim(fld)[3]
    for (attn in names(attributes(fld))) {
        att.i <- attr(fld, attn)
        if (length(att.i) == ntim) {
            attr(fld.new, attn) <- att.i[!fld.allnas]
        }
    }
    ## attr(fld.new, "umids") <- attr(fld, "umids")[!fld.allnas]
    ## attr(fld.new, "years") <- attr(fld, "years")[!fld.allnas]
    mat <- fieldNdmat(fld.new)
    cst.vals <- (apply(mat, 2, sd) == 0)
    mat <- fieldNdmat.remove.vals(fld.new, cst.vals)
    for (attn in names(attributes(fld.new))) {
        att.i <- attr(fld.new, attn)
        if (length(att.i) == ntim) {
            if (attn %in% names(attributes(mat))) {
                stop("** ERROR ** trying to overwrite attribute ****")
            }
            attr(mat, attn) <- att.i[!fld.allnas]
        }
    }        
    return(mat)
}
