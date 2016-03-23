my.map.plot <- function(z,x,y,
                        grid.atts=get.grid.atts(z),
                        database="world",
                        lines.args,
                        ...) {

    source(file.path(r.generics.path, "4_mapFuncs/my.image.plot.R"))

    if (FALSE) {
        source(file.path(r.generics.path, "4_mapFuncs/my.map.plot.R"))
    }

    ## if (!is.null(grid.atts$grid.type)) {
    ##     if (all(grid.atts$grid.type != c("lonlat", "rotpol"))) stop("** ERROR ** unexpected grid.type *****")
    ## }

    if (missing(x)) {
        x <- attr(z, grid.att.names(grid.atts$grid.type, what="grid.cors")[1])
        if (is.null(x)) x <- 1:dim(z)[1]
    }
    if (missing(y)) {
        y <- attr(z, grid.att.names(grid.atts$grid.type, what="grid.cors")[2])
        if (is.null(y)) y <- 1:dim(z)[2]
    }

    if (missing(grid.atts)) grid.atts <- list(grid.type="lonlat",
                                              lon=x,lat=y)
    my.image.plot(z,x,y, ...)
    
    ## make map boundaries
    source(file.path(mapFuncs.path, "map.add.R"))
    map.args <- list(grid.atts=grid.atts, database=database)
    map.args <- c(map.args, lines.args)
    print(map.args)
    do.call(map.add, map.args)
#    map.add(grid.atts, database=database, lines.args)
    
}
