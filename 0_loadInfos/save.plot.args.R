save.plot.args <- function(func, file, height, width, ...) {

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function to save all input to a plotting function before plotting into a file 'file'
    ## 
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

    if (FALSE) {
        source(file.path(r.infos.path, "save.plot.args.R"))
        source(file.path(mapFuncs.path, "my.plot.map.R"))
        func <- image.plot
        file <- file.path(r.infos.path, "test.eps")
        data <- matrix(sample(25), 5, 5)
        save.func.args(func, file, data)
    }

    ## source saving function
    source(file.path(r.infos.path, "save.func.args.R"))
    
    ## save plot
    ps.print(file=file, height=height, width=width)
    ## make plot with saving input
    tryCatch({
        save.func.args(func, file, ...)
        dev.off()
    },
             error = function(e) {
                 dev.off()
                 print("plotting error is caught")
                 stop(e)
             })

}
