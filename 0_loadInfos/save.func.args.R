save.func.args <- function(func, file, ...) {

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function to save all input to a plotting function before plotting
    ## 
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

    if (FALSE) {
        source(file.path(mapFuncs.path, "my.plot.map.R"))
        func <- image.plot
        file <- file.path(r.infos.path, "test.eps")
        data <- matrix(sample(25), 5, 5)
        save.func.args(func, file, data)
    }

    ## save data
    file.data <- paste0(file, ".rdata")
    func.input <- list(...)
    save(func.input, file=file.data)

    ## make plot
    do.call(func, func.input)

}
