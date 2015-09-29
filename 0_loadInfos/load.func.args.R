load.func.args <- function(file) {

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function to save all input to a plotting function before plotting
    ## 
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

    if (FALSE) {
        source(file.path(r.infos.path, "load.func.args.R"))
        source(file.path(mapFuncs.path, "my.plot.map.R"))
        func <- image.plot
        file <- file.path(r.infos.path, "test.eps")
        data <- matrix(sample(25), 5, 5)
        
        test <- load.func.args(file)
        do.call(func, test)
        
    }

    ## save data
    file.data <- paste0(file, ".rdata")
    load(file=file.data)

    ## make plot
    return(func.input)

}
