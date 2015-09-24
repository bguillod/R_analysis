get.list.files <- function(experiment="fredi",
                           umid = "all",
                           years = "all") {

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function to get a data frame with years, umid and files from WAH runs
    ## 
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

    if (umid != "all") {
        stop("BE CAREFUL - THIS SCRIPT MIGHT NOT WORK IF UMID != 'ALL' *****")
    }
    
    if (all(length(years) == 1 & years == "all")) {
        stop("** years must be specified **")
    }
    
    if (experiment == "fredi") {
        ##      if (rcm) stop("** ERROR ** only GCM available for 'fredi' *****")
        path.in <- "/ouce-home/staff/cenv0256/EXCLUDEFROMBACKUP"
        region.name <- "eu"
    } else if (experiment == "pnw.1985-2013") {
        path.in <- file.path(data.path, "USdrought/1985-2013")
        region.name <- "pnw"
    } else {
        stop("** ERROR ** unexpected value for 'experiment' *****")
    }
    data.list <- data.frame(year=numeric(), umid=character(), file=character())
    umid.orig <- umid
    for (y in years) {
        if (umid.orig == "all") umid <- unique(substr(list.files(file.path(path.in, y), pattern=paste0("hadam3p_", region.name, "_")), 10+nchar(region.name), 13+nchar(region.name)))
        for (uid in umid) {
            folders.in <- list.files(file.path(path.in, y), pattern=paste("hadam3p_", region.name, "_", uid, "_", sep=""))
            file.in <- file.path(path.in, y, folders.in)
            if (length(file.in) == 0) next
            for (i in 1:length(file.in)) data.list <- rbind(data.list, data.frame(year=y, umid=uid, file=file.in[i]))
        }
    }

    ## format to character vectors
    data.list$umid <- as.character(data.list$umid)
    data.list$file <- as.character(data.list$file)
    
    ## return data frame
    return(data.list)
}
