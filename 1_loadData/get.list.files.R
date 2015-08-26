get.list.files <- function(experiment="fredi",
                           umid = "all",
                           years = "all") {

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function to get a data frame with years, umid and files from WAH runs
    ## 
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

    if (experiment == "fredi") {
        warning("BE CAREFUL - THIS SCRIPT MIGHT NOT WORK IF UMID != 'ALL' *****")
        ##      if (rcm) stop("** ERROR ** only GCM available for 'fredi' *****")
        path.in <- "/ouce-home/staff/cenv0256/EXCLUDEFROMBACKUP"
        data.list <- data.frame(year=numeric(), umid=character(), file=character())
        if (all(length(years) == 1 & years == "all")) stop("** for fredi's experiment, years must be specified **")
        umid.orig <- umid
        for (y in years) {
            if (umid.orig == "all") umid <- unique(substr(list.files(file.path(path.in, y), pattern="hadam3p_eu_"), 12, 15))
            for (uid in umid) {
                folders.in <- list.files(file.path(path.in, y), pattern=paste("hadam3p_eu_", uid, "_", sep=""))
                file.in <- file.path(path.in, y, folders.in)
                if (length(file.in) == 0) next
                for (i in 1:length(file.in)) data.list <- rbind(data.list, data.frame(year=y, umid=uid, file=file.in[i]))
                                        #              print(file.in)
            }
        }
    } else {
        stop("** ERROR ** unexpected value for 'experiment' *****")
    }

    ## return data frame
    return(data.list)
}
