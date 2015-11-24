get.list.files.from.path <- function(files.data.path,
                                     umid = "all",
                                     years = "all") {

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function to get a data frame with years, umid and files from WAH runs
    ## modified 24/11/2015: now works with both raw cpdn output and output extract with Neil's wah_extract.py script (automatically detects which one it is)
    ## 
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

    if (FALSE) {

        files.data.path <- "/data/ouce-cpdn/nmassey/wah_data/OSTIA_global/2011"
        test <- get.list.files.from.path(files.data.path)

        files.data.path <- "/data/ouce-cpdn/nmassey/wah_data/OSTIA_global"
        test <- get.list.files.from.path(files.data.path)
        test <- get.list.files.from.path(files.data.path, year="2011")

        test <- get.list.files.from.path("/data/ouce-cpdn/nmassey/wah_data/OSTIA_natural/2011")

        
    }
    
    if (umid != "all") {
        stop("BE CAREFUL - THIS SCRIPT MIGHT NOT WORK IF UMID != 'ALL' *****")
    }
    
    ## if (all(length(years) == 1 & years == "all")) {
    ##     stop("** years must be specified **")
    ## }

    ## what is in the input dir? Years or runs?
    fnames <- list.dirs(files.data.path, recursive=FALSE, full.names=FALSE)
    if (all(nchar(fnames) == 4)) {
        years.subdir <- TRUE
        years.avail <- fnames
        if (years != "all") {
            years.in <- years %in% years.avail
            if (any(!years.in)) stop("** ERROR ** not all requested years are available *****")
            years.in <- years
        } else {
            years.in <- years.avail
        }
        file.dirs <- data.frame(dirs=vector(length=0), fnames=vector(length=0), stringsAsFactors=FALSE)
        for (i in 1:length(years.in)) {
            dir.in <- file.path(files.data.path, years.in[i])
            files.in <- list.dirs(dir.in, recursive=FALSE, full.names=FALSE)
            file.dirs.add <- data.frame(dirs=rep(dir.in, length(files.in)),
                                        fnames=files.in,
                                        stringsAsFactors=FALSE)
            file.dirs <- rbind(file.dirs, file.dirs.add)
        }
    } else if (all(substr(fnames, 1, 7) == "hadam3p")) {
        years.subdit <- FALSE
        file.dirs <- data.frame(dirs = files.data.path,
                                fnames = fnames,
                                stringsAsFactors=FALSE)
    } else {
        stop("** ERROR ** the input directory does not contain years or 'hadam3p' runs folders *****")
    }

    ## now identify umids and years from folder names
    fnames <- as.character(file.dirs$fnames)
    fnames.split <- strsplit(fnames, "[_]")
    umids.avail <- sapply(fnames.split, function(v) v[3])
    years.avail <- sapply(fnames.split, function(v) v[4])

    ## sub-select years and umid
    if (years == "all") {
        years.which.in <- rep(TRUE, length(fnames))
    } else {
        years.which.in <- years.avail %in% years
        if (years.subdir & any(!years.which.in)) {
            stop("** ERROR ** year subdirectory exists but there are non-matching years from folder names *****")
        }
    }
    if (umid == "all") {
        umid.which.in <- rep(TRUE, length(fnames))
    } else {
        umid.which.in <- umids.avail %in% umid
    }

    ## subselect
    which.in <- years.which.in & umid.which.in
    file.dirs <- file.dirs[which.in, ]
    file.dirs$year <- years.avail[which.in]
    file.dirs$umid <- umids.avail[which.in]

    ## identify the type of the data:
    ## raw CPDN data or processed from Neil's script?
    test.content <- c()
    i <- 1
    while (length(test.content) == 0) {
        test.content <- list.files(file.path(file.dirs$dirs[i], file.dirs$fnames[i]))
        attr(test.content, "umid") <- file.dirs$umid[i]
        attr(test.content, "year") <- file.dirs$year[i]
        source(file.path(r.infos.path, "decade.letter.R"))
    }
    is.nc <- substr(test.content, nchar(test.content)-2, nchar(test.content)) == ".nc"
    is.umid <- substr(test.content, 1,4) == attr(test.content, "umid")
    is.year.ok <- substr(test.content, 10,11) == paste0(decade.letter(as.numeric(attr(test.content, "year"))), as.numeric(substr(as.character(attr(test.content, "year")), 4, 4))) |
        substr(test.content, 10,11) == paste0(decade.letter(as.numeric(attr(test.content, "year"))+1), as.numeric(substr(as.character(attr(test.content, "year")), 4, 4))+1)
    is.zip <- substr(test.content, nchar(test.content)-3, nchar(test.content)) == ".zip"
    if (all(test.content %in% c("ga.pd", "ga.pe", "ma.pc"))) {
        cpdn.data.type <- "neil"
        
    } else if (all((is.nc & is.umid & is.year.ok) | is.zip)) {
        cpdn.data.type <- "raw"
        
    } else {
        cpdn.data.type <- NA
        warning("** unknown cpdn data type **")
    }
    attr(file.dirs, "cpdn.data.type") <- cpdn.data.type

    ## return the data frame
    return(file.dirs)

}
