decade.letter <- function(x) {

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function to convert either (1) a year into the decade letter, or
    ## (2) a decade letter into the first year of the decade (e.g., 1990)
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

    if (FALSE) {
        source(file.path(r.infos.path, "decade.letter.R"))
    }

    alphanum <- c(as.character(0:9), letters)
    if (is.numeric(x) | all(nchar(x)==4)) {
        decade <- floor((as.numeric(x)-1800)/10)+1
        output <- alphanum[decade]
    } else if (all(nchar(x) == 1)) {
        decade <- which(alphanum==x)
        output <- (decade-1)*10+1800
    } else {
        stop("** ERROR ** x should be either a single letter (decade in UM) or a year *****")
    }
    return(output)
}
