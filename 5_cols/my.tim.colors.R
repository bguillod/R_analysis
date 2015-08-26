my.tim.colors <- function(n, type=1, midcol) {
    if (type == 1) {
        ## type 1
        require(fields)
        orig <- tim.colors(64+11)
        require(plotmap)
        modif <- cool2warm.colors(31)
        
        ## make my own mix
        new.palette <- c(orig[1:28], modif[13:19], orig[48:75])
        orig <- new.palette
        
        if (n == 62)  return(orig[-32])
        rgb.tim <- t(col2rgb(orig))
        temp <- matrix(NA, ncol = 3, nrow = n)
        x <- seq(0, 1, , 63)
        xg <- seq(0, 1, , n)
        for (k in 1:3) {
            hold <- splint(x, rgb.tim[, k], xg)
            hold[hold < 0] <- 0
            hold[hold > 255] <- 255
            temp[, k] <- round(hold)
        }
        output <- rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)

        ## type 2
    } else if (type == 2) {
        output <- colorRampPalette(c("purple", "darkblue", "steelblue", "lightcyan", "white", "lemonchiffon", "orange", "red", "darkred"))(n)
    } else if (type == 3) {
        ## TYPE 3
        ntot <- ceiling(n*1.125)
        dn <- ntot-n
#        temp <- colorRampPalette(c("purple", "darkblue", "steelblue", "lightcyan", "white", "lemonchiffon", "orange", "red", "darkred"))(ntot)
        temp <- colorRampPalette(c("darkblue", "royalblue", "steelblue", "lightcyan", "white", "lemonchiffon", "orange", "red", "darkred"))(ntot)
        temp <- colorRampPalette(c("darkblue", "royalblue3", "steelblue2", "lightcyan", "white", "lemonchiffon", "orange", "red", "darkred"))(ntot)

        ## temp <- colorRampPalette(c("darkblue", "blue2", "steelblue", "lightcyan", "white", "lemonchiffon", "orange", "red", "darkred"))(ntot)
        output <- temp[-(ntot/2+c((-dn/2+1):(dn/2)))]
    } else if (type == 4) {
        ntot <- ceiling(n*1.125)
        dn <- ntot-n
        temp <- colorRampPalette(c("purple", "darkblue", "steelblue", "lightsteelblue1", "white", "lemonchiffon", "orange", "red", "darkred"))(ntot)
        output <- temp[-(ntot/2+c((-dn/2+1):(dn/2)))]
    } else if (type == 5) {
        output <- colorRampPalette(c("purple", "darkblue", "steelblue", "lightblue1", "white", "lemonchiffon", "orange", "red", "darkred"))(n)
    } else if (type == 6) {
        output <- colorRampPalette(c("purple", "darkblue", "steelblue", "lightsteelblue1", "white", "lemonchiffon", "orange", "red", "darkred"))(n)
    } else {
        stop("** ERROR ** unexpected value for 'type' *****")
    }
    if (!missing(midcol) & n%%2 == 1) {
        output[n/2+0.5] <- midcol
    }
    return(output)
}
