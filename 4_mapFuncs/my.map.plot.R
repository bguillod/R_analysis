my.map.plot <- function(z,x,y,
                          breaks,
                          col,
                          grey.col="grey",
                          grey.is=is.na(z),
                          white.is=array(F, dim=dim(z)),
                          bigplot=c(0,0.83, 0,0.9),
                          smallplot=c(0.85,0.88,0.1,0.8),
                          plts="", # if 'auto', bigplot and smallplot are defined automatically
                          map.add="n", # 'n' for no map, 'continents' for continents only, 'countries' for all country borders
                          map.args=list(),
                          add.func=function() return(),
                          box.lwd=1,
                          axis.args=list(),
                        type = "diff",# 'diff' or 'abs'
                        axes=FALSE,
                          ...) {

    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------
    ## function 'my.image.plot' (started 11.10.2013)
    ## like 'image.plot' but with non-stopping breaks
    ## 
    ## ---------------------------------------------------------------------
    ## ---------------------------------------------------------------------

    require(fields)
    ## order x and y
    if (missing(x)) {
        x <- attr(z, "lon")
        if (is.null(x)) x <- 1:dim(z)[1]
    }
    if (missing(y)) {
        y <- attr(z, "lat")
        if (is.null(y)) y <- 1:dim(z)[2]
    }
    z  <- z[order(x), order(y)]
    x  <- sort(x)
    y <- sort(y)
    
    if (missing(breaks)) {
      if (type == "diff") {
#        qabs <- max(abs(quantile(z,0.05, na.rm=TRUE)), abs(quantile(z,0.95, na.rm=TRUE)))
#        breaks <- seq(-qabs, qabs, length.out=12)
        temp <- z
        qabs <- quantile(temp, c(0.05, 0.95), na.rm=TRUE)
        temp[temp < qabs[1]] <- NA
        temp[temp > qabs[2]] <- NA
        breaks <- pretty(temp)
        if (breaks[1] != -breaks[length(breaks)]) {
          maxi <- which(max(abs(breaks)) == abs(breaks))
          mini <- which(min(abs(breaks)) == abs(breaks))
          if (maxi == 1) {
            where.is.bnd <- ifelse(length(mini==1), mini-1, mini[1])
            breaks <- breaks[1:where.is.bnd]
            breaks <- c(breaks, -rev(breaks))
          } else if (maxi == length(breaks)) {
            where.is.bnd <- ifelse(length(mini==1), mini+1, mini[2])
            breaks <- breaks[where.is.bnd:maxi]
            breaks <- c(-rev(breaks), breaks)
          } else {
            stop("** ERROR ** in breaks definition")
          }
          rm(maxi, mini, where.is.bnd)
        }
      } else {
        breaks <- pretty(z)
      }
      lab.breaks <- breaks
  }
    if (missing(col)) {
        source(file.path(colFuncs.path, "my.tim.colors.R"))
        if (type == "diff") {
          col <- my.tim.colors
        } else {
          col <- tim.colors
        }
    }


    ## define bigplot and smallplot if needed
    if (plts == "auto") {
      old.par <- par(no.readonly = TRUE)
      image.plot.info(x, y, z, breaks, axes = axes, ...)
      temp <- image.plot.plt(add = FALSE, legend.shrink = 0.9,
                             legend.width = 1.2,
                             legend.mar = 5.1,
                             horizontal = FALSE,
                             bigplot = NULL, smallplot = NULL)
      smallplot <- temp$smallplot
      bigplot <- temp$bigplot
    }
    
    if (is.function(col)) {
        colbar <- col(length(breaks)+1)
    } else if (length(col) != length(breaks)+1) {
        stop("** ERROR ** 'col' should be a function or a vector of length 'length(breaks)+1' *****")
    } else {
        colbar <- col
    }
    zval <- array(findInterval(z, breaks), dim=dim(z))
    zval[grey.is] <- length(breaks)+1
    zval[white.is] <- NA
    axat <- 0.5:(length(breaks)-0.5)
    axlabs <- as.character(breaks)
    if (!is.null(axis.args$at)) {
      #axat <- approx(x=axlabs, y=axat, xout=axis.args$at)$y
      axat <- spline(x=axlabs, y=axat, xout=axis.args$at, method="fmm")$y
      axlabs <- as.character(axis.args$at)
    }
    brk.leg <- (-0.5:(length(breaks)+0.5))
    brk.image <- seq(-0.5, length(breaks)+1.5, by=1)
    ## image.plot(x,y,zval, col=c(colbar, grey.col), breaks=c(brk, length(breaks)+1.5),
    ##            axis.args=list(at=axat, labels=axlabs),
    ##            legend.args=list(col=colbar, text="TFS", breaks=brk))
    if (!is.null(list(...)$add)) {
        new <- list(...)$add
    }
    par(plt=bigplot, new=new)
    image(x,y,zval, col=c(colbar, grey.col), breaks=brk.image, axes = axes, xlab="", ylab="", ...)
    box(lwd=box.lwd)
    if (map.add == "continents") {
        require(maps)
        if (all(sapply(names(map.args), function(v) all(v != c("xlim", "ylim"))))) {
            map.data <- map(plot=FALSE, database = ifelse(any(lon<0), "world", "world2"))
            do.call(what=map, c(map.args, list(add=TRUE, xlim=par()$xlim, ylim=par()$ylim, interior=FALSE, database = ifelse(any(lon<0), "world", "world2"))))
            lakes.regions <- map.data$names[grep("Lake", map.data$names)]
            seas.regions <- map.data$names[grep("Sea", map.data$names)]
            do.call(what=map, c(map.args, list(add=TRUE, xlim=par()$xlim, ylim=par()$ylim, regions=c(lakes.regions, seas.regions), database = ifelse(any(lon<0), "world", "world2"))))
        } else {
            do.call(what=map, c(map.args, list(add=TRUE, interior=FALSE)))
        }
    } else if (map.add == "countries") {
        require(maps)
        if (all(sapply(names(map.args), function(v) all(v != c("xlim", "ylim"))))) {
            do.call(what=map, c(map.args, list(add=TRUE, xlim=par()$xlim, ylim=par()$ylim)))
        } else {
            do.call(what=map, c(map.args, list(add=TRUE)))
        }
    } else if (map.add != "n") {
        stop("** ERROR ** unexpected values for 'map.add' *****")
    }
    
    add.func()
    if (all(!is.na(smallplot))) {
        axis.args$at <- axat
        axis.args$labels <- axlabs
        image.plot(x,y, zval, legend.only=TRUE,
                   col=colbar, breaks=brk.leg,
                   legend.args=list(col=colbar, text=""),
                   axis.args=axis.args,
                   smallplot=smallplot, zlim=range(brk.leg)+c(+0.5,-0.5))
    }
          ##      smallplot=NULL)
          ## axis.args=list(at=axat, labels=axlabs),
          ## legend.args=list(col=colbar, text="TFS", breaks=brk))

}

## my.image.plot <- function(x,y,z,
##                           breaks = NULL,
##                           col,
##                           na.col="grey",
##                           na.is=array(dim=dim(z), data=F)) {

##     zval <- z
##     colbar <- col(length(breaks)+1)
##     for (i in 1:(length(breaks)-1)) {
##         zval[(z<breaks[i+1]) & (z>=breaks[i])] <- i
##     }
##     zval[z < breaks[1]] <- 0
##     zval[z>breaks[length(breaks)]] <- length(breaks)
##     axat <- 1:length(breaks)-0.5
##     axlabs <- as.character(breaks)
##     brk <- seq(-0.5, length(breaks)+0.5, by=1)
##     image.plot(x,y,zval, col=colbar, breaks=brk,
##                axis.args=list(at=axat, labels=axlabs))

## }
