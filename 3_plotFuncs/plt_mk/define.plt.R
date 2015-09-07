define.plt <- function(nc,
                       nr,
                       xlim = c(0.1, 0.9),
                       ylim=c(0.1, 0.9)) {

  ## -------------------------------------------------------------
  ## function 'define.plt'
  ## define, for a given number of columns and rows (nc, nr), the plt argument to make small plots
  ## returns a list of plt plotting row by row from left to right, top to bottom
  ## -------------------------------------------------------------

  ## check
  if (length(xlim)!= 2) {
    stop("** ERROR ** length(xlim)!= 2 *****")
  }
  if (length(ylim)!= 2) {
    stop("** ERROR ** length(ylim)!= 2 *****")
  }
  if (xlim[2] <= xlim[1]) {
    stop("** ERROR ** xlim[2] <= xlim[1] *****")
  }
  if (ylim[2] <= ylim[1]) {
    stop("** ERROR ** ylim[2] <= ylim[1] *****")
  }

  ## define steps
  dx <- (xlim[2]-xlim[1])/nc
  dy <- (ylim[2]-ylim[1])/nr
  xls <- xlim[1] + (0:nc)*dx
  yls <- ylim[2] - (0:nr)*dy

  ## define list of plt
  plt.list <- list()
  k <- 1
  for (i in 1:nr) {
    for (j in 1:nc) {
      plt.list[[k]] <- c(xls[j:(j+1)], yls[(i+1):i])
      k <- k+1
    }
  }

  return(plt.list)
}
