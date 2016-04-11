
options(repos = "http://stat.ethz.ch/CRAN/")

## libdir.ouce  <- file.path(Sys.getenv("HOME"), "R/x86_64-redhat-linux-gnu-library/3.2/")
## libdir.jasmin  <- file.path(Sys.getenv("HOME"), "R/x86_64-redhat-linux-gnu-library/3.2/")
libdir <- libdir.jasmin
libdir <- stop("SPECIFY THE LIBDIR PLEASE")

install.path <- file.path(r.scripts.path, "install_packages")
owndir <- file.path(install.path, "own_pkgs")
destdir <- file.path(install.path, "cran_pkgs")

already.pkgs <- c("abind", "akima", "fields", "fitdistrplus", "gdata", "lattice", "lmomco", "manipulate", "mapdata", "mapproj", "maps", "ncdf4", "ncdf4.helpers", "PCICt", "SCI", "spam", "xts", "zoo", "maptools", "sp", "rgdal", "rgeos", "optparse", "spacetime", "XML")

new.pkgs <- c("Hmisc")



## config.args <- c(ncdf4 = "--with-netcdf-include=/usr/local/include
## 		                --with-netcdf-lib=/usr/local/lib",
##                  RNetCDF = "--with-netcdf-include=/usr/local/include
## 		                --with-netcdf-lib=/usr/local/lib")
config.args <- NULL
dependencies.install <- c("Depends", "Imports", "LinkingTo")

## INSTALL CRAN PACKAGES
for (i in 1:length(new.pkgs)) {
    if (is.null(config.args)) {
        tryCatch({install.packages(new.pkgs[i],
                                   destdir=destdir,
                                   dependencies=dependencies.install)},
                 warning = function(w) stop(paste("WARNING: ", w)),
                 error = function(e) stop(paste("ERROR:", e)))
    } else {
        tryCatch({install.packages(new.pkgs[i],
                                   destdir=destdir,dependencies=dependencies.install,
                                   configure.args = config.args)},
                 warning = function(w) stop(paste("WARNING: ", w)),
                 error = function(e) stop(paste("ERROR:", e)))
    }
        print(paste("**** package", new.pkgs[i], "installed ****"))
}


## INSTALL OWN PACKAGES (CHRIGEL)
new.own.pkgs <- c("trend_1.5.1", "gevXgpd_1.4.2", "geocors_1.2.8", "plotmap_2.3.7", "pcaXcca_1.4.1", "ACWD_2.0.0", "sm_2.2-5.4", "vioplot_0.2")

for (i in 1:length(new.own.pkgs)) {
    tryCatch({install.packages(file.path(owndir, paste0(new.own.pkgs[i], ".tar.gz")),
#                               lib=libdir,
                               repos=NULL, depencencies=F)},
             warning = function(w) stop(paste("WARNING: ", w)),
             error = function(e) stop(paste("ERROR:", e)))
    print(paste("**** package", new.own.pkgs[i], "installed ****"))
}
