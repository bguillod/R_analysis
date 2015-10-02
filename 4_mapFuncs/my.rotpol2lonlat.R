my.rotpol2lonlat <- 
function (rlon, rlat, plon, plat) 
{
    polphi <- plat
    pollam <- plon
    if (class(rlon) == "list") {
        a <- rlon[[1]]
        b <- rlon[[2]]
        rlon <- a
        rlat <- b
        rm(a, b)
    }
    if (class(rlon) == "matrix") {
        if (dim(rlon)[2] != 2) {
            stop("Matrix rlon does not have two columns.")
        }
        a <- rlon[, 1]
        b <- rlon[, 2]
        rlon <- a
        rlat <- b
        rm(a, b)
    }
    nlon <- length(rlon)
    nlat <- length(rlat)
    rlon.mat <- matrix(rlon, nrow=nlon, ncol=nlat, byrow=FALSE)
    rlat.mat <- matrix(rlat, nrow=nlon, ncol=nlat, byrow=TRUE)
    rlon <- as.vector(rlon.mat)
    rlat <- as.vector(rlat.mat)
    if (abs(polphi - 90) < 0.001) {
        return(matrix(c(rlon, rlat), ncol = 2))
    }
    zrpi18 <- 360/(2 * pi)
    zpir18 <- 2 * pi/360
    zsinpol <- sin(zpir18 * polphi)
    zcospol <- cos(zpir18 * polphi)
    zlampol <- zpir18 * pollam
    zlat <- zpir18 * rlat
    zlon <- rlon
    ii <- (zlon > 180)
    zlon[ii] <- zlon[ii] - 360
    zlon <- zpir18 * zlon
    aarg <- zcospol * cos(zlat) * cos(zlon) + zsinpol * sin(zlat)
    lat <- zrpi18 * asin(aarg)
    zarg1 <- sin(zlampol) * (-zsinpol * cos(zlon) * cos(zlat) + 
        zcospol * sin(zlat)) - cos(zlampol) * sin(zlon) * cos(zlat)
    zarg2 <- cos(zlampol) * (-zsinpol * cos(zlon) * cos(zlat) + 
        zcospol * sin(zlat)) + sin(zlampol) * sin(zlon) * cos(zlat)
    ii1 <- ((abs(zarg2) < 1e-30) & (abs(zarg1) < 1e-30))
    ii2 <- ((abs(zarg2) < 1e-30) & (abs(zarg1) >= 1e-30) & (zarg1 > 
        0))
    ii3 <- ((abs(zarg2) < 1e-30) & (abs(zarg1) >= 1e-30) & (zarg1 <= 
        0))
    ii4 <- (abs(zarg2) >= 1e-30)
    lon <- zlon
    lon[ii1] <- 0
    lon[ii2] <- 90
    lon[ii3] <- -90
    lon[ii4] <- zrpi18 * atan2(zarg1, zarg2)
    list(lon=matrix(lon, nrow=nlon, ncol=nlat, byrow=FALSE),
         lat=matrix(lat, nrow=nlon, ncol=nlat, byrow=FALSE))
#    matrix(c(lon, lat), ncol = 2)
}
