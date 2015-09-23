

library("Cairo")
library("ncdf")
library("rworldmap")
library("fields")
library("sp")
library("maps")
library("mapproj")
library("maptools")
library("spatstat")
require("grDevices")
require("akima")


nc.file <- open.ncdf("WAH_act_PDVpositive_geopot500_invlat_test.nc")
geopot <- get.var.ncdf(nc.file,"field1")
lat <- get.var.ncdf(nc.file,"latitude0")
lon <- get.var.ncdf(nc.file,"longitude0")
close(nc.file)


png(file="WAH_act_PDVpositive_geopot500_invlat_test2.png",width = 700, height = 650,bg = "white")

rgb.palette <- colorRampPalette(c("purple4","darkblue","dodgerblue4","dodgerblue"
,"lightskyblue", "snow2","orange","red","firebrick","darkred","tomato4"))

#filled.contour(lon,lat,geopot,col=rgb.palette(30),main="TEST",axes=T
#,xlim=c(0,360),ylim=c(0,90),zlim=c(-30,30),nlevels=30, plot.axes={map("world2",add=T)})

new <- mapproject(rep(lon,times=length(lat)),rep(lat,each=length(lon)), projection="orthographic", orientation=c(60, -100, 0))

#hist(new$x)
#hist(new$y)

new.x <- seq(-1,1,by=0.01)
new.y <- seq(-1,1,by=0.01)

any.na <- is.na(new$x) | is.na(new$y) | is.na(c(geopot))

geopot.interp <- interp(new$x[!any.na], new$y[!any.na], c(geopot)[!any.na], new.x, new.y)

rx <- cos(seq(0,2*pi, by=0.01))
ry <- sin(seq(0,2*pi, by=0.01))



filled.contour(new.x, new.y, geopot.interp$z,col=rgb.palette(30),main="TEST",axes=T
,zlim=c(-30,30),nlevels=30, plot.axes={map("world2",add=T, projection="orthographic", orientation=c(60, -100, 0)) ; lines(rx,ry)})

dev.off()