

library("Cairo")
library("ncdf")
library("RColorBrewer")
#library("rworldmap")
library("fields")
library("sp")
library("maps")
library("mapproj")
library("maptools")
#library("spatstat")
library("graphics")
require("grDevices")
require("akima")
source("/ouce-home/staff/cenv0270/R/filled.legend.R")


path2="/ouce-home/staff/cenv0270/CPDN/California_Drought/geopot_200/clim4ostia_200/"
path3="/ouce-home/staff/cenv0270/CPDN/California_Drought/precip/"

namein0 <- paste(path3,"zeroone_lsm_n96_inv.nc",sep="")
nc.file <- open.ncdf(namein0)
lsm <- get.var.ncdf(nc.file,"lsm")
lat0 <- get.var.ncdf(nc.file,"latitude")
lon0 <- get.var.ncdf(nc.file,"longitude")
close(nc.file)


for (i in 1986:2015) {

namein1 <- paste("ostia_sst_",i,"_12-05_N96_anom_mean_inv.nc",sep="")
namein2 <- paste(path2,"WAH_act_",i,"_12-05_geopot200hPa_NH_200_anom_mean.nc",sep="")
namein3 <- paste(path3,"WAH_act_",i,"_12-05_precip_glob_200_anom_mean_inv.nc",sep="")
nameout <- paste("ostia_sst_",i,"_12-05_N96_anom_mean_geopot200.png",sep="")
year1 = i - 1 
year2 = i
print (namein1)

nc.file <- open.ncdf(namein1)
tempsfc <- get.var.ncdf(nc.file,"temp")
lat1 <- get.var.ncdf(nc.file,"latitude")
lon1 <- get.var.ncdf(nc.file,"longitude")
close(nc.file)

if (i==2012) { namein2 <- paste(path2,"WAH_act_2011_12-05_geopot200hPa_NH_200_anom_mean.nc",sep="") }
nc.file <- open.ncdf(namein2)
geopot <- get.var.ncdf(nc.file,"field1")
lat2 <- get.var.ncdf(nc.file,"latitude0")
lon2 <- get.var.ncdf(nc.file,"longitude0")
close(nc.file)

nc.file <- open.ncdf(namein3)
precip <- get.var.ncdf(nc.file,"field90")
lat3 <- get.var.ncdf(nc.file,"latitude0")
lon3 <- get.var.ncdf(nc.file,"longitude0")
close(nc.file)
precip=precip*30*24*60*60


anom.palette <- colorRampPalette(c("darkorchid3","darkorchid4","navy","blue","royalblue3","dodgerblue3","deepskyblue"
,"lightskyblue","snow2","snow2","orange","darkorange","red","red3","firebrick","darkred","deeppink4","deeppink3"))
prcp.palette <- brewer.pal(10,"BrBG") 

new0 <- mapproject(rep(lon0,times=length(lat0)),rep(lat0,each=length(lon0)), projection="orthographic", orientation=c(60, -100, 0))
new1 <- mapproject(rep(lon1,times=length(lat1)),rep(lat1,each=length(lon1)), projection="orthographic", orientation=c(60, -100, 0))
new2 <- mapproject(rep(lon2,times=length(lat2)),rep(lat2,each=length(lon2)), projection="orthographic", orientation=c(60, -100, 0))
new3 <- mapproject(rep(lon3,times=length(lat3)),rep(lat3,each=length(lon3)), projection="orthographic", orientation=c(60, -100, 0))

lon.range <- c( 0,360)
lat.range <- c(-90,90)
dlon <- dlat <- 15
lon.lines <- seq(lon.range[1], lon.range[2], dlon)
lat.lines <- seq(lat.range[1], lat.range[2], dlat)
lon.lines.m <- lapply(lon.lines, function(l) {
	lats <- seq(lat.range[1], lat.range[2], 0.5)
	return(data.frame(x=rep(l, length(lats)), y=lats)) })
lon.lines.proj <- lapply(lon.lines.m, mapproject, projection="orthographic", orientation=c(60, -100, 0))
lat.lines.m <-  lapply(lat.lines, function(l) {
        lons <- seq(lon.range[1], lon.range[2], 0.5)
        return(data.frame(x=lons, y=rep(l, length(lons)) )) })
lat.lines.proj <- lapply(lat.lines.m, mapproject, projection="orthographic", orientation=c(60, -100, 0))

new.x <- seq(-1,1,by=0.005)
new.y <- seq(-1,1,by=0.005)

any.na0 <- is.na(new0$x) | is.na(new0$y) | is.na(c(lsm))
lsm.interp  <- interp(new0$x[!any.na0], new0$y[!any.na0], c(lsm)[!any.na0] , new.x, new.y,duplicate = "strip")
any.na1 <- is.na(new1$x) | is.na(new1$y) | is.na(c(tempsfc))
tempsfc.interp <- interp(new1$x[!any.na1], new1$y[!any.na1], c(tempsfc)[!any.na1], new.x, new.y,duplicate = "strip")
any.na2 <- is.na(new2$x) | is.na(new2$y) | is.na(c(geopot))
geopot.interp  <- interp(new2$x[!any.na2], new2$y[!any.na2], c(geopot)[!any.na2] , new.x, new.y,duplicate = "strip")
any.na3 <- is.na(new3$x) | is.na(new3$y) | is.na(c(precip))
precip.interp  <- interp(new3$x[!any.na3], new3$y[!any.na3], c(precip)[!any.na3] , new.x, new.y)

rx <- cos(seq(0,2*pi, by=0.005))
ry <- sin(seq(0,2*pi, by=0.005))

new.l <- lsm.interp$z
new.p <- precip.interp$z
new.t <- tempsfc.interp$z
new.z <- geopot.interp$z

for (j in 1:401) { for (k in 1:401) { if(!is.na(new.l[j,k]) && (new.l[j,k]>=0.5)) new.p[j,k]=NA }}

zlim <- c(-2,2)
nlevels <- 20
levels <- pretty(zlim, nlevels)
levels[1] <- min(c(new.t, levels[1]), na.rm=TRUE)
levels[length(levels)] <- max(c(new.t, levels[length(levels)]), na.rm=TRUE)

zlim <- c(-120,-10)
nlevels <- 10
mylevels1 <- pretty(zlim, nlevels)
mylevels1[1] <- min(c(new.z, mylevels1[1]), na.rm=TRUE)
zlim <- c(10,120)
nlevels <- 10
mylevels2 <- pretty(zlim, nlevels)
mylevels2[length(mylevels2)] <- max(c(new.z, mylevels2[length(mylevels2)]), na.rm=TRUE)

zlim <- c(-24,24)
nlevels <- 12
plevels <- pretty(zlim, nlevels)
plevels[1] <- min(c(new.p, plevels[1]), na.rm=TRUE)
plevels[length(plevels)] <- max(c(new.p, plevels[length(plevels)]), na.rm=TRUE)
breaks=plevels


png(file=nameout,width = 850, height = 710,bg = "white")

if (i==2012) {
filled.contour(new.x, new.y, new.t,color.palette=anom.palette,axes=F,
,main=paste("OSTIA SST, HadAM3P Precip & Geopot 200 Anomalies \n Dec ",year1," - May ",year2," (baseline 1986-2014)",sep="")
,levels=levels,frame.plot=F,cex.main=2,cex.lab=1.4,asp=1,xaxs="i",cex.sub=0.75,cex.axis=1.2
,plot.axes={
image(new.x, new.y, new.p,col=prcp.palette,add = T,frame.plot=F,breaks=breaks)
;map("world2",add=T, projection="orthographic", orientation=c(60, -100, 0))
;lines(rx,ry,lwd=2);lapply(c(lon.lines.proj,lat.lines.proj),lines,lwd=0.8,lty=3,col="grey50")})
#,key.axes={axis(4, seq(-4,4,0.2),cex.axis=1.6)})
par(new=T)
rect(0.72,-1.2,1.2,1E11,col="white",border=NA,density=NA)
par(new=T,plt = c(0.825,0.855,0.11,0.90),las = 1)
filled.legend(new.x,new.y,new.p,col=prcp.palette,levels=breaks,key.axes={axis(4, seq(-80,80,10),cex.axis=1.6)})
par(new=T,plt = c(0.91,0.94,0.11,0.90),las = 1)
filled.legend(new.x,new.y,new.t,color.palette=anom.palette,levels=levels,key.axes={axis(4, seq(-4,4,0.2),cex.axis=1.6)})
} else {
filled.contour(new.x, new.y, new.t,color.palette=anom.palette,axes=F,
,main=paste("OSTIA SST, HadAM3P Precip & Geopot 200 Anomalies \n Dec ",year1," - May ",year2," (baseline 1986-2014)",sep="")
,levels=levels,frame.plot=F,cex.main=2,cex.lab=1.4,asp=1,cex.sub=0.75,cex.axis=1.2
,plot.axes={image(new.x, new.y, new.p,col=prcp.palette,add = T,frame.plot=F,breaks=breaks)
;map("world2",add=T, projection="orthographic", orientation=c(60, -100, 0))
;contour(new.x,new.y,new.z,levels=mylevels1,add=T,col="grey33",lwd=2,lty=2)
;contour(new.x,new.y,new.z,levels=0,add=T,col="grey11",lwd=2,lty=1)
;contour(new.x,new.y,new.z,levels=mylevels2,add=T,col="grey33",lwd=2,lty=1)
;lines(rx,ry,lwd=2);lapply(c(lon.lines.proj,lat.lines.proj),lines,lwd=0.8,lty=3,col="grey50")})
#,key.axes={axis(4, seq(-4,4,0.2),cex.axis=1.6)})
par(new=T)
rect(0.72,-1.2,1.2,1E11,col="white",border=NA,density=NA)
par(new=T,plt = c(0.825,0.855,0.11,0.90),las = 1)
filled.legend(new.x,new.y,new.p,col=prcp.palette,levels=breaks,key.axes={axis(4, seq(-80,80,10),cex.axis=1.6)})
par(new=T,plt = c(0.91,0.94,0.11,0.90),las = 1)
filled.legend(new.x,new.y,new.t,color.palette=anom.palette,levels=levels,key.axes={axis(4, seq(-4,4,0.2),cex.axis=1.6)})
}

dev.off()

}
