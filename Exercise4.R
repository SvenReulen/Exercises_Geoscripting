# Author: Sven Reulen
# Date: 12-11-2013
# Dependencies: 
# Description: Fourth exercise for the course Applied Geoscripting.  
# Variables:

library(sp)
library(rgdal)
library(rgeos)
library(rasta)

##### Assignment1: Describe in your own words the function CreateHarvestTracks
# CreateHarvestTracks is a function that takes as input a data frame and an object of class CRS (Coordinate Reference System) to
# create a SpatialLinesDataFrame which consists of the tracks that a harvester has driven, these tracks include dates which are 
# ordered before input in the data frame.

# Preparation: Download file, set coordinate system, set date, create the lines,
download.file("http://rasta.r-forge.r-project.org/kroonven.csv", "kroonven.csv")
borne_data = read.table("kroonven.csv", sep = ",", header = TRUE)
buffered_lines <- gBuffer(sp_lines_df,byid=T, width=0.5*sp_lines_df$width,capStyle="FLAT")
coordinates(borne_data) <- c("lon.degr.","lat.degr.")
borne_data@proj4string <- prj_string_WGS
all_rd <- spTransform(borne_data, prj_string_RD)
dimnames(all_rd@coords)[[2]] <- c("x", "y")
all_rd$datetime <- as.POSIXct(paste(paste(all_rd$year, all_rd$month, all_rd$day,
                                          sep="-"), paste(all_rd$hr, all_rd$min, all_rd$sec,
                                                          sep=":")), tz="Europe/Andorra")
all_rd <- as.data.frame(all_rd)
all_rd <- all_rd[order(all_rd$datetime),]
sp_lines_df <- CreateHarvestTracks(all_rd, prj_string_RD)
spplot(sp_lines_df, zcol="ID", lwd=1.5, col.regions =
         bpy.colors(nlevels(sp_lines_df$ID)))

#1. buffer the cleaned lines to create harvest blocks, i.e. blocks where a particular load came from;
sp_polys <- gBuffer(sp_lines_df,byid=T, width=0.5*sp_lines_df$width,capStyle="Round")
plot(sp_polys)

#2. Fill small holes within these blocks by swelling and shrinking;
# Swelling
sp_polys <- gBuffer(sp_polys, byid=T,id=rownames(sp_polys), width = 2.0)
plot(sp_polys)
# Shrinking
sp_polys <- gBuffer(sp_polys, byid=T,id=rownames(sp_polys), width = -2.0)
plot(sp_polys)

#3. compute for each block the yield per hectare and add this attribute to the spatial polygons data frame;
areas <- gArea(sp_polys, byid=T)
loadsperhectar<-(areas/10000)/sp_polys$loads
sp_polys$loadsperhectare <-loadsperhectar 
sp_polys$loadsperhectare

#4. make a map showing the the yield per hectare of each block, using spplot;
spplot(sp_polys, zcol = 'red')
?spplot
#5. export the spatial polygons data frame to display polygon boundaries in Google Earth using writeOGR;


library(sp)
library(rgdal)
library(rgeos)

Point1 <- cbind(5.670029, 51.982942)
Point2 <- cbind(5.666899, 51.969465)
coords <- rbind(Point1, Point2)
prj_string_WGS <- CRS("+proj=longlat +datum=WGS84")
mypoints <- SpatialPoints(coords, proj4string=prj_string_WGS)
class(mypoints)
str(mypoints)

mydata <- data.frame(cbind(id = c(1,2),
                           Name = c("Footballfield Wageningen", "Busstation Wageningen")))
plot(mydata)
mypointsdf <- SpatialPointsDataFrame(coords, data = mydata, proj4string=prj_string_WGS)
plot(mypointsdf)
class(mypointsdf) 
names(mypointsdf)
str(mypointsdf)
spplot(mypointsdf, zcol="Name", col.regions = c("red", "blue"), xlim = bbox(mypointsdf)[1, ]+c(-0.01,0.01),
       ylim = bbox(mypointsdf)[2, ]+c(-0.01,0.01), scales= list(draw = TRUE))
simple_line <- Line(coords)
lines_obj <- Lines(list(simple_line), "1")
spatlines <- SpatialLines(list(lines_obj), proj4string=prj_string_WGS)
line_data <- data.frame(Name = "straight line", row.names="1")
mylinesdf <- SpatialLinesDataFrame(spatlines, line_data)
class(mylinesdf)
str(mylinesdf)
spplot(mylinesdf, col.regions = "blue",xlim = bbox(mypointsdf)[1, ]+c(-0.01,0.01),
       ylim = bbox(mypointsdf)[2, ]+c(-0.01,0.01),scales= list(draw = TRUE))
dirdat <- "M:/Applied_geoscripting/data"
writeOGR(mypointsdf, file.path(dirdat,"mypointsGE.kml"),
         "mypointsGE", driver="KML", overwrite_layer=TRUE)
writeOGR(mylinesdf, file.path(dirdat,"mylinesGE.kml"),
         "mylinesGE", driver="KML", overwrite_layer=TRUE)
myroute <- readOGR(file.path(dirdat,"Route.kml"), "Route.kml")
myroute@proj4string <- prj_string_WGS
names(myroute)
myroute$Description <- NULL
mylinesdf <- rbind(mylinesdf, myroute)
plot(mylinesdf, col= 'red')
plot(myroute)
prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889
                     + +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,
                     + -0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")
mylinesRD <- spTransform(mylinesdf, prj_string_RD)
plot(mylinesRD, col= "blue")
mylinesdf$length <- gLength(mylinesRD, byid=T)
mypointsRD <- spTransform(mypointsdf, prj_string_RD)
pnt1_rd <- coordinates(mypointsRD)[1,]
pnt2_rd <- coordinates(mypointsRD)[2,]
ang <- pi*0:200/100
circle1x <- pnt1_rd[1] + cos(ang) * mylinesdf$length[1]
circle1y <- pnt1_rd[2] + sin(ang) * mylinesdf$length[1]
circle2x <- pnt2_rd[1] + cos(ang) * mylinesdf$length[1]
circle2y <- pnt2_rd[2] + sin(ang) * mylinesdf$length[1]
circle1 <- Polygons(list(Polygon(cbind(circle1x, circle1y))),"1")
circle2 <- Polygons(list(Polygon(cbind(circle2x, circle2y))),"2")
spcircles <- SpatialPolygons(list(circle1, circle2), proj4string=prj_string_RD)
circledat <- data.frame(mypointsRD@data, row.names=c("1", "2"))
circlesdf <- SpatialPolygonsDataFrame(spcircles, circledat)
buffpoint <- gBuffer(mypointsRD[1,], width=mylinesdf$length[1], quadsegs=25)
mydiff <- gDifference(circlesdf[1,], buffpoint)
gArea(mydiff)
myintersection <- gIntersection(circlesdf[1,], buffpoint)
gArea(myintersection)
print(paste("The difference in area =", round(100 * gArea(mydiff) / gArea(myintersection),3), "%"))
spplot(circlesdf, zcol="Name", col.regions=c("gray60", "gray40"),
       sp.layout=list(list("sp.points", mypointsRD, col="red", pch=19, cex=1.5),
                      list("sp.lines", mylinesRD, lwd=1.5)))
download.file("http://rasta.r-forge.r-project.org/kroonven.csv", "kroonven.csv")
borne_data = read.table("kroonven.csv", sep = ",", header = TRUE)
borne_data
names(borne_data)
coordinates(borne_data) <- c("lon.degr.","lat.degr.")
borne_data@proj4string <- prj_string_WGS
all_rd <- spTransform(borne_data, prj_string_RD)
dimnames(all_rd@coords)[[2]] <- c("x", "y")
all_rd
spplot(all_rd, zcol="yield.ton.ha.", colorkey=T, zlim=c(0,100),
       col.regions=c(bpy.colors(25), rep("yellow", 75)), pch=19,
       cex=0.25, main="Recorded yields ton/ha")
all_rd$datetime <- as.POSIXct(paste(paste(all_rd$year, all_rd$month, all_rd$day,
                                          sep="-"), paste(all_rd$hr, all_rd$min, all_rd$sec,
                                                          sep=":")), tz="Europe/Andorra")
?POSIXct
all_rd <- as.data.frame(all_rd)
all_rd <- all_rd[order(all_rd$datetime),]
all_rd
library(rasta)
sp_lines_df <- CreateHarvestTracks(all_rd, prj_string_RD)
prj_string_RD
names(sp_lines_df)
spplot(sp_lines_df, zcol="ID", lwd=1.5, col.regions =
         bpy.colors(nlevels(sp_lines_df$ID)))
??CreateHarvestTracks

# Buffer lines to make swaths
sp_polys <- gBuffer(sp_lines_df,byid=T, width=0.5*sp_lines_df$width,capStyle="FLAT")
plot(sp_polys)
# fill small holes by swelling and shrinking
sp_polys <- gBuffer(sp_polys, byid=T,id=rownames(sp_polys), width = 2.0)
plot(sp_polys)
sp_polys <- gBuffer(sp_polys, byid=T,id=rownames(sp_polys), width = -2.0)
plot(sp_polys)
sp_polys_df <- SpatialPolygonsDataFrame(sp_polys, sp_lines_df@data)
plot(sp_polys_df)
# Remove line segments that are within already harvested swaths using
# gDifference
tmp_lines <- sp_lines_df@lines # just a list with geometries
for (i in 2:length(sp_lines_df)){
  tmline <- sp_lines_df[i,]$datim
  for (j in 1:(i-1)){
    tmpoly <- sp_polys_df[j,]$datim
    if (difftime(tmline, tmpoly, units = "secs") > 0){
      tmp_line <- SpatialLines(tmp_lines[i], prj_string_RD)
      if (gIntersects(tmp_line, sp_polys_df[j,])){
        # compute difference
        tmp_lines[[i]] <- gDifference(tmp_line, sp_polys_df[j,])@lines[[1]]
        tmp_lines[[i]]@ID <- sp_lines_df[i,]@lines[[1]]@ID
      }
    }
  }
}
tmp_lines <- SpatialLines(tmp_lines, prj_string_RD)
cln_lines_df <- SpatialLinesDataFrame(tmp_lines, sp_lines_df@data)
plot(cln_lines_df)