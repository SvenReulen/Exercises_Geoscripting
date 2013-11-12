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
sp_polys$new <- (areas/10000)/sp_polys$loads

#4. make a map showing the the yield per hectare of each block, using spplot;
spplot(sp_polys, zcol="yield.ton.ha.", colorkey=T, zlim=c(0,100),
       col.regions=c(bpy.colors(25), rep("yellow", 75)), pch=19,
       cex=0.25, main="Recorded yields ton/ha")
#5. export the spatial polygons data frame to display polygon boundaries in Google Earth using writeOGR;