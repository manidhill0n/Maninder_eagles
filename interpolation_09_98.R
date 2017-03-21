library(raster)
library(SparseM)
library(colorspace)
library(tmap)
library(RStoolbox)
library(rgdal)
library(sp)
library(randomForest)
library(kernlab)
install.packages("gstat")
library(ggplot2)
# assigning the working directory
workingdir<-("H:\\Pakistan_punjab\\R_coding\\Practice_1")
setwd(workingdir)
#Uploading the dem of UB
dem1<-brick("H:\\Eagle_subjects\\Sem1\\geostatistics\\Class_project\\practice_1\\UB.tif")
plot(dem1)
# Uploading the point shapefile of UB for the tear 2009
vector_09<-readOGR("H:\\Eagle_subjects\\Sem1\\geostatistics\\Class_project\\practice_1","ub09")
vector_98<-readOGR("H:\\Eagle_subjects\\Sem1\\geostatistics\\Class_project\\practice_1","ub98")
# Extracting the values of dem for the pont file UB
set.seed(20)
ptvals_98<-dem1[vector_98]
ptvals_09<-dem1[vector_09]
# assigning spatial cordinates to the vector file
xy<-coordinates(vector_98)
obs_98<-data.frame(X=x,Y=y)
coordinates(obs_98)<-c("X","Y")

xy<-coordinates(vector_09)
obs_09<-data.frame(X=x,Y=y)
coordinates(obs_09)<-c("X","Y")
# binding together both attributes
vector_98<-cbind(vector_98,ptvals_98)
vector_09<-cbind(vector_09,ptvals_09)
vector_98@data
vector_09@data
vector_98$hydraulic_head<-vector_98$UB-vector_98$YEAR98_JUN
vector_98@data
vector_09$hydraulic_head<-vector_09$UB-vector_09$YEAR09_JUN
vector_98@data
# Uploading the shapefile of UB
Shape_ub<-readOGR("H:\\Eagle_subjects\\Sem1\\geostatistics\\Class_project\\practice_1","UB")
# Creating the extent same
vector_98@bbox<-Shape_ub@bbox
vector_09@bbox<-Shape_ub@bbox
#tm_shape(Shape_ub) + tm_polygons() +
#  tm_shape(vector) +
# tm_dots(col="hydraulic_head", palette = "RdBu", auto.palette.mapping = FALSE,
#        title="hydraulic head \n(in meter)", size=0.7) +
#  tm_text("hydraulic_head", just="left", xmod=.5, size = 0.7) +
# tm_legend(legend.outside=TRUE)
# creating the variogram by fitting first degree polynomial equation

f.1 <- as.formula(hydraulic_head ~ X + Y) 
vector_98$X <- coordinates(vector_98)[,1]
vector_98$Y <- coordinates(vector_98)[,2]
var.smpl_98 <- variogram(f.1, vector_98, cloud = FALSE, cutoff=1000000, width=89900)
dat.fit_98  <- fit.variogram(var.smpl_98, fit.ranges = FALSE, fit.sills = FALSE,
                             vgm(psill=5, model="Sph", range=5900, nugget=0))
plot(var.smpl_98, dat.fit_98, xlim=c(0,10000))

vector_09$X <- coordinates(vector_09)[,1]
vector_09$Y <- coordinates(vector_09)[,2]
var.smpl_09 <- variogram(f.1, vector_09, cloud = FALSE, cutoff=1000000, width=89900)
dat.fit_09  <- fit.variogram(var.smpl_09, fit.ranges = FALSE, fit.sills = FALSE,
                             vgm(psill=5, model="Sph", range=5900, nugget=0))
plot(var.smpl_09, dat.fit_09, xlim=c(0,10000))
# creating an empty grid with same projection as vector file
grd_98              <- as.data.frame(spsample(vector_98, "regular", n=50000))
names(grd_98)       <- c("X", "Y")
coordinates(grd_98) <- c("X", "Y")
gridded(grd_98)     <- TRUE  # Create SpatialPixel object
fullgrid(grd_98)    <- TRUE  # Create SpatialGrid object
proj4string(grd_98) <- proj4string(vector_98)

grd_09              <- as.data.frame(spsample(vector_09, "regular", n=50000))
names(grd_09)       <- c("X", "Y")
coordinates(grd_09) <- c("X", "Y")
gridded(grd_09)     <- TRUE  # Create SpatialPixel object
fullgrid(grd_09)    <- TRUE  # Create SpatialGrid object
proj4string(grd_09) <- proj4string(vector_09)

# applying universal kriging on the vector and save it in empty grid
Shape_ub$X <- coordinates(Shape_ub)[,1]
Shape_ub$Y <- coordinates(Shape_ub)[,2]
dat.krg_98 <- krige( f.1, vector_98, grd_98, dat.fit_98)
r_98 <- raster(dat.krg_98)

dat.krg_09 <- krige( f.1, vector_09, grd_09, dat.fit_09)
r_09 <- raster(dat.krg_09)
# masking the raster to shapefile of UB
r.m_98 <- mask(r_98, Shape_ub)
r.m_98
r.m_09 <- mask(r_09, Shape_ub)
r.m_09
# Resampling DEM according to the created krigged raster
e<-extent(916047.4, 1009872, 1014926, 1116170)
s<-raster(e, nrows=232, ncols=215)
dem_98<-resample(dem1, s)
Rastr_98<-dem_98-r.m_98
Rastr_98
plot(Rastr_98)


e<-extent(916047.4, 1009872, 1014926, 1116170)
s<-raster(e, nrows=232, ncols=215)
dem_09<-resample(dem1, s)
# Subtracting both rstars to get final water table raster
Rastr_09<-dem_09-r.m_09
Rastr_09
plot(Rastr_09)
CombineRaster<-stack(Rastr_98,Rastr_09)
plot(CombineRaster)
