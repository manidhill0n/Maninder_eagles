# Frame 1
# Spatial Biomass growth using a Light Using Efficiency model in R
# Needed Libraries
library(raster)
library(moveVis)
library(ggplot2)
library(rgdal)
library(sf)

# Setting directories
wd<-"~\\animation"
setwd(wd)

# Loading the required data
# lue raster file with per day biomass growth
lue <- brick("~\\lue_2015.tif")
# Insitu sample points
WW_points <- readOGR("~\\plotsandfields","WW")
# Shapefile of the fields
WW_shape <- sf::st_read("~\\plotsandfields", "WW-felder")
# Assigning the same projection
WW_shape <- st_transform(WW_shape, st_crs(WW_points))

# Applying the breaks with minimum and maximum intervals.
br <- seq(min(minValue(lue)), max(maxValue(lue)), len=8)
data.points <- as.data.frame(WW_points,xy=TRUE)
data.points$label <- "points & fields"
# creating a colour ramp for the point using colorRampPalette function
d= densCols(data.points$x, data.points$x, colramp = colorRampPalette(brewer.pal(12,"Paired")))

i.max <- nlayers(lue)
# crearing fra,es using ggplot
frames <- mapply(x = unstack(lue)[1:i.max], y = paste0("DOY ", c(80:(79+nlayers(lue))))[1:i.max], function(x, y){
  RStoolbox::ggR(x, ggObj = T, geom_raster = T) +
    scale_fill_distiller(palette="Spectral", na.value="white",limits=c(min(br), max(br)), name = y) + theme_bw() +
    scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
    labs(x = "Easting", y = "Northing", title = "Spatial biomass growth (g/m^2) using a Light Use Efficiency model",
         caption = "Projection: UTM WGS84 Zone 33N. Source: Synthetic StarFM Landsat/MODIS data.\nDate: 02.04.2019. Author: Maninder Singh Dhillon, Thorsten Dahms") +
     theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +
    geom_point(data = data.points, aes(x=coords.x1, y=coords.x2,color=d),show.legend = FALSE) +
    geom_sf(data = WW_shape, colour = "black", fill = NA, show.legend = NA) +
    guides(color = guide_legend(order = 1)) +# to fix the legend order
    coord_sf(crs = sf::st_crs(x), datum = sf::st_crs(x))
}, SIMPLIFY = F)

# Frame 2
# Plotwise temporal pattern of biomass growth of winter wheat in 2015
# Needed libraries
library(tidyr)
library(xlsx)
library(dplyr)
library(ggplot2)
library(gifski)
library(data.table)
library(stringr)

#reading the excel file 
df<-read.xlsx("~\\lue.xlsx", sheetName = "Sheet4")
head(df)
# fixing the dates into year, month and day
colnames(df)[2]<-"cycle"  # rename first column
df$year<-substr(df$cycle,1,4) #create a year
df$month<-as.numeric(substr(df$cycle,7,7)) #create a
df$date<-as.Date(df$cycle)
# Transposing the columns into rows
colnames(df)[1]<-"DOY"
df %>% select(-cycle,-year,-month) %>% 
  gather(Plots,values,-date,-DOY) ->Growth_curve.df

dt<-Growth_curve.df
dt<-data.table(dt)[year(date)>0,]
dt<-Growth_curve.df
df3<-dt

# data for plots
clist<-c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18")
dt2<-data.table(df3)[ Plots %in% clist,]
dlist<-unique(dt2[year(date)>0]$date)
N<-length(dlist)

# rescale data ----
dt3 <- dt2 %>% 
  group_by(Plots) %>% 
  
  ungroup() %>% data.table

nlist<-as.data.frame(as.character(c(80:252)))
# creating frames with ggplot
frames_lp <- lapply(1:length(dlist), function(i){
  ggplot(data=dt3[ 
    date<=dlist[i] &
      Plots %in% clist],
    aes(x=date,y=values,color=Plots,label=Plots))+
    geom_line(size=1.1,linetype="dotted")+
    scale_x_date(breaks=seq(dlist[1],dlist[N],"1 month"),
                 date_labels="%B",limits=c(dlist[1],dlist[N]))+
    theme_minimal()+ #geom_hline(yintercept=100,linetype=2)+
    scale_y_continuous(breaks=c(100,200,400,800,1200,1500,1800,2200,2600,3000),limits=c(0,3200))+ 
    labs(x="Month",y="Growth rate (g/m^2)",fill="Legend",title = "Plot-wise temporal pattern of biomass growth in 2015")+
    theme_bw() + theme(plot.title = element_text(hjust = 0.5),
                       panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())+
    theme(axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1), aspect.ratio = 1)
})

# Joining both temporal line plot and spatial plot with moveVis function
frames_joined <- join_frames(list(frames, frames_lp), ncol = 2, nrow = 1, axis = "tb", align = "hv")


# Animating the joined frames in the form of a GIF
animate_frames(frames_joined, "Spatio_temporal_growth_5fps_res.gif", overwrite = T, fps = 5, height = 750, width = 1400,res=125)
# Animating the joined frames in the form of a movie
animate_frames(frames_joined, "Spatio_temporal_growth_5fps.mov", overwrite = T, fps = 5, height = 750, width = 1400)


