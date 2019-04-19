# temporal analysis-Animation-1
# Needed libraries
library(ggplot2)
library(gifski)
library(data.table)
library(stringr)
library(gganimate)
library(dplyr)
library(xlsx)
library(magick)


# reading the excel file
df<-read.xlsx("/media/mad52nd/Daten/Manny_data/Bennys_pc/Download/World_yield.xlsx",sheetName = "Sheet2")
head(df)


#taking the subset of the map
df<-subset(df,df$Item=="Wheat" & df$Element=="Yield",select=c("Area.Code","Area2","Year","Value","country_iso3c"))
# Area2 signifies country names
# Value signifies yield values
# country iso3c is needed in the excel file to link with spatial map of the world
head(df)# check if you have all these selected row names in your table.

#taking the distict cities
countries <- df %>%  select("Area.Code","Area2","country_iso3c") %>% distinct()
#countries <- df %>%  select("Area.Code","Area2") %>% distinct()
#creating sequence of the years
all_years <- data.frame(Year = seq(1961, 2017, 1))
# merging thesequenced years with the contries
all_combos<-merge(countries, all_years)

#preparing dada for interpolation
# this step is imporatnt to make a nice bar race. we can also exclude this if we have every year data but then the bar race chart would fluctuate.
all_data_interp <- merge(df, all_combos, all.y = T)
# doing interpolation
all_data_interp <- all_data_interp %>%
  group_by(Area2) %>%
  mutate(Value=approx(Year,Value,Year)$y)

# creating the ranks
data$Area2
data <- all_data_interp %>%
  group_by(Year) %>%
  arrange(-Value) %>%
  mutate(rank=row_number()) %>%
  filter(rank<=15)
# changing the colname Area2 into country 
colnames(data)[2]<-"Country"
# creating the bar race

p <- data %>%
  ggplot(aes(x = -rank,y = Value, group = Country)) +
  geom_tile(aes(y = Value / 2, height = Value, fill = Value),colour = "grey50", width = 0.9) + #with legends
  geom_text(aes(label = Country), hjust = "right", colour = "black", fontface = "bold", nudge_y = 1) +
  coord_flip(clip="off") +
  scale_fill_distiller(name="Wheat Yield (kg/ha)",limits = c(0,vmax),palette = "Spectral",na.value="gray99") +
  labs(y = "Wheat Yield (kg/ha)",x="")+
  theme(plot.title = element_text(face="bold",size=16,hjust = 0.5),plot.subtitle=element_text(size=14, hjust=0.5, face="bold",color="black"),
        plot.caption = element_text(size=12,color="black"),panel.background = element_blank(),panel.grid.major.y=element_blank(),
        plot.margin=unit(c(0.3,4,0,0.7),"cm"),# plot margins are imporatnt to adjust this with spatial map
        panel.grid.minor.x=element_blank(),
        legend.position = "none",
        axis.text.x = element_text(colour = "black", size = rel(1.2)),
        axis.text.y=element_blank(),line = element_blank()) +
  
  # gganimate code to transition by year:
  #transition_states(Year, transition_length = 1, state_length = 1,wrap = TRUE)+
  labs(title='Temporal analysis of selected 15 efficient wheat yield (kg/ha) producing countries',subtitle='{round(frame_time, 0)}',caption="Data Source: www.fao.org")+
  transition_time(Year)+
  ease_aes("cubic-in-out",interval = 0.000001)

animate(p, fps=5,width=1300,height=500,end_pause = 20)
anim_save("temporal_world_new.gif")

# Spatial map 
# Needed libraries
library(curl)
library(readxl)
library(data.table)
library(rworldmap)
library(ggplot2)
library(dplyr)
library(tweenr)
library(ggthemes)
library(viridis)
library(rgeos)
library(devtools)
library(gganimate)
library(countrycode) # for country codes
library(gifski)
library(xlsx)
library(transformr)

# considering data without any rankings for spatial 
data <- all_data_interp %>%
  group_by(Year) %>%
  arrange(-Value) %>%
  mutate(rank=row_number())


head(data)
# get Min and Max values of wheat yield
vmax <- max(data$Value, na.rm=T)
vmin <- min(data$Value, na.rm=T)

# for getting spatial data / world map
wmap <- getMap(resolution="low")

#wmap <- spTransform(wmap, CRS("+proj=robin")) # if you need to reproject
wmap <-   subset(wmap, !(NAME %like% "Antar")) # Remove Antarctica from the world map


# get centroids of countries (if we need bubble diagrams on a map)
centroids <- gCentroid( wmap , byid=TRUE, id = wmap@data$ISO3)
centroids <- data.frame(centroids)
setDT(centroids, keep.rownames = TRUE)[]
setnames(centroids, "rn", "country_iso3c")
#
wmap_df <- fortify(wmap, region = "ISO3")

# combining spatial data with temporal data (wmap_df with data)

wmap_df <- left_join(wmap_df, data, by = c('id'='country_iso3c'))        # data
wmap_df2<-left_join(wmap_df, centroids, by = c('id'='country_iso3c'))
wmap_df <- left_join(wmap_df, centroids, by = c('id'='country_iso3c')) # centroids
wmap_df<-wmap_df[complete.cases(wmap_df), ] # remove NA values

colnames(wmap_df2)[10]<-"yyear"# Assiging the 10 column name different than year

o <- ggplot() +
  geom_polygon(data=wmap_df2,aes(x = long, y = lat, group = group),fill=NA,color="gray90") +
  geom_polygon(data=wmap_df,(aes(x = long, y = lat, group = group, fill=wmap_df$Value)), color="gray90") +
  scale_fill_distiller(name="Wheat Yield (kg/ha)",limits = c(0,vmax),palette = "Spectral",na.value="gray99") +
  theme_bw() +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  labs(x = "Longitude", y = "Latitude")+
  theme(plot.title = element_text(face="bold",size=16,hjust = 0.5),plot.subtitle=element_text(size=14, hjust=0.5, face="bold",color="black"),panel.background = element_blank(),
        legend.position = "right") +
  coord_sf(crs = sf::st_crs(wmap), datum = sf::st_crs(wmap))+
  labs(title='Spatial analysis of wheat yield from 1961 to 2017',subtitle='{round(frame_time, 0)}')+
  transition_time(Year)+
  ease_aes("cubic-in-out",interval = 0.000001)
animate(o, fps=5,width=1300,height=500,end_pause = 20,res=110)
anim_save("spatial_world_new.gif")

# Now we combine both plots together
