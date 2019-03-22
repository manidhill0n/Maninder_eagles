library(tidyr)
library(xlsx)
library(dplyr)
library(gtools)
library(ggplot2)
library(gganimate)
library(gifski)
library(data.table)
library(ggthemes)
library(lubridate)
library(stringr)


#Bubble plots

fusiondata<-read.xlsx("D:\\Master_thesis_fin\\My_results_fin\\RESULTS\\animation\\lue.xlsx", sheetName = "comp")
head(fusiondata)


fusiondata$Date<-as.Date(fusiondata$Date)

#Adding the dates in proper order
fusiondata1 = fusiondata %>% 
  arrange(Date, RMSE) %>% 
  mutate(order = 1:n())
#Create a ggplot
RMSE<-ggplot(fusiondata1,aes(x=Bands,y=RMSE, label=Bands,color=Bands))+
  geom_point(stat="identity",size=15)+
  geom_segment(aes(y=0.01,x=Bands,yend=RMSE,xend=Bands))+
  geom_text(color="black",size=3)+
  coord_flip()+
  theme(legend.position = "none")+
  scale_color_manual(values=c("#9999CC","#66CC99","#E69F00","#CC6666"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),plot.title = element_text(size = 14, face = "bold",hjust = 0.5),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))+
  transition_states(Date, transition_length = 1, state_length = 1)+
  labs(title='RMSE: {closest_state}',x="Bands",y="RMSE")+
  ease_aes("linear")

anim_save("RMSE.gif")

