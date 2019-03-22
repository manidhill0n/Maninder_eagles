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

df<-read.xlsx("D:\\Master_thesis_fin\\My_results_fin\\RESULTS\\animation\\lue.xlsx", sheetName = "Sheet1")
head(df)

colnames(df)[2]<-"cycle"  # rename first column
df$year<-substr(df$cycle,1,4) #create a year
df$month<-as.numeric(substr(df$cycle,7,7)) #create a
df$date<-as.Date(df$cycle)

colnames(df)[1]<-"DOY"
df %>% select(-cycle,-year,-month) %>% 
  gather(Growth_rate,values,-date,-DOY) ->Growth_curve.df


dt<-Growth_curve.df
dt<-data.table(dt)[year(date)>0,]
dt<-Growth_curve.df
df3<-dt
df3$Growth_rate[df3$Growth_rate=="Optimal.Growth"]<-"Optimal Growth"
df3$Growth_rate[df3$Growth_rate=="Actual.Growth"]<-"Actual Growth"
#create a caption for attribution to source
df3$Growth_rate

# data for plots
clist<-c("Optimal Growth","Actual Growth")
dt2<-data.table(df3)[ Growth_rate %in% clist,]
dlist<-unique(dt2[year(date)>0]$date)
N<-length(dlist)

# rescale data ----
dt3 <- dt2 %>% 
  group_by(Growth_rate) %>% 
  
  ungroup() %>% data.table

nlist<-as.data.frame(as.character(c(80:252)))


Growth_curve.plot<-function(i=1){
  ggplot(data=dt3[ 
    date<=dlist[i] &
      Growth_rate %in% clist],
    aes(x=date,y=values,color=Growth_rate,label=Growth_rate))+
    geom_line(size=1.1)+
    scale_x_date(breaks=seq(dlist[1],dlist[N],"1 month"),
                 date_labels="%B",limits=c(dlist[1],dlist[N]))+
    scale_color_manual(values=c("#CC6666","black"))+
    
    theme_minimal()+ #geom_hline(yintercept=100,linetype=2)+
    
    scale_y_continuous (breaks=c(0,20,50,100,200,300,400,500,600,700,800,900,1000),limits=c(0,1000))+ 
    
    labs(x="Month",y="Growth rate",fill="Legend")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme_bw() + theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())+
    theme(axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1))
}



Growth_curve.plot(N)


mydir <- "D:\\Maninder\\RESULTS\\animation" #change
j=N
gif_file <- save_gif({for (i in seq(1,j)){  
  g<- Growth_curve.plot(i)
  print(g)
  print(paste(i,"out of",j))
}
  # for (ii in 1:1){
  #  print(g)
  #  print(paste(ii,"out of",1))
  # }
}, gif_file= paste0(mydir,"/Actual_versus_optimal_growth.gif"),width = 1200, height = 800, res = 144, delay=1/10)
# show your plot:
utils::browseURL(gif_file)

