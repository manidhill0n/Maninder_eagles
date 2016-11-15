#dataframe to raster
install.packages("raster")
length(df$measure1)
library(raster)
r2<-raster(nrows=100,ncols=100)
r2

r2[]<-df$measure2[1:1000]
plot(r2)
r2

r1<-raster(nrows=100,ncols=100)
r1

r1[]<-df$measure2[1:1000]
plot(r1)
r1

r12<-stack(r1,r2)
r12
plot(r12[[1]])
r12[[1]]

r12$new<-r12[[1]]*r12[[2]]^2
r12

rset<-raster(nrows=30,ncols=30)
rset

df12<-r12[]
head(df12)
