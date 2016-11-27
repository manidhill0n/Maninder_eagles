df <- data.frame(plot="location_name_1",measure1=runif(100)*1000,measure2=round(runif(100)*100),
                 value=rnorm(100,2,1),ID=rep(LETTERS,100))

head(df)

df_2 <- data.frame(plot="location_name_2",measure1=runif(50)*1000,measure2=round(runif(50)*10),
                   value=rnorm(50),ID=rep(LETTERS,50))
df <- rbind(df,df_2)





r1 <- raster(nrows=100,ncols=100)
r1[] <- df$measure2[1:1000]

r2 <- raster(nrows=100,ncols=100)
r2[] <- df$measure1[1:1000]

r12 <- stack(r1,r2)
r12$new <- r12[[1]]*r12[[2]]^2
r12$hans <- rnorm(50,mean=13,sd=19)

plot(r12)

r1d <- as.data.frame(r12)
plot(r1d)

func_ndvi<-function(layer.1,layer.2)
{
  (layer.1-layer.2)/(layer.1+layer.2)
}

func_ndvi<-function(r12[[1]],r12[[2]])
{
  (r12[[1]]-r12[[2]])/(r12[[1]]+r12[[2]])
}


ndvi_fun<-function(nir,red)
{
  (nir-red)/(nir+red)
}

ndvi<-function(r1[[4]],r1[[3]])
plot(ndvi)


library(cluster)
r2d<-as.data.frame(r12)
plot(r12)

kmeans(r12,iter.max=5L,nstart = 3L)
idx<-kmeans(r12,5L)

kmeans_out<-kmeans(r12,12,iter.max=100,nstart=10)
