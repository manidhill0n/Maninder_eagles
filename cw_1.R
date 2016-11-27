#R software as a calculator
result1<-5+6
result1
# use of the sequence function
seq(1,9,by=2)
plot(seq(100))#plot function in R
#use of c function by adding A to the numbers ranging 1 to 100.
c("A",1:100)
c(1,2,3,4)
# Quit function
q()
n
# concept of precipitation in R
prec_avg<-c(34,51,66,71)
plot(prec_avg)
#Plotting precipitation where pch changes the shape of the points used in plotting and cex enlarges the shape and size of the point.
plot(prec_avg,pch=19,cex=3,col="#00ff0020")
lines(lowess(prec_avg,f=.2))
plot(prec_avg)
# task given in the class
x1<-c(1,2,3,4)# assign values to X1
plot(x1,pch=19,cex=3,col="#00ff0045")# plot x1 with diffrent color
log(x1)# apply logarithm function on x1
# Precipitaion data 
library(raster)
germany<-getData("GADM",country="DEU",level=2)
plot(germany)
prec<-getData("worldclim",var="prec",res=.5,lon=10,lat=51)
plot(prec)
prec_ger1<-crop(prec,germany)
spplot(prec_ger1)# spplot is the spatial plotting of the points
prec_ger2<-mask(prec_ger1,germany)
spplot(prec_ger2)
# calculating the mean of the prec using cellstats function
prec_avg<-cellStats(prec_ger2,stat="mean")
prec_avg
#Extracting the value of month july
prec_avg[7]
#plotting the values from april to sept
plot(prec_avg[4:9])
# substract jan from feb
prec_avg[2]-prec_avg[1]
min(prec_avg)# shows the minimum value
which.min(prec_avg)# shows the minmum value in which month
diff(prec_avg)# shows the difference in precipitation
# how to show precipitation closest to any value
min(abs(prec_avg='50'))




