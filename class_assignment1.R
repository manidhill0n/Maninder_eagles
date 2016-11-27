X<-matrix(c(4,6,1,7,4,5),nrow=2)
X
X[2,2]
X[,1]
#creating the dataframe
numbers_1<-rnorm(80,mean=0,sd=1)
numbers_1
mat_1<-matrix(numbers_1,nrow=20,ncol=4)
mat_1
#convert it into data frame
df_1<-data.frame(mat_1)
df_1
names(df_1)<-c("var1","var2","var3","var4")
df_1
head(df_1)
tail(df_1)
df_1[,1]
df_1$var1
df_1$var1[1:5]
#genrating a vector
x<-seq(1,100,by=5)
x
x[5]
x[1:5]
x[length(x)]
x[length(x)-1]
x[-2]
idx<-c(1,3,16)
x[idx]
x[-idx]
x>20
x>20|x<10
#change values
x2<-numeric(length(x))
x2
x2[x<7]<-1
x2
x2[x>10&x>30]<-2
x2
install.packages("car")
library(car)
x2<-recode(x,"0:30=1;30:70=2;else=3")
x2
summary(x)
sum(x)
cumsum(x)
rev(x)
sample(x,10)
# 1.difference between sort and order
sort(x,decreasing=TRUE)
order(x,decreasing=FALSE)
length(x)
# Cut and Quantile commands
cut(x,10)
quantile(x,0)
quantile(x,0.6)
#genrate a data frame with two columns
test<-data.frame(a=c(1,2,3),b=c("a1","b1","c1"))
test
test[,1]