a<-sqrt(2)

if(a*a=2)
{
  print("R is great")
}

else
{
  print("ggggg")
}

j<-0
while(j<1)
{
  j<-j+0.1;
  print(j)
}

myfunction<-function(x,y){
 z<-x+y
  return(z)
}

myfunction(4,3)

func_ndvi<-function(nir,red)
{
  (nir-red)/(nir+red)
}
func_ndvi(0.4,0.6)

library(cluster)







