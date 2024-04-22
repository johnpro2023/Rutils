library(sp)
library(rgdal)

#Function
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

# Example
# Latitude  Longitude Y          	X
# 50.812185	-100.6467	5630231.78	383989.06

longv1 <-c( -94.99729,-94.99726,-94.89457,-94.99458,-94.99729,-100.6467)
latv1  <-c(  50.17112, 50.17107, 50.17273, 50.17278, 50.17112, 50.812185)

longv2 <-c( -94.99729,-94.89726,-94.99457,-95.99458,-96.99729,-103.6467)
latv2  <-c(  50.07112, 50.17107, 50.07273, 51.17278, 52.17112,  53.812185)

# Long == x
# Lat  == y
x<-c(-100.6467)
y<-c(50.812185)
foo = LongLatToUTM(x,y,14)
x1 = foo$X[1]
message(" X = ",x1)
y1 = foo$Y[1]
message(" Y = ",y1)  

n1 = length(longv1)

for (i in 1:n1) {
  long1  = longv1[i]
  lat1   = latv1[i]
  long2  = longv2[i]
  lat2   = latv2[i]
  foo141 = LongLatToUTM(long1,lat1,14)
  x141   = foo141$X[1]
  y141   = foo141$Y[1]
  foo142 = LongLatToUTM(long2,lat2,14)
  x142   = foo142$X[1]
  y142   = foo142$Y[1]
  
  foo151 = LongLatToUTM(long1,lat1,15)
  x151   = foo151$X[1]
  y151   = foo151$Y[1]
  foo152 = LongLatToUTM(long2,lat2,15)
  x152   = foo152$X[1]
  y152   = foo152$Y[1]
  
  d14 = sqrt((x142-x141)^2 + (y142-y141)^2)
  d15 = sqrt((x152-x151)^2 + (y152-y151)^2)
  
  dgc = 0
  
  derr = max(abs(d14-d15),abs(d14-dgc),abs(d15-dgc))

  message("  ")
  message(" >>> Case: ",i," Dist=",dgc,"  Max Err=",derr," Long1=",long1," Lat1=",lat1," Long2=",long2," Lat2=",lat2)
  message(" >>>        UTM14 Dist=", d14, " x1=",x141," y1=",y141," x2=",x142," y2=",y142)
  message(" >>>        UTM15 Dist=", d15, " x1=",x151," y1=",y151," x2=",x152," y2=",y152)

  
}



