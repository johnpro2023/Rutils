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

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

###
### from: WWA
###
# """ from http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/393241
# 	much more info at: http://en.wikipedia.org/wiki/Great_circle_distance
# 
# this is using the haversine arcsin method (which won't work
# well for points on opposite sides of the earth, but that
# shouldn't be a problem for this data)
# 
# this will return the distance in METERS
# 
# "other" can be another FixPoint or a 2-tuple of (lat,lng)
# 
# """
# 
# (alat,alng) = a
# (blat,blng) = b
# 
# lngdist = math.radians(blng - alng)
# latdist = math.radians(blat - alat)
# 
# alat = math.radians(alat)
# blat = math.radians(blat)
# alng = math.radians(alng)
# blng = math.radians(blng)
# 
# a = (math.sin(latdist / 2))**2 + math.cos(alat) * math.cos(blat) * (math.sin(lngdist / 2))**2
# c = 2 * math.asin(min(1, math.sqrt(a)))
# # dist = 6399.592 * c  # magic number for poles
# # dist = 6335.437 * c  # magic number for equator
# dist = 6372.795 * c # average arcradius of Earth (in km)
# return int(dist * 1000)              # return meters, not kilometers (don't return decimals; up to 0.5% error anyway)

gdist <-
  function(lon.1, lat.1, lon.2, lat.2, units = 'nm', a = 6378137.0, b = 6356752.3142, verbose = FALSE)
  {
    #
    # Calculate geodesic distance (in nm) between two points specified by latitude/longitude
    # using Vincenty inverse formula for ellipsoids
    # Reference: Direct and inverse solutions of geodesics on the ellipsoid with application
    #  of nested equations.  Survey Review XXII, 176, April 1975.
    #
    # Inspired by: http://www.movable-type.co.uk/scripts/LatLongVincenty.html
    #
    #   DATE WRITTEN:  10 January 2005      LAST REVISED:   04 January 2010
    #   AUTHOR:  John R. Wallace: Imap.for.R@gmail.com
    #
    #
    if(any(!is.finite(c(lon.1, lat.1, lon.2, lat.2))))
      return(NA)
    
    # a, b = major & minor semiaxes of the ellipsoid in meters
    # flat = flattening (a-b)/a
    # lat.1, lat.2 = geodetic latitude
    # L = difference in longitude
    
    
    rad <- pi/180
    lon.1 <- lon.1 * rad
    lat.1 <- lat.1 * rad
    lon.2 <- lon.2 * rad
    lat.2 <- lat.2 * rad
    
    flat <- (a - b)/a
    L <- lon.1 - lon.2
    U1 <- atan((1 - flat) * tan(lat.1))
    U2 <- atan((1 - flat) * tan(lat.2))
    
    lamda <- L
    lamda.old <- 2 * pi
    if(verbose)
      cat("\nStarting lamda =", lamda, "\n\n")
    
    i <- 1
    while(abs(lamda - lamda.old) > 1e-011) {
      sin.sigma <- sqrt((cos(U2) * sin(lamda))^2 + (cos(U1) * sin(U2) - sin(U1) * cos(U2) * cos(lamda))^2)
      cos.sigma <- sin(U1) * sin(U2) + cos(U1) * cos(U2) * cos(lamda)
      sigma <- atan2(sin.sigma, cos.sigma)
      sin.alpha <- (cos(U1) * cos(U2) * sin(lamda))/ifelse(sin(sigma) == 0, 1e-025, sin(sigma))
      cos2.alpha <- 1 - sin.alpha^2
      cos2.sigma.m <- cos(sigma) - (2 * sin(U1) * sin(U2))/ifelse(cos2.alpha == 0, 1e-025, cos2.alpha)
      C. <- (flat/16) * (cos2.alpha * (4 + flat * (4 - 3 * cos2.alpha)))
      if(verbose) {
        cat("sin.sigma =", sin.sigma, "\n")
        cat("cos.sigma =", cos.sigma, "\n")
        cat("sigma =", sigma, "\n")
        cat("sin.alpha =", sin.alpha, "\n")
        cat("cos2.alpha =", cos2.alpha, "\n")
        cat("cos2.sigma.m =", cos2.sigma.m, "\n")
        cat("C =", C., "\n")
        cat("lamda diff =", lamda - lamda.old, "\n")
      }
      lamda.old <- lamda
      lamda <- L + (1 - C.) * flat * sin.alpha * (sigma + C. * sin.sigma * (cos2.sigma.m + C. * cos.sigma * (-1 + 2 * cos2.sigma.m^2)))
      if(verbose)
        cat("New lamda =", lamda, "\n\n")
      if(i > 20) {
        warning("lamda did not converge")
        return(NA)
      }
      i <- i + 1
    }
    
    u2 <- (cos2.alpha * (a^2 - b^2))/b^2
    A <- 1 + (u2/16384) * (4096 + u2 * (-768 + u2 * (320 - 175 * u2)))
    B <- (u2/1024) * (256 + u2 * (-128 + u2 * (74 - 47 * u2)))
    delta.sigma <- B * sin(sigma) * (cos2.sigma.m + (B/4) * (cos(sigma) * (-1 + 2 * cos2.sigma.m^2) - (B/6) * cos2.sigma.m * (-3 + 4 * sin(sigma)^2) * (
      -3 + 4 * cos2.sigma.m^2)))
    
    if(verbose) {
      alpha1 <- atan((cos(U2) * sin(lamda))/(cos(U1) * sin(U2) - sin(U1) * cos(U2) * cos(lamda)))
      cat("\nalpha1 =", alpha1/rad, "\n")
      alpha2 <- atan((cos(U1) * sin(lamda))/( - sin(U1) * cos(U2) + cos(U1) * sin(U2) * cos(lamda)))
      cat("alpha2 =", alpha2/rad, "\n\n")
      ""
      cat("2*sigma.m =", acos(cos2.sigma.m), "\n")
      ""
      cat("b =", b, "\n")
      cat("A =", A, "\n")
      cat("sigma (radians) =", sigma, "\n")
      cat("delta.sigma (radians) =", delta.sigma, "\n")
      cat("Distance: s = b * A * (sigma - delta.sigma)\n\n")
      ""
    }
    
    s <- (b * A * (sigma - delta.sigma))
    
    switch(units, 
           m = s,
           km = s/1000,
           nm = s/1852,
           miles = s/1609.344)
    
    
    
  }

# Example
# Latitude  Longitude Y          	X
# 50.812185	-100.6467	5630231.78	383989.06

# UTM14
# Between 102°W and 96°W, northern hemisphere between equator and 84°N, onshore and offshore. 
# Canada - Manitoba; Nunavut; Saskatchewan. Mexico. United States (USA).

# UTM15
# Between 96°W and 90°W, northern hemisphere between equator and 84°N, onshore and offshore. 
# Canada - Manitoba; Nunavut; Ontario. Ecuador -Galapagos. Guatemala. Mexico. United States (USA).

longv1 <-c( -96.02080,-94.900,-100.4521067, -95.95722,-95.99053,-96.03122, -94.55567)
latv1  <-c(  50.48186, 50.100,  50.9536081,  50.46606, 50.48395, 50.48625,  51.45413)

longv2 <-c( -94.900,-94.800,-100.4921433, -95.99053,-96.03122,-96.03549, -96.90819)
latv2  <-c(  50.100, 50.100, 50.9367483,   50.48395, 50.48625, 50.47795,  49.73388)

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
  
  gcd1 = gcd.slc(long1,lat1,long2,lat2)
  gcd2 = gdist(long1,lat1,long2,lat2,units = "m")
  
  Derr     = max(abs(d14-d15))
  UTM14err = max(abs(d14-gcd2))
  UTM15err = max(abs(d15-gcd2))
  perr     = abs(Derr/d14)*100

  message("  ")
  message(" >>> Case: ",i," GCD2=",gcd2, " UTM14vsUTM15=",Derr," Long1=",long1," Lat1=",lat1," Long2=",long2," Lat2=",lat2)
  message(" >>>   UTM14 Dist=", d14,"  UTM14vsGCD=",UTM14err," x1=",x141," y1=",y141," x2=",x142," y2=",y142)
  message(" >>>   UTM15 Dist=", d15,"  UTM15vsGCD=",UTM15err," x1=",x151," y1=",y151," x2=",x152," y2=",y152)
  message(" >>>   UTM14 vs UTM15 Error%=", perr)
  
}



