rm(list=ls(all=TRUE))

library(maptools)
gpclibPermit()

## Location of Helsinki, Finland, in decimal degrees,
## as listed in NOAA's website
#         Long = 24.97  Lat = 60.17 
hels <- matrix(c(24.97, 60.17), nrow=1)

# RMNP
#         Long = -100.47372  Lat = 50.92868
hels <- matrix(c(-100.47372, 50.92868), nrow=1)
#Hels <- SpatialPoints(hels, proj4string=CRS("+proj=longlat +datum=WGS84"))
Hels <- SpatialPoints(hels, proj4string=CRS("+proj=longlat +datum=NAD83"))
d041224 <- as.POSIXct("2015-12-24", tz="GMT")
## Astronomical dawn
crepuscule(hels, d041224, solarDep=18, direction="dawn", POSIXct.out=TRUE)
crepuscule(Hels, d041224, solarDep=18, direction="dawn", POSIXct.out=TRUE)
## Nautical dawn
crepuscule(hels, d041224, solarDep=12, direction="dawn", POSIXct.out=TRUE)
crepuscule(Hels, d041224, solarDep=12, direction="dawn", POSIXct.out=TRUE)
## Civil dawn
crepuscule(hels, d041224, solarDep=6, direction="dawn", POSIXct.out=TRUE)
crepuscule(Hels, d041224, solarDep=6, direction="dawn", POSIXct.out=TRUE)
solarnoon(hels, d041224, POSIXct.out=TRUE)
solarnoon(Hels, d041224, POSIXct.out=TRUE)
solarpos(hels, as.POSIXct(Sys.time(), tz="EET"))
solarpos(Hels, as.POSIXct(Sys.time(), tz="EET"))
sunriset(hels, d041224, direction="sunrise", POSIXct.out=TRUE)
sunriset(Hels, d041224, direction="sunrise", POSIXct.out=TRUE)
## Using a sequence of dates
Hels_seq <- seq(from=d041224, length.out=365, by="days")
up <- sunriset(Hels, Hels_seq, direction="sunrise", POSIXct.out=TRUE)
down <- sunriset(Hels, Hels_seq, direction="sunset", POSIXct.out=TRUE)
day_length <- down$time - up$time
plot(Hels_seq, day_length, type="l")

## Nautical Dawn
## Using a sequence of dates

nau_dawn <- crepuscule(Hels, Hels_seq, solarDep=12, direction="dawn", POSIXct.out=TRUE)
nau_dusk <- crepuscule(Hels, Hels_seq, solarDep=12, direction="dusk", POSIXct.out=TRUE)
day_length2 <- nau_dusk$time - nau_dawn$time
plot(Hels_seq, day_length2, type="l")

# Dawn Duration

dawn_length <- up$time - nau_dawn$time 
plot(Hels_seq, dawn_length, type="l")




