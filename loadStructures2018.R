setwd("~/Desktop/geologyGeometry")
source("~/Desktop/geologyGeometry/library/all.R")
library(viridis)
library(colourvalues)
library(dplyr)


#if called 'hues', this overwrites another similar function, change fn name to something else to get that fn back
hue <- function(xs, a=min(xs), b=max(xs), palette = "viridis", ...) {
  colour_values(c(xs, a, b), ...)[1:length(xs)]
}

#loads all data. Combines data from all 2017 and 2018.  

SWlavas <- geoDataFromFile("icelandData/lavasAll.csv")
SWveins <- geoDataFromFile("icelandData/veinsAll.csv")
SWbzs <- geoDataFromFile("icelandData/bzsAll.csv")
faults <- geoDataFromFile("icelandData/faultsAll.csv")
cataclasites <- geoDataFromFile("icelandData/cataclasiteAll.csv")
SWdikes <- geoDataFromFile("icelandData/dikesAll.csv")
SWbigFaults <- geoDataFromFile("icelandData/hugeFaultsAll.csv")
SWslicks <- geoDataFromFile("icelandData/SlicksFaults.csv")
#pmag
pMag <- geoDataFromFile("icelandData/flatey/ourPmagAll.csv")
pMagSites <- geoDataFromFile("icelandData/flatey/allOurSites_zone27.csv")
pMag <- left_join(pMag,pMagSites,by = "station")
horstPmag <- geoDataFromFile("icelandData/flatey/horst_flateyFlow_inSitu.csv")



SWlavas$year <- "pre-2018"
SWveins$year <- "pre-2018"
SWbzs$year <- "pre-2018"
faults$year <- "pre-2018"
cataclasites$year <- "pre-2018"
SWdikes$year <- "pre-2018"
SWbigFaults$year <- "pre-2018"
SWslicks$year <- "pre-2018"

NHveins <- geoDataFromFile("icelandData/Veins_2018.csv")
NHbzs <- geoDataFromFile("icelandData/BrecciaZones_2018.csv")
NHbigfaults <- geoDataFromFile("icelandData/Faults_2018.csv")
NHdikes <- geoDataFromFile("icelandData/dikes_2018.csv")
NHslicks <- geoDataFromFile("icelandData/SlicksFaults_2018.csv")
fractures <- geoDataFromFile("icelandData/Fractures_2018.csv")
NHlavas <- geoDataFromFile("icelandData/Bedding_2018.csv")

NHveins$year <- "2018"
NHbzs$year <- "2018"
NHdikes$year <- "2018"
NHslicks$year <- "2018"
fractures$year <- "2018"
NHlavas$year <- "2018"
NHbigfaults$year <- "2018"

veins <- rbind(NHveins, SWveins)
bzs <- rbind(NHbzs, SWbzs)
slicksFaults <- rbind(NHslicks, SWslicks)
lavas <- rbind(NHlavas, SWlavas)
dikes <- rbind(NHdikes, SWdikes)
bigFaults <- rbind(NHbigfaults, SWbigFaults, cataclasites)

siteUTM <- geoDataFromFile("icelandData/allStructuresUTM.csv")

#glue site location information to each structure type data frame
lavas <- left_join(lavas,siteUTM,by = "station")
veins <- left_join(veins,siteUTM,by = "station")
bzs <- left_join(bzs,siteUTM,by = "station")
faults <- left_join(faults,siteUTM,by = "station")
cataclasites <- left_join(cataclasites,siteUTM,by = "station")
dikes <- left_join(dikes,siteUTM,by = "station")
bigFaults <- left_join(bigFaults,siteUTM,by = "station")
slicksFaults <- left_join(slicksFaults,siteUTM,by = "station")
fractures <- left_join(fractures,siteUTM,by = "station")
#add bigStations 
#veins
veins$station <- as.character(veins$station)
veins$bigStation <- lapply(strsplit(veins$station, split=" "), "[[", 1)
veins$station <- factor(veins$station)
veins$bigStation <- as.character(veins$bigStation)
veins$bigStation <- factor(veins$bigStation)
#bzs
bzs$station <- as.character(bzs$station)
bzs$bigStation <- lapply(strsplit(bzs$station, split=" "), "[[", 1)
bzs$station <- factor(bzs$station)
bzs$bigStation <- as.character(bzs$bigStation)
bzs$bigStation <- factor(bzs$bigStation)
#slicksFaults
slicksFaults$station <- as.character(slicksFaults$station)
slicksFaults$bigStation <- lapply(strsplit(slicksFaults$station, split=" "), "[[", 1)
slicksFaults$station <- factor(slicksFaults$station)
slicksFaults$bigStation <- as.character(slicksFaults$bigStation)
slicksFaults$bigStation <- factor(slicksFaults$bigStation)
#dikes
dikes$station <- as.character(dikes$station)
dikes$bigStation <- lapply(strsplit(dikes$station, split=" "), "[[", 1)
dikes$station <- factor(dikes$station)
dikes$bigStation <- as.character(dikes$bigStation)
dikes$bigStation <- factor(dikes$bigStation)
#fractures
fractures$station <- as.character(fractures$station)
fractures$bigStation <- lapply(strsplit(fractures$station, split=" "), "[[", 1)
fractures$station <- factor(fractures$station)
fractures$bigStation <- as.character(fractures$bigStation)
fractures$bigStation <- factor(fractures$bigStation)
#bigFaults
bigFaults$station <- as.character(bigFaults$station)
bigFaults$bigStation <- lapply(strsplit(bigFaults$station, split=" "), "[[", 1)
bigFaults$station <- factor(bigFaults$station)
bigFaults$bigStation <- as.character(bigFaults$bigStation)
bigFaults$bigStation <- factor(bigFaults$bigStation)



#import other peoples data
#Bergerat
BGslicks <- geoDataFromFile("icelandData/BergeratFaultsAll.csv")
BGsiteUTM <- geoDataFromFile("icelandData/BergeratSitesUTM.csv")
BGslicks <- left_join(BGslicks,BGsiteUTM,by = "station")
#Fjader
FJveins <- geoDataFromFile("icelandData/FjaderVeinsAll.csv")
FJsiteUTM <- geoDataFromFile("icelandData/FjaderVeinsUTM.csv")
FJveins <- left_join(FJveins,FJsiteUTM,by = "station")
FJdikes <- geoDataFromFile("icelandData/fjaderEtAl_dikes.csv")
#Young/Jancin
YoungDikesAve <- geoDataFromFile("icelandData/youngEtAl_dikeAve.csv")
YoungLavas <- geoDataFromFile("icelandData/youngEtAl_lavas.csv")
YoungFaultStrikes <- geoDataFromFile("icelandData/youngEtAlFaults.csv")
JancinDikes <- geoDataFromFile("icelandData/jancinDike.csv")
#Garcia & Dhont
GDslicks <- geoDataFromFile("icelandData/Garcia_DhontFaultsAll.csv")
GDsiteUTM <- geoDataFromFile("icelandData/Garcia_DhontSitesUTM.csv")
GDslicks <- left_join(GDslicks,GDsiteUTM,by = "station")
#Karson 2018
KarsonHriseySlicks <- geoDataFromFile("icelandData/SlicksFaults_Karson_Hirsey.csv") #from Hirsey island, EY
KarsonHriseyDikes <- geoDataFromFile("icelandData/Dikes_Karson_Hirsey.csv")
# Here are two easting-northings along the Husavik-Flatey fault. (This is post-discovery of the error about converting between UTM zones)
iceA2 <- c(605253.1667,7351731.888)
iceB3 <- c(668779.8402,7328905.312)

# Given three points in 2D, returns distance from first point to the line through the other two.
distanceFromLine <- function(x, a, b) {
  v <- b - a
  w <- x - a
  proj <- (dot(w, v) / dot(v, v)) * v
  perp <- w - proj
  sqrt(dot(perp, perp))
}

# given three points in 2D, returns distance of first point along the line formed by the second two.
distanceAlongLine <- function(x, a, b) {
  v <- b - a
  w <- x - a
  proj <- (dot(w, v) / dot(v, v)) * v
  sqrt(dot(proj, proj))
}

# function to add location, distance from fault, and distance PARALLEL to the fault to all data frames 
addDistanceColumns <- function(df, point1, point2) {
  df$location <- lapply(1:nrow(df), 
                        function(i) c(df$easting[[i]], df$northing[[i]]))
  df$distance <- sapply(df$location, function(x) distanceFromLine(x, point1, point2))
  df$strikedist <- sapply(df$location, function(x) distanceAlongLine(x,point1, point2))
  df
}

#function to compute sense of shear from the rotation matrix of fault slip data
slipSense <- function(rotation) {
  rakeDeg <- geoStrikeDipRakeDegFromRotation(rotation)[3]
  rakeMod <- mod(rakeDeg, 315)
  sense <- character()
  if (rakeMod <= 45) {
    sense <- "L"
  } else if (rakeMod <= 135) {
    sense <- "N"
  } else if (rakeMod <= 225) {
    sense <- "R"
  } else {
    sense <- "T"
  }
  sense
}

#add sense of slip and vorticity-vector form of rotation matrix to slick data
slicksFaults$slipSense <- lapply(slicksFaults$rotation, slipSense)
BGslicks$slipSense <- lapply(BGslicks$rotation, slipSense)
GDslicks$slipSense <- lapply(GDslicks$rotation, slipSense)

slicksFaults$Rots <- lapply(slicksFaults$rotation, geoPoleVorticityFromPoleHanging)
BGslicks$Rots <- lapply(BGslicks$rotation, geoPoleVorticityFromPoleHanging)
GDslicks$Rots <- lapply(GDslicks$rotation, geoPoleVorticityFromPoleHanging)

#add distance columns to all the dataframes
lavas <- addDistanceColumns(lavas, iceA2, iceB3)
veins <- addDistanceColumns(veins, iceA2, iceB3)
bzs <- addDistanceColumns(bzs, iceA2, iceB3)
faults <- addDistanceColumns(faults, iceA2, iceB3)
cataclasites <- addDistanceColumns(cataclasites, iceA2, iceB3)
dikes <- addDistanceColumns(dikes, iceA2, iceB3)
bigFaults <- addDistanceColumns(bigFaults, iceA2, iceB3)
slicksFaults <- addDistanceColumns(slicksFaults, iceA2, iceB3)
BGslicks <- addDistanceColumns(BGslicks, iceA2, iceB3)
FJveins <- addDistanceColumns(FJveins, iceA2, iceB3)
GDslicks <- addDistanceColumns(GDslicks, iceA2, iceB3)
SWbigFaults <- addDistanceColumns(SWbigFaults, iceA2, iceB3)
SWbzs <- addDistanceColumns(SWbzs, iceA2, iceB3) 
SWdikes<- addDistanceColumns(SWdikes, iceA2, iceB3)
SWlavas <- addDistanceColumns(SWlavas, iceA2, iceB3)
fractures <- addDistanceColumns(fractures, iceA2, iceB3)
YoungDikesAve <- addDistanceColumns(YoungDikesAve, iceA2, iceB3)
FJdikes <- addDistanceColumns(FJdikes, iceA2, iceB3)
YoungFaultStrikes <- addDistanceColumns(YoungFaultStrikes, iceA2, iceB3)
YoungLavas <- addDistanceColumns(YoungLavas, iceA2, iceB3)
KarsonHriseyDikes <- addDistanceColumns(KarsonHriseyDikes, iceA2, iceB3)
KarsonHriseySlicks <- addDistanceColumns(KarsonHriseySlicks, iceA2, iceB3)
JancinDikes <- addDistanceColumns(JancinDikes, iceA2, iceB3)
pMag <- addDistanceColumns(pMag, iceA2, iceB3)
pMag$hues <- hues(pMag$distance, c(0, 28000))
horstPmag <- addDistanceColumns(horstPmag, iceA2, iceB3)


lavas$curves <- lapply(lavas$pole, rayGreatCircle)
veins$curves <- lapply(veins$pole, rayGreatCircle)
bzs$curves <- lapply(bzs$pole, rayGreatCircle)
faults$curves <- lapply(faults$pole, rayGreatCircle)
cataclasites$curves <- lapply(cataclasites$pole, rayGreatCircle)
dikes$curves <- lapply(dikes$pole, rayGreatCircle)
bigFaults$curves <- lapply(bigFaults$pole, rayGreatCircle)
slicksFaults$curves <- lapply(slicksFaults$pole, rayGreatCircle)
BGslicks$curves <- lapply(BGslicks$pole, rayGreatCircle)
FJveins$curves <- lapply(FJveins$pole, rayGreatCircle)
GDslicks$curves <- lapply(GDslicks$pole, rayGreatCircle)
fractures$curves <- lapply(fractures$pole, rayGreatCircle)


#grab all sites from Flateyjarskagi and make smaller data frames
FLveins <- filter(veins, substr(station, 1, 2) == "FL")
FLlavas <- filter(lavas, substr(station, 1, 2) == "FL")
FLdikes <- filter(dikes, substr(station, 1, 2) == "FL")
FLfaults <- filter(faults, substr(station, 1, 2) == "FL")
FLbzs <- filter(bzs, substr(station, 1, 2) == "FL")
FLcataclasites <- filter(cataclasites, substr(station, 1, 2) == "FL")
FLslicksFaults <- filter(slicksFaults, substr(station, 1, 2) == "FL")
FLbigFaults <- filter(bigFaults, substr(station, 1, 2) == "FL")

#grab all sites from Tjörnes and make smaller data frames
TJveins <- filter(veins, substr(station, 1, 2) == "TJ")
TJlavas <- filter(lavas, substr(station, 1, 2) == "TJ")
TJdikes <- filter(dikes, substr(station, 1, 2) == "TJ")
TJfaults <- filter(faults, substr(station, 1, 2) == "TJ")
TJbzs <- filter(bzs, substr(station, 1, 2) == "TJ")
TJcataclasites <- filter(cataclasites, substr(station, 1, 2) == "TJ")
TJslicks <- filter(slicksFaults, substr(station, 1, 2) == "TJ")
TJbigFaults <- filter(bigFaults, substr(station, 1, 2) == "TJ")
TJfractures <- filter(fractures, substr(station,1,2)=="TJ")

GDslicks$rakeDeg <- "NA"
BGslicks$rakeDeg <- "NA"
allSlicks <- dplyr::select(slicksFaults, "station", "pole", "direction", "rotation", "easting", "northing", "slipSense", "Rots", "distance", "strikedist", "curves", "strikeDeg", "dipDeg", "rakeDeg")
GDslicks_all <- dplyr::select(GDslicks, "station", "pole", "direction", "rotation", "easting", "northing", "slipSense", "Rots", "distance", "strikedist", "curves", "strikeDeg", "dipDeg", "rakeDeg")
BGslicks_all <- dplyr::select(BGslicks, "station", "pole", "direction", "rotation", "easting", "northing", "slipSense", "Rots", "distance", "strikedist", "curves", "strikeDeg", "dipDeg", "rakeDeg")

allSlicks <- rbind(allSlicks, GDslicks_all, BGslicks_all)
allSlicks$rotation <- lapply(allSlicks$rotation, geoPoleVorticityFromPoleHanging)
allSlicksTo28 <- filter(allSlicks, distance < 28000)
allSlicksTo28 <- filter(allSlicksTo28, distance !="NA")
allSlicksTo28$hues <- hues(allSlicksTo28$distance)

#function to grab a fault slip regression of vorticity vector matrices and show what the fault planes look like at increasing distance from the HFF
slickPredictor <- function(regression, distance) {
  #use the prediction function from the regression to create a series of fault slips with increasing distance from HFF
  dists <- linspace(1, distance, 20)
  preds <- lapply(dists, regression$prediction)
  preds <- lapply(preds, geoPoleHangingFromPoleVorticity)
  
  #store rotations in data frame
  predsDf <- data.frame(distance = dists)
  predsDf$rots <- preds
  predsDf$hue <- hues(predsDf$distance)
  
  #disect matrices into directions and poles
  predsDf$pole <- lapply(predsDf$rots, function(x) x[1,])
  predsDf$direction <- lapply(predsDf$rots, function(x) x[2,])
  predsDf$curves <- lapply(predsDf$pole, rayGreatCircle)
  predsDf
}

# Given an orientation regression, find the axis and rotation  
regAxis <- function(regression) {
  rotAnti <- t(regression$b) %*% regression$m %*% regression$b
  axis <- -(rotVectorFromAntisymmetric(rotAnti))
  mag <- sqrt(sum(axis^2)) * 1000 / degree
  axisDeg <- geoTrendPlungeDegFromCartesian(axis)
  list(axisCartesian = axis, axisTrendPlunge = axisDeg , magnitude = mag)
}

