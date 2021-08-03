#A regression that binds dikes and lavas

stringsAsFactors=TRUE
setwd("~/Desktop/geologyGeometry")
source("~/Desktop/geologyGeometry/library/all.R")
source("~/Desktop/geologyGeometry/loadStructures2018.R")

library(tidyverse)
library(dplyr)
#I first bin data, then for each bin, I create a rotation (each row is a line in Cartesian coordinates):
# D cross L 
#(D cross L) cross D (serves as an adjusted L)
#D
#where D is the average dike pole and L is the average lava pole in that bin. 

#run the RotationFigure.R code to load data frames. 

#a function to make rotations from the data in each bin:
rotationFromDikeLava <- function(dike, lava){ #dikeList is list of dike poles, lava list is list of lava poles
  row1 <- c(cross(unlist(dike), unlist(lava)))
  row1 <- row1/(sqrt(row1[1]^2+row1[2]^2+row1[3]^2))
  row2 <-  c(cross(unlist(row1), unlist(dike)))
  rotation <- rotProjectedMatrix(rbind(row1, row2, unlist(dike)))
  return(rotation)}

#make ~2x2 km rectangular bins, plot bins by the number of dikes in each
OurDikes <- dplyr::select(dikes, "pole", "easting", "northing", "distance", "strikedist") #this will include dikes from 2018 too...
FJdikes <- dplyr::select(FJdikes, "pole", "easting", "northing", "distance", "strikedist")
YoungDikesAve <- dplyr::select(YoungDikesAve, "pole", "easting", "northing", "distance", "strikedist")
JancinDikes <- dplyr::select(JancinDikes, "pole", "easting", "northing", "distance", "strikedist")
TitusDikes <- rbind(FJdikes, YoungDikesAve, OurDikes, JancinDikes)
TitusDikes <- filter(TitusDikes, distance != 'NA')
TitusDikes$hues <- hues(TitusDikes$distance, c(0, 28000))
TitusDikes <- filter(TitusDikes, easting < 660000, easting > 620000)
plot(TitusDikes$easting, TitusDikes$northing, col=TitusDikes$hues)
lineEqualAreaPlot(TitusDikes$pole, colors = TitusDikes$hues)

TitusDikes <- filter(TitusDikes, distance < 17000)



#BINS: Each bin is denoted by a 2 digit number (in the $bin column). the first digit corresponds to the position along fault strike, cur up into 10 sections, the second corresponds to fault-normal distance, cut up into 10 sections 
minSD <- min(TitusDikes$strikedist)
TitusDikes$strikedist <- sapply(TitusDikes$strikedist, function(x) x - minSD)

FLlavas1 <- filter(FLlavas, strikedist != "NA")
FLlavas1 <-  dplyr::select(FLlavas1, "pole", "easting", "northing", "distance", "strikedist")
YoungLavas <-  dplyr::select(YoungLavas, "pole", "easting", "northing", "distance", "strikedist")
FLlavas1 <- rbind(FLlavas1, YoungLavas)
FLlavas1 <- filter(FLlavas1, easting < 645000, distance < 17000, strikedist >minSD) #remove structures near anticlinal flexure axis or beyond rotation gradient
FLlavas1$strikedist <- sapply(FLlavas1$strikedist, function(x) x - minSD)#make the farthest west dike measurement be the 0 for easting for both lavas and dikes 



numcol <- 10
numrow <- 10

TitusDikes$bin <- "NA"
for(i in (1:nrow(TitusDikes))){
  dike <- TitusDikes[i,]
  digit1 <- floor(numcol*(dike$strikedist/(max(FLlavas1$strikedist)+1)))
  digit2 <- floor(numrow*(dike$distance/17000))
  10*(digit1)+digit2
  TitusDikes$bin[i] <- 10*(digit1)+digit2
}
 
FLlavas1$bin <- "NA"
for(i in (1:nrow(FLlavas1))){
  lava <- FLlavas1[i,]
  digit1 <- floor(numcol*(lava$strikedist/(max(FLlavas1$strikedist)+1)))
  digit2 <- floor(numrow*(lava$distance/17000))
  FLlavas1$bin[i] <- 10*(digit1)+digit2
}
FLlavas1$bin <- as.numeric(FLlavas1$bin)
TitusDikes$bin <- as.numeric(TitusDikes$bin)
plot(FLlavas1$easting, FLlavas1$northing, col=hues(FLlavas1$bin)) #check to make sure bins are assigned correctly--nice and rainbowy
plot(TitusDikes$easting, TitusDikes$northing, col= hues(TitusDikes$bin))

FLlavas1$bin <- as.factor(FLlavas1$bin)
TitusDikes$bin <- as.factor(TitusDikes$bin)




#build a rotation for each bin
rotations <- data.frame(stations = factor(),
                        dikePole=double(),
                        lavaPole=double(),
                        rotation=double(),
                        distance = double(),
                        easting=double(),
                        northing=double(),
                        deviation = double(),
                        stringsAsFactors=FALSE)

for (i in levels(FLlavas1$bin)) {#make one orientation per bin
  
  lavaDf <- filter(FLlavas1, bin==i)
  dikeDf <- filter(TitusDikes, bin==i)
  
  if (nrow(dikeDf) > 0){
    #average lavas then find rotation from that poles orientation to vertical (0,0,-1)
    lavaMean <- lineProjectedMean(lavaDf$pole)
    dikeMean <- lineProjectedMean(dikeDf$pole)
    
    rotation <- rotationFromDikeLava(list(dikeMean), list(lavaMean))
    deviation <- rotAxisAngleFromMatrix(rotSmallestRotationFromTwoLines(lavaMean, rotation[2,]))[4]
    
    rotations <- rbind(rotations, data.frame(cbind(i, list(dikeMean), list(lavaMean), list(rotation), lavaDf$distance[1], lavaDf$easting[1], lavaDf$northing[1], deviation)))
    #build list of lava and dike averages from each station where there are measurements of both
  }}
rotations <- rotations %>% rename("bin" = i) 
rotations <- rotations %>% rename("distance" = V5) 
rotations <- rotations %>% rename("easting" = V6) 
rotations <- rotations %>% rename("northing" = V7) 
rotations <- rotations %>% rename("rotation" = V4) 
rotations$line1 <- lapply(rotations$rotation, function(x) x[1,])
rotations$line2 <- lapply(rotations$rotation, function(x) x[2,])
rotations$line3 <- lapply(rotations$rotation, function(x) x[3,])#should be the same as the dike poles--yes looks good. 
rotations$deviation <- sapply(rotations$deviation, unlist)
rotations$distance <- as.numeric(rotations$distance)
rotations$hues <- hue(rotations$distance, c(0, 28000))

lineEqualAreaPlotTwo(rotations$V2, rotations$V3, colorA = rotations$hues, colorB = hue(rotations$distance)) #plots dike poles and lava poles (V3, V2)
lineEqualAreaPlotTwo(rotations$line3, rotations$line2, colorA = rotations$hues, colorB = hue(rotations$distance)) #plots dike poles and pseudo lava poles that are perp to dikes 



#PLOT BINS

rotations$bins <- sapply(rotations$bin, as.numeric)
plot(rotations$easting, rotations$northing, col= hues(rotations$bins)) #bins on a map. pretty good coverage. fix colors
#plot boundaries
par(new=TRUE)
abline(7569213, -0.3593228) #adds HFF. at 19.8° to horizontal ~110 azimuth
for (i in (1:10)){
  par(new=TRUE)
abline(7569213-1806.8*i, -0.3593228)#subtract 1700/cos(19.7) to subtract NS distance across each bin
}
for (i in (0: 10)){
  par(new=TRUE)
  abline(5422530-7799.54*i, 3.07509) #this might be wrong
}




#make data frame to regress:
lineEqualAreaPlotTwo(rotations$line3, rotations$line2, colorA = rotations$hues, colorB = hue(rotations$distance))
rotations <- filter(rotations, bin != 99) #remove outlier. just one dike observation 
rotations <- filter(rotations, distance > 1000)
lineEqualAreaPlotTwo(rotations$line3, rotations$line2, colorA = rotations$hues, colorB = hue(rotations$distance))


rotations$lnDistance <- sapply(rotations$distance, log)
x0 <- min(rotations$lnDistance)
x1 <- max(rotations$lnDistance)
rotations$lnDistance <- lapply(rotations$lnDistance, function(x) (x-x0)/(x1-x0))
rotReg <- oriGeodesicRegression(rotations$lnDistance, rotations$rotation, group = oriLineInPlaneGroup, numSteps = 10000)
#I don't think there is a small circle regression

rotReg
xs <- c(1:16) #rotations contains data up to 15985 m
hues <- hues(xs)
xs <- xs*1000
xs <- sapply(xs, log)
x1 <- max(xs)
x0 <- min(xs)
xs <- sapply(xs, function(x) (x-x0)/(x1-x0))
poles <- lapply(xs, rotReg$prediction)

pdf(paste("lava_dike_reg_1-16km-dikesandpseuolavas.pdf", sep = ""), useDingbats = FALSE, width = 10, height = 6)
par(pty="s")
par(mar=c(2,2,2,2))
lineEqualAreaPlotTwo(rotations$line3, rotations$line2, colorA = rotations$hues, colorB = hue(rotations$distance))
lineEqualAreaPlotTwo(lapply(poles, function(x) x[2,]), lapply(poles, function(x) x[3,]), colorA = hues, colorB = hues)
dev.off()




#PREDICTIONS
#load far-fault averages
farLL <- filter(allSlicks, slipSense=="L", distance > 17000, easting < 641500)
farRL <- filter(allSlicks, slipSense=="R", distance > 17000, easting < 641500)
farRL <- farRL[-33,] #remove one outlier with a strike of 77
farRL <- farRL[-16,] #another outlier with a dip of 42
farN <- filter(allSlicks, slipSense == "N", distance > 17000, easting < 641500)
allSlicks$strikeDeg <- lapply(allSlicks$pole, geoStrikeDipDegFromCartesian)
allSlicks$dipDeg <- lapply(allSlicks$strikeDeg, function(x) x[[2]])
allSlicks$strikeDeg <- lapply(allSlicks$strikeDeg, function(x) x[[1]])
farN_1a <- filter(allSlicks, slipSense == "N", distance > 17000, strikeDeg <85, easting < 641500)
farN_1b <- filter(allSlicks, slipSense == "N", distance > 17000, strikeDeg >270, easting < 641500)
farN_2 <- filter(allSlicks, slipSense == "N", distance > 17000, strikeDeg > 110, strikeDeg < 270, easting < 641500)
farN_1 <- rbind(farN_1a, farN_1b)

#look at axis and rate of rotation:
rotPole <- t(rotReg$b) %*% rotAxisAngleFromMatrix(rotExp(rotReg$m * 0.5))[-4]
lineEqualAreaPlot(list(rotPole)) 
rotAxisAngleFromMatrix(rotExp(rotReg$m))[4]#rotation between 1 and 16 km

b <- rotReg$b #get the un-rescaled version from the unrescaled regression -- it looks like this might be wrong... NOT SURE HOW THIS IS DIF FROM ORIGINAL PRED FUNCTION
m <- rotReg$m 
xs <- c(1:16) #rotations contains data up to 15985 m
xs <- xs*1000
xs <- as.numeric(sapply(xs, log))#log xs if regression is logarithmic
x0 <- min(xs)
x1 <- max(xs)
xs <- sapply(xs, function(x) (x-x0)/(x1-x0))

b <- oriMeanVariance(farN_2$rotation, group =oriRayInPlaneGroup) #plug in fault type of choice, find average
b <- rotProjectedMatrix(b$mean) 
lineEqualAreaPlotThree(list(b[1,]), list(b[2,]), list(b[3,]), colorB = "red", colorC="blue")
b <- (t(rotReg$b) %*%(rotExp(1 * m))) %*% (rotReg$b %*%t(b)) #the b we put in is the orientation at 17 km--we want b to be the orientation at 0 km. So we rotate our current b forward 17 km and set that as the new 0.
b <- t(b)
lineEqualAreaPlotThree(list(b[1,]), list(b[2,]), list(b[3,]), colorB = "red", colorC="blue")


f <- function(x){
  t((t(rotReg$b) %*%(rotExp(-x * m))) %*% (rotReg$b %*% t(b)))} #rotating around the right axis but the initial location is wrong. seems like its also being rotated by b...not sure how to fix 

pred <- lapply(xs, f)

pdf(paste("N2pred.pdf", sep = ""), useDingbats = FALSE, width = 10, height = 6)
par(pty="s")
par(mar=c(2,2,2,2))
lineEqualAreaPlotThree(lapply(pred, function(x) x[1,]), lapply(pred, function(x) x[2,]), lapply(pred, function(x) x[3,]), colorA=hues(xs)) #when pred is just a line. it's rotating around the axis you'd think it would rotate around, but seems very wrong
dev.off()



#plot predictions for confidence regions. 

#prediction at 1 km from fault:
f1 <- function(b){
  t((t(rotReg$b) %*%(rotExp(rotReg$m))) %*% (rotReg$b %*% t(b)))} 
#prediction 2 km from fault:
f2 <- function(b){
  b <- f1(b)
  t((t(rotReg$b) %*%(rotExp(-(log(2)/log(16)) * rotReg$m))) %*% (rotReg$b %*% t(b)))}#0.9767226 is ln(15)/ln(16), which is the modified dist value corresponding to a dist of 2
#prediction 5 km from fault:
f5 <- function(b){
  b <- f1(b)
  t((t(rotReg$b) %*%(rotExp(-(log(5)/log(16)) * rotReg$m))) %*% (rotReg$b %*% t(b)))}
#prediction 10 km from fault:
f10 <- function(b){
  b <- f1(b)
  t((t(rotReg$b) %*%(rotExp(-(log(10)/log(16)) * rotReg$m))) %*% (rotReg$b %*% t(b)))}

f16 <- function(b){
  t((t(rotReg$b) %*%(rotExp((log(1)/log(16)) * rotReg$m))) %*% (rotReg$b %*% t(b)))}



plotCR <- function(boots) { #takes bootstrapping output (bootN1CR, bootN2CR, etc...)

  center <- boots$center 
  leftCovarInv <- boots$leftCovarInv
  level <- boots$q095
  triangles <- rotEllipsoidTriangles(center, leftCovarInv, level, numNonAdapt=5)
  
  matrices <- lapply(triangles, function(x) x[[1]])# dissect all of the triangles into individual matrices
  matrices <- rbind(matrices, lapply(triangles, function(x) x[[2]]))
  matrices <- rbind(matrices, lapply(triangles, function(x) x[[3]]))
  #rotEqualVolumePlot(matrices) #to view in rotation space if you'd like
  
  matrices1 <- lapply(matrices, f1)
  matrices2 <- lapply(matrices, f2)
  matrices5 <- lapply(matrices, f5)
  matrices10 <- lapply(matrices, f10)
  lineEqualAreaPlot(lapply(matrices, function(x) x[1,]), shapes=c(".")) #plots far-fault average CR
  par(new=TRUE)
  lineEqualAreaPlot(lapply(matrices1, function(x) x[1,]), shapes=c(".")) #plot CR rotated to 1 km from fault
  par(new=TRUE)
  lineEqualAreaPlot(lapply(matrices2, function(x) x[1,]), shapes=c(".")) #rotated to 2km
  par(new=TRUE)
  lineEqualAreaPlot(lapply(matrices5, function(x) x[1,]), shapes=c(".")) #rotated to 5km
  par(new=TRUE)
  lineEqualAreaPlot(lapply(matrices10, function(x) x[1,]), shapes=c(".")) #rotated to 10km
}
#compute CRs:
boots <- oriBootstrapInference(farLL$rotation, 1000, oriRayInPlaneGroup)

#make pdf:
pdf(paste("N_2CRpred.pdf", sep = ""), useDingbats = FALSE, width = 10, height = 6)
par(pty="s")
par(mar=c(2,2,2,2))
plotCR(boots)
dev.off()



#Bootstrapping

#function to bootstrap oriGeodessicRegression.

oriGeodesicRegressionBootstrap <- function(xs, rs, numSteps=1000) {
  indices <- sample(1:length(rs), length(rs), replace=TRUE)
  oriGeodesicRegression(xs[indices], rs[indices], group = oriLineInPlaneGroup, numSteps=1000)
}

# This function just returns the bootstraps. It doesn't try to fit an ellipsoid to them, for example.
oriGeodesicRegressionBootstraps <- function(xs, rs, numBoots, numSteps=1000) {
  boots <- replicate(
    numBoots, 
    oriGeodesicRegressionBootstrap(xs, rs, numSteps), 
    simplify=FALSE)
}
#dissect axis and rotation magnitude from rotation 
AxisAngleFromBootRotsGeod <- function(boots){
  Boots <- as.data.frame(matrix(ncol =1, nrow = 0)) #change nrow to number of boots
  Boots$rotation <- lapply(boots, function(boot) boot$rotation)
  Boots$axis <- lapply(Boots$rotation, function(rotation) rotation[,3])
  Boots$angle <- lapply(boots, function(boots) boots$a)
  Boots$angle <- as.numeric(Boots$angle)
  Boots}



#BOOTSTRAP (see bootsrapGeodesicReg.R in IcelandScripts-->2019Experimentation)

num <- 1000
boots <- oriGeodesicRegressionBootstraps(rotations$lnDistance, rotations$rotation, numBoots = num, numSteps=1000)


min <- 5
max <- 0
angles <- data.frame(rotAxisAngleFromMatrix(rotExp(boots[[1]]$m))[4])
for (i in (1:num)){
  m <- boots[[i]]$m
  angle <- rotAxisAngleFromMatrix(rotExp(m))[4]
  angles <- rbind(angles, angle)
  if (angle > max){
    max <- angle
  }
  if (angle < min){
    min <- angle
  }}
angles <- angles[-1,]

hist(angles)#histogram of the rotation btwn 0 and 16 km in radians
angles <- sapply(angles, function(x) x*360/(2*3.14159))
hist(angles) # histogram in degrees

pdf(paste("bootDikeLavaAxes_coloredbyAngle.pdf", sep = ""), useDingbats = FALSE, width = 10, height = 6)
par(pty="s")
par(mar=c(2,2,2,2))
for (i in (1:num)){
  m <- boots[[i]]$m
  b <- boots[[i]]$b
  axis <- t(b) %*% rotAxisAngleFromMatrix(rotExp(m))[-4]
  angle <- rotAxisAngleFromMatrix(rotExp(m))[4]
  lineEqualAreaPlot(list(axis), colors=hues(angle, c(min, max)))
  par(new=TRUE)
}
lineEqualAreaPlot(list(rotPole), colors="red")
dev.off()


#check to make sure boots[[1]] prediction looks reasonable
xs <- c(1:16) #rotations contains data up to 15985 m
hues <- hues(xs)
xs <- xs*1000
xs <- sapply(xs, log)
x1 <- max(xs)
x0 <- min(xs)
xs <- sapply(xs, function(x) (x-x0)/(x1-x0))
poles <- lapply(xs, boots[[1]]$prediction)
lineEqualAreaPlotTwo(lapply(poles, function(x) x[2,]), lapply(poles, function(x) x[3,]), colorA = hues, colorB = hues)





#deviation between lava poles and poles used in the regression:
rotations$deviation <- sapply(rotations$deviation, function(x) x*360/(2*3.14159))
hist(rotations$deviation, 
     main="Histogram deviation between observed and artificial lava poles", 
     xlab="deviation (degrees)",
     ylab="count",
     border="black", 
     col="grey",
     xlim=c(0, 180),
     breaks=10)





hues <- function(xs, a=min(xs), b=max(xs), palette = "viridis", ...) {
  colour_values(c(xs, a, b), ...)[1:length(xs)]
}
