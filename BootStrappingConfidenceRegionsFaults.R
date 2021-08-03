
#Bootstrapped confidence regions for orientations (as seen in Figures 7,8)

setwd("/users/hummeln/Documents/geologyGeometry")
library(dplyr)
source("/users/hummeln/Documents/geologyGeometry/library/all.R")
source("/users/hummeln/Documents/geologyGeometry/loadStructures2018.R")



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

lineEqualAreaPlot(farN$pole)
#data to bootstrap:
lineEqualAreaPlot(farN_1a$pole)
lineEqualAreaPlot(farN_2$pole)
lineEqualAreaPlot(farLL$pole)
lineEqualAreaPlot(farRL$pole)

oriAxisAnglePlot(farN_1$rotation, group = oriRayInPlaneGroup)
oriAxisAnglePlot(farN_2$rotation, group = oriRayInPlaneGroup)
oriAxisAnglePlot(farLL$rotation, group = oriRayInPlaneGroup)
oriAxisAnglePlot(farRL$rotation, group = oriRayInPlaneGroup)



#BOOTSRTAPPING FAULTS
bootN1CR <- oriBootstrapInference(farN_1a$rotation, 1000, oriRayInPlaneGroup)
bootN2CR <- oriBootstrapInference(farN_2$rotation, 1000, oriRayInPlaneGroup)
bootLLCR <- oriBootstrapInference(farLL$rotation, 1000, oriRayInPlaneGroup)
bootLLpole <- lineBootstrapInference(farLL$pole, 1000)

bootRLCR <- oriBootstrapInference(farRL$rotation, 1000, oriRayInPlaneGroup)
bootRLpole <- lineBootstrapInference(farRL$pole, 1000)

#FIT CONFIDENCE REGIONS

#rotEllipsoidTriangles gets output from oriBootstraps (q095, center, invLerftCpVar, numNonAdapt=5 or 6)
#produces list of 'triangles', each is 3 rotation matrices. 
#break these triangles down into three individual matrices (or break 1000 traingles into 3000 matrices) and plot those rotations.  Rather, plot the fault poles from those rotations. Trace the outside of the points for a confidence region. 

#CRS
plotCR <- function(x) { #takes bootstrapping output (bootN1CR, bootN2CR, etc...)
  boots <- x
  center <- boots$center 
  leftCovarInv <- boots$leftCovarInv
  level <- boots$q095
  triangles <- rotEllipsoidTriangles(center, leftCovarInv, level, numNonAdapt=5)
  
  matrices <- lapply(triangles, function(x) x[[1]])# dissect all of the triangles into individual matrices
  matrices <- rbind(matrices, lapply(triangles, function(x) x[[2]]))
  matrices <- rbind(matrices, lapply(triangles, function(x) x[[3]]))
  rotEqualVolumePlot(matrices)
  par(new=TRUE)
  rotEqualVolumePlot(list(geoPoleVorticityFromPoleHanging(geoRotationFromStrikeDipRakeDeg(c(147, 90, 0)))))
  lineEqualAreaPlot(lapply(matrices, function(x) x[1,]), shapes=c(".")) 
  }

#PLOT BOOTSTRAPS

~~~~~~~~~~~~~~~N~~~~~~~~~~~~~~~
lineEqualAreaPlot(farN_1a$pole)
rotEqualAnglePlot(bootN1CR$bootstraps, simplePoints=TRUE)
lineEqualAreaPlot(lapply(bootN1CR$bootstraps, function(r) r[1,]), shapes=c("."))
par(new=TRUE)
lineEqualAreaPlot(list(geoCartesianFromStrikeDipDeg(c(012, 60))), colors="blue") #previously, predicted dip was 70, but maybe best to stick to andersonian prediction and explain difference. 
plotCR(bootN1CR)

lineEqualAreaPlot(farN_2$pole)
rotEqualAnglePlot(bootN2CR$bootstraps, simplePoints=TRUE)
lineEqualAreaPlot(lapply(bootN2CR$bootstraps, function(r) r[1,]), shapes=c("."))
par(new=TRUE)
lineEqualAreaPlot(list(geoCartesianFromStrikeDipDeg(c(192, 60))), colors="blue")
plotCR(bootN2CR)

~~~~~~~~LL~~~~~~~~~~~~~~~~
lineEqualAreaPlot(farLL$pole)
rotEqualAnglePlot(bootLLCR$bootstraps, simplePoints=TRUE)
lineEqualAreaPlot(lapply(bootLLCR$bootstraps, function(r) r[1,]), shapes=c("."))
par(new=TRUE)
lineEqualAreaPlot(list(geoCartesianFromStrikeDipDeg(c(222, 90))), colors="blue")
plotCR(bootLLCR)

lineEqualAreaPlot(bootLLpole$us, shapes = c("."))
par(new=TRUE)
lineEqualAreaPlot(list(geoCartesianFromStrikeDipDeg(c(222, 90))), colors="blue")


~~~~~~~~~~~RL~~~~~~~~~~~~~
lineEqualAreaPlot(farRL$pole)
rotEqualAnglePlot(bootRLCR$bootstraps, simplePoints=TRUE)
lineEqualAreaPlot(lapply(bootRLCR$bootstraps, function(r) r[1,]), shapes=c("."))
par(new=TRUE)
lineEqualAreaPlot(list(geoCartesianFromStrikeDipDeg(c(162, 90))), colors="blue")
plotCR(bootRLCR)

lineEqualAreaPlot(bootRLpole$us, shapes = c("."))
par(new=TRUE)
lineEqualAreaPlot(list(geoCartesianFromStrikeDipDeg(c(162, 90))), colors="blue")




pdf(paste("kinPreds_poles.pdf", sep = ""), useDingbats = FALSE, width = 10, height = 6)
par(pty="s")
par(mar=c(1,1,1,1))
lineEqualAreaPlot(lapply(matrices, function(x) x[1,]), shapes=c(".")) 
par(new=TRUE)
lineEqualAreaPlot(list(geoCartesianFromStrikeDipDeg(c(12, 45))), colors="blue") #prediction
par(new=TRUE)
lineEqualAreaPlot(list(geoCartesianFromStrikeDipDeg(c(192, 45))), colors="red") #prediction
par(new=TRUE)
lineEqualAreaPlot(list(geoCartesianFromStrikeDipDeg(c(147, 90))), colors="purple")
par(new=TRUE)
lineEqualAreaPlot(list(geoCartesianFromStrikeDipDeg(c(57, 90))), colors="purple")#prediction
dev.off()




