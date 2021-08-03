#code to create Figure 2 

source("/users/hummeln/Documents/geologyGeometry/loadStructures2018.R")


OurDikes <- dplyr::select(dikes, "pole", "easting", "northing", "distance", "strikedist") #this will include dikes from 2018 too...
FJdikes <- dplyr::select(FJdikes, "pole", "easting", "northing", "distance", "strikedist")
YoungDikesAve <- dplyr::select(YoungDikesAve, "pole", "easting", "northing", "distance", "strikedist")
JancinDikes <- dplyr::select(JancinDikes, "pole", "easting", "northing", "distance", "strikedist")
TitusDikes <- rbind(FJdikes, YoungDikesAve, OurDikes, JancinDikes)
TitusDikes <- filter(TitusDikes, distance != 'NA')
TitusDikes$hues <- hues(TitusDikes$distance, c(0, 28000))
TitusDikes <- filter(TitusDikes, easting < 660000, easting > 620000)
plot(TitusDikes$easting, TitusDikes$northing, col=TitusDikes$hues)
lineEqualAreaPlot(TitusDikes$pole, colors = TitusDikes$hues) #n = 535, too many by a litle bit, not sure where the extras are coming from. 

midTitusDikes <- filter(TitusDikes, distance < 18000, distance > 1500) #regress these data, but not until you figure out whether pmag and dikes can be regressed together
lineEqualAreaPlot(midTitusDikes$pole, colors = midTitusDikes$hues)

pdf(paste("TitusEtAlDikes.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(1,1,1,1))
lineEqualAreaPlot(TitusDikes$pole, colors = TitusDikes$hues)
dev.off()


#LAVAS
YoungLavas <- dplyr::select(YoungLavas, "pole", "easting", "northing", "distance", "strikedist") 
OurLavas <- dplyr::select(lavas, "pole", "easting", "northing", "distance", "strikedist") 
OurLavas <- filter(OurLavas, easting < 660000)
TitusLavas <- rbind(YoungLavas, OurLavas)
TitusLavas <- filter(TitusLavas, distance !="NA")
TitusLavas$hues <- hues(TitusLavas$distance, c(0, 28000))
lineEqualAreaPlot(TitusLavas$pole, colors = TitusLavas$hues)
pdf(paste("TitusEtAlLavas.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(1,1,1,1))
lineEqualAreaPlot(TitusLavas$pole, colors = TitusLavas$hues)
dev.off()


#PMAG 
TitusPmag <- pMag[1,]
TitusPmag$vector <- "NA"
for (i in (1:nrow(pMag))){
  row <- pMag[i,]
  trend <- row$trendDeg
  plunge <- row$plungeDeg
  row$vector <- list(geoCartesianFromTrendPlungeDeg(c(trend, plunge)))
  TitusPmag <- rbind(TitusPmag, row)
}
TitusPmag <- TitusPmag[-1,]

HorstPmag <- horstPmag[1,]
HorstPmag$vector <- "NA"
for (i in (1:nrow(horstPmag))){
  row <- horstPmag[i,]
  trend <- row$trendDeg
  plunge <- row$plungeDeg
  row$vector <- list(geoCartesianFromTrendPlungeDeg(c(trend, plunge)))
  HorstPmag <- rbind(HorstPmag, row)
}
HorstPmag <- HorstPmag[-1,]


TitusPmag <- dplyr::select(TitusPmag, "vector", "easting", "northing", "distance", "strikedist", "lithology")
HorstPmag <- dplyr::select(HorstPmag, "vector", "easting", "northing", "distance", "strikedist", "lithology")
TitusPmag <- rbind(TitusPmag, HorstPmag)
TitusPmag$hues <- hues(TitusPmag$distance, c(0, 28000))
FLpmag <- filter(TitusPmag, easting < 660000)
lineEqualAreaPlot(TitusPmag$pole, colors = TitusPmag$hues)
TitusPmagDikes <- filter(TitusPmag, lithology =="D")
TitusPmagLavas <- filter(TitusPmag, lithology =="L")

lineEqualAreaPlot(TitusPmagLavas$vector, colors = TitusPmagLavas$hues)
TJpmag <- filter(TitusPmag, easting > 660000)
lineEqualAreaPlot(TJpmag$vector, colors=TJpmag$hues)

lineEqualAreaPlot(TitusPmagDikes$vector, colors = TitusPmagDikes$hues)

pdf(paste("TitusEtAlLavaPmag.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(1,1,1,1))
lineEqualAreaPlot(TitusPmagLavas$vector, colors = TitusPmagLavas$hues)
dev.off()

pdf(paste("TitusEtAlDikePmag.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(1,1,1,1))
lineEqualAreaPlot(TitusPmagDikes$vector, colors = TitusPmagDikes$hues)
dev.off()

pdf(paste("lavas_17to50km.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(1,1,1,1))
lineKambPlot(farLavas$pole, colors = farLavas$hues)
dev.off()



