source("~/Desktop/geologyGeometry/library/all.R")

source("~/Desktop/geologyGeometry/loadStructures2018.R")

#To make plots for Figure 9

TJslicks <- filter(allSlicks, easting > 660000)
TJslicks <- filter(TJslicks, easting < 675000)
TJslicks <- filter(TJslicks, northing > 7330000)


TJlavas$hues <- hue(TJlavas$distance, c(0,7000))
TJslicks$hues <- hue(TJslicks$distance, c(0, 7000))
#TJslicks$hues <- color_values(c(TJslicks$distance, 0, 7000))
TJfractures$hues <- hue(TJfractures$distance, c(0, 7000))
TJdikes$hues <- hues(TJdikes$distance, c(0, 7000))
TJfaults$hues <- hues(TJfaults$distance, c(0, 7000))
TJbigFaults$hues <- hues(TJbigFaults$distance, c(0, 7000))
TJveins$hues <- hues(TJveins$distance, c(0, 7000))

plot(TJslicks$easting, TJslicks$northing, col=TJslicks$hues)
par(new=TRUE)   #TJ maps code
abline(7571213,-0.3593228)
#abline(9260629, -2.8829) #adds another strand of HFF in Tjornes--wrong?


TJslicks1 <- filter(TJslicks, distance < 2000)
TJslicks2 <- filter(TJslicks, distance > 2000, distance < 5000)
TJslicks1R <- filter(TJslicks1, slipSense=="R")
TJslicks1L <-filter(TJslicks1, slipSense=="L")
TJslicks1N <- filter(TJslicks1, slipSense=="N")
TJslicks2R <- filter(TJslicks2, slipSense=="R")
TJslicks2L <-filter(TJslicks2, slipSense=="L")
TJslicks2N <- filter(TJslicks2, slipSense=="N")
TJdikes1 <- filter(TJdikes, distance < 2000)
TJdikes2 <- filter(TJdikes, distance >2000, distance < 5000)
TJfracturesSouth <- filter(TJfractures, northing <7330000)
TJfaultsNorth <- filter(TJfaults, northing > 7330000)
TJbigFaultsSouth <-filter(TJbigFaults, northing < 7330000)

pdf(paste("TJSlicks1R.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(2,2,2,2))
lineKambPlot(TJslicks1R$pole, colors=TJslicks1R$hues)
dev.off()
pdf(paste("TJSlicks1L.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(2,2,2,2))
lineKambPlot(TJslicks1L$pole, colors=TJslicks1L$hues)
dev.off()
pdf(paste("TJSlicks1N.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(2,2,2,2))
lineKambPlot(TJslicks1N$pole, colors=TJslicks1N$hues)
dev.off()

pdf(paste("TJSlicks2R.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(2,2,2,2))
lineKambPlot(TJslicks2R$pole, colors=TJslicks2R$hues)
dev.off()
pdf(paste("TJSlicks2L.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(2,2,2,2))
lineKambPlot(TJslicks2L$pole, colors=TJslicks2L$hues)
dev.off()
pdf(paste("TJSlicks2N.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(2,2,2,2))
lineKambPlot(TJslicks2N$pole, colors=TJslicks2N$hues)
dev.off()

pdf(paste("TJfracturesSouthHFF.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(2,2,2,2))
lineKambPlot(TJfracturesSouth$pole, colors=TJfracturesSouth$hues)
dev.off()

pdf(paste("TJdikes1coloredto7km.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(2,2,2,2))
lineKambPlot(TJdikes1$pole, colors=TJdikes1$hues)
dev.off()

pdf(paste("TJdikes2coloredto7km.pdf", sep = ""), useDingbats = FALSE, width = 4, height = 4)
par(pty="s")
par(mar=c(2,2,2,2))
lineKambPlot(TJdikes2$pole, colors=TJdikes2$hues)
dev.off()

lineKambPlot(TJfaultsNorth$pole, colors=TJfaultsNorth$hues)
lineKambPlot(TJbigFaultsSouth$pole, colors=TJbigFaultsSouth$hues)

lineEqualAreaPlot(TJlavas$pole, colors=TJlavas$hues)
