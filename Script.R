#install.packages("nnet")
#install.packages("timeSeries")
load("~/R/Sample_1/03_fuelCell.RData")
#plot(A$I)
#plot(B$I)
library(timeSeries)
library(nnet)

#z-score scaling => zu werten zwischen -pi und pi
zAB <- data.frame(c(A$I,B$I),c(A$T,B$T),c(A$P_Air,B$P_Air),c(A$U,B$U))
names(zAB) <- c("I","T","P_Air","U")
zAB <- scale(zAB, center= TRUE, scale = TRUE)
# fatch the scale from AB
scaleFactorCenter = attr(zAB, "scaled:center")
scaleFactorScale = attr(zAB, "scaled:scale")

# read to A and B
zA <- zAB[0:5501,0:4]
zB <- zAB[5501:11002,0:4]


# TE test TR train
zATrainData = zA[0:3000,0:4]
zATestData = zA[3000:5501,0:3]
zATestLabel = zA[3000:5501,4]
zBTrainData = zB[0:3000,0:3]
zBTrainLabel = zB[0:3000,4]
zBTestData = zB[3000:5501,0:3]
zBTestLabel = zB[3000:5501,4]

#creating nn with function without labels
nnA <- nnet(U ~ I + T + P_Air,zATrainData,size=3,linout=TRUE,maxit=1000.)
# predict values for test data
pA <- predict(nnA, zATestData)
# to timeseries
tsA <- timeSeries(pA)
tsAX <- timeSeries(zATestLabel)
# plotten
plot(tsA)
title(main="A predict", col.main="black", font.main=4)
plot(tsAX)
title(main="A real", col.main="black", font.main=4)
# calc error (scaled)
ErrorA <- sum(abs(pA-zATestLabel))


# training with labels
nnB <- nnet(zBTrainData,zBTrainLabel,size=3,linout=TRUE,maxit=1000.)
# predict
pB <- predict(nnB, zBTestData)
# timeseries
tsB <- timeSeries(pB)
tsBX <- timeSeries(zBTestLabel)
# multiple plot
plot(tsB, type="o", col="blue")
lines(tsBX, type="o", pch=22, lty=2, col="green")
title(main="B", col.main="black", font.main=4)
# calc error (scaled)
ErrorB <- sum(abs(pB-zBTestLabel))


# datapruef 

zDataPruef <- scale(data_Pruef, scaleFactorCenter[0:3], scaleFactorScale[0:3])
pPA <- predict(nnA,zDataPruef)
#E1 <- sum() 
pPB <- predict(nnB,zDataPruef)
#E2

# Error -> which type is the battery A or B


