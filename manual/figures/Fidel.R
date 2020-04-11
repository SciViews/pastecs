# For drawing graphs illustrating cumulated fidelities

# Stack two graphs
par(mfrow=c(2,1))

# First graph: distances classiques
sigma <- 0.4
x <- 560:680/20

mu1 <- 29
y1 <- 1.2/(sqrt(2*pi)*sigma)* exp(-((x - mu1)^2/(2*sigma^2)))-0.2
y1[y1 <= 0] <- 0
y1b <- y1
y1b[x > 30] <- NA
plot(x,y1b, type="l", xlab="Salinité", ylab="Abondance", main=expression(paste("Distances classiques: ", italic(D["A,B"]) == italic(D["B,C"]) == italic(D["A,C"]) == 0)), ylim=c(0,1), bty="L")

mu2 <- 31
y2 <- 1.2/(sqrt(2*pi)*sigma)* exp(-((x - mu2)^2/(2*sigma^2)))-0.2
y2[y2 <= 0] <- 0
y2b <- y2
y2b[x < 30 | x > 32] <- NA
lines(x,y2b, col=2)

mu3 <- 33
y3 <- 1.2/(sqrt(2*pi)*sigma)* exp(-((x - mu3)^2/(2*sigma^2)))-0.2
y3[y3 <= 0] <- 0
y3b <- y3
y3b[x < 32] <- NA
lines(x,y3b, col=4)
text(29, 0.4, "A", col=1, cex=1.5)
text(31, 0.4, "B", col=2, cex=1.5)
text(33, 0.4, "C", col=4, cex=1.5)

# Second graph: fidélités cumulées
y1c <- cumsum(y1)/sum(y1)
y2c <- cumsum(y2)/sum(y2)
y3c <- cumsum(y3)/sum(y3)
plot(x,y1c, type="l", xlab="Salinité", ylab="Fidélités cumulées", main=expression(paste("Distances sur fidélités cumulées: ", italic(D["A,B"]) < italic(D["A,C"]) *"    "* italic(D["B,C"]) < italic(D["A,C"]))), ylim=c(0,1), bty="L")
y2c[x < 28.3] <- NA
y3c[x < 30.3] <- NA
lines(x,y2c, col=2)
lines(x,y3c, col=4)
text(28.6, 0.5, "A", col=1, cex=1.5)
text(30.6, 0.5, "B", col=2, cex=1.5)
text(32.6, 0.5, "C", col=4, cex=1.5)

# Reset option for single graph
par(mfrow=c(1,1))