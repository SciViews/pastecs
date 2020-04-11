#################################################
# PASTECS - Manuel de l'utilisateur, 12/11/2002 #
#                                               #
#  Philippe Grosjean - phgrosjean@sciviews.org  #
#    Frédéric Ibanez - ibanez@obs-vlfr.fr       #
#                                               #
#   code des exemples pour R v. 1.6.0 ou plus   #
#################################################

# Copiez et coller la portion de code voulue dans
# la fenêtre de commande de R pour l'exécuter.
# Attention! Ne copiez pas plus qu'une portion
# de code continue à la fois!
# N'oubliez pas de toujours entrer
library(pastecs)
# Avant d'utiliser les fonctions de la librairie


#### Fonctions de base ####
library(pastecs)

## Obtenir de l'aide sur une fonction
?AutoD2
args(AutoD2)

## Importation de données
# Le code suivant nécessite un fichier 'mydata.txt' dans 'c:\temp'
#dat <- read.table("c:/temp/mydata.txt", header=TRUE, sep="\t",
#na.strings="NA")

data(marbio)
names(marbio)
marbio[1:5, 1:5]

## Gestion des variables
new.data <- 1
objects()
remove("new.data")
objects()

## Créer ses propres fonctions
levelmap <- function(x, level = 0) {
  data <- deparse(substitute(x))
  x <- as.matrix(x)
  image(1:ncol(x), 1:nrow(x), t(x > level)[, nrow(x):1], yaxt = "n",
    xlab = "variables", ylab = "", main = data)
}
levelmap(marbio)

## Exploration et manipulation des données
plot(marbio$ClausocalanusA, type = "l")

boxplot(as.data.frame(log(marbio[8:11] + 1)), col = 7)

hist(log(marbio$ClausocalanusA + 1), col = 7)

pairs(log(marbio[8:10] + 1))

marbio2 <- as.matrix(marbio)
marbio2[marbio2 == -99] <- NA # Remplace tous les -99 par NA

X <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
X
Y <- matrix(c(5, 6, 7, 8), nrow = 2, ncol = 2)
Y
X * Y             # Multiplication élément par élément
X %*% Y           # Multiplication matricielle

marbio.cen <- scale(marbio, center = TRUE, scale = TRUE)
marbio2 <- marbio   # Copie de marbio
marbio2[1, ] <- scale(marbio2[1, ], center = TRUE, scale = FALSE)

Z <- abs(rnorm(10000))
table(cut(Z, breaks = 0:5, labels = 1:5))

## Codage en classes et Buys-Ballot
Z <- c(abs(rnorm(8000)), rep(0, 2000))   # Don. artif, 20% zeros
table(cut(Z, breaks = 5)) 	# Laisse le programme choisir la coupure

Z2 <- Z[Z != 0]				# Données à l'exclusion des 0 dans Z2
cuts <- c(-1e-10, 1e-10, quantile(Z2, 1:5/5, na.rm = TRUE))
cuts
table(cut(Z, breaks = cuts))
disjoin(cut(Z, breaks = cuts))[1:10, ]

data(releve)
buysbal(releve$Day, releve$Melosul, frequency = 4, units = "days",
  datemin = "21/03/1989", dateformat = "d/m/Y")
buysbal(releve$Day, releve$Melosul, frequency = 4, units = "days",
  datemin = "21/03/1989", dateformat = "d/m/Y", count = TRUE)


#### Statistiques descriptives ####
data(marbio)
summary(marbio[, 13:16])

stat.desc(marbio[, 13:16], basic = TRUE, desc = TRUE, norm = TRUE, p = 0.95)

## Estimateur de Pennington
data(marbio)
stat.pen(marbio[, c(4, 14:16)], basic = TRUE, desc = TRUE)

pennington(marbio[,"Copepodits2"])
Pm <- pennington(marbio[,"Copepodits2"], calc = "mean", na.rm = TRUE)
Pm

## Statistiques glissantes
data(marbio)
statsl <- stat.slide(1:68, marbio[, "ClausocalanusA"], xmin = 0, n = 7,
  deltat = 10)
statsl
plot(statsl, stat = "mean", leg = TRUE, lpos = c(55, 2500),
  xlab = "Station", ylab = "ClausocalanusA")

statsl2 <- stat.slide(1:68, marbio[, "ClausocalanusA"],
  xcut = c(0, 17, 25, 30, 41, 46, 70), basic = TRUE, desc = TRUE, norm = TRUE,
  pen = TRUE, p = 0.95)
statsl2
plot(statsl2, stat = "median", xlab = "Stations", ylab = "Counts",
  main = "Clausocalanus A")                       # Médiane
lines(statsl2, stat = "min")                      # Minimum
lines(statsl2, stat = "max")                      # Maximum
lines(c(17, 17), c(-50, 2600), col = 4, lty = 2)  # Séparations
lines(c(25, 25), c(-50, 2600), col = 4, lty = 2)
lines(c(30, 30), c(-50, 2600), col = 4, lty = 2)
lines(c(41, 41), c(-50, 2600), col = 4, lty = 2)
lines(c(46, 46), c(-50, 2600), col = 4, lty = 2)
text(c(8.5, 21, 27.5, 35, 43.5, 57), 2300,
  labels = c("Peripheral Zone", "D1", "C", "Front", "D2", "Central Zone"))
legend(0, 1900, c("series", "median", "range"), col = 1:3, lty = 1)
statsl2$stat[c("mean", "pos.mean", "geo.mean", "pen.mean"), ]
statsl2$y
statsl2$x
statsl2$xcut


#### Sélection des descripteurs ####
## Méthode d'Escoufier
data(marbio)
marbio.esc <- escouf(marbio, verbose = TRUE)
summary(marbio.esc)
plot(marbio.esc)
marbio.esc$vr

marbio.esc$level <- 0.95
marbio.esc$level <- identify(marbio.esc)

marbio2 <- extract(marbio.esc)
names(marbio2)

## Tri par abondance
data(bnr)
bnr.abd <- abund(bnr)
summary(bnr.abd)
plot(bnr.abd, dpos = c(105, 100))

bnr.abd$n <- identify(bnr.abd)

bnr.abd$vr
bnr2 <- extract(bnr.abd)
names(bnr2)  # Montre les variables extraites

bnr.abd$n <- identify(bnr.abd)           # Identifie un second seuil

bnr3 <- extract(bnr.abd, left = FALSE)   # Extrait à droite
bnr3.abd <- abund(bnr3, f = 1)
plot(bnr3.abd, dpos = c(26, 100))

bnr3.abd$n <- identify(bnr3.abd)         # Choisi le seuil

bnr4 <- extract(bnr3.abd, left = FALSE)  # Extrait à nouveau
names(bnr4)


#### Régularisation ####
options("chron.origin")  # Regarde sa valeur
options(chron.origin = c(month = 1, day = 1, year = 1990))
options("chron.origin")

options(chron.origin = NULL)    # Reset default value
options("chron.origin")

data(releve)
releve$Day
length(releve$Day)
ecarts <- releve$Day[2:61] - releve$Day[1:60]
ecarts
range(ecarts)
mean(ecarts)

regul.screen(releve$Day, xmin = 0:11, deltat = 16:27, tol = 1.05)

regul.adj(releve$Day, xmin = 9, deltat = 21)

rel.reg <- regul(releve$Day, releve[3:8], xmin = 9, n = 63, deltat = 21,
  tol = 1.05, methods = c("s", "c", "l", "a", "s", "a"), window = 21)
rel.reg
plot(rel.reg, 5)

weight <- identify(rel.reg, 5, col = 4) * 4 + 1
weight

regul.screen(releve$Day, weight, xmin = -1:10, deltat = c(16:23), tol = 2.2)

regul.adj(releve$Day, xmin = 0, deltat = 17)

melo.reg <- regul(releve$Day, releve$Melosul, xmin = 0, n = 78, deltat = 17,
  tol = 2.125, methods = "linear")
plot(rel.reg, 5, plot.pts = FALSE)
lines(melo.reg, col = 4, plot.pts = FALSE)
legend(820, 60000,
  c("initial series", "trial1 (x=9, d=21)", "trial2 (x=0, d=17)"),
  col = c(1, 2, 4), lty = 1)

regul.screen(releve$Day, weight, xmin = -1:14, deltat = 365.25 / 24, tol = 2.2)

regul.adj(releve$Day, xmin = 6, deltat = 365.25 / 24)

melo.regy <- regul(releve$Day, releve$Melosul, xmin = 6, n = 87,
  units = "daystoyears", frequency = 24, tol = 2.2, methods = "linear",
  datemin = "21/03/1989", dateformat = "d/m/Y")
melo.regy
plot(melo.regy, main = "Regulation of Melosul")
melo.ts <- tseries(melo.regy)
is.tseries(melo.ts)
melo.ts2 <- extract(rel.reg, series = "Melosul")

## Régularisation par valeur constante
data(releve)
reg <- regconst(releve$Day, releve$Melosul)
plot(releve$Day, releve$Melosul, type = "l")
lines(reg$x, reg$y, col = 2)

plot(releve$Day, releve$Melosul, type = "p")
lines(regconst(releve$Day, releve$Melosul, n = length(releve$Day) * 10),
  col = 2, lty = 2)

## Régularistation linéaire
data(releve)
reg <- reglin(releve$Day, releve$Melosul)
plot(releve$Day, releve$Melosul, type = "l")
lines(reg$x, reg$y, col = 4)

plot(releve$Day, releve$Melosul, type = "p")
lines(reglin(releve$Day, releve$Melosul, n = length(releve$Day) * 10),
  col = 4, lty = 2)

## Régularisation par courbes splines
data(releve)
reg <- regspline(releve$Day, releve$Melosul)
plot(releve$Day, releve$Melosul, type = "l")
lines(reg$x, reg$y, col = 3)

plot(releve$Day, releve$Melosul, type = "p")
lines(regspline(releve$Day, releve$Melosul, n = length(releve$Day) * 10),
  col = 3, lty = 2)

## Régularisation par la méthode des aires
data(releve)
reg <- regarea(releve$Day, releve$Melosul, window = 25)
reg2 <- regarea(releve$Day, releve$Melosul, window = 50)
plot(releve$Day, releve$Melosul, type = "l")
lines(reg$x, reg$y, col = 2)
lines(reg2$x, reg2$y, col = 4)

plot(releve$Day, releve$Melosul, type = "p")
lines(regarea(releve$Day, releve$Melosul, window = 25,
  n = length(releve$Day) * 10), col = 2, lty = 2)

plot(releve$Day, releve$Melosul, type = "p")
lines(regarea(releve$Day, releve$Melosul, window = 50,
  n = length(releve$Day) * 10), col = 4, lty = 2)


#### Manipulation de base des séries régulières ####
library(ts)

## Création et représentation graphique d'une série
tser <- ts(sin((1:100) / 6 * pi) + rnorm(100, sd = 0.5),
  start = c(1998, 4), frequency = 12)
tser
class(tser)
is.tseries(tser) # retourne TRUE, si l'objet est une time series
ts.plot(tser)

mtser <- ts.intersect(tser, lag(tser, 5))
plot(mtser)

## Manipulation des paramètres temporels d'une série
start(tser)
end(tser)
frequency(tser)
time(tser)
tser.cycle <- cycle(tser)
tser.cycle
boxplot(split(tser, tser.cycle), names = month.abb, col = 3)
aggregate(tser, 4, mean)
window(tser, start = c(1999, 1), end = c(2001, 12))


#### Analyse de séries spatio-temporelles ####
## Autocorrelation, autocovariance,...
tser <- ts(sin((1:100) / 6 * pi) + rnorm(100, sd = 0.5),
  start = c(1998, 4), frequency = 12)
acf(tser)

mtser <- ts.intersect(tser, stats::lag(tser, 5))
ccf(mtser[, 1], mtser[, 2])

## Autocorrelation multiple
data(marphy)
names(marphy)
marphy.ts <- as.ts(as.matrix(marphy[, 1:3]))
plot(marphy.ts)

AutoD2(marphy.ts)

CrossD2(marphy.ts, marphy.ts)

marphy.d2 <- CenterD2(marphy.ts, window = 16)
lines(c(17, 17), c(-1, 15), col = 4, lty = 2)    # Séparations
lines(c(25, 25), c(-1, 15), col = 4, lty = 2)
lines(c(30, 30), c(-1, 15), col = 4, lty = 2)
lines(c(41, 41), c(-1, 15), col = 4, lty = 2)
lines(c(46, 46), c(-1, 15), col = 4, lty = 2)
text(c(8.5, 21, 27.5, 35, 43.5, 57), 11,
  labels = c("Peripheral Zone", "D1", "C", "Front", "D2", "Central Zone"))
time(marphy.ts)[marphy.d2$D2 > marphy.d2$chisq]

## Analyse harmonique et analyse spectrale
par(mfrow = c(1, 2))             # Place 2 graphes côte à côte
spectrum(tser)                   # Périodogramme brut
spectrum(tser, spans = c(3, 5))  # Périodogramme lissé
par(mfrow = c(1, 1))             # Valeur par défaut pour mfrow

cpgram(tser)

## Analyse spectrale croisée
mtser.spc <- spectrum(mtser, spans = c(3, 3))
plot(mtser.spc, plot.type = "c")

plot(mtser.spc, plot.type = "p")

## Points de retournement
data(marbio)
plot(marbio[, "Nauplii"], type = "l")
Nauplii.tp <- turnpoints(marbio[, "Nauplii"])
summary(Nauplii.tp)

plot(Nauplii.tp)

plot(marbio[, "Nauplii"], type = "l")
lines(Nauplii.tp)
title("Raw data, envelope maxi., mini. and median line")

data(releve)
weight.tp <- extract(turnpoints(releve[, "Melosul"]),
  no.tp = 1, peak = 5, pit = 5)
weight.tp

## Tournogramme
data(bnr)
bnr4 <- as.ts(bnr[, 4])
plot(bnr4, type = "l", main = "bnr4: raw data", xlab = "Time")

bnr4.turno <- turnogram(bnr4)
summary(bnr4.turno)

turnogram(bnr4, complete = TRUE)

bnr4.interv3 <- extract(bnr4.turno)
plot(bnr4, type = "l", lty = 2, xlab = "Time")
lines(bnr4.interv3, col = 2)
title("Original bnr4 (dotted) versus max. info. curve (plain)")

plot(bnr4.turno)
bnr4.turno$level <- identify(bnr4.turno, col = 3)

bnr4.turno$level <- 6
bnr4.interv6 <- extract(bnr4.turno)
plot(bnr4, type = "l", lty = 2, xlab = "Time")
lines(bnr4.interv3, col = 2)
lines(bnr4.interv6, col = 3)
legend(70, 580, c("original", "interval=3", "interval=6"),
  col = 1:3, lty = c(2, 1, 1))
title("Original versus interval 3 and interval 6 curves")

## Variogramme
data(bnr)
vario(bnr[, 4])

## Distogramme
data(bnr)
disto(bnr)

## Tendance générale
data(marbio)
trend.test(marbio[, 8])

marbio8.trend.test <- trend.test(marbio[, 8], R = 999)
marbio8.trend.test
plot(marbio8.trend.test)

boot.ci(marbio8.trend.test, conf = c(0.95, 0.99), type = "norm")
marbio8.trend.test$p.value

## Tendance locale (méthode des sommes cumulées)
data(bnr)
bnr8.lt <- local.trend(bnr[, 8])
identify(bnr8.lt)


#### Décomposition de séries spatio-temporelles ####
data(releve)
rel.regy <- regul(releve$Day, releve[3:8], xmin = 6, n = 87,
  units = "daystoyears", frequency = 24, tol = 2.2, methods = "linear",
  datemin = "21/03/1989", dateformat = "d/m/Y")
rel.ts <- tseries(rel.regy)
rel.dec <- tsd(rel.ts, method = "loess", s.window = 13, trend = FALSE)
rel.dec
plot(rel.dec, series = 5, col = 1:3)

rel.des <- extract(rel.dec, series = 3:6, components = "deseasoned")
rel.des[1:10, ]
rel.des.dec <- tsd(rel.des, method = "average", order = 2, times = 10)
plot(rel.des.dec, series = 3, col = c(2, 4, 6))

plot(rel.des.dec, series = 3, col = c(2, 4), stack = FALSE, resid = FALSE,
  labels = c("without season cycle", "trend"), lpos = c(0, 55000))

rel.res2 <- extract(rel.des.dec, components = "residuals")
spectrum(rel.res2[, 3], spans = c(5, 7))

## Filtrage linéaire par la méthode des différences
data(marbio)
ClausoB.ts <- ts(log(marbio$ClausocalanusB + 1))
ClausoB.dec <- decdiff(ClausoB.ts, lag = 1, order = 2, ends = "fill")
plot(ClausoB.dec, col = c(1, 4, 2), xlab = "stations")

## Filtrage linéaire par les moyennes mobiles
data(marbio)
ClausoB.ts <- ts(log(marbio$ClausocalanusB + 1))
ClausoB.dec <- decaverage(ClausoB.ts, order = 2, times = 10, sides = 2,
  ends = "fill")
plot(ClausoB.dec, col = c(1, 3, 2), xlab = "stations")

plot(ClausoB.dec, col = c(1, 3), xlab = "stations", stack = FALSE,
  resid = FALSE, lpos = c(53, 4.3))

data(releve)
melo.regy <- regul(releve$Day, releve$Melosul, xmin = 9, n = 87,
  units = "daystoyears", frequency = 24, tol = 2.2, methods = "linear",
  datemin = "21/03/1989", dateformat = "d/m/Y")
melo.ts <- tseries(melo.regy)
melo.dec <- decaverage(melo.ts, order = "periodic", times = 1, sides = 2,
  ends = "periodic")
plot(melo.dec, col = c(1, 6, 2))

## Filtrage non linéaire par les médianes mobiles
data(marbio)
ClausoB.ts <- ts(log(marbio$ClausocalanusB + 1))
ClausoB.dec <- decmedian(ClausoB.ts, order = 2, times = 10, ends = "fill")
plot(ClausoB.dec, col = c(1, 4, 2), xlab = "stations")

plot(ClausoB.dec, col = c(0, 2), xlab = "stations", stack = FALSE,
  resid = FALSE)
lines(c(17, 17), c(0, 10), col = 4, lty = 2)
lines(c(25, 25), c(0, 10), col = 4, lty = 2)
lines(c(30, 30), c(0, 10), col = 4, lty = 2)
lines(c(41, 41), c(0, 10), col = 4, lty = 2)
lines(c(46, 46), c(0, 10), col = 4, lty = 2)
text(c(8.5, 21, 27.5, 35, 43.5, 57), 8.7,
  labels = c("Peripheral Zone", "D1", "C", "Front", "D2", "Central Zone"))

## Filtrage par les vecteurs propres
data(releve)
melo.regy <- regul(releve$Day, releve$Melosul, xmin = 9, n = 87,
  units = "daystoyears", frequency = 24, tol = 2.2, methods = "linear",
  datemin = "21/03/1989", dateformat = "d/m/Y")
melo.ts <- tseries(melo.regy)
acf(melo.ts)

melo.evf <- decevf(melo.ts, lag = 4, axes = 1)
plot(melo.evf, col = c(1, 4, 2))

plot(melo.evf, col = c(1, 4), xlab = "stations", stack = FALSE, resid = FALSE,
  lpos = c(0, 60000))

## Estimation de la tendance par régression
# Modèle linéaire de la tendance
data(marphy)
density <- ts(marphy[, "Density"])
Time <- time(density)
density.lin <- lm(density ~ Time)
summary(density.lin)
xreg <- predict(density.lin)
plot(density)
lines(xreg, col = 3)
title("Linear model for trend in 'density'")

density.dec <- decreg(density, xreg)
plot(density.dec, col = c(1, 3, 2), xlab = "stations")

# Modèle polynomial de la tendance
density.poly <- lm(density ~ Time + I(Time^2))
summary(density.poly)
xreg2 <- predict(density.poly)
plot(density)
lines(xreg2, col = 3)
title("Order 2 polynome for trend in 'density'")

density.dec2 <- decreg(density, xreg2)
plot(density.dec2, col = c(1, 3, 2), xlab = "stations")

# Modèle non linéaire de la tendance
library(nls)
density.logis <- nls(density ~ SSlogis(Time, Asym, xmid, scal))
summary(density.logis)
xregl <- predict(density.logis)
plot(density)
lines(xregl, col = 3)
title("Logistic model for trend in 'density'")

density.decl <- decreg(density, xregl)
plot(density.decl, col = c(1, 3, 2), xlab = "stations")

# Modèle sinusoidal de tendance cyclique
tser <- ts(sin((1:100) / 12 * pi) + rnorm(100, sd = 0.3),
  start = c(1998, 4), frequency = 24)
Time <- time(tser)
tser.sin <- lm(tser ~ I(cos(2 * pi * Time)) + I(sin(2 * pi * Time)))
summary(tser.sin)
tser.reg <- predict(tser.sin)
tser.dec <- decreg(tser, tser.reg)
plot(tser.dec, col = c(1, 4), xlab = "stations", stack = FALSE, resid = FALSE,
  lpos = c(0, 4))

plot(tser.dec, col = c(1, 4, 2), xlab = "stations")

## Décomposition par CENSUS II
data(releve)
rel.regy <- regul(releve$Day, releve[2:7], xmin = 6, n = 87,
  units = "daystoyears", frequency = 24, tol = 2.2, methods = "linear",
  datemin = "21/03/1989", dateformat = "d/m/Y")
rel.ts <- tseries(rel.regy)
start(rel.ts)
end(rel.ts)
rel.ts2 <- window(rel.ts, end = c(1992, 5))
rel.dec2 <- deccensus(rel.ts2[, "Melosul"], trend = TRUE)

plot(rel.dec2, col = c(1, 4, 3, 2))

## Décomposition par LOESS
data(releve)
melo.regy <- regul(releve$Day, releve$Melosul, xmin = 9, n = 87,
  units = "daystoyears", frequency = 24, tol = 2.2, methods = "linear",
  datemin = "21/03/1989", dateformat = "d/m/Y")
melo.ts <- tseries(melo.regy)
melo.dec <- decloess(melo.ts, s.window = "periodic")
plot(melo.dec, col = 1:3)

melo.dec2 <- decloess(melo.ts, s.window = 13, s.degree = 1)
plot(melo.dec2, col = 1:3)

melo.tsd <- tseries(melo.dec2)
melo.spc <- spectrum(melo.tsd, spans = c(3, 3), plot = FALSE)

plot(melo.spc, plot.type = "c")

melo.dec3 <- decloess(melo.ts, s.window = "periodic", trend = TRUE)
plot(melo.dec3, col = c(1, 4, 3, 2))

#### Méthodes d'ordination et de classification de données multivariées ####
## Analyse en composantes principales (ACP)
data(marbio)
marbio.pca <- princomp(log(marbio + 1), cor = TRUE)
summary(marbio.pca)
plot(marbio.pca)

marbio.pc <- predict(marbio.pca)
plot(marbio.pc[, 1], marbio.pc[, 2], xlab = "first principal component",
ylab = "second principal component", type = "n")
text(marbio.pc[, 1], marbio.pc[, 2], labels = 1:nrow(marbio.pc))

## Analyse en composantes principales biplot
data(marbio)
marbio.pca <- princomp(log(marbio + 1), cor = TRUE)
biplot(marbio.pca)

## Classification: matrice de distance & dendrogramme
library(mva)
data(marbio)
obs.dist <- dist(as.matrix(marbio[1:10, 1:5]), "man")
obs.dist
esp.dist <- dist(t(log(marbio[1:10, 1:5] + 1)), "eucl")
esp.dist
obs.dist <- dist(log(marbio[1:10, 1:10] + 1), "eucl")
hc <- hclust(obs.dist, method = "average")
plot(hc)

lines.hclust <- function(hclust.obj, h, lty = 2, ...) {
  lines(c(-1, length(hclust.obj$order) + 1), c(h, h), lty = lty, ...)
}
lines.hclust(hc, 4.1, col = "gray")
rect.hclust(hc, h = 4.1, which = c(2, 3), border = c(2, 4))

esp.l <- t(log(marbio[1:10, 1:10] + 1))  # Matrice log transposée
esp.dist <- dist(esp.l, "eucl")          # Matrice distances
hc <- hclust(esp.dist, "average")        # Construire dendro.
plot(hc, hang = -1)                      # Afficher dendro.
rect.hclust(hc, k = 3)                   # Encadrer 3 groupes

group <- cutree(hc, k = 3)               # Crée 3 groupes
group                                    # Les affiche

group <- as.factor(group)                # Les transforment en facteurs
esp.l <- cbind(esp.l, group)             # et les ajoutent à la matrice

## Cadrage multidimensionnel (MDS)
library(MASS)
data(marbio)
esp.dist <- dist(t(log(marbio + 1)), "eucl")
esp.mds <- isoMDS(esp.dist)
plot(esp.mds$points, type = "n")
text(esp.mds$points, labels = as.character(1:ncol(marbio)))

esp.sh <- Shepard(esp.dist, esp.mds$points)
plot(esp.sh)
lines(esp.sh$x, esp.sh$yf, type = "S", col = 2)
title("Shepard's diagram")


#### Partie II ####
## daystoyears()
data(releve)
rel.time <- daystoyears(releve$Day, datemin = "10/05/2000")

## first()
a <- c(NA, 1, 2, NA, 3, 4, NA)
first(a, na.rm = TRUE)

## GetUnitText()
timeser <- ts(1:24, frequency = 12)   # 12 observations per year
attr(timeser, "units") <- "years"
GetUnitText(timeser)

## last()
a <- c(NA, 1, 2, NA, 3, 4, NA)
last(a, na.rm = TRUE)

## pgleissberg()
pgleissberg(20, 8, lower.tail = TRUE, two.tailed = FALSE)
pgleissberg(20, 10, lower.tail = TRUE, two.tailed = FALSE)

## specs.regul()
data(releve)
rel.reg <- regul(releve$Day, releve$Melosul, xmin = 0, n = 78, deltat = 17,
  tol = 2.125, methods = "linear")
specs(rel.reg)
new.reg <- regul(releve$Day, releve$Navi, specs = specs(rel.reg),
  methods = "area")

## specs.tsd()
data(releve)
rel.reg <- regul(releve$Day, releve$Melosul, xmin = 0, n = 78, deltat = 17,
  tol = 2.125, methods = "linear")
rel.ts <- tseries(rel.reg)
melo.dec <- tsd(rel.ts, method = "average", order = 2, times = 10, sides = 2,
  ends = "fill")
specs(melo.dec)
new.dec <- tsd(rel.ts, specs = specs(melo.dec), times = 1)

## yearstodays()
data(releve)
# Transforme de "days" en "years"
rel.years <- daystoyears(releve$Day, datemin = "10/05/2000")
# Retransforme en unité "days" en partant de 1
rel.days <- yearstodays(rel.years, xmin = 1)
rel.days
