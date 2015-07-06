## logo for Burlington Data Scientists meetup
##
## includes tagline and Long Trail elevation profile as mountain shape
##
## data (dat4) are distances and elevations along the Long Trail
## from ca. Mt. Abraham to Mountain Rd in Stowe (Smugglers' Notch)
## taken by Brent using GPS
##
## selected data are from Mountain Rd to around Nebraska Notch
##
## original code by Kristian 2015-07-01

## for eqscplot
library(MASS)

dat4 <- read.csv("MansfieldGPSdata.xlsx - MansfieldGPSdata.csv")
str(dat4)
names(dat4)[1:2] <- c("dist", "elev")

plot(dat4$dist, dat4$elev, type = "l")

min(dat4$elev[dat4$dist > 180000 & dat4$dist < 190000])
dat4$dist[dat4$elev < 556 & dat4$dist > 180000 & dat4$dist < 190000]

abline(h = 555.89)
abline(v = 187905.9)

dat4[dat4$dist >= 187500 & dat4$dist <= 188500,]

dat4 <- dat4[dat4$dist >= 188000,]

eqscplot(dat4$dist, dat4$elev, type = "l")

## but we look at it from the west
dat4$dist <- -(dat4$dist - max(dat4$dist))
eqscplot(dat4$dist, dat4$elev, type = "l")

tagLine <- "Mountains of Data"
nchar(tagLine)

## a smooth spline to enable spacing those letters along the profile
fm <- smooth.spline(x = dat4$dist, y = dat4$elev, all.knots = TRUE)
letterDist <- seq(min(dat4$dist), max(dat4$dist),
                  length.out = 2 * nchar(tagLine) + 1)
letterElev <- as.data.frame(predict(fm, letterDist))
points(letterElev)
## but only use the even-numbered ones
letterElev <- letterElev[seq(2, nrow(letterElev), 2),]
points(letterElev, pch = 3)

for(i in 1:nchar(tagLine)){
    text(letterElev$x[i],
         letterElev$y[i],
         adj = c(0.5, -1),
         labels = substr(tagLine, start = i, stop = i), cex = 2)
     }

## lowercase monospaced characters in green for hiking tagline
tagLine <- tolower(tagLine)
thisColor <- "darkgreen"

png(filename = "logoPrototype.png", width = 960, height = 590)
eqscplot(dat4$dist, dat4$elev,
         type = "l",
         lwd = 15,
         lend = "square",
         col = thisColor,
         ## nothing but the line!
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
for(i in 1:nchar(tagLine)){
    text(letterElev$x[i],
         letterElev$y[i],
         adj = c(0.5, -1),
         family = "mono", font = 2,
         labels = substr(tagLine, start = i, stop = i),
         cex = 2.5, col = thisColor)
     }
text(x = median(range(dat4$dist)),
     y = median(range(dat4$elev)),
     adj = c(0.5, 0.5),
     labels = "Burlington\n\nData Scientists",
     cex = 10.75)
dev.off()

## much as I like the conceptual purity of distance and elevation on
## the same scale, here is 2x vertical exaggeration
png(filename = "logoExaggerated.png", width = 960, height = 590)
eqscplot(dat4$dist, dat4$elev * 2,
         type = "l",
         lwd = 15,
         lend = "square",
         col = thisColor,
         ## nothing but the line!
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
for(i in 1:nchar(tagLine)){
  text(letterElev$x[i],
       letterElev$y[i] * 2,
       adj = c(0.5, -1),
       family = "mono", font = 2,
       labels = substr(tagLine, start = i, stop = i),
       cex = 2.5, col = thisColor)
}
text(x = median(range(dat4$dist)),
     y = median(range(dat4$elev)) * 2,
     adj = c(0.5, 0.5),
     labels = "Burlington\n\nData Scientists",
     cex = 10.75)
dev.off()

## and here's a square one
png(filename = "logoExaggeratedSquare.png", width = 960, height = 960)
eqscplot(dat4$dist, dat4$elev * 2,
         type = "l",
         lwd = 15,
         lend = "square",
         col = thisColor,
         ## nothing but the line!
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
for(i in 1:nchar(tagLine)){
    text(letterElev$x[i],
         letterElev$y[i] * 2,
         adj = c(0.5, -1),
         family = "mono", font = 2,
         labels = substr(tagLine, start = i, stop = i),
         cex = 2.5, col = thisColor)
     }
text(x = median(range(dat4$dist)),
     y = median(range(dat4$elev)) * 2,
     adj = c(0.5, 0.5),
     labels = "Burlington\n\nData Scientists",
     cex = 10.75)
dev.off()
