## for eqscplot
library(MASS)

## dat4 <- read.csv("MansfieldGPSdata.xlsx - MansfieldGPSdata.csv")
## str(dat4)
## dat4 <- dat4[,1:2]
## names(dat4) <- c("dist", "elev")
## save(dat4, file = "MansfieldGPSdata.RData")
load("MansfieldGPSdata.RData")

plot(dat4$dist, dat4$elev, type = "l")

min(dat4$elev[dat4$dist > 180000 & dat4$dist < 190000])
dat4$dist[dat4$elev < 556 & dat4$dist > 180000 & dat4$dist < 190000]

abline(h = 555.89)
abline(v = 187905.9)

dat5 <- dat4[dat4$dist >= 187905.9,]

eqscplot(dat5$dist, dat5$elev, type = "l")
tagLine <- "Mountains of Data"
nchar(tagLine)
diff(range(dat5$dist))/nchar(tagLine)
diff(range(dat5$dist))/nchar(tagLine)/2
(targets <- seq(from = min(dat5$dist) + diff(range(dat5$dist))/nchar(tagLine)/2,
                by = diff(range(dat5$dist))/nchar(tagLine),
                length.out = 17))

if(exists("dat6")) rm(dat6)
for(i in targets){
    sqDist <- (dat5$dist - i)^2
    addRow <- dat5[sqDist == min(sqDist), 1:2]
    if(!exists("dat6")) dat6 <- addRow
    else dat6 <- rbind(dat6, addRow)
}
dat6

tagLine <- toupper(tagLine)

png(filename = "logoPrototype.png", width = 960, height = 590)
eqscplot(dat5$dist, dat5$elev, type = "l",
         xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         bty = "n")
for(i in 1:nchar(tagLine)){
    text(targets[i],#dat6$dist[i],
         dat6$elev[i],
         adj = c(0, -1),
         labels = substr(tagLine, start = i, stop = i), cex = 2)
     }
text(x = median(dat5$dist), y = 3000, labels = "Burlington", cex = 10)
text(x = median(dat5$dist), y = -500, labels = "Data Scientists", cex = 10)
dev.off()

## looking from the west
head(dat7 <- dat5[,1:2])
dat7$dist <- -(dat7$dist - max(dat7$dist))

(targets <- seq(from = min(dat7$dist) + diff(range(dat7$dist))/nchar(tagLine)/2,
                by = diff(range(dat7$dist))/nchar(tagLine),
                length.out = 17))

if(exists("dat6")) rm(dat6)
for(i in targets){
    sqDist <- (dat7$dist - i)^2
    addRow <- dat7[sqDist == min(sqDist), 1:2]
    if(!exists("dat6")) dat6 <- addRow
    else dat6 <- rbind(dat6, addRow)
}
dat6

tagLine <- tolower(tagLine)
thisColor <- "darkgreen"

png(filename = "logoPrototype.png", width = 960, height = 590)
eqscplot(dat7$dist, dat7$elev,
         type = "l",
         lwd = 15,
         lend = "square",
         col = thisColor,
         xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         bty = "n")
for(i in 1:nchar(tagLine)){
    text(targets[i],#dat6$dist[i],
         dat6$elev[i],
         family = "mono", font = 2,
         adj = c(0, -1),
         labels = substr(tagLine, start = i, stop = i),
         cex = 2.5, col = thisColor)
     }
text(x = max(dat7$dist)/2, y = 1200,
     labels = "Burlington\n\nData Scientists",
     cex = 10.75)
dev.off()

timePoints <- sort(c(seq(0, 150, 30), seq(29.99, 179.99, 30)))
semiannual <- rep(16.50*6, length(timePoints))
monthly <- 19.99*rep((1:6), each = 2)
plot(x = timePoints, y = monthly, type = "l")
lines(x = timePoints, y = semiannual)
