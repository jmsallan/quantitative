### R code from vignette source 'IntroR-ES.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: ops7
###################################################
x <- 2
X <- 3
x
X
x <- x +1
x


###################################################
### code chunk number 2: ops8
###################################################
a <- "Hello World"
a
b <- "203"
b
c <- as.numeric(b)
c


###################################################
### code chunk number 3: ops8
###################################################
estado <- c("tas", "qld", "sa", "sa", "sa", "vic", "nt",
"act", "qld", "nsw", "wa", "nsw", "nsw", "vic", "vic",
"vic", "nsw", "qld", "qld", "vic", "nt", "wa", "wa",
"qld", "sa", "tas", "nsw", "nsw", "wa", "act")
estado <- factor(estado)
levels(estado)


###################################################
### code chunk number 4: IntroR-ES.Rnw:163-165
###################################################
d <- numeric(5)
d


###################################################
### code chunk number 5: IntroR-ES.Rnw:170-172
###################################################
e <- c(4,-1,2,3)
e


###################################################
### code chunk number 6: IntroR-ES.Rnw:180-182
###################################################
f <- c("ab", "l", "fz", "a")
f


###################################################
### code chunk number 7: IntroR-ES.Rnw:187-189
###################################################
g <- c(TRUE, FALSE, TRUE)
g


###################################################
### code chunk number 8: IntroR-ES.Rnw:202-205
###################################################
s <- c(2, 3, 4, 6, 9, 1, 3)
which(s>5)
s[which(s>5)]


###################################################
### code chunk number 9: IntroR-ES.Rnw:225-227
###################################################
A <- matrix(1:16, 4, 4)
A


###################################################
### code chunk number 10: IntroR-ES.Rnw:233-236
###################################################
B <- matrix(1:16, 4, 4, 
            byrow = TRUE)
B


###################################################
### code chunk number 11: IntroR-ES.Rnw:256-257
###################################################
A


###################################################
### code chunk number 12: IntroR-ES.Rnw:263-266
###################################################
A[2, 3]
A[ ,3]
A[2, ]


###################################################
### code chunk number 13: IntroR-ES.Rnw:279-282
###################################################
list <- list(albert = 54, bryan = A, carlos = c(1,2,3))
list[[1]]
list$carlos


###################################################
### code chunk number 14: IntroR-ES.Rnw:307-309
###################################################
head(mtcars) #principio del df
tail(mtcars)#final del df


###################################################
### code chunk number 15: IntroR-ES.Rnw:320-324
###################################################
length(mtcars) #variables
nrow(mtcars) #observaciones
mtcars[3, ] #observación 3
mtcars$mpg[1:10] #primeros 10 valores de mpg


###################################################
### code chunk number 16: IntroR-ES.Rnw:437-439
###################################################
head(airquality)
tail(airquality)


###################################################
### code chunk number 17: IntroR-ES.Rnw:450-452
###################################################
str(airquality)
summary(airquality)


###################################################
### code chunk number 18: IntroR-ES.Rnw:464-469
###################################################
aq.clean <- airquality[which(!is.na(airquality$Ozone) 
                                     & !is.na(airquality$Solar.R)), ]
nrow(airquality)
nrow(aq.clean)
summary(aq.clean)


###################################################
### code chunk number 19: IntroR-ES.Rnw:480-483
###################################################
par(mfrow=c(1,2))
hist(aq.clean$Ozone, col="red", main="Ozono")
hist(aq.clean$Solar.R, col="blue", main="Rad. solar")


###################################################
### code chunk number 20: IntroR-ES.Rnw:494-499
###################################################
d.ozone <- density(aq.clean$Ozone)
d.solar <- density(aq.clean$Solar.R)
par(mfrow=c(1,2))
plot(d.ozone, main="Ozono")
plot(d.solar, main="Rad. Solar")


###################################################
### code chunk number 21: IntroR-ES.Rnw:511-512
###################################################
plot(aq.clean$Wind, aq.clean$Ozone, pch=16)


###################################################
### code chunk number 22: IntroR-ES.Rnw:523-526
###################################################
par(mfrow=c(1,2))
plot(aq.clean$Wind, aq.clean$Ozone, pch=16, xlab="Wind", ylab="Ozone")
plot(aq.clean$Wind, aq.clean$Solar.R, pch=16, xlab="Wind", ylab="Solar.R")


###################################################
### code chunk number 23: IntroR-ES.Rnw:535-536
###################################################
plot(aq.clean, pch=16)


###################################################
### code chunk number 24: IntroR-ES.Rnw:546-548
###################################################
plot(aq.clean, pch=16,
    col=aq.clean$Month)


###################################################
### code chunk number 25: IntroR-ES.Rnw:559-562
###################################################
aq.clean$Month <- factor(aq.clean$Month)
airquality.59 <- aq.clean[which(aq.clean$Month==5 | aq.clean$Month==9), ]
t.test(Temp ~ Month, data=airquality.59)


###################################################
### code chunk number 26: IntroR-ES.Rnw:573-575
###################################################
library(psych)
corr.test(aq.clean[, 1:4])


###################################################
### code chunk number 27: IntroR-ES.Rnw:586-591
###################################################
library(corrplot)
par(mfrow=c(1,2))
cor.aq <- cor(aq.clean[, 1:4])
corrplot(cor.aq, method = "circle")
corrplot(cor.aq, method = "number")


