### R code from vignette source 'IntroR-EN.Rnw'
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
### code chunk number 4: IntroR-EN.Rnw:163-165
###################################################
d <- numeric(5)
d


###################################################
### code chunk number 5: IntroR-EN.Rnw:170-172
###################################################
e <- c(4,-1,2,3)
e


###################################################
### code chunk number 6: IntroR-EN.Rnw:180-182
###################################################
f <- c("ab", "l", "fz", "a")
f


###################################################
### code chunk number 7: IntroR-EN.Rnw:187-189
###################################################
g <- c(TRUE, FALSE, TRUE)
g


###################################################
### code chunk number 8: IntroR-EN.Rnw:204-209
###################################################
s <- c(2, 3, 4, 6, 9, 1, 3)
s[1]
s[c(2,5)]
which(s>5)
s[which(s>5)]


###################################################
### code chunk number 9: IntroR-EN.Rnw:225-227
###################################################
A <- matrix(1:16, 4, 4)
A


###################################################
### code chunk number 10: IntroR-EN.Rnw:233-236
###################################################
B <- matrix(1:16, 4, 4, 
            byrow = TRUE)
B


###################################################
### code chunk number 11: IntroR-EN.Rnw:255-256
###################################################
A


###################################################
### code chunk number 12: IntroR-EN.Rnw:262-265
###################################################
A[2, 3]
A[ ,3]
A[2, ]


###################################################
### code chunk number 13: IntroR-EN.Rnw:278-281
###################################################
list <- list(albert = 54, bryan = A, carlos = c(1,2,3))
list[[1]]
list$carlos


###################################################
### code chunk number 14: IntroR-EN.Rnw:305-307
###################################################
head(mtcars) #first df rows
tail(mtcars) #last df rows


###################################################
### code chunk number 15: IntroR-EN.Rnw:317-321
###################################################
length(mtcars) # of variables
nrow(mtcars)   # of observations
mtcars[3, ]
mtcars$mpg[1:10]


###################################################
### code chunk number 16: IntroR-EN.Rnw:445-447
###################################################
head(airquality)
tail(airquality)


###################################################
### code chunk number 17: IntroR-EN.Rnw:457-459
###################################################
str(airquality)
summary(airquality)


###################################################
### code chunk number 18: IntroR-EN.Rnw:470-475
###################################################
aq.clean <- airquality[which(!is.na(airquality$Ozone) 
                                     & !is.na(airquality$Solar.R)), ]
nrow(airquality)
nrow(aq.clean)
summary(aq.clean)


###################################################
### code chunk number 19: IntroR-EN.Rnw:485-488
###################################################
par(mfrow=c(1,2))
hist(aq.clean$Ozone, col="red", main="Ozone")
hist(aq.clean$Solar.R, col="blue", main="Solar rad.")


###################################################
### code chunk number 20: IntroR-EN.Rnw:498-503
###################################################
d.ozone <- density(aq.clean$Ozone)
d.solar <- density(aq.clean$Solar.R)
par(mfrow=c(1,2))
plot(d.ozone, main="Ozone")
plot(d.solar, main="Solar rad.")


###################################################
### code chunk number 21: IntroR-EN.Rnw:513-514
###################################################
boxplot(Ozone ~ Month, data=aq.clean, pch=16, main="Ozone")


###################################################
### code chunk number 22: IntroR-EN.Rnw:525-526
###################################################
plot(aq.clean$Wind, aq.clean$Ozone, pch=16)


###################################################
### code chunk number 23: IntroR-EN.Rnw:536-539
###################################################
par(mfrow=c(1,2))
plot(aq.clean$Wind, aq.clean$Ozone, pch=16, xlab="Wind", ylab="Ozone")
plot(aq.clean$Wind, aq.clean$Solar.R, pch=16, xlab="Wind", ylab="Solar.R")


###################################################
### code chunk number 24: IntroR-EN.Rnw:547-548
###################################################
plot(aq.clean, pch=16)


###################################################
### code chunk number 25: IntroR-EN.Rnw:557-558
###################################################
plot(aq.clean, pch=16, col=aq.clean$Month)


###################################################
### code chunk number 26: IntroR-EN.Rnw:568-571
###################################################
aq.clean$Month <- factor(aq.clean$Month)
airquality.59 <- aq.clean[which(aq.clean$Month==5 | aq.clean$Month==9), ]
t.test(Temp ~ Month, data=airquality.59)


###################################################
### code chunk number 27: IntroR-EN.Rnw:581-583
###################################################
library(psych)
corr.test(aq.clean[, 1:4])


###################################################
### code chunk number 28: IntroR-EN.Rnw:593-598
###################################################
library(corrplot)
par(mfrow=c(1,2))
cor.aq <- cor(aq.clean[, 1:4])
corrplot(cor.aq, method = "circle")
corrplot(cor.aq, method = "number")


