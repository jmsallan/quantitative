### R code from vignette source 'main.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: mtcars01
###################################################
head(mtcars)


###################################################
### code chunk number 2: mtcars02
###################################################
mtcars01 <- lm(mpg ~ hp + wt + qsec, data=mtcars)
summary(mtcars01)


###################################################
### code chunk number 3: mtcars03
###################################################
mtcars01 <- lm(mpg ~ hp + wt + qsec, data=mtcars)
summary(mtcars01)


###################################################
### code chunk number 4: mtcars03
###################################################
par(mfrow=c(1,2), pty="s")
plot(mtcars01, which=1)
plot(mtcars01, which=2)


###################################################
### code chunk number 5: sked01
###################################################
x <- rep(c(-1,0,1), 50)
set.seed(2323)
err1 <- rnorm(150, sd=rep(c(1,2,4), 50))
err2 <- rnorm(150)

y1 <- 1 + x + err1
y2 <- 1 + x + err2

mod.y1 <- lm(y1 ~ x)

mod.y2 <- lm(y2 ~ x)

par(mfrow=c(1,2))
par(pty="s")
plot(x, y1, pch=19, col="orange", xlab="x", ylab="y1", cex.lab=0.7, cex.axis=0.7, main=c("heterocedástico"))
abline(mod.y1)
par(pty="s")
plot(x, y2, pch=19, col="orange", xlab="x", ylab="y2", cex.lab=0.7, cex.axis=0.7, main=c("homocedástico"))
abline(mod.y2)


###################################################
### code chunk number 6: sked02
###################################################
coef(summary(mod.y1))
coef(summary(mod.y2))


###################################################
### code chunk number 7: points
###################################################
set.seed(1414)
n <- 100
x <- rnorm(n)
y <- x + rnorm(n, sd=0.3)
plot(c(-3, 6), c(-3, 6), frame=FALSE, type="n", xlab="x", ylab="y", cex.lab=0.7, cex.axis=0.7)
abline(lm(y ~ x), lwd = 2)
points(x, y, pch=19, col="lightblue")
points(0, 0, pch=19, col="orange")
text(0, 0, "1", pos=3, cex= 0.7)
points(0, 5, pch=19, col="orange")
text(0, 5, "2", pos=3, cex= 0.7)
points(5, 5, pch=19, col="orange")
text(5, 5, "3", pos=3, cex= 0.7)
points(5, 0, pch=19, col="orange")
text(5, 0, "4", pos=3, cex= 0.7)


###################################################
### code chunk number 8: influence
###################################################
plot(mtcars01, which=4)


###################################################
### code chunk number 9: dfbetas
###################################################
library(car)
dfbetaPlots(mtcars01)


###################################################
### code chunk number 10: longley01
###################################################
library(corrplot)
corrplot(cor(longley), method="circle")


###################################################
### code chunk number 11: longley02
###################################################
longley01 <- lm(Employed ~ ., data=longley)
library(car)
vif(longley01)
longley02 <- lm(Employed ~ GNP + Armed.Forces, data=longley)
vif(longley02)


###################################################
### code chunk number 12: longley03
###################################################
coef(summary(longley01))
coef(summary(longley02))


###################################################
### code chunk number 13: mtcars2
###################################################
levels(as.factor(mtcars$gear))
mtcars02 <- lm(mpg ~ wt + hp + factor(gear), data=mtcars)
coef(summary(mtcars02))


###################################################
### code chunk number 14: OCB01
###################################################
hierarchical <- read.csv(file="hierarchical.csv")
vars <- data.frame(hierarchical$OCB, hierarchical$AGE, hierarchical$GEN, hierarchical$FRCC, hierarchical$RBSE)
names(vars) <- c("OCB", "AGE", "GEN", "FRCC", "RBSE")
library(corrplot)
corrplot(cor(vars), method="circle")


###################################################
### code chunk number 15: OCB02
###################################################
hierarchical <- read.csv(file="hierarchical.csv")
hr01 <- lm(OCB ~ AGE +factor(GEN), data=hierarchical)
hr02 <- lm(OCB ~ AGE +factor(GEN) + FRCC + RBSE, data=hierarchical)
library(stargazer)
stargazer(hr01, hr02, type="text")


###################################################
### code chunk number 16: mediation
###################################################
set.seed(3333)

pred <- rnorm(100, 2, 1)
med <- 3 + 2*pred + rnorm(100, sd=0.3)
cri <- 2 + med + rnorm(100, sd=0.2)

bk01 <- lm(med ~ pred)
bk02 <- lm(cri ~ pred)
bk03 <- lm(cri ~ pred + med)


###################################################
### code chunk number 17: mediation02
###################################################
coef(summary(bk01))
coef(summary(bk02))
coef(summary(bk03))


###################################################
### code chunk number 18: modeation01
###################################################
summary(lm(mpg ~ am*wt, mtcars))


###################################################
### code chunk number 19: moderation02
###################################################
mtcars0 <- mtcars[which(mtcars$am==0), ]
mtcars1 <- mtcars[which(mtcars$am==1), ]

col <- rep("red", nrow(mtcars))
col[which(mtcars$am==1)] <- "blue"

plot(mtcars$wt, mtcars$mpg, pch=21,  col="black", bg=col, cex.lab=0.9, xlab="weight", ylab="mpg", mgp=c(2, 1, 0))

abline(lm(mpg ~ wt, mtcars0), col="red")
abline(lm(mpg ~ wt, mtcars1), col="blue")
legend("topright", legend=c("Automatic", "Manual"), col=c("red", "blue"), fill=c("red", "blue"), cex=0.6)


