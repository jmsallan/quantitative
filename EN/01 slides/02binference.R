### R code from vignette source 'inference.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: inference.Rnw:68-71
###################################################
set.seed(1313)
a <-  runif(50, -5, 5)
hist(a, xlab="", ylab="", col="grey", xlim=c(-5,5), main="Change in blood pressure")


###################################################
### code chunk number 2: inference.Rnw:103-119
###################################################
par(mfrow=c(1,2))

x <- seq(-3, 3, length.out = 100)
norm <- dnorm(x)
plot(x, norm, type="l", yaxt="n", xlab="", ylab="")
title(main = "Normal distribution")

x2 <- seq(0, 10, length.out = 100)
chisq <- dchisq(x2, 3)
chisq4 <- dchisq(x2, 4)
chisq5 <- dchisq(x2, 5)
plot(x2, chisq, type="l", yaxt="n", xlab="", ylab="")
lines(x2, chisq5, col="blue")
lines(x2, chisq4, col="red")
legend("topright", c("df=3", "df=4", "df=5"), col = c("black", "red", "blue"), lty = c(1,1,1))
title(main = "Chi-squared distribution")


###################################################
### code chunk number 3: inference.Rnw:159-167
###################################################
x <- seq(-3, 3, length.out = 100)

plot(x, dnorm(x), type="l", yaxt="n", xlab="", ylab="")
lines(x, dt(x, 10), col="green")
lines(x, dt(x, 20), col="blue")
lines(x, dt(x, 4), col="red")
title(main = "Student's t-distribution")
legend("topright", c("n=4", "n=10", "n=20", "norm"), col = c("red", "green", "blue", "black"), lty = c(1,1,1,1),cex = 0.7)


###################################################
### code chunk number 4: inference.Rnw:243-261
###################################################
x <- seq(-3, 3, length.out = 100)
norm <- dnorm(x)

xlow <- x[which(x<=qnorm(0.025))]
normlow <- norm[which(x<=qnorm(0.025))]
xpolylow <- c(xlow, rev(xlow))
ypolylow <- c(rep(0, length(xlow)), rev(normlow))

xhigh <- x[which(x>=qnorm(0.975))]
normhigh <- norm[which(x>=qnorm(0.975))]
xpolyhigh <- c(xhigh, rev(xhigh))
ypolyhigh <- c(rep(0, length(xhigh)), rev(normhigh))


plot(x, norm, type="l", yaxt="n", xlab="", ylab="")
title(main = "Two-tails with alpha=0.05")
polygon(xpolylow, ypolylow, col="blue")
polygon(xpolyhigh, ypolyhigh, col="blue")


###################################################
### code chunk number 5: inference.Rnw:274-275
###################################################
t.test(a)


###################################################
### code chunk number 6: inference.Rnw:286-289
###################################################
set.seed(222)
b <-  runif(50, -7, 4)
hist(b, xlab="", ylab="", col="grey", xlim=c(-5,5), main="Change in blood pressure")


###################################################
### code chunk number 7: inference.Rnw:300-301
###################################################
t.test(b)


###################################################
### code chunk number 8: inference.Rnw:345-348
###################################################
set.seed(111)
ps.mean0 <- sapply(1:1000, function(x) t.test(runif(50, -4, 4))$p.value)
hist(ps.mean0, breaks=seq(0, 1, by=0.05), xlab="", ylab="", col="grey", xlim=c(0,1), main="mean=0, n=50")


###################################################
### code chunk number 9: inference.Rnw:365-367
###################################################
ps.mean1.50 <- sapply(1:1000, function(x) t.test(runif(50, -6, 4))$p.value)
hist(ps.mean1.50, xlab="", breaks=seq(0, 1, by=0.05), ylab="", col="grey", xlim=c(0,1), main="mean -1, n=50")


###################################################
### code chunk number 10: inference.Rnw:379-384
###################################################
ps.mean1.100 <- sapply(1:1000, function(x) t.test(runif(100, -6, 4))$p.value)
ps.mean1.1000 <- sapply(1:1000, function(x) t.test(runif(1000, -6, 4))$p.value)
par(mfrow=c(1,2))
hist(ps.mean1.100, xlab="",breaks=seq(0, 1, by=0.05), ylab="", col="grey", xlim=c(0,1), main="mean -1, n=100")
hist(ps.mean1.1000, xlab="", breaks=seq(0, 1, by=0.05), ylab="", col="grey", xlim=c(0,1), main="mean -1, n=1000")


