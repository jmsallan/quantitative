### R code from vignette source 'main_source.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: figlog
###################################################
vals <- seq(-10, 10, length.out = 100)
logit <- exp(vals)/(1+exp(vals))
plot(vals, logit, type="l", xlab="x", ylab = "exp(x)/(1+exp(x)")


###################################################
### code chunk number 2: ravens
###################################################
load("ravensData.rda")
logit.ravens <- glm(ravenWinNum ~ ravenScore, 
                    data=ravensData, family = "binomial")


###################################################
### code chunk number 3: ravens.summlogit
###################################################
summary(logit.ravens)


###################################################
### code chunk number 4: ravens.sumlogit.coef
###################################################
odds <- exp(coef(logit.ravens))
odds


###################################################
### code chunk number 5: probitfit1 (eval = FALSE)
###################################################
## plot(ravensData$ravenScore, logit.ravens$fitted,
##      pch=16, xlab = "Score", ylab="prob. win")


###################################################
### code chunk number 6: probitfit2
###################################################
plot(ravensData$ravenScore, logit.ravens$fitted, pch=16,
     xlab = "Score", ylab="prob. win")


###################################################
### code chunk number 7: anova.logit
###################################################
anova(logit.ravens, test = "Chisq")


###################################################
### code chunk number 8: pseudo.R2logit
###################################################
logit.ravens0 <- update(logit.ravens, . ~ 1)
pseudoR2 <- 1 - 
  as.vector(logLik(logit.ravens)/logLik(logit.ravens0))
pseudoR2


###################################################
### code chunk number 9: logitprobit
###################################################
vals <- seq(-10, 10, length.out = 100)
logit <- exp(vals)/(1+exp(vals))
probit <- pnorm(vals)
plot(vals, logit, type="n", xlab="x", ylab = "p")
lines(vals, logit, col="blue")
lines(vals, probit, col="red")
legend("topleft", c("logit", "probit"), lty=c(1,1), col=c("blue", "red"), cex=0.8)


###################################################
### code chunk number 10: probitravens
###################################################
probit.ravens <- glm(ravenWinNum ~ ravenScore, 
                    data=ravensData, 
                    family = binomial("probit"))


###################################################
### code chunk number 11: ravens.summprobit
###################################################
summary(probit.ravens)


###################################################
### code chunk number 12: rdPoisson
###################################################
library(AER)
data("RecreationDemand")
rd.poisson <- glm(trips ~ ., data=RecreationDemand,
                  family = poisson)


###################################################
### code chunk number 13: summrdPoisson
###################################################
coeftest(rd.poisson)


###################################################
### code chunk number 14: main_source.Rnw:421-422
###################################################
var(RecreationDemand$trips)/mean(RecreationDemand$trips)


###################################################
### code chunk number 15: rdbn
###################################################
library(MASS)
rd.nb <- glm.nb(trips ~ ., data=RecreationDemand,
                init.theta = 0.9)


###################################################
### code chunk number 16: main_source.Rnw:465-466
###################################################
coeftest(rd.nb)


