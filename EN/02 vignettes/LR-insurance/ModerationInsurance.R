## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
insurance <- read.csv("insurance.csv")

## ------------------------------------------------------------------------
plot(insurance[ , c(1,3,4,6,7)], pch=19, col=insurance$smoker)

## ------------------------------------------------------------------------
plot(insurance$bmi, insurance$charges, pch=19, col=insurance$smoker, xlab="bmi", ylab="charges")

## ------------------------------------------------------------------------
bmi.interaction <- lm(charges ~ bmi*smoker, data=insurance)
summary(bmi.interaction)

## ------------------------------------------------------------------------
smokers <- insurance[which(insurance$smoker=="yes"), ]
nonsmokers <- insurance[which(insurance$smoker=="no"), ]

bmi.smokers <- lm(charges ~ bmi, data=smokers)
bmi.nonsmokers <- lm(charges ~ bmi, data=nonsmokers)

summary(bmi.smokers)
summary(bmi.nonsmokers)

## ------------------------------------------------------------------------
plot(insurance$bmi, insurance$charges, col=insurance$smoker, xlab="bmi", ylab="charges")
abline(bmi.smokers, col="red", lwd=3)
abline(bmi.nonsmokers, col="black", lwd=3)

## ------------------------------------------------------------------------
model1 <- lm(charges ~ age + sex + bmi + smoker, data=insurance)
model2 <- lm(charges ~ age + sex  + bmi*smoker, data=insurance)
library(stargazer)
stargazer(model1, model2, type="text")

## ------------------------------------------------------------------------
anova(model1, model2)

