#Performing a multivariate regression analysis of the \verb|attitude| dataset

linear.1 <- lm(rating ~ complaints + privileges + learning + raises + critical, data=attitude)

summary(linear.1)

plot(linear.1)


linear.2 <- lm(rating ~ complaints, data=attitude)

summary(linear.2)

#Assesing the normality of residuals for the linear model defined in example~\ref{ex:attitude}

jpeg("resnorm.jpg")
par(mfrow=c(2,1), mar=c(2,2,2,2))
plot(linear.1, which=2)
hist(resid(linear.1), main=NULL)
dev.off()

pdf("resnorm2.pdf")
par(mfrow=c(2,2), mar=c(2,2,2,2))
plot(linear.2)
dev.off()

#Assessing homoscedasticity for the linear model defined in example~\ref{ex:attitude}

jpeg("attitude_resfit.jpg")
	plot(linear.1, which=1)
dev.off()

library(lmtest)

bptest(linear.1)


#Building and testing an heteroscedastic and an homoscedastic model

## generate a regressor
    x <- rep(c(-1,1,2), 50)
    
## generate heteroskedastic (err1) 
##and homoskedastic (err2) disturbances
set.seed(1)
err1 <- rnorm(150, sd=rep(c(1,2,4), 50))
err2 <- rnorm(150)

## generate a linear relationship
y1 <- 1 + x + err1
y2 <- 1 + x + err2

mod.y1 <- lm(y1 ~ x)

library(lmtest)
bptest(mod.y1)

mod.y2 <- lm(y2 ~ x)

library(lmtest)
bptest(mod.y2)

#Assessing autocorrelation issues in the \verb|bondyield| dataset

modelCH <- lm(RAARUS ~ MOOD + EPI + EXP + RUS, data=bondyield)

library(lmtest)

dwtest(modelCH)

bgtest(modelCH)

#Assessing multicollinearity issues in a regression model

mod04 <- lm(Employed ~ GNP.deflator + GNP + Unemployed + Armed.Forces + Population + Year, data=longley, x= TRUE)

summary(mod04)

cor(longley)

library(HH)

vif(mod04, y.name = "Employed")

mod04c <- lm(Employed ~ GNP, data=longley, x= TRUE)

summary(mod04b)

mod04c <- lm(Armed.Forces ~ Unemployed + Population, data=longley, x= TRUE)

summary(mod04c)

#Categorical variables in the \verb|hsb2| dataset

hsb2 <- read.csv("http://www.ats.ucla.edu/stat/data/hsb2.csv")

hsb2$race.f <- factor(hsb2$race)

levels(hsb2$race.f) <- c("hispanic", "asian", "african-amer", "white")

hsb2$female.f <- factor(hsb2$female)

levels(hsb2$female.f) <- c("male", "female")

summary(lm(write ~ race.f, data = hsb2))

summary(lm(write ~ female.f, data = hsb2))

#A hierarchical regression model

hierarchical <- read.csv(file="hierarchical.csv", header=T)

mod01 <- lm(OCB ~ ID + AGE +factor(GEN) + EXP + ANT, data=hierarchical)

mod02 <- lm(OCB ~ ID + AGE +factor(GEN) + EXP + ANT + FRCC + RBSE, data=hierarchical)

anova(mod01, mod02)

#A test of mediation

xyz <- read.csv("medxyz.csv", header=T)

mod01 <- lm(z ~ x, data = xyz)
mod02 <- lm(y ~ x, data = xyz)
mod03 <- lm(y ~ x + z, data = xyz)

summary(mod01)
summary(mod02)
summary(mod03)

#Testing the interaction effect to detect moderation in the \verb|cars| dataset

cars <- read.csv(file="cars.csv", header=T)

cars$weightc <- scale(cars$weight, center=TRUE, scale=FALSE)
cars$accelc <- scale(cars$accel, center=TRUE, scale=FALSE)

mod01c <- lm(mpg ~ weightc + accelc, data=cars)
summary(mod01c)

mod02c <- lm(mpg ~ weightc*accelc, data=cars)
summary(mod02c)

anova(mod02c,mod01c)

library(pequod)

mod.centered <- lmres(mpg~weight*accel, 
centered = c("weight", "accel"), data=cars)

summary.lmres(mod.centered)

mod.simpleslope <- simpleSlope(mod.centered, pred="weight", mod1="accel", coded="none")

summary.simpleSlope(mod.simpleslope)

jpeg("simpleslope.jpg")
PlotSlope(mod.simpleslope)
dev.off()



