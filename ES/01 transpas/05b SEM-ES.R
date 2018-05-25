### R code from vignette source 'mainSEM-es.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: holz01
###################################################
holz01 <- 'v =~ x1 + x2 + x3
t =~ x4 + x5 + x6
s =~ x7 + x8 + x9'


###################################################
### code chunk number 2: holz02
###################################################
holz <- 'v =~ x1 + x2 + x3
t =~ x4 + x5 + x6
s =~ x7 + x8 + x9

v ~~ 0*t
v ~~ 0*s
t ~~ 0*s'


###################################################
### code chunk number 3: holz03
###################################################
library(lavaan)
holz01.fit <- cfa(holz01, data = HolzingerSwineford1939, 
likelihood="wishart")


###################################################
### code chunk number 4: holz04
###################################################
fitMeasures(holz01.fit)


###################################################
### code chunk number 5: holz05
###################################################
head(parameterEstimates(holz01.fit))


###################################################
### code chunk number 6: wheaton
###################################################
wheaton.model <-'
ali67 =~ ano67 +  pow67
ali71 =~ ano71 +  pow71
ses =~ edu + sei
ali71 ~ ali67 + ses
ali67 ~ ses
ano67 ~~ ano71
pow67 ~~ pow71'


###################################################
### code chunk number 7: wheaton02
###################################################
wheaton.cov <- matrix(
  c(11.834,        0,      0,        0,      0,       0, 
6.947,     9.364,      0,        0,      0,       0, 
6.819,     5.091,  12.532,       0,      0,       0, 
4.783,     5.028,   7.495,   9.986,      0,       0, 
-3.839,   -3.889,  -3.841,  -3.625,  9.610,       0, 
-21.899, -18.831, -21.748, -18.775, 35.522, 450.288), 
6,  6, byrow=TRUE)
colnames(wheaton.cov) <- rownames(wheaton.cov) <- 
  c("ano67", "pow67", "ano71", "pow71", "edu", "sei")
fit.wheaton <- 
  sem(wheaton.model, sample.cov=wheaton.cov, sample.nobs=932)


###################################################
### code chunk number 8: wheaton03
###################################################
fitMeasures(fit.wheaton)


###################################################
### code chunk number 9: wheaton04
###################################################
head(parameterEstimates(fit.wheaton))


###################################################
### code chunk number 10: holz05
###################################################
holz01.mi <- modindices(holz01.fit)
tail(holz01.mi)


