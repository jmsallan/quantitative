#-------------Hypothesis testing for the mean-------------

#case 1: two samples from the same population N(10, 100)

set.seed(1)

#two samples from N(10, 100)

x1 <- rnorm(10, mean=10, sd=100)

x2 <- rnorm(1000, mean=10, sd=100)

#null hypothesis: mu = 0

t.test(x1, mu=0) #null ypothesis is accepted, when it is false (type II error)

t.test(x2, mu=0) #null hypothesis is rejected, and it is false (rigt decision)

#------------Two paired-data tests----------------------

set.seed(1)

init1 <- rnorm(100, mean=100, sd=100)
treatA1 <- init1 + rnorm(100, mean=0, sd=20)
treatB1 <- init1 - rnorm(100, mean=1, sd=20)

t.test(init1, treatA1, paired=T)
t.test(init1, treatB1, paired=T) #effect not detected (type II error)

init2 <- rnorm(6000, mean=100, sd=100)
treatA2 <- init2 + rnorm(6000, mean=0, sd=20)
treatB2 <- init2 - rnorm(6000, mean=1, sd=20)

t.test(init2, treatA2, paired=T)
t.test(init2, treatB2, paired=T) #effect detected

#plotting the three variables

boxplot(init2, treatA2, treatB2)

#----------Correlations of the sat.act dataset-------------

library(psych)

data(sat.act)

sat.act.scores <- data.frame(sat.act$ACT, sat.act$SATV, sat.act$SATQ)

pairs(sat.act.scores) #plotting correlations

cor.sat.act.scores <- cor(sat.act.scores) #storign correlations on a matrix

cortest.sat.act.scores <- corr.test(sat.act.scores) #storing info about correlations

cortest.sat.act.scores$r #correlation test

cortest.sat.act.scores$p #significance levels


