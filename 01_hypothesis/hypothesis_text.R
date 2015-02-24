#Confidence interval and hypothesis testing for the mean in $R$

x <- c(4,4,8,0,6,5,2,6,3,6,5,4,0,9,9,1,9,6,1,1)

t.test(x,mu=6)

t.test(x,mu=4)

#Computing the correlation and testing the null hypothesis with $R$

install.packages("psych")

library(psych)

data(sat.act)

cor(sat.act)

pdf("scatter.pdf")
pairs(~age+ACT+SATV+SATQ,data=sat.act, main="Simple Scatterplot Matrix")
dev.off()

cor.test(sat.act$ACT, sat.act$SATV)

matrix <- corr.test(sat.act)

matrix$r

matrix$p


