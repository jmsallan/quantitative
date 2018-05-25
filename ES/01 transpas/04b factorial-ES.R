### R code from vignette source 'main.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: scores
###################################################
scores.cor <- matrix(c(
1.0,  0.439,	0.410,	0.288,	0.329,	0.248,
0.439,	1.0,	0.351,	0.354,	0.320,	0.329,
0.410,	0.351,	1.0,	0.164,	0.190,	0.181,
0.288,	0.354,	0.164,	1.0,	0.595,	0.470,
0.329,	0.320,	0.190,	0.595,	1.0,	0.464,
0.248,	0.329,	0.181,	0.470,	0.464,	1.0),
6,	6,	byrow="TRUE")

colnames(scores.cor) <- rownames(scores.cor) <- c("Gaélico", "Inglés", "Historia", "Aritmética", "Álgebra", "Geometría")
scores.cor


###################################################
### code chunk number 2: function01
###################################################
library(psych)
pr.scores <- principal(r=scores.cor, nfactors=2, 
                              rotate="none")


###################################################
### code chunk number 3: function02
###################################################
library(psych)
pa.scores <- fa(r=scores.cor, nfactors=2, fm="pa",
                   rotate="none")
ml.scores <- fa(r=scores.cor, nfactors=2, fm="ml", 
                rotate="none")


###################################################
### code chunk number 4: loadings01
###################################################
pr.scores$loadings


###################################################
### code chunk number 5: function03
###################################################
pr.scores.varimax <- principal(r=scores.cor, nfactors=2,
                               rotate="varimax")
pa.scores.oblimin <- fa(r=scores.cor, nfactors=2, 
                           fm="pa")
ml.scores.quartimax <- fa(r=scores.cor, nfactors=2, fm="ml", 
                rotate="quartimax")


###################################################
### code chunk number 6: loadings02
###################################################
pr.scores.varimax$loadings


###################################################
### code chunk number 7: loadings03
###################################################
print(pr.scores.varimax$loadings, cutoff=0.4)


###################################################
### code chunk number 8: data
###################################################
data <- read.csv("datascale.csv")
vars <- names(data)
focus <- data[ 
  ,which(grepl("pre", vars) | grepl("pro", vars))]
cor.focus <- cor(focus)


###################################################
### code chunk number 9: corrplot
###################################################
library(corrplot)
corrplot(cor.focus, method="circle")


###################################################
### code chunk number 10: prelim01
###################################################
library(psych)
KMO(cor.focus)
cortest.bartlett(cor.focus, n=150)


###################################################
### code chunk number 11: prelim02
###################################################
fa.parallel(cor.focus, n.obs=150)


###################################################
### code chunk number 12: factor01
###################################################
pa.data.oblimin <- fa(r=cor.focus, nfactors=2, fm="pa") 


###################################################
### code chunk number 13: factor02
###################################################
pa.data.oblimin


###################################################
### code chunk number 14: factor03
###################################################
print(pa.data.oblimin$loadings, cutoff=0.4)


###################################################
### code chunk number 15: scores01
###################################################
pa.data.oblimin.scores <- 
  factor.scores(focus, pa.data.oblimin, method="Thurstone")
scores <- 
  as.data.frame(pa.data.oblimin.scores$scores)


###################################################
### code chunk number 16: scores02
###################################################
colors <- rep("blue", 150)
colors[which(data$sexo==1)] <- "red"
plot(scores$PA1, scores$PA2, pch=19, col=colors, xlab="PA1 (prevention)", ylab="PA2 (promotion)", 
     cex.lab=0.7, cex.axis=0.7)
abline(lm(scores$PA2 ~ scores$PA1))


