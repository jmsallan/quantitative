library(psych)

#example 1: Principal components of a matrix of student scores

scores.cor <- matrix(c(
1.0,	0.439,	0.410,	0.288,	0.329,	0.248,
0.439,	1.0,	0.351,	0.354,	0.320,	0.329,
0.410,	0.351,	1.0,	0.164,	0.190,	0.181,
0.288,	0.354,	0.164,	1.0,	0.595,	0.470,
0.329,	0.320,	0.190,	0.595,	1.0,	0.464,
0.248,	0.329,	0.181,	0.470,	0.464,	1.0),
6,	6,	byrow="TRUE")

colnames(scores.cor) <- rownames(scores.cor) <- c("Gaelic", "English", "History", "Arithmetic", "Algebra", "Geometry")

eigen(scores.cor)$values

principal.scores <- principal(r=scores.cor, nfactors=2, rotate="none")

principal.scores.varimax <- principal(r=scores.cor, nfactors=2)
print(principal.scores.varimax, cut=0.4)

#example 2: Factor analysis of a matrix of student scores

paxis.scores <- fa(r=scores.cor, nfactors=2, fm="pa", rotate="none")
paxis.scores.varimax <- fa(r=scores.cor, nfactors=2, fm="pa")
print(paxis.scores.varimax, cut=0.4)

ml.scores <- fa(r=scores.cor, nfactors=2, fm="ml", rotate="none")
ml.scores.varimax <- fa(r=scores.cor, nfactors=2, fm="ml")
print(ml.scores.varimax, cut=0.4)

#example 3: The rotated factor loadings of a matrix of student scores

ml.scores.oblimin <- fa(r=scores.cor, nfactors=2, fm="ml", rotate="oblimin")
print(ml.scores.oblimin, cut=0.4)

#analysis of the bfi dataset

bfi.items <- subset(bfi, select=-c(gender, education, age))

cor.bfi.items.complete <- cor(bfi.items, use="complete.obs")

cor.bfi.items <- cor.bfi.items.complete

KMO(cor.bfi.items)

cortest.bartlett(cor.bfi.items, n = 2800)

jpeg("parallelbfi.jpg")
fa.parallel(cor.bfi.items, n.obs=2800)
dev.off()

principal.bfi <-  principal(r=cor.bfi.items, nfactors=6)
print(principal.bfi.oblimin, cut=0.4)

pa.bfi.oblimin <- fa(r=cor.bfi.items, nfactors=5, fm="pa", rotate="oblimin")
print(pa.bfi.oblimin, cut=0.4)

#example 4: confirmatory factor analysis

library(lavaan) #library for structural equation modelling

mod.cfa <- 'math =~ NA*Arithmetic + Algebra + Geometry
lang =~ NA*Gaelic + English + History
math ~~ 1*math
lang ~~ 1*lang'

fit.mod.cfa <- cfa(mod.cfa, sample.cov=scores.cor, sample.nobs=220)

fitMeasures(fit.mod.cfa)

parameterEstimates(fit.mod.cfa)
