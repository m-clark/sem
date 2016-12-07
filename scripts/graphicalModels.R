

library(bnlearn)
# example(bnlearn)
str(gaussian.test)

library(lavaan)

mod = '
F ~ G + A + D + E
D ~ B
C ~ A + B
'

modlavaan = sem(mod, gaussian.test, meanstructure = T)
summary(modlavaan)

library(semPlot)
semPaths(modlavaan)

data(learning.test)

# learn the network structure.
# res = gs(learning.test)
# # set the direction of the only undirected arc, A - B.
# res = set.arc(res, "A", "B")
# # estimate the parameters of the Bayesian network.
# fitted = bn.fit(res, learning.test)
# fitted$D

res = hc(gaussian.test)
# estimate the parameters of the Bayesian network.
fitted = bn.fit(res, gaussian.test)
# replace the parameters of the node F.
sapply(fitted, `[`, 'coefficients')
coef(modlavaan)

lmC = lm(C ~ A + B, data=gaussian.test)
lmD = lm(D ~ B, data=gaussian.test)
fitD = fitted(lmD)
lmF = lm(F ~ G + A + fitD + E, data=gaussian.test)
lmCoefs = sapply(list(C = lmC, D=lmD, F=lmF), coef)
coef(modlavaan)
lmCoefs


round(cor(gaussian.test),2)


## simple mediation model
mod = '
F ~ C + A
C ~ A
'

modlavaan = sem(mod, gaussian.test, meanstructure = T)
summary(modlavaan)

library(semPlot)
semPaths(modlavaan)
lmC = lm(C~A, gaussian.test)
lmF = lm(F~C + A, gaussian.test)
sapply(list(C = lmC, F=lmF), coef)
coef(modlavaan)
