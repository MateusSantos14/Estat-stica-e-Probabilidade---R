prob = function (k) {
    termo1 <- dbinom(x=k, size = 20,prob = 0.6)*0.91
    termo2 <- dbinom(x=k, size = 20,prob = 0.5)*0.09
    resultado = termo1+termo2
    return(resultado)
}

x = seq(0,20,by=1)

probabilidades <- lapply(x,prob)

# Plot PMF
par(mar=c(6,5,4,2))
plot(x,probabilidades,type='h',col='red',lwd=2,
     xlab=TeX('x'),
     ylab=TeX('P_X(x)'),
     cex.lab=1.3,
     cex.axis=1.2)
points(x,probabilidades, pch=19, col='red')
abline(h=0)
