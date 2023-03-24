library(latex2exp)
rm(list = ls ())

#3
x = seq(0,15,1)
prob = dpois(x,5)
# Plot PMF
par(mar=c(6,5,4,2))
plot(x,prob,type='h',col='red',lwd=2,
     xlab=TeX('k'),
     ylab=TeX('P_x(k)'),
     cex.lab=1.3, cex.axis=1.2)
points(x,prob, pch=19, col='red')
abline(h=0)

#Plot CDF
cdf = c(0,cumsum(prob))
cdf.plot = stepfun(x,cdf,f=0)
par(mar=c(6,5,4,2))
plot.stepfun(cdf.plot,col='blue',
             xlab=TeX('x'),
             ylab=TeX('F_X(x)'),
             verticals=FALSE,main='',
             do.points=TRUE,
             pch=19, lwd=2,
             cex.lab=1.3,
             cex.axis=1.2)
abline(h=0)