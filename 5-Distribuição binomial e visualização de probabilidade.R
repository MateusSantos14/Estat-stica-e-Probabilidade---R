library(latex2exp)
rm(list = ls ()) 

x = seq(0,31,by=1)
prob = dbinom(x,31,0.6)
# Plot PMF
par(mar=c(6,5,4,2))
plot(x,prob,type='h',col='red',lwd=2,
     xlab=TeX('x'),
     ylab=TeX('P_X(x)'),
     cex.lab=1.3,
     cex.axis=1.2)
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


maior_11 <- 1-pbinom(11,31,0.6)
entre_16_19 <- pbinom(18,31,0.6)-pbinom(16,31,0.6)
exatamente_22 <- dbinom(22,31,0.6)