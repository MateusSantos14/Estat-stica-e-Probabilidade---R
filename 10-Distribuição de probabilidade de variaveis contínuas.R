library(latex2exp)

# Plot the graph.
graphics.off()
curve(dnorm(x, mean=4,sd=0.22), 0, 8, col="black",lwd=2,main = "Função densidade de probabilidade",xlab=TeX('x'),ylab=TeX('F(x)'))


# Plot the graph.
graphics.off()
curve(pnorm(x, mean=4,sd=0.22), 0, 8, col="red",lwd=2,main = "Função distribuição acumulada",xlab=TeX('x'),ylab=TeX('F(x)'))

#1.2

#P(X>5)
1-pnorm(4.54)

#P(X<2)
pnorm(-9.09)

#P(3> X >2.5)
pnorm(-4.54) - pnorm(-6.82)