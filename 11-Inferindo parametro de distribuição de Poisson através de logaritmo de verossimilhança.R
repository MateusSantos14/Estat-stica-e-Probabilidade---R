rm(list=ls())
graphics.off()

valores = c(25,18,12,16,26,7,13,19,19,22,17,18)


log_poisson <- function(lam,vec = valores) { 
  n <- length(valores)
  likehood <- -n*lam + sum(vec)*log(lam,base=exp(1)) + sum(log(factorial(vec),base=exp(1)))
  return(likehood)
  }

logP = optimise(log_poisson,c(0,50),maximum=T,vec=valores)
lambda_opt=logP$maximum; 
lambda_opt
curve(log_poisson(x,vec=valores),0,50, ylab = "Log-Verossimilhança") 
abline(v=lambda_opt,lty=2)



poisson <- function(lam,vec = valores) { 
  pmf <- dpois(valores,lam)
  likehood <- prod(pmf)
  return(likehood)
}


P = optimise(poisson,c(0,50),maximum=T,vec=valores)
poisson_opt=P$maximum; 
poisson_opt
h <- Vectorize(poisson)
curve(h,0,50) 
abline(v=poisson_opt,lty=2)