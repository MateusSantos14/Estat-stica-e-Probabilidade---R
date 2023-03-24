rm(list=ls())
graphics.off()

library(ggplot2)
library(ggfortify)

body_mass <- c(4700,5700,5800,4700,5550,4750,5000,5100,5200,4700,5800,4600,6000,4750,5950,4625,5450,4725,5350,4750)
flipper_length <- c(219,230,229,220,223,216,221,221,217,216,230,209,220,215,223,212,221,212,224,212)

dados = data.frame(body_mass,flipper_length)

#3.1
ggplot(dados, aes(x=body_mass, y=flipper_length)) + 
  geom_point()+ 
  labs(title = "Body Mass em função Flipper Length", x = "Body Mass", y = "Flipper Length")


beta1 <- function(X,Y) { 
  covariancia <- sum(((X-mean(X))*(Y-mean(Y))))/length(X)
  variancia <- sum((X-mean(X))^2)/length(X)
  return(covariancia/variancia)
}

b1 <- cov(dados$body_mass,dados$flipper_length)/var(dados$body_mass)
b0 <- mean(dados$flipper_length) - b1*mean(dados$body_mass)


#3.2
ggplot(dados, aes(x=body_mass, y=flipper_length)) + 
  geom_point()+
  stat_smooth(formula = y ~ x,method = "lm",col = "#C42126",se = FALSE,size = 1)+ 
  labs(title = "Body Mass em função Flipper Length com reta de regressão", x = "Body Mass", y = "Flipper Length")


model <-lm(flipper_length~body_mass, data=dados)
autoplot(model)


#y = a+bx => y = Body Mass / x = Flipper Lentgh
a = model$coefficients[1]
b = model$coefficients[2]

#3.4

ggplot(model, aes(.fitted, .resid)) + theme_bw(base_size=24)+
  geom_point(colour = "blue", size = 3)+
  geom_hline(yintercept = 0) + 
  labs(title = "Residuos de regressão", x = "Flipper Length", y = "Resíduo")


summary(model)
sum(residuals(model))
