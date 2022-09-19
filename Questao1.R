library(ggplot2)
rm(list=ls())

#Dados da Argila
Argila <- c(18.2, 21.2, 23.1, 18.5, 15.6, 20.8, 19.4, 15.4, 10.2, 21.2, 13.4, 16.4, 18.7, 18.2, 19.6, 14.3, 16.6, 24.0, 17.6, 17.8, 22.3, 20.2, 17.4, 23.6, 17.5, 20.3, 16.6, 19.3, 18.5, 19.3, 21.2, 13.9, 20.5, 19.0, 17.6, 22.3, 18.4, 21.2, 20.4, 21.4, 20.3, 20.1, 10.2, 19.6, 20.6, 14.8, 19.7, 20.5, 18.0, 20.8, 15.8, 23.1, 10.2, 25.6, 28.6, 8.1)

#Gráfico ramo de folhas
stem(Argila)

#Media mediana
mean(Argila)
median(Argila)

#Quartis
Q1 <- quantile(Argila,0.25, type=2)
Q2 <- quantile(Argila,0.50, type=2)
Q3 <- quantile(Argila,0.75, type=2)

#Variância e Desvio Padrão
variancia <- var(Argila) 	# sum((unique(Argila))^2*ni)-mean(Argila)^2
desvio_padrao <- sd(Argila)	# sqrt(var(X))

#Frequências
argila_intervalos <- cut(Argila,breaks=seq(8, 30, by=2))
N <- length(argila_intervalos)
ni <- table(argila_intervalos); ni 		    # Absolute frequencies
fi <- ni/N ; fi			                      # Relative frequencies
Ni <- cumsum(ni); Ni	                    # Cumulative absolute frequencies
Fi <- cumsum(fi); Fi	                    # Cumulative relative frequencies

#Histograma
#pdf("")
histograma <- ggplot(data.frame(Argila=Argila), aes(x=Argila))+geom_histogram(fill="#cc7051",color = "black",breaks=seq(8, 30, by=2))+
  labs(title="Retração por secagem da argila",x="Porcentagem(%)", y = "Quantidade")+
  scale_x_continuous(breaks=seq(8, 30, by=2))

histograma
#dev.off()

#Grafico de caixa
#pdf("")
box_plot <- ggplot(data = data.frame(Argila), aes(x = "", y = Argila)) +
  stat_boxplot(geom = "errorbar",
               width = 0.5) +
  geom_boxplot(fill = "#4271AE",
               outlier.colour = "red",
               alpha = 0.9) +
  ggtitle("Retração por secagem da argila")+
  xlab("Porcentagem") +
  coord_flip()
box_plot
#dev.off()