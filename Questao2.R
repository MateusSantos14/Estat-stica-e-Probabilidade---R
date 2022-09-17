library(ggplot2)
rm(list=ls())

idade <- c(41,35,30,51,29,52,31,41,26,23,39,35,29,43,34,37,52,35,29,31)
nacionalidade <- c('Belga','Alemanha','Belga','Inglesa','Inglesa','Alemanha','Belga','Francesa','Espanhola','Alemanha','Alemanha','Italiana','Inglesa','Belga','Francesa','Belga','Belga','Italiana','Espanhola','Italiana')
renda <-c(2.3,1.6,1.2,1.9,2.1,2.6,1.8,1.4,1.2,0.8,3.4,2.7,1.6,2.0,1.1,2.5,2.0,1.7,2.1,3.2)
experiencia <- c(5,8,5,30,5,29,5,12,2,1,10,12,3,12,4,14,21,7,3,4)

dados <- data.frame(Idade=idade, Nacionalidade=nacionalidade, Renda=renda, Experiencia=experiencia)

#1

#Idade
idade_intervalos <-cut(idade,breaks=c(20, 28,30,32,34,36,40,48,52))
frequencia_absoluta_idade <- table(idade_intervalos)
frequencia_absoluta_idade
frequencia_culmutativa_absoluta_idade <- cumsum(frequencia_absoluta_idade)
frequencia_culmutativa_absoluta_idade
frequencia_relativa_idade <- table(idade_intervalos)/length(idade_intervalos)
frequencia_relativa_idade
frequencia_culmutativa__relativa_idade <- cumsum(table(idade_intervalos)/length(idade_intervalos))
frequencia_culmutativa__relativa_idade

#Nacionalidade
frequencia_relativa_nacionalidade <- table(nacionalidade)/length(nacionalidade)
frequencia_culmutativa_nacionalidade <- cumsSum(table(nacionalidade)/length(nacionalidade))

#Renda
renda_intervalos <- cut(renda,breaks=c(0.5,1.0,1.5,1.75,2,2.5,3.5))
frequencia_absoluta_renda <- table(renda_intervalos)
frequencia_absoluta_renda
frequencia_culmutativa_absoluta_renda <- cumsum(frequencia_absoluta_renda)
frequencia_culmutativa_absoluta_renda
frequencia_relativa_renda<- table(renda_intervalos)/length(renda_intervalos)
frequencia_relativa_renda
frequencia_culmutativa__relativa_renda <- cumsum(table(renda_intervalos)/length(renda_intervalos))
frequencia_culmutativa__relativa_renda

#Experiencia
experiencia_intervalos <- cut(experiencia,breaks=c(0.5,1.5,3,5,10,15,30))
frequencia_absoluta_experiencia <- table(experiencia_intervalos)
frequencia_absoluta_experiencia
frequencia_culmutativa_absoluta_experiencia <- cumsum(frequencia_absoluta_experiencia)
frequencia_culmutativa_absoluta_experiencia
frequencia_relativa_experiencia<- table(experiencia_intervalos)/length(experiencia_intervalos)
frequencia_relativa_experiencia
frequencia_culmutativa__relativa_experiencia <- cumsum(table(experiencia_intervalos)/length(experiencia_intervalos))
frequencia_culmutativa__relativa_experiencia

#2 - Definitivo
#Nacionalidade
barplot(table(dados$Nacionalidade),
        main="Nacionalidades",
        xlab="Nacionalidade",
        ylab="Frequencia",
        border="red",
        col="blue",
        density=10
)

#Renda
p <- ggplot(dados, aes(x=Renda))+geom_histogram()+
  geom_histogram(color = "black",fill="#7B3F00")+
  labs(title="Salarios",x="Renda", y = "Quantidade")
p

#3 - Definitivo
media_idade <- mean(dados$Idade)
media_renda <- mean(dados$Renda)
media_experiencia <- mean(dados$Experiencia)

#4 #Qualidade dos gráficos e metodo de calcular quartis
Q1 <- quantile(dados$Renda, probs = 0.25)
Q2 <- quantile(dados$Renda, probs = 0.50)
Q3 <- quantile(dados$Renda, probs = 0.75)

ggplot(data = data.frame(dados$Renda), aes(x = "", y = dados$Renda)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(fill = "#4271AE",       
               outlier.colour = "red",
               alpha = 0.9) +
  ggtitle("Nível de renda")+
  ylab("Renda") + 
  coord_flip()