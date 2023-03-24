#Dúvidas: Mediana e quartis da quantidade de chuva são 0 # Por que não tá mudando as cores
library(tidyverse)
library(dplyr)
rm(list=ls())

dados <- read.csv("C:/Users/Mateus/Desktop/Mateus/UFC/Estatisticas/TI0111_HW1_assignment/Códigos R/HW1_bike_rental.csv")

#1
data <- dados$Date #qualitativo ordinal
horas <- dados$Hour #qualitativo ordinal
quantidade <- dados$Count #quantitativo discreto
temperatura <- dados$Temperature #quantitativo continuo
visibilidade <- dados$Visibility #quantitativo continuo
chuva <- dados$Rainfall #quantitativo continuo
estacao <- dados$Seasons #qualitativo nominal

data_inicial <- format(min(as.Date(data,format="%d/%m/%Y")),"%d/%m/%Y")
data_inicial
data_final <- format(max(as.Date(data,format="%d/%m/%Y")),"%d/%m/%Y")
data_final
numero_observacoes <- nrow(dados)
numero_observacoes

#2
media_quantidade <- round(mean(quantidade),digits=2)
q1_quantidade <- round(quantile(quantidade, probs = 0.25),digits=2)
mediana_quantidade <- round(median(quantidade),digits=2)
q3_quantidade <- round(quantile(quantidade, probs = 0.75),digits=2)


media_temperatura <- round(mean(temperatura),digits=2)
q1_temperatura <- round(quantile(temperatura, probs = 0.25),digits=2)
mediana_temperatura <- round(median(temperatura),digits=2)
q3_temperatura <- round(quantile(temperatura, probs = 0.75),digits=2)

media_visibilidade <- round(mean(visibilidade),digits=2)
q1_visibilidade <- round(quantile(visibilidade, probs = 0.25),digits=2)
mediana_visibilidade <- round(median(visibilidade),digits=2)
q3_visibilidade <- round(quantile(visibilidade, probs = 0.75),digits=2)

media_chuva <- round(mean(chuva),digits=2)
q1_chuva <- round(quantile(chuva, probs = 0.25),digits=2)
mediana_chuva <- round(median(chuva),digits=2)
q3_chuva <- round(quantile(chuva, probs = 0.75),digits=2)

indicadores <- data.frame(Indicadores=c('Quantidade','Temperatura','Visibilidade',"Chuva"),
                                Medias=c(media_quantidade,media_temperatura,media_visibilidade,media_chuva),
                                Primeiro_Quartil=c(q1_quantidade,q1_temperatura,q1_visibilidade,q1_chuva),
                                Medianas=c(mediana_quantidade,mediana_temperatura,mediana_visibilidade,mediana_chuva),
                                Terceiro_Quartil=c(q3_quantidade,q3_temperatura,q3_visibilidade,q3_chuva))
indicadores

#3
#grafico_barras_estacoes_quantidade <- ggplot(dados, aes(x=Seasons, fill = Seasons,y=Count)) +
#  geom_bar(stat="identity")+scale_fill_manual(values=c("#680e03", "#479003", "#fc5e1d","#1126a5"))+
#  labs(title="Quantidade de bicicletas alugadas por estação do ano",x="Estações", y = "Quantidade")
#grafico_barras_estacoes_quantidade


media_temp_seasons <- dados %>% group_by(Seasons)  %>% summarise(Media_count = mean(Count),Media_temp = mean(Temperature),Media_rain = mean(Rainfall))

grafico_barras_estacoes_quantidade_media <- ggplot(media_temp_seasons, aes(x=Seasons, fill = Seasons,y=Media_count)) +
  geom_bar(stat="identity")+scale_fill_manual(values=c("#680e03", "#479003", "#fc5e1d","#1126a5"))+
  labs(title="Quantidade media de bicicletas alugadas por estação do ano",x="Estações", y = "Quantidade media")+
  theme(text = element_text(size = 20))+
  theme(legend.position="none")
grafico_barras_estacoes_quantidade_media


grafico_barras_estacoes_temperatura_media <- ggplot(media_temp_seasons, aes(x=Seasons, fill = Media_temp,y=Media_temp))+
  geom_bar(stat="identity")+
  labs(title="Temperatura média por estação do ano",x="Estações", y = "Temperatura média(ºC)")+
  theme(text = element_text(size = 20))+
  theme(legend.position="none")
grafico_barras_estacoes_temperatura_media

grafico_barras_estacoes_chuva_media <- ggplot(media_temp_seasons, aes(x=Seasons, fill = Media_rain,y=Media_rain))+
  geom_bar(stat="identity")+
  labs(title="Chuva média por estação do ano",x="Estações", y = "Chuva média(mm)")+
o+
  theme(legend.position="none")
grafico_barras_estacoes_chuva_media

media_rainfall_count <- dados %>% group_by(Rainfall)  %>% summarise(Media_count = mean(Count))
chuva_count <- ggplot(media_rainfall_count, aes(x=Rainfall, y=Media_count)) + 
  geom_point()+
  labs(title="Quantidade média de bicicletas alugadas por chuva",x="Chuva(mm)", y = "Quantiade média de bicicletas alugadas")+
  theme(text = element_text(size = 20))
chuva_count

ggplot(dados, aes(x=Rainfall, y=Count))+
  geom_point()+
  labs(title="Chuva(mm) x Quantidade de bicicletas ",x="Chuva(mm)", y = "Quantidade de bicicletas")+
  theme(text = element_text(size = 20))

#4
#Serie temporal
p <- ggplot(dados, aes(x = as.Date(Date,format="%d/%m/%Y"), y = Count))+
  geom_line(color = "#00AFBB", size = 0.8)+
  labs(title="Série temporal contagem de bicicletas",x="Data", y = "Quantidade de bicicletas")+
  theme(text = element_text(size = 20))


p

p <- ggplot(dados, aes(x = as.Date(Date,format="%d/%m/%Y"), y = Temperature))+
  geom_line(color = "#00AFBB", size = 0.8)+
  labs(title="Série temporal temperatura",x="Data", y = "Temperatura")+
  theme(text = element_text(size = 20))

p

p <- ggplot(dados, aes(x = as.Date(Date,format="%d/%m/%Y"), y = Visibility))+
  geom_line(color = "#00AFBB", size = 0.8)
p

p <- ggplot(dados, aes(x = as.Date(Date,format="%d/%m/%Y"), y = Rainfall))+
  geom_line(color = "#00AFBB", size = 1)+
  labs(title="Série temporal chuvas",x="Data", y = "Chuva(mm)")+
  theme(text = element_text(size = 20))

p
