#Dúvidas: Mediana e quartis da quantidade de chuva são 0 # Por que não tá mudando as cores

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
grafico_barras_estacoes_quantidade <- ggplot(dados, aes(x=Seasons, fill = Seasons,y=Count)) +
  geom_bar(stat="identity")+scale_fill_manual(values=c("#680e03", "#479003", "#fc5e1d","#1126a5"))+
  labs(title="Quantidade de bicicletas alugadas por estação do ano",x="Estações", y = "Quantidade")
grafico_barras_estacoes_quantidade
#Chuvas # Terminar ainda zzzz
grafico_barras_estacoes_quantidade <- ggplot(dados, aes(x=Seasons, fill = Seasons,y=Rainfall)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#680e03", "#479003", "#fc5e1d","#1126a5"))+
  labs(title="Quantidade de bicicletas alugadas por estação do ano",x="Estações", y = "Chuva total(mm)")
grafico_barras_estacoes_quantidade
#Como fazer gráfico das chuvas
grafico_barras_chuva_quantidade <- ggplot(dados, aes(x=Count, fill = Rainfall,y=Rainfall))+
  geom_bar(stat="identity")
grafico_barras_chuva_quantidade

#4
#Serie temporal
p <- ggplot(dados, aes(x = as.Date(Date,format="%d/%m/%Y"), y = Count))+
  geom_line(color = "#00AFBB", size = 2)

p <- p + scale_x_date(limits = c(min(quantidade_datas$Datas), max(quantidade_datas$Datas)))

p

p <- ggplot(dados, aes(x = as.Date(Date,format="%d/%m/%Y"), y = Count, fill = Rainfall))+
  geom_line(color = "#00AFBB", size = 2)+
  geom_bar(stat="identity")

p <- p + scale_x_date(limits = c(min(quantidade_datas$Datas), max(quantidade_datas$Datas)))

p

p <- ggplot(dados, aes(x = as.Date(Date,format="%d/%m/%Y"), y = Rainfall))+
  geom_line(color = "#00AFBB", size = 1)

p <- p + scale_x_date(limits = c(min(quantidade_datas$Datas), max(quantidade_datas$Datas)))

p
