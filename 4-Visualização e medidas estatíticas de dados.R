#install.packages("palmerpenguins")
#pdf(".pdf")
#dev.off()

rm(list=ls()) 			# clean the working space
graphics.off()			# close all the graphic windows

library(ggplot2)
library(palmerpenguins)
library(GGally)
penguins_data <-na.omit(penguins)

#Adelie: #ff6c02
#Chinstrap: #c55eca
#Gentoo: #0e7175
#3

#Gráfico de barra especies
#pdf(".pdf")
grafico_barras_species <- ggplot(penguins_data, aes(x=species, fill = species)) +
  geom_bar()+
  scale_fill_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))+
  theme(legend.position="none")+
  theme(text = element_text(size = 20))+
  labs(title="Quantidade de pinguins por espécie",x="Espécie", y = "Quantidade")+
  

grafico_barras_species
#dev.off()
#Grafico de barras ilhas
#pdf(".pdf")
grafico_barras_island <- ggplot(penguins_data, aes(x=island, fill = island)) +
  geom_bar()+
  theme_minimal()+
  theme(legend.position="none")+
  theme(text = element_text(size = 20))+
  labs(title="Quantidade de pinguins por ilha",x="Ilhas", y = "Quantidade")

grafico_barras_island
#dev.off()
#Grafico de caixa - Bill Length
box_plot_bill_length <- ggplot(penguins_data, aes(x = "", y = bill_length_mm)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(fill = "#e14948",       
               outlier.colour = "red",
               alpha = 0.9) +
  coord_flip()+
  labs(title="Grafico de caixa - Bill Length",x=")", y = "Bill Length(mm)")+
  theme(text = element_text(size = 20))
box_plot_bill_length 

#Grafico de caixa - Bill Depth
box_plot_bill_depth <- ggplot(penguins_data, aes(x = "", y = bill_depth_mm)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(fill = "#91c95c",       
               outlier.colour = "red",
               alpha = 0.9) +
  coord_flip()+
  labs(title="Grafico de caixa - Bill Depth",x="", y = "Bill Depth(mm)")+
  theme(text = element_text(size = 20))

box_plot_bill_depth

#Grafico de caixa - Flipper Length
box_plot_flipper_length <- ggplot(penguins_data, aes(x = "", y = flipper_length_mm)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(fill = "#f5a125",       
               outlier.colour = "red",
               alpha = 0.9) +
  coord_flip()+
  labs(title="Grafico de caixa - Flipper Length",x="", y = "Flipper Length(mm)")+
  theme(text = element_text(size = 20))

box_plot_flipper_length

#Grafico de caixa - Body Mass
box_plot_body_mass <- ggplot(penguins_data, aes(x = "", y = body_mass_g)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(fill = "#626463",       
               outlier.colour = "red",
               alpha = 0.9) +
  coord_flip()+
  labs(title="Grafico de caixa - Body Mass",x="", y = "Body Mass(g)")+
  theme(text = element_text(size = 20))

box_plot_body_mass

#Grafico de barra ano
grafico_barras_ano <- ggplot(penguins_data, aes(x=year)) +
  geom_bar()+
  theme(text = element_text(size = 20))+
  labs(title="Quantidade de pinguins por ano",x="Ano", y = "Quantidade")

grafico_barras_ano

#Grafico de barra sex
grafico_barras_sexo <- ggplot(penguins_data, aes(x=sex)) +
  geom_bar()+
  theme(text = element_text(size = 20))+
  labs(title="Quantidade de pinguins por sexo",x="Sexo", y = "Quantidade")

grafico_barras_sexo


#Analise bivariada 
#x = Bill_length
#pdf("biassorted_.pdf")
bill_length_depth <- ggplot(penguins_data, aes(x=bill_length_mm, y=bill_depth_mm,color=species)) + 
  geom_point()+
  scale_color_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))+
  theme(text = element_text(size = 18),legend.title = element_text(size=15),legend.text = element_text(size=13))+
  labs(title="Bill Length x Bill Depth",x="Bill Length",y="Bill Depth",color="Species")
bill_length_depth

cor(penguins_data$bill_length_mm,penguins_data$bill_depth_mm)

#biassorted_bill_length_flipper_length
bill_length_flipper_length <- ggplot(penguins_data, aes(x=bill_length_mm, y=flipper_length_mm,color=species))+ 
  geom_point()+
  scale_color_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))+
  theme(text = element_text(size = 18),legend.title = element_text(size=15),legend.text = element_text(size=13))+
  labs(title="Bill Length x Flipper Length",x="Bill Length",y="Flipper Length",color="Species")
bill_length_flipper_length

cor(penguins_data$bill_length_mm,penguins_data$flipper_length_mm)

#biassorted_bill_length_body_mass
bill_length_body_mass <- ggplot(penguins_data, aes(x=bill_length_mm, y=body_mass_g,color=species))+
  geom_point()+
  scale_color_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))+
  theme(text = element_text(size = 18),legend.title = element_text(size=15),legend.text = element_text(size=13))+
  labs(title="Bill Length x Body Mass",x="Bill Length",y="Body Mass",color="Species")
bill_length_body_mass

cor(penguins_data$bill_length_mm,penguins_data$body_mass_g)


#biassorted_bill_depth_flipper_length
bill_depth_flipper_length <- ggplot(penguins_data, aes(x=bill_depth_mm, y=flipper_length_mm,color=species))+ 
  geom_point()+
  scale_color_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))+
  theme(text = element_text(size = 18),legend.title = element_text(size=15),legend.text = element_text(size=13))+
  labs(title="Bill Depth x Flipper Length",x="Bill Depth",y="Flipper Length",color="Species")

bill_depth_flipper_length

cor(penguins_data$bill_depth_mm,penguins_data$flipper_length_mm)

#biassorted_bill_depth_body_mass
bill_depth_body_mass <- ggplot(penguins_data, aes(x=bill_depth_mm, y=body_mass_g,color=species))+
  geom_point()+
  scale_color_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))+
  theme(text = element_text(size = 18),legend.title = element_text(size=15),legend.text = element_text(size=13))+
  labs(title="Bill Depth x Body Mass",x="Bill Depth",y="Body Mass",color="Species")

bill_depth_body_mass

cor(penguins_data$bill_depth_mm,penguins_data$body_mass_g)

#biassorted_flipper_length_body_mass
flipper_length_body_mass <- ggplot(penguins_data, aes(x=flipper_length_mm, y=body_mass_g,color=species))+
  geom_point()+
  scale_color_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))+
  theme(text = element_text(size = 18),legend.title = element_text(size=15),legend.text = element_text(size=13))+
  labs(title="Flipper Length x Body Mass",x="Flipper Length",y="Body Mass",color="Species")

flipper_length_body_mass

cor(penguins_data$flipper_length_mm,penguins_data$body_mass_g)

#island

ggpairs(penguins_data, columns = c(2,3), ggplot2::aes(colour=island),legend=1)+
  theme(text = element_text(size = 18),legend.text = element_text(size=13),legend.position = "bottom")

ggpairs(penguins_data, columns = c(2,4), ggplot2::aes(colour=island),legend=1)+
  theme(text = element_text(size = 18),legend.text = element_text(size=13),legend.position = "bottom")

ggpairs(penguins_data, columns = c(2,5), ggplot2::aes(colour=island),legend=1)+
  theme(text = element_text(size = 18),legend.text = element_text(size=13),legend.position = "bottom")

ggpairs(penguins_data, columns = c(2,6), ggplot2::aes(colour=island),legend=1)+
  theme(text = element_text(size = 18),legend.text = element_text(size=13),legend.position = "bottom")

#sex

ggpairs(penguins_data, columns = c(7,3), ggplot2::aes(colour=sex),legend=1)+
  theme(text = element_text(size = 18),legend.text = element_text(size=13),legend.position = "bottom")

ggpairs(penguins_data, columns = c(7,4), ggplot2::aes(colour=sex),legend=1)+
  theme(text = element_text(size = 18),legend.text = element_text(size=13),legend.position = "bottom")

ggpairs(penguins_data, columns = c(7,5), ggplot2::aes(colour=sex),legend=1)+
  theme(text = element_text(size = 18),legend.text = element_text(size=13),legend.position = "bottom")

ggpairs(penguins_data, columns = c(7,6), ggplot2::aes(colour=sex),legend=1)+
  theme(text = element_text(size = 18),legend.text = element_text(size=13),legend.position = "bottom")

#Year

penguins_data$year <- as.factor(penguins_data$year)

ggpairs(penguins_data, columns = c(8,3), ggplot2::aes(colour=year),legend=1)+
  theme(text = element_text(size = 18),legend.text = element_text(size=13),legend.position = "bottom")

ggpairs(penguins_data, columns = c(8,4), ggplot2::aes(colour=year),legend=1)+
  theme(text = element_text(size = 18),legend.text = element_text(size=13),legend.position = "bottom")

ggpairs(penguins_data, columns = c(8,5), ggplot2::aes(colour=year),legend=1)+
  theme(text = element_text(size = 18),legend.text = element_text(size=13),legend.position = "bottom")

ggpairs(penguins_data, columns = c(8,6), ggplot2::aes(colour=year),legend=1)+
  theme(text = element_text(size = 18),legend.text = element_text(size=13),legend.position = "bottom")
#Tabela indicadores

media_bill_length_mm <- round(mean(penguins_data$bill_length_mm),digits=2)
Q1_bill_length_mm <- quantile(penguins_data$bill_length_mm,0.25, type=2)
mediana_bill_length_mm <- median(penguins_data$bill_length_mm)
Q3_bill_length_mm <- quantile(penguins_data$bill_length_mm,0.75, type=2)


media_bill_depth_mm <- round(mean(penguins_data$bill_depth_mm),digits=2)
Q1_bill_depth_mm <- quantile(penguins_data$bill_depth_mm,0.25, type=2)
mediana_bill_depth_mm <- median(penguins_data$bill_depth_mm)
Q3_bill_depth_mm <- quantile(penguins_data$bill_depth_mm,0.75, type=2)

media_flipper_length_mm <- round(mean(penguins_data$flipper_length_mm),digits=2)
Q1_flipper_length_mm <- quantile(penguins_data$flipper_length_mm,0.25, type=2)
mediana_flipper_length_mm <- median(penguins_data$flipper_length_mm)
Q3_flipper_length_mm <- quantile(penguins_data$flipper_length_mm,0.75, type=2)

media_body_mass_g <- round(mean(penguins_data$body_mass_g),digits=2)
Q1_body_mass_g <- quantile(penguins_data$body_mass_g,0.25, type=2)
mediana_body_mass_g <- median(penguins_data$body_mass_g)
Q3_body_mass_g <- quantile(penguins_data$body_mass_g,0.75, type=2)


indicadores <- data.frame(Indicadores=c('bill_length_mm','bill_depth_mm','flipper_length_mm',"body_mass_g"),
                          Medias=c(media_bill_length_mm,media_bill_depth_mm,media_flipper_length_mm,media_body_mass_g),
                          Primeiro_Quartil=c(Q1_bill_length_mm,Q1_bill_depth_mm,Q1_flipper_length_mm,Q1_body_mass_g),
                          Medianas=c(mediana_bill_length_mm,mediana_bill_depth_mm,mediana_flipper_length_mm,mediana_body_mass_g),
                          Terceiro_Quartil=c(Q3_bill_length_mm,Q3_bill_depth_mm,Q3_flipper_length_mm,Q3_body_mass_g))
indicadores