#install.packages("palmerpenguins")
#pdf(".pdf")
#dev.off()

rm(list=ls()) 			# clean the working space
graphics.off()			# close all the graphic windows

library(ggplot2)
library(palmerpenguins)
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
  labs(title="Quantidade de pinguins por espécie",x="Espécie", y = "Quantidade")

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
  labs(title="Grafico de caixa - Bill Length",x=")", y = "Bill Length(mm)")
box_plot_bill_length 

#Grafico de caixa - Bill Depth
box_plot_bill_depth <- ggplot(penguins_data, aes(x = "", y = bill_depth_mm)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(fill = "#91c95c",       
               outlier.colour = "red",
               alpha = 0.9) +
  coord_flip()+
  labs(title="Grafico de caixa - Bill Depth",x=")", y = "Bill Depth(mm)")

box_plot_bill_depth

#Grafico de caixa - Flipper Length
box_plot_flipper_length <- ggplot(penguins_data, aes(x = "", y = flipper_length_mm)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(fill = "#f5a125",       
               outlier.colour = "red",
               alpha = 0.9) +
  coord_flip()+
  labs(title="Grafico de caixa - Flipper Length",x="", y = "Flipper Length(mm)")

box_plot_flipper_length

#Grafico de caixa - Body Mass
box_plot_body_mass <- ggplot(penguins_data, aes(x = "", y = body_mass_g)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(fill = "#626463",       
               outlier.colour = "red",
               alpha = 0.9) +
  coord_flip()+
  labs(title="Grafico de caixa - Body Mass",x="", y = "Body Mass(g)")

box_plot_body_mass

#Grafico de barra ano
grafico_barras_ano <- ggplot(penguins_data, aes(x=year)) +
  geom_bar()+
  theme(text = element_text(size = 20))+
  labs(title="Quantidade de pinguins por ano",x="Ano", y = "Quantidade")

grafico_barras_ano


#Analise bivariada x = Bill_length
bill_length_depth <- ggplot(penguins_data, aes(x=bill_length_mm, y=bill_depth_mm,color=species)) + 
  geom_point()+
  scale_color_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))
bill_length_depth

bill_length_flipper_length <- ggplot(penguins_data, aes(x=bill_length_mm, y=flipper_length_mm,color=species))+ 
  geom_point()+
  scale_color_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))
bill_length_flipper_length

bill_length_body_mass <- ggplot(penguins_data, aes(x=bill_length_mm, y=body_mass_g,color=species))+
  geom_point()+
  scale_color_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))
bill_length_body_mass

#x = bill_depth

bill_depth_flipper_length <- ggplot(penguins_data, aes(x=bill_depth_mm, y=flipper_length_mm,color=species))+ 
  geom_point()+
  scale_color_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))

bill_depth_flipper_length

bill_depth_body_mass <- ggplot(penguins_data, aes(x=bill_depth_mm, y=body_mass_g,color=species))+
  geom_point()+
  scale_color_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))
bill_depth_body_mass

#flipper length
flipper_length_body_mass <- ggplot(penguins_data, aes(x=flipper_length_mm, y=body_mass_g,color=species))+
  geom_point()+
  scale_color_manual(values=c("#ff6c02", "#c55eca", "#0e7175"))
flipper_length_body_mass

#Grafico de barra ano
grafico_barras_ano_bill_length <- ggplot(penguins_data, aes(x=year,y=bill_length_mm,fill=bill_length_mm)) +
  geom_bar(stat="identity")

grafico_barras_ano_bill_length