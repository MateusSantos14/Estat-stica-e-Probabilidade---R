library(ggplot2)
rm(list = ls ())

#Cara:1
#coroa:0

moeda <- c(0,1)

simulacao_500 <- sample(moeda, size=500, replace = TRUE)
comparacao_500 <- table(simulacao_500)
qtd_caras <- length(which(simulacao_500==1))
qtd_caras

grafico_qtd_jogadas= function(n_jogadas) {
  moeda <- c(0,1)
  
  simulacao <- sample(moeda, size=n_jogadas, replace = TRUE)
  comparacao <- table(simulacao)
  
  media_cumultativa <- data.frame(n=seq(1,n_jogadas),proporcao=cumsum(simulacao)/seq_along(simulacao))
  
  ggplot(media_cumultativa, aes(x=n, y=proporcao)) +
    geom_line( color="red", size=1, alpha=0.9)+ 
    labs(title = "Proporção de caras em relação a quantidade de testes.", x = "Quantidade de testes", y = "Media cumultativa do número de caras")+
    scale_y_continuous(limits=c(0,1))+
    theme(text = element_text(size = 18))
}

grafico_qtd_jogadas(500)

grafico_qtd_jogadas(100000)