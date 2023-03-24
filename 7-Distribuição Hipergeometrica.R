rm(list = ls ()) 

#1

prob_devolvido <- 1-dhyper(0, 10, 90, 5, log = FALSE)
prob_devolvido

#2 
prob_mais_de_um <- dhyper(0, 10, 90, 1, log = FALSE)
prob_mais_de_um

#3
prob_três <- dhyper(0, 10, 90, 2, log = FALSE) * dhyper(1, 10, 88, 1, log = FALSE)
prob_três