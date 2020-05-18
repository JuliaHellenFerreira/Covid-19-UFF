#Pacotes

library(tidyverse)
library(readxl)

# Dicionario (Formulario B)

FormularioB <- read.csv("C:/Users/jorge/Downloads/BOLSISTA Contact Information Form B.csv")

# Renomenando as colunas

colunas <- ncol(FormularioB)
colnames(FormularioB) <- c("Data da Entrevista", paste0("Q",c(seq(2:colunas))))

# Fator Ordenado

## Função para unir linhas com mesmo entrevistado - Em andamento


FormularioB$`Data da Entrevista` <- as.Date(FormularioB$`Data da Entrevista`)
class(FormularioB$`Data da Entrevista`)

Classificacao <- function(x){
  linhas <- nrow(x)
  y <- data.frame(Q1 = c(NULL),
                  Status = c(NULL))
  for (i in 1:linhas) {
      if (x[i,2] == x[i+1, 2]){
        y[i,2] <- x[i,2]
        y[i,3] <- print("Continuação da entrevista")
      }
      else{
        y[i,1] <- x[i,2]
        y[i,2] <- print("1° entresvista")
      }
  }
  colnames(y) <- c("Q1", "Status")
  return(y)
}

Entrevista <- Classificacao(FormularioB)

View(Entrevista)
  
FormularioB <-inner_join(FormularioB,
                         Entrevista,
                         by ="Q1")



