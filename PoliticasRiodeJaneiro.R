# Politicas Publicas - Estado do Rio de Janeiro

## Pacotes

pacotes <- c("magrittr","knitr","dplyr","shiny","plotly","devtools", "readxl")
for(pacote in pacotes){
  if(!is.element(pacote,installed.packages())){install.packages(pacote)}
}

library(magrittr)
library(knitr)
library(dplyr)
library(shiny)
library(plotly)
library(devtools)
library(readxl)

## Leitura dos Dados - Politicas Publicas nos municipios do RJ

## Importacao - Casos confirmados e obitos do RJ

Link = "https://sites.google.com/site/portaldadosuffcontracovid/home"
File = "GET.UFF.READ.RData"
download.file(url = paste0(Link,"/",File), destfile = File)
load(File) 
GET.UFF.READ(c("CASOS.CONFIRMADOS.RJ.R","OBITOS.RJ.R"))

## Data da primeira medida de cada municipio - Tem erro!

Medidas <- Politicas_RiodeJaneiro_1_ %>%
  select(Municipio, Inicio, Classificao) %>%
  filter(Classificao != "Outros")
View(Medidas)

PrimeiraMedida <- function(x){
  linhas <- nrow(x)
  colunas <- ncol(x)
  d_medida <- data.frame(Municipio = c(NULL), Dia = c(NULL))
  t = 0
  for (i in 1:linhas){
    for (j in 1:colunas){
      if (x[i,j] == "Medidas de Prevenção"){
        t = t + 1
        d_medida[t,1] = rownames(x)[i]
        d_medida[t,2] = x[i,2]
        break
      }
    }
  }
  colnames(d_medida) <- c("Municipio", "Data")
  return(d_medida)
}

## Data do primeiro caso confirmado 

PrimeiroCaso <- function(x){
  linhas <- nrow(x)
  colunas <- ncol(x)
  ocor <- data.frame(Municipio = c(NULL), Ocorrencia = c(NULL))
  t = 0
  for (i in 1:linhas){
    for (j in  2:colunas){
      if (x[i,j] != 0){
        t = t+  1
        ocor[t,1] <- rownames(x)[i]
        ocor [t,2] <- colnames(x)[j]
        break
      }
    }
  }
  colnames(ocor) <- c("Municipio", "Ocorrencia")
  return(ocor)
}

## Distancia (em dias) ate o primeiro caso confirmado - Em andamento

# Grafico - Em andamento 


Municipios <- plot_ly(Politicas_RiodeJaneiro, 
                      x = "Municipio",
                      y = "Inicio",
                      type = "bar")
