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

## Leitura dos Dados - Politicas Publicas nos municipios do RJ (((((( Retirar do Site))))))

## Importacao - Casos confirmados e obitos do RJ

Link = "https://sites.google.com/site/portaldadosuffcontracovid/home"
File = "GET.UFF.READ.RData"
download.file(url = paste0(Link,"/",File), destfile = File)
load(File) 
GET.UFF.READ(c("CASOS.CONFIRMADOS.RJ.R","OBITOS.RJ.R"))

## Data da primeira medida de cada municipio

###################### Medidas de cada municipio ########################

Politicas_RiodeJaneiro_1_$Inicio <- as.Date(Politicas_RiodeJaneiro_1_$Inicio)
Medidas <- Politicas_RiodeJaneiro_1_ %>%
  select(Municipio, Inicio, Classificacao) %>%
  filter(Classificacao != "Outros") %>%
  arrange(Inicio)

### ((((Separar por medidas)))):

## Data do primeiro caso confirmado:

######################### Funcao para encontrar o primeiro caso confirmado ###############################

PrimeiroCaso <- function(x){
  linhas <- nrow(x)
  colunas <- ncol(x)
  ocor <- data.frame(Municipio = c(NULL),
                     Ocorrencia = c(NULL))
  for (i in 1:linhas){
    for (j in  2:colunas){
      if (x[i,j] != 0){
        ocor[i,1] = rownames(x)[i]
        ocor [i,2] = colnames(x)[j]
        break
      }
    }
  }
  colnames(ocor) <- c("Municipio", "Ocorrencia")
  return(ocor)
}
CASOS.CONFIRMADOS.RJ <- PrimeiroCaso(CASOS.CONFIRMADOS.RJ)

############################ Filtrando os municipios que ire usar ##########################################

CASOS.CONFIRMADOS.RJ$Municipio = rownames(CASOS.CONFIRMADOS.RJ)
Municipios_PrimeiroCaso <- PrimeiroCaso(CASOS.CONFIRMADOS.RJ)
RJ_PrimeiroCaso <- Municipios_PrimeiroCaso %>%
                   filter(Municipio == "Itaboraí/R" | 
                          Municipio == "Volta Redonda/RJ" |
                          Municipio == "Niterói/RJ" |
                          Municipio == "Rio de Janeiro/RJ" |
                          Municipio == "São João de Meriti/RJ" |
                          Municipio == "Mesquita/RJ")

## Distancia (em dias) ate o primeiro caso confirmado - Em andamento

DADOS$Distancia <- with(DADOS, as.Date(DATA_FIM, "%d/%m/%Y") - as.Date(DATA_INICIO, "%d/%m/%Y"))
DADOS

# Grafico - Em andamento 

Municipios <- plot_ly(Politicas_RiodeJaneiro, 
                      x = "Municipio",
                      y = "Inicio",
                      type = "bar")
