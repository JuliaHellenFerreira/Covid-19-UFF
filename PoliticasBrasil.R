### Politicas Publicas - Brasil

## Pacotes

pacotes <- c("magrittr","knitr","dplyr", "lubridate","shiny","plotly","devtools", "readxl")
for(pacote in pacotes){
  if(!is.element(pacote,installed.packages())){install.packages(pacote)}
}

library(lubridate)
library(magrittr)
library(knitr)
library(dplyr)
library(shiny)
library(plotly)
library(devtools)
library(readxl)
library(ggplot2)

## Leitura dos Dados - Politicas Publicas nos municipios do RJ 

url <- "https://github.com/JuliaHellenFerreira/Covid-19-UFF/blob/master/RespostasPoliticasBrasil.xlsx?raw=true"
destfile <- "EventosCOVIDMunicipiosUFs.xlsx"
curl::curl_download(url, destfile)
PoliticasBR <- read_excel(destfile)

## Importacao - Casos confirmados e obitos do RJ ##

LINK = "https://sites.google.com/site/portaldadosuffcontracovid/home"
FILE = "GET.UFF.READ.RData"
download.file(url = paste0(LINK,"/",FILE), destfile=FILE)
load(FILE)  
GET.UFF.READ(c("CASOS.CONFIRMADOS.BR.UF.R","OBITOS.BR.UF.R"))
               
## Dist�ncia ( em dias ) at� 1� caso confirmado :

PoliticasBR$`Dist�nia` <- (as.Date(PoliticasBR$`Publica��o do Decreto`) - as.Date(PoliticasBR$`1� Caso de COVID-19`))
PoliticasBR$Dist�nia <- as.numeric(PoliticasBR$Dist�nia)
View(OBITOS.BR.UF)

# Numero de casos confirmados ate o dia do decreto:

CasosConfirmados <- function(x){
  Estado <- as.list(PoliticasBR$Sigla)
  Medid <- as.list(PoliticasBR$Medidas)
  Dias <- as.list(PoliticasBR$`Publica��o do Decreto`)
  CasosBR <- data.frame(Municipio = c(NULL),
                        Medida = c(NULL),
                        Casos = c(NULL))
  for (i in 1: length(Estado)){
    Est <- Estado[[i]][1]
    Med <- Medid[[i]][1]
    Dia <- as.character.Date(Dias[[i]][1])
    Cas <- as.numeric(CASOS.CONFIRMADOS.BR.UF[Est, Dia])
    CasosBR[i,1] <- Estado[[i]][1]
    CasosBR[i,2] <- Medid[[i]][1]
    CasosBR[i,3] <- Cas
  }
  colnames(CasosBR) <- c("Estados", "Medidas", "Casos")
  return(CasosBR)
}
  
  


