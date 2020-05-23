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
               
## Distância ( em dias ) até 1° caso confirmado :

PoliticasBR$`Distância` <- (as.Date(PoliticasBR$`Publicação do Decreto`) - as.Date(PoliticasBR$`1º Caso de COVID-19`))
PoliticasBR$Distância <- as.numeric(PoliticasBR$Distância)

# Numero de casos confirmados ate o dia do decreto:

CasosConfirmados <- function(x){
  Estado <- as.list(x$Siglas)
  Medid <- as.list(x$Medidas)
  Dias <- as.list(x$`Publicação do Decreto`)
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

Total_Casos <- CasosConfirmados(PoliticasBR) 
PoliticasBR$`Casos Confirmados` <- Total_Casos$Casos

# Numero de Obitos ate o dia do decreto:

Obitos <- function(x){
  Estado <- as.list(x$Siglas)
  Medid <- as.list(x$Medidas)
  Dias <- as.list(x$`Publicação do Decreto`)
  ObitosBR <- data.frame(Municipio = c(NULL),
                        Medida = c(NULL),
                        Casos = c(NULL))
  for (i in 1: length(Estado)){
    Est <- Estado[[i]][1]
    Med <- Medid[[i]][1]
    Dia <- as.character.Date(Dias[[i]][1])
    Obi <- as.numeric(OBITOS.BR.UF[Est, Dia])
    ObitosBR[i,1] <- Estado[[i]][1]
    ObitosBR[i,2] <- Medid[[i]][1]
    ObitosBR[i,3] <- Obi
  }
  colnames(ObitosBR) <- c("Estados", "Medidas", "Obitos")
  return(ObitosBR)
}

Total_Obitos <- Obitos(PoliticasBR) 
PoliticasBR$`Número de óbitos` <- Total_Obitos$Obitos

## Visualização dos gráficos ##

### Gráficos por medidas:

titulo = "Respostas Políticas"

# 01 - Suspensão de eventos:

medida1 <- "- Suspensão de eventos"

Politicas_SuspensãoEventos <- PoliticasBR %>%
  select(Estados, Distância, Medidas,`Casos Confirmados`,`Número de óbitos`) %>%
  filter(Medidas == "Suspensão de eventos")

SuspensãoEventos <- ggplot(Politicas_SuspensãoEventos,
                           aes(x = Estados,
                               y = Distância,
                               label = `Casos Confirmados`,
                               label1 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           aes(fill = Estados)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(paste(titulo, medida1)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

SuspensãoEventos <- ggplotly(SuspensãoEventos,
                             tooltip = c("x", "y", "label", "label1"))

saveRDS(SuspensãoEventos, file = "Suspensão de eventos.rds")

# 02 - Suspensão das Aulas:

medida2 <- "- Suspensão das Aulas"

Politicas_SuspensãoAulas <- PoliticasBR %>%
  select(Estados, Distância, Medidas,`Casos Confirmados`,`Número de óbitos`) %>%
  filter(Medidas == "Suspensão das Aulas")

SuspensãoAulas <- ggplot(Politicas_SuspensãoAulas,
                           aes(x = Estados,
                               y = Distância,
                               label = `Casos Confirmados`,
                               label1 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           aes(fill = Estados)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(paste(titulo, medida2)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

SuspensãoAulas <- ggplotly(SuspensãoAulas,
                             tooltip = c("x", "y", "label", "label1"))

saveRDS(SuspensãoAulas, file = "Suspensão das Aulas.rds")

# 03 - Calamidade pública:

medida3 <- "- Calamidade pública"

Politicas_Calamidadepública <- PoliticasBR %>%
  select(Estados, Distância, Medidas,`Casos Confirmados`,`Número de óbitos`) %>%
  filter(Medidas == "Calamidade pública")

Calamidadepública <- ggplot(Politicas_Calamidadepública,
                         aes(x = Estados,
                             y = Distância,
                             label = `Casos Confirmados`,
                             label1 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           aes(fill = Estados)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(paste(titulo, medida3)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

Calamidadepública <- ggplotly(Calamidadepública,
                           tooltip = c("x", "y", "label", "label1"))

saveRDS(Calamidadepública, file = "Calamidade pública.rds")
