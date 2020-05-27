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
  colnames(ObitosBR) <- c("Estado", "Medidas", "Obitos")
  return(ObitosBR)
}

Total_Obitos <- Obitos(PoliticasBR) 
PoliticasBR$`Número de óbitos` <- Total_Obitos$Obitos

## Data do primeiro registro de óbitos: ##

PrimeiroObito <- function(x){
  linhas <- nrow(x)
  colunas <- ncol(x)
  ocor <- data.frame(Estado = c(NULL),
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
  colnames(ocor) <- c("Siglas", "Registro do 1º dia com óbitos")
  return(ocor)
}

Dia_Obito <- PrimeiroObito(OBITOS.BR.UF)
Dia_Obito <- Dia_Obito %>% 
  select(Siglas, `Registro do 1º dia com óbitos`) %>% 
  filter(Siglas == "SP" |
           Siglas == "RJ" |
           Siglas == "CE" |
           Siglas == "PE" |
           Siglas == "AM" |
           Siglas == "BA" |
           Siglas == "MA" |
           Siglas == "MG" |
           Siglas == "ES" |
           Siglas == "SC" )

PoliticasBR <- inner_join(PoliticasBR, Dia_Obito,
                          by = "Siglas")

## Visualização dos gráficos ##
### Gráficos por medidas:

titulo = "Respostas Políticas"

# 01 - Suspensão de eventos:

medida1 <- "- Suspensão de eventos"

Politicas_SuspensãoEventos <- PoliticasBR %>%
  select(Estado, Distância, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`) %>%
  filter(Medidas == "Suspensão de eventos")

Politicas_SuspensãoEventos$`Publicação do Decreto` <- format(Politicas_SuspensãoEventos$`Publicação do Decreto`,
                                                               "%d/%m/%Y")

Politicas_SuspensãoEventos$`1º Caso de COVID-19`<- format(Politicas_SuspensãoEventos$`1º Caso de COVID-19`,
                                                          "%d/%m/%Y")

Politicas_SuspensãoEventos$`Registro do 1º dia com óbitos` <- as.Date(Politicas_SuspensãoEventos$`Registro do 1º dia com óbitos`)
Politicas_SuspensãoEventos$`Registro do 1º dia com óbitos` <- format(Politicas_SuspensãoEventos$`Registro do 1º dia com óbitos`,
                                                                     "%d/%m/%Y")


SuspensãoEventos <- ggplot(Politicas_SuspensãoEventos,
                           aes(x = Estado,
                               y = Distância,
                               label = `Publicação do Decreto`,
                               label1 = `1º Caso de COVID-19`,
                               label2 = `Casos Confirmados`,
                               label3 = `Registro do 1º dia com óbitos`,
                               label4 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           aes(fill = Estado)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(paste(titulo, medida1)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

SuspensãoEventos <- ggplotly(SuspensãoEventos,
                             tooltip = c("x", "y", "label", "label1",
                                         "label2","label3","label4"))

saveRDS(SuspensãoEventos, file = "Suspensão de eventos.rds")

# 02 - Suspensão das Aulas:

medida2 <- "- Suspensão das Aulas"

Politicas_SuspensãoAulas <- PoliticasBR %>%
  select(Estado, Distância, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`) %>%
  filter(Medidas == "Suspensão das Aulas")

Politicas_SuspensãoAulas$`Publicação do Decreto` <- format(Politicas_SuspensãoAulas$`Publicação do Decreto`,
                                                             "%d/%m/%Y")

Politicas_SuspensãoAulas$`1º Caso de COVID-19`<- format(Politicas_SuspensãoAulas$`1º Caso de COVID-19`,
                                                          "%d/%m/%Y")

Politicas_SuspensãoAulas$`Registro do 1º dia com óbitos` <- as.Date(Politicas_SuspensãoAulas$`Registro do 1º dia com óbitos`)
Politicas_SuspensãoAulas$`Registro do 1º dia com óbitos` <- format(Politicas_SuspensãoAulas$`Registro do 1º dia com óbitos`,
                                                                     "%d/%m/%Y")

SuspensãoAulas <- ggplot(Politicas_SuspensãoAulas,
                           aes(x = Estado,
                               y = Distância,
                               label = `Publicação do Decreto`,
                               label1 = `1º Caso de COVID-19`,
                               label2 = `Casos Confirmados`,
                               label3 = `Registro do 1º dia com óbitos`,
                               label4 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           aes(fill = Estado)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(paste(titulo, medida2)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

SuspensãoAulas <- ggplotly(SuspensãoAulas,
                             tooltip = c("x", "y", "label", "label1",
                                         "label2","label3","label4"))

saveRDS(SuspensãoAulas, file = "Suspensão das Aulas.rds")

# 03 - Calamidade pública:

medida3 <- "- Calamidade pública"

Politicas_Calamidadepública <- PoliticasBR %>%
  select(Estado, Distância, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`) %>%
  filter(Medidas == "Calamidade pública")
  
Politicas_Calamidadepública$`Publicação do Decreto` <- format(Politicas_Calamidadepública$`Publicação do Decreto`,
                                                           "%d/%m/%Y")

Politicas_Calamidadepública$`1º Caso de COVID-19`<- format(Politicas_Calamidadepública$`1º Caso de COVID-19`,
                                                        "%d/%m/%Y")

Politicas_Calamidadepública$`Registro do 1º dia com óbitos` <- as.Date(Politicas_Calamidadepública$`Registro do 1º dia com óbitos`)
Politicas_Calamidadepública$`Registro do 1º dia com óbitos` <- format(Politicas_Calamidadepública$`Registro do 1º dia com óbitos`,
                                                                   "%d/%m/%Y")


Calamidadepública <- ggplot(Politicas_Calamidadepública,
                           aes(x = Estado,
                               y = Distância,
                               label = `Publicação do Decreto`,
                               label1 = `1º Caso de COVID-19`,
                               label2 = `Casos Confirmados`,
                               label3 = `Registro do 1º dia com óbitos`,
                               label4 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           aes(fill = Estado)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(paste(titulo, medida3)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

Calamidadepública <- ggplotly(Calamidadepública,
                           tooltip = c("x", "y", "label", "label1",
                                       "label2","label3","label4"))

saveRDS(Calamidadepública, file = "Calamidade pública.rds")
