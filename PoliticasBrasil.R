# GET UFF contra o COVID-19

# Politicas Publicas - Brasil
###### Este script tem como objetivo calcular a distancia, em dias,
###### do primeiro caso confirmado ate um determinado decreto de 
###### cada estado do Brasil.

## Pacotes ##

pacotes <- c("magrittr","knitr","gridGraphics","png","dplyr",
             "lubridate","shiny","plotly","devtools", "readxl")
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
library(png)
library(gridGraphics)

## Leitura dos Dados - Politicas Publicas nos municipios do RJ ##

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

PoliticasBR$Dist�ncia0 <- (as.Date(PoliticasBR$`Publica��o do Decreto`) - as.Date(PoliticasBR$`1� Caso de COVID-19`))
PoliticasBR$Dist�ncia0 <- as.numeric(PoliticasBR$Dist�ncia0)

# Numero de casos confirmados ate o dia do decreto:

CasosConfirmados <- function(x){
  Estado <- as.list(x$Siglas)
  Medid <- as.list(x$Medidas)
  Dias <- as.list(x$`Publica��o do Decreto`)
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
  Dias <- as.list(x$`Publica��o do Decreto`)
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
PoliticasBR$`N�mero de �bitos` <- Total_Obitos$Obitos

## Data do primeiro registro de �bitos: ##

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
  colnames(ocor) <- c("Siglas", "Registro do 1� dia com �bitos")
  return(ocor)
}

Dia_Obito <- PrimeiroObito(OBITOS.BR.UF)
Dia_Obito <- Dia_Obito %>% 
  select(Siglas, `Registro do 1� dia com �bitos`) %>% 
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

## Ranking: 

Posicao <- function(v){
  linhas <- nrow(v)
  Eventos <- data.frame(NULL)
  Aulas <- data.frame(NULL)
  Calamidade <- data.frame(NULL)
  nada <- as.vector(NULL)
  j = 1
  l = 1
  m = 1
  e = 1
  for (i in 1: linhas){
    if(v$Medidas[i] == "Suspens�o de eventos"){
      Eventos[j,1] <- v$Siglas[i]
      Eventos[j,2] <- v$Estado[i]
      Eventos[j,3] <- v$`Publica��o do Decreto`[i]
      Eventos[j,4] <- v$Medidas[i]
      Eventos[j,5] <- v$`1� Caso de COVID-19`[i]
      Eventos[j,6] <- v$`Casos Confirmados`[i]
      Eventos[j,7] <- v$`Registro do 1� dia com �bitos`[i]
      Eventos[j,8] <- v$`N�mero de �bitos`[i]
      Eventos[j,9] <- v$Dist�ncia0[i]
      j = j + 1
    }
    if (v$Medidas[i] == "Suspens�o das Aulas"){
      Aulas[l,1] <- v$Siglas[i]
      Aulas[l,2] <- v$Estado[i]
      Aulas[l,3] <- v$`Publica��o do Decreto`[i]
      Aulas[l,4] <- v$Medidas[i]
      Aulas[l,5] <- v$`1� Caso de COVID-19`[i]
      Aulas[l,6] <- v$`Casos Confirmados`[i]
      Aulas[l,7] <- v$`Registro do 1� dia com �bitos`[i]
      Aulas[l,8] <- v$`N�mero de �bitos`[i]
      Aulas[l,9] <- v$Dist�ncia0[i]
      l = l + 1
    }
    if (v$Medidas[i] == "Calamidade p�blica"){
      Calamidade[m,1] <- v$Siglas[i]
      Calamidade[m,2] <- v$Estado[i]
      Calamidade[m,3] <- v$`Publica��o do Decreto`[i]
      Calamidade[m,4] <- v$Medidas[i]
      Calamidade[m,5] <- v$`1� Caso de COVID-19`[i]
      Calamidade[m,6] <- v$`Casos Confirmados`[i]
      Calamidade[m,7] <- v$`Registro do 1� dia com �bitos`[i]
      Calamidade[m,8] <- v$`N�mero de �bitos`[i]
      Calamidade[m,9] <- v$Dist�ncia0[i]
      m = m + 1
    }
    else{
      nada[e] <- v$`Casos Confirmados`[i]
      e = e + 1
    }
  }
  Eventos = Eventos %>% arrange(V9)
  Aulas = Aulas %>% arrange(V9)
  Calamidade = Calamidade %>% arrange(V9)
  Politicas <- bind_rows(Eventos, Aulas, Calamidade)
  Politicas = as.data.frame(Politicas)
  colnames(Politicas) <- c("Siglas", "Estado", "Publica��o do Decreto",
                   "Medidas", "1� Caso de COVID-19","Casos Confirmados",
                   "Registro do 1� dia com �bitos", "N�mero de �bitos",
                   "Dist�ncia0")
  return(Politicas)
}

PoliticasBR <- Posicao(PoliticasBR)
Rank <- as.data.frame(paste0(rep(1:10,3),"�"))
colnames(Rank) <- "Rank"
PoliticasBR$`Rank` <- Rank$Rank

## Visualiza��o dos gr�ficos ##
### Gr�ficos por medidas:

titulo = "Respostas Pol�ticas"

#################### 01 - Suspens�o de eventos: #################

# Organizando por medida:

Politicas_Suspens�oEventos <- PoliticasBR %>%
  select(Estado, Dist�ncia0, Medidas,`Publica��o do Decreto`,
         `1� Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1� dia com �bitos`,`N�mero de �bitos`,
         Rank) %>%
  filter(Medidas == "Suspens�o de eventos")

# Colocando no formato data:

Politicas_Suspens�oEventos$`Publica��o do Decreto` <- format(Politicas_Suspens�oEventos$`Publica��o do Decreto`,
                                                               "%d/%m/%Y")

Politicas_Suspens�oEventos$`1� Caso de COVID-19`<- format(Politicas_Suspens�oEventos$`1� Caso de COVID-19`,
                                                          "%d/%m/%Y")

Politicas_Suspens�oEventos$`Registro do 1� dia com �bitos` <- as.Date(Politicas_Suspens�oEventos$`Registro do 1� dia com �bitos`)
Politicas_Suspens�oEventos$`Registro do 1� dia com �bitos` <- format(Politicas_Suspens�oEventos$`Registro do 1� dia com �bitos`,
                                                                     "%d/%m/%Y")
# Reclassificando para colocar em ordem crescente:

Politicas_Suspens�oEventos = arrange(Politicas_Suspens�oEventos,
                                     Dist�ncia0)

Politicas_Suspens�oEventos$Estado = factor(Politicas_Suspens�oEventos$Estado,
                                           levels = Politicas_Suspens�oEventos$Estado)

Politicas_Suspens�oEventos$Dist�ncia0 = as.integer(Politicas_Suspens�oEventos$Dist�ncia0)

# Gr�fico no ggplot:

Politicas_Suspens�oEventos$Dist�ncia = as.character(Politicas_Suspens�oEventos[,2])

Suspens�oEventos <- ggplot(Politicas_Suspens�oEventos,
                           aes(x = Estado,
                               y = Dist�ncia0,
                               label = Dist�ncia,
                               label1 = Rank,
                               label2 = `Publica��o do Decreto`,
                               label3 = `1� Caso de COVID-19`,
                               label4 = `Casos Confirmados`,
                               label5 = `Registro do 1� dia com �bitos`,
                               label6 = `N�mero de �bitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           fill = c("seagreen","peru",
                    "seashell4","royalblue2",
                    "orange1","rosybrown1",
                    "mediumaquamarine","tomato",
                    "maroon", "yellowgreen") ,
           aes(fill = Estado)) +
  xlab("") +
  ylab("Dist�ncia (em dias) para a confirma��o do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gr�fico interativo:

Suspens�oEventos <- ggplotly(Suspens�oEventos,
                             tooltip = c("x","label", "label1",
                                         "label2","label3","label4",
                                         "label5", "label6"))

# Salvando gr�fico:

saveRDS(Suspens�oEventos, file = "Suspens�o de eventos.rds")

##################### 02 - Suspens�o das Aulas: #################

# Organizando por medida:

Politicas_Suspens�oAulas <- PoliticasBR %>%
  select(Estado, Dist�ncia0, Medidas,`Publica��o do Decreto`,
         `1� Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1� dia com �bitos`,`N�mero de �bitos`,
         Rank) %>%
  filter(Medidas == "Suspens�o das Aulas")

# Colocando no formato data:

Politicas_Suspens�oAulas$`Publica��o do Decreto` <- format(Politicas_Suspens�oAulas$`Publica��o do Decreto`,
                                                             "%d/%m/%Y")

Politicas_Suspens�oAulas$`1� Caso de COVID-19`<- format(Politicas_Suspens�oAulas$`1� Caso de COVID-19`,
                                                          "%d/%m/%Y")

Politicas_Suspens�oAulas$`Registro do 1� dia com �bitos` <- as.Date(Politicas_Suspens�oAulas$`Registro do 1� dia com �bitos`)
Politicas_Suspens�oAulas$`Registro do 1� dia com �bitos` <- format(Politicas_Suspens�oAulas$`Registro do 1� dia com �bitos`,
                                                                     "%d/%m/%Y")
# Reclassificando para colocar em ordem crescente:

Politicas_Suspens�oAulas = arrange(Politicas_Suspens�oAulas,
                                     Dist�ncia0) 

Politicas_Suspens�oAulas$Estado = factor(Politicas_Suspens�oAulas$Estado,
                                           levels = Politicas_Suspens�oAulas$Estado)

Politicas_Suspens�oAulas$Dist�ncia0 = as.integer(Politicas_Suspens�oAulas$Dist�ncia0)

# Gr�fico no ggplot:

Politicas_Suspens�oAulas$Dist�ncia = as.character(Politicas_Suspens�oAulas[,2])

Suspens�oAulas <- ggplot(Politicas_Suspens�oAulas,
                           aes(x = Estado,
                               y = Dist�ncia0,
                               label = Dist�ncia,
                               label1 = Rank,
                               label2 = `Publica��o do Decreto`,
                               label3 = `1� Caso de COVID-19`,
                               label4 = `Casos Confirmados`,
                               label5 = `Registro do 1� dia com �bitos`,
                               label6 = `N�mero de �bitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           fill = c("seagreen","peru",
                    "orange1","seashell4",
                    "royalblue2","rosybrown1",
                    "mediumaquamarine","tomato",
                    "maroon", "yellowgreen"),
           aes(fill = Estado)) +
  xlab("") +
  ylab("Dist�ncia (em dias) para a confirma��o do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gr�fico interativo:

Suspens�oAulas <- ggplotly(Suspens�oAulas,
                             tooltip = c("x", "label", "label1",
                                         "label2","label3","label4",
                                         "label5", "label6"))

# Salvando gr�fico:

saveRDS(Suspens�oAulas, file = "Suspens�o das Aulas.rds")

###################### 03 - Calamidade p�blica: ###################

# Organizando por medida:

Politicas_Calamidadep�blica <- PoliticasBR %>%
  select(Estado, Dist�ncia0, Medidas,`Publica��o do Decreto`,
         `1� Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1� dia com �bitos`,`N�mero de �bitos`,
         Rank) %>%
  filter(Medidas == "Calamidade p�blica")

# Colocando no formato data:

Politicas_Calamidadep�blica$`Publica��o do Decreto` <- format(Politicas_Calamidadep�blica$`Publica��o do Decreto`,
                                                           "%d/%m/%Y")

Politicas_Calamidadep�blica$`1� Caso de COVID-19`<- format(Politicas_Calamidadep�blica$`1� Caso de COVID-19`,
                                                        "%d/%m/%Y")

Politicas_Calamidadep�blica$`Registro do 1� dia com �bitos` <- as.Date(Politicas_Calamidadep�blica$`Registro do 1� dia com �bitos`)
Politicas_Calamidadep�blica$`Registro do 1� dia com �bitos` <- format(Politicas_Calamidadep�blica$`Registro do 1� dia com �bitos`,
                                                                   "%d/%m/%Y")
# Reclassificando para colocar em ordem crescente:

Politicas_Calamidadep�blica = arrange(Politicas_Calamidadep�blica,
                                   Dist�ncia0) 

Politicas_Calamidadep�blica$Estado = factor(Politicas_Calamidadep�blica$Estado,
                                         levels = Politicas_Calamidadep�blica$Estado)

Politicas_Calamidadep�blica$Dist�ncia0 = as.numeric(Politicas_Calamidadep�blica$Dist�ncia0)

# Gr�fico no ggplot:

Politicas_Calamidadep�blica$Dist�ncia = as.character(Politicas_Calamidadep�blica[,2])

Calamidadep�blica <- ggplot(Politicas_Calamidadep�blica,
                           aes(x = Estado,
                               y = Dist�ncia0,
                               label = Dist�ncia,
                               label1 = Rank,
                               label2 = `Publica��o do Decreto`,
                               label3= `1� Caso de COVID-19`,
                               label4 = `Casos Confirmados`,
                               label5 = `Registro do 1� dia com �bitos`,
                               label6 = `N�mero de �bitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           fill = c("seagreen","peru",
                    "rosybrown1","seashell4",
                    "royalblue2","tomato",
                    "mediumaquamarine","orange1",
                    "yellowgreen","maroon"),
           aes(fill = Estado)) +
  xlab("") +
  ylab("Dist�ncia (em dias) para a confirma��o do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gr�fico interativo:

Calamidadep�blica <- ggplotly(Calamidadep�blica,
                           tooltip = c("x", "label", "label1",
                                       "label2","label3","label4",
                                       "label5", "label6"))
# Salvando gr�fico:

saveRDS(Calamidadep�blica, file = "Calamidade p�blica.rds")
