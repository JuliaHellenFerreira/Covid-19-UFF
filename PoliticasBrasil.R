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
               
## Distância ( em dias ) até 1° caso confirmado :

PoliticasBR$Distância0 <- (as.Date(PoliticasBR$`Publicação do Decreto`) - as.Date(PoliticasBR$`1º Caso de COVID-19`))
PoliticasBR$Distância0 <- as.numeric(PoliticasBR$Distância0)

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
    if(v$Medidas[i] == "Suspensão de eventos"){
      Eventos[j,1] <- v$Siglas[i]
      Eventos[j,2] <- v$Estado[i]
      Eventos[j,3] <- v$`Publicação do Decreto`[i]
      Eventos[j,4] <- v$Medidas[i]
      Eventos[j,5] <- v$`1º Caso de COVID-19`[i]
      Eventos[j,6] <- v$`Casos Confirmados`[i]
      Eventos[j,7] <- v$`Registro do 1º dia com óbitos`[i]
      Eventos[j,8] <- v$`Número de óbitos`[i]
      Eventos[j,9] <- v$Distância0[i]
      j = j + 1
    }
    if (v$Medidas[i] == "Suspensão das Aulas"){
      Aulas[l,1] <- v$Siglas[i]
      Aulas[l,2] <- v$Estado[i]
      Aulas[l,3] <- v$`Publicação do Decreto`[i]
      Aulas[l,4] <- v$Medidas[i]
      Aulas[l,5] <- v$`1º Caso de COVID-19`[i]
      Aulas[l,6] <- v$`Casos Confirmados`[i]
      Aulas[l,7] <- v$`Registro do 1º dia com óbitos`[i]
      Aulas[l,8] <- v$`Número de óbitos`[i]
      Aulas[l,9] <- v$Distância0[i]
      l = l + 1
    }
    if (v$Medidas[i] == "Calamidade pública"){
      Calamidade[m,1] <- v$Siglas[i]
      Calamidade[m,2] <- v$Estado[i]
      Calamidade[m,3] <- v$`Publicação do Decreto`[i]
      Calamidade[m,4] <- v$Medidas[i]
      Calamidade[m,5] <- v$`1º Caso de COVID-19`[i]
      Calamidade[m,6] <- v$`Casos Confirmados`[i]
      Calamidade[m,7] <- v$`Registro do 1º dia com óbitos`[i]
      Calamidade[m,8] <- v$`Número de óbitos`[i]
      Calamidade[m,9] <- v$Distância0[i]
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
  colnames(Politicas) <- c("Siglas", "Estado", "Publicação do Decreto",
                   "Medidas", "1º Caso de COVID-19","Casos Confirmados",
                   "Registro do 1º dia com óbitos", "Número de óbitos",
                   "Distância0")
  return(Politicas)
}

PoliticasBR <- Posicao(PoliticasBR)
Rank <- as.data.frame(paste0(rep(1:10,3),"º"))
colnames(Rank) <- "Rank"
PoliticasBR$`Rank` <- Rank$Rank

## Visualização dos gráficos ##
### Gráficos por medidas:

titulo = "Respostas Políticas"

#################### 01 - Suspensão de eventos: #################

# Organizando por medida:

Politicas_SuspensãoEventos <- PoliticasBR %>%
  select(Estado, Distância0, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`,
         Rank) %>%
  filter(Medidas == "Suspensão de eventos")

# Colocando no formato data:

Politicas_SuspensãoEventos$`Publicação do Decreto` <- format(Politicas_SuspensãoEventos$`Publicação do Decreto`,
                                                               "%d/%m/%Y")

Politicas_SuspensãoEventos$`1º Caso de COVID-19`<- format(Politicas_SuspensãoEventos$`1º Caso de COVID-19`,
                                                          "%d/%m/%Y")

Politicas_SuspensãoEventos$`Registro do 1º dia com óbitos` <- as.Date(Politicas_SuspensãoEventos$`Registro do 1º dia com óbitos`)
Politicas_SuspensãoEventos$`Registro do 1º dia com óbitos` <- format(Politicas_SuspensãoEventos$`Registro do 1º dia com óbitos`,
                                                                     "%d/%m/%Y")
# Reclassificando para colocar em ordem crescente:

Politicas_SuspensãoEventos = arrange(Politicas_SuspensãoEventos,
                                     Distância0)

Politicas_SuspensãoEventos$Estado = factor(Politicas_SuspensãoEventos$Estado,
                                           levels = Politicas_SuspensãoEventos$Estado)

Politicas_SuspensãoEventos$Distância0 = as.integer(Politicas_SuspensãoEventos$Distância0)

# Gráfico no ggplot:

Politicas_SuspensãoEventos$Distância = as.character(Politicas_SuspensãoEventos[,2])

SuspensãoEventos <- ggplot(Politicas_SuspensãoEventos,
                           aes(x = Estado,
                               y = Distância0,
                               label = Distância,
                               label1 = Rank,
                               label2 = `Publicação do Decreto`,
                               label3 = `1º Caso de COVID-19`,
                               label4 = `Casos Confirmados`,
                               label5 = `Registro do 1º dia com óbitos`,
                               label6 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           fill = c("seagreen","peru",
                    "seashell4","royalblue2",
                    "orange1","rosybrown1",
                    "mediumaquamarine","tomato",
                    "maroon", "yellowgreen") ,
           aes(fill = Estado)) +
  xlab("") +
  ylab("Distância (em dias) para a confirmação do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

SuspensãoEventos <- ggplotly(SuspensãoEventos,
                             tooltip = c("x","label", "label1",
                                         "label2","label3","label4",
                                         "label5", "label6"))

# Salvando gráfico:

saveRDS(SuspensãoEventos, file = "Suspensão de eventos.rds")

##################### 02 - Suspensão das Aulas: #################

# Organizando por medida:

Politicas_SuspensãoAulas <- PoliticasBR %>%
  select(Estado, Distância0, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`,
         Rank) %>%
  filter(Medidas == "Suspensão das Aulas")

# Colocando no formato data:

Politicas_SuspensãoAulas$`Publicação do Decreto` <- format(Politicas_SuspensãoAulas$`Publicação do Decreto`,
                                                             "%d/%m/%Y")

Politicas_SuspensãoAulas$`1º Caso de COVID-19`<- format(Politicas_SuspensãoAulas$`1º Caso de COVID-19`,
                                                          "%d/%m/%Y")

Politicas_SuspensãoAulas$`Registro do 1º dia com óbitos` <- as.Date(Politicas_SuspensãoAulas$`Registro do 1º dia com óbitos`)
Politicas_SuspensãoAulas$`Registro do 1º dia com óbitos` <- format(Politicas_SuspensãoAulas$`Registro do 1º dia com óbitos`,
                                                                     "%d/%m/%Y")
# Reclassificando para colocar em ordem crescente:

Politicas_SuspensãoAulas = arrange(Politicas_SuspensãoAulas,
                                     Distância0) 

Politicas_SuspensãoAulas$Estado = factor(Politicas_SuspensãoAulas$Estado,
                                           levels = Politicas_SuspensãoAulas$Estado)

Politicas_SuspensãoAulas$Distância0 = as.integer(Politicas_SuspensãoAulas$Distância0)

# Gráfico no ggplot:

Politicas_SuspensãoAulas$Distância = as.character(Politicas_SuspensãoAulas[,2])

SuspensãoAulas <- ggplot(Politicas_SuspensãoAulas,
                           aes(x = Estado,
                               y = Distância0,
                               label = Distância,
                               label1 = Rank,
                               label2 = `Publicação do Decreto`,
                               label3 = `1º Caso de COVID-19`,
                               label4 = `Casos Confirmados`,
                               label5 = `Registro do 1º dia com óbitos`,
                               label6 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           fill = c("seagreen","peru",
                    "orange1","seashell4",
                    "royalblue2","rosybrown1",
                    "mediumaquamarine","tomato",
                    "maroon", "yellowgreen"),
           aes(fill = Estado)) +
  xlab("") +
  ylab("Distância (em dias) para a confirmação do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

SuspensãoAulas <- ggplotly(SuspensãoAulas,
                             tooltip = c("x", "label", "label1",
                                         "label2","label3","label4",
                                         "label5", "label6"))

# Salvando gráfico:

saveRDS(SuspensãoAulas, file = "Suspensão das Aulas.rds")

###################### 03 - Calamidade pública: ###################

# Organizando por medida:

Politicas_Calamidadepública <- PoliticasBR %>%
  select(Estado, Distância0, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`,
         Rank) %>%
  filter(Medidas == "Calamidade pública")

# Colocando no formato data:

Politicas_Calamidadepública$`Publicação do Decreto` <- format(Politicas_Calamidadepública$`Publicação do Decreto`,
                                                           "%d/%m/%Y")

Politicas_Calamidadepública$`1º Caso de COVID-19`<- format(Politicas_Calamidadepública$`1º Caso de COVID-19`,
                                                        "%d/%m/%Y")

Politicas_Calamidadepública$`Registro do 1º dia com óbitos` <- as.Date(Politicas_Calamidadepública$`Registro do 1º dia com óbitos`)
Politicas_Calamidadepública$`Registro do 1º dia com óbitos` <- format(Politicas_Calamidadepública$`Registro do 1º dia com óbitos`,
                                                                   "%d/%m/%Y")
# Reclassificando para colocar em ordem crescente:

Politicas_Calamidadepública = arrange(Politicas_Calamidadepública,
                                   Distância0) 

Politicas_Calamidadepública$Estado = factor(Politicas_Calamidadepública$Estado,
                                         levels = Politicas_Calamidadepública$Estado)

Politicas_Calamidadepública$Distância0 = as.numeric(Politicas_Calamidadepública$Distância0)

# Gráfico no ggplot:

Politicas_Calamidadepública$Distância = as.character(Politicas_Calamidadepública[,2])

Calamidadepública <- ggplot(Politicas_Calamidadepública,
                           aes(x = Estado,
                               y = Distância0,
                               label = Distância,
                               label1 = Rank,
                               label2 = `Publicação do Decreto`,
                               label3= `1º Caso de COVID-19`,
                               label4 = `Casos Confirmados`,
                               label5 = `Registro do 1º dia com óbitos`,
                               label6 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           fill = c("seagreen","peru",
                    "rosybrown1","seashell4",
                    "royalblue2","tomato",
                    "mediumaquamarine","orange1",
                    "yellowgreen","maroon"),
           aes(fill = Estado)) +
  xlab("") +
  ylab("Distância (em dias) para a confirmação do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

Calamidadepública <- ggplotly(Calamidadepública,
                           tooltip = c("x", "label", "label1",
                                       "label2","label3","label4",
                                       "label5", "label6"))
# Salvando gráfico:

saveRDS(Calamidadepública, file = "Calamidade pública.rds")
