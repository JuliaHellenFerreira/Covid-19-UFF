# GET UFF contra o COVID-19

# Politicas Publicas - Estado do Rio de Janeiro
###### Este script tem como objetivo calcular a distancia, em dias,
###### do primeiro caso confirmado ate um determinado decreto de 
###### cada municipio do estado do Rio de Janeiro.


## Pacotes ##

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

## Leitura dos Dados - Politicas Publicas nos municipios do RJ ##

url <- "https://github.com/JuliaHellenFerreira/Covid-19-UFF/blob/master/EventosCOVIDMunicipiosUFs.xlsx?raw=true"
destfile <- "EventosCOVIDMunicipiosUFs.xlsx"
curl::curl_download(url, destfile)
PoliticasRJ <- read_excel(destfile)

## Importacao - Casos confirmados e obitos do RJ ##

Link = "https://sites.google.com/site/portaldadosuffcontracovid/home"
File = "GET.UFF.READ.RData"
download.file(url = paste0(Link,"/",File), destfile = File)
load(File) 
GET.UFF.READ(c("CASOS.CONFIRMADOS.RJ.R","OBITOS.RJ.R"))

## Data da primeira medida de cada municipio ##

# Medidas de cada municipio:

PoliticasRJ$Início <- as.Date(PoliticasRJ$Início)
Medidas <- PoliticasRJ %>%
  select(`Estado/Municípios`, Início, Classificação) %>%
  filter(`Estado/Municípios` == "Itaboraí/RJ" | 
           `Estado/Municípios` == "Volta Redonda/RJ" |
           `Estado/Municípios` == "Niterói/RJ" |
           `Estado/Municípios` == "Rio de Janeiro/RJ" |
           `Estado/Municípios` == "São João de Meriti/RJ" |
           `Estado/Municípios` == "Mesquita/RJ" |
           `Estado/Municípios` == "Nova Iguaçu/RJ" |
           `Estado/Municípios` == "Duque de Caxias/RJ" |
           `Estado/Municípios` == "São Gonçalo/RJ" |
           `Estado/Municípios` == "Belford Roxo/RJ",
         Classificação != "Outros") %>%
  arrange(Início)

# Funcao para separar por medidas:

PrimeiraMedida <- function(x){
  Med <- c("Instalações Hospitalares", "Medidas de Prevenção",
           "Gabinete de Crise", "Situação de Emergência",
           "Flexibilização de funcionamento dos comércios", 
           "Fechamento de comércio", "Servidores em grupo de risco",
           "Calamidade Pública", "Funcionamento do transporte",
           "Regras de Isolamento", "Funcionamento de supermercados",
           "Disk Aglomeração", "Cestas Básica",
           "Circulação de Acesso", "Suspensão de aulas",
           "Auxílio Emergencial", "Uso de Máscara",
           "Fundo de Crédito Emergencial", "Fechamento de feiras livres",
           "Procedimentos em funerárias")
  Mun <- c("Rio de Janeiro/RJ", "Niterói/RJ", "São Gonçalo/RJ",
           "Mesquita/RJ", "Belford Roxo/RJ", "Volta Redonda/RJ",
           "Itaboraí/RJ", "São João de Meriti/RJ", "Duque de Caxias/RJ",
           "Nova Iguaçu/RJ")
  linhas <- nrow(x)
  colunas <- ncol(x)
  Medida <- data.frame(Municipio = c(NULL),
                       Ocorrencia = c(NULL),
                       Medida = c(NULL))
  i <- 0
  for (l in 1:length(Med)){
    Medi <- Med[l]
    loc <- NULL
    for (k in 1: length(Mun)){
      Muni <- Mun[k]
      loc <- which(x$`Estado/Municípios` == Muni &
                     x$Classificação == Medi)
      if (length(loc) != 0 ){
        i = i + 1
        lin <- loc[1]
        Medida[i,1] <- x[lin,1]
        Medida[i,2] <- x[lin,2]
        Medida[i,3] <- x[lin,3]
      }
    }  
  }
  colnames(Medida) <- c("Municipio", "Dia_Decreto", "Medidas")
  return(Medida)
}

municipio_rj <- PrimeiraMedida(Medidas)

## Data do primeiro caso confirmado ##

# Funcao para encontrar o primeiro caso confirmado: 

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
  colnames(ocor) <- c("Municipio", "Primeiro_Caso")
  return(ocor)
}


# Filtrando os municipios que irei usar:

CASOS.CONFIRMADOS.RJ$Municipio = rownames(CASOS.CONFIRMADOS.RJ)
Municipios_PrimeiroCaso <- PrimeiroCaso(CASOS.CONFIRMADOS.RJ)
RJ_PrimeiroCaso <- Municipios_PrimeiroCaso %>%
  filter(Municipio == "Itaboraí/RJ" | 
           Municipio == "Volta Redonda/RJ" |
           Municipio == "Niterói/RJ" |
           Municipio == "Rio de Janeiro/RJ" |
           Municipio == "São João de Meriti/RJ" |
           Municipio == "Mesquita/RJ" |
           Municipio == "Nova Iguaçu/RJ" |
           Municipio == "Duque de Caxias/RJ" |
           Municipio == "São Gonçalo/RJ" |
           Municipio == "Belford Roxo/RJ")

# Criando um data.frame com todos os dado ##

PoliticasPublicas <- inner_join(municipio_rj, RJ_PrimeiroCaso,
                                by = "Municipio")

# Numero de casos confirmados ate o dia do decreto:

CasosConfirmados <- function(x){
  Municip <- as.list(x$Municipio)
  Medid <- as.list(x$Medidas)
  Dias <- as.list(x$Dia_Decreto)
  CasosRJ <- data.frame(Municipio = c(NULL),
                        Medida = c(NULL),
                        Casos = c(NULL))
  for (i in 1: length(Municip)){
    Muni <- Municip[[i]][1]
    Med <- Medid[[i]][1]
    Dia <- as.character.Date(Dias[[i]][1])
    Cas <- as.numeric(CASOS.CONFIRMADOS.RJ[Muni,Dia])
    CasosRJ[i,1] <- Municip[[i]][1]
    CasosRJ[i,2] <- Medid[[i]][1]
    CasosRJ[i,3] <- Cas
  }
  colnames(CasosRJ) <- c("Municipio","Medidas","Casos")
  return(CasosRJ)
}

Total_Casos <- CasosConfirmados(PoliticasPublicas)

# Numero de obitos ate o dia do decreto:

Obitos <- function(x){
  Municip <- as.list(x$Municipio)
  Medid <- as.list(x$Medidas)
  Dias <- as.list(x$Dia_Decreto)
  ObitosRJ <- data.frame(Municipio = c(NULL),
                        Medida = c(NULL),
                        Obitos = c(NULL))
  for (i in 1: length(Municip)){
    Muni <- Municip[[i]][1]
    Med <- Medid[[i]][1]
    Dia <- as.character.Date(Dias[[i]][1])
    Obi <- as.character(OBITOS.RJ[Muni,Dia])
    Obi <- as.numeric(Obi)
    ObitosRJ[i,1] <- Municip[[i]][1]
    ObitosRJ[i,2] <- Medid[[i]][1]
    ObitosRJ[i,3] <- Obi
  }
  colnames(ObitosRJ) <- c("Municipio","Medidas","Óbitos")
  return(ObitosRJ)
}

Total_Obitos <- Obitos(PoliticasPublicas)

## Acrescentando o total de casos confirmados e obitos: - erro

PoliticasPublicas$`Total de Casos` = Total_Casos$Casos
PoliticasPublicas$`Total de Óbitos` = Total_Obitos$Óbitos

# Distancia (em dias) ate o primeiro caso confirmado:

PoliticasPublicas$Distancia <- (as.Date(PoliticasPublicas$Dia_Decreto) - as.Date(PoliticasPublicas$Primeiro_Caso))
PoliticasPublicas$Distancia <- as.numeric(PoliticasPublicas$Distancia)
View(PoliticasPublicas)

## Visualização dos gráficos ##

### Gráficos por medidas:

titulo = "Respostas Políticas"

# 01 - Medidas de Prevencao:

medida1 <- "- Medidas de Prevenção"

Politicas_MedidasPrevencao <- PoliticasPublicas %>%
  select(Municipio, Distancia, Medidas, `Total de Casos`, `Total de Óbitos`) %>%
  filter(Medidas == "Medidas de Prevenção")

MedidasPrevencao <- ggplot(Politicas_MedidasPrevencao,
                           aes(x = Municipio,
                               y = Distancia,
                               label = `Total de Casos`,
                               label1 = `Total de Óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           aes(fill = Municipio)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(paste(titulo, medida1)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

MedidasPrevencao <- ggplotly(MedidasPrevencao,
                             tooltip = c("x", "y", "label", "label1"))

saveRDS(MedidasPrevencao, file = "Medidas de Prevenção.rds")

# 02 - Situacao de Emergencia:

medida2 <- "- Situaçao de Emergência"

Politicas_SE <- PoliticasPublicas %>%
  select(Municipio, Distancia, Medidas, `Total de Casos`, `Total de Óbitos`) %>%
  filter(Medidas == "Situação de Emergência")

SituacaoEmegencia <- ggplot(Politicas_SE,
                            aes(x = Municipio,
                                y = Distancia,
                                label = `Total de Casos`,
                                label1 = `Total de Óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           aes(fill = Municipio)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(paste(titulo, medida2)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size=10),
        legend.position = "none")

SituacaoEmegencia <- ggplotly(SituacaoEmegencia,
                              tooltip = c("x", "y", "label", "label1"))

saveRDS(MedidasPrevencao, file = "Situação de Emergência.rds")

# 03 - Fechamento de comércio:

medida3 <- "- Fechamento de comércio"

Politicas_FC <- PoliticasPublicas %>%
  select(Municipio, Distancia, Medidas, `Total de Casos`, `Total de Óbitos`) %>%
  filter(Medidas == "Fechamento de comércio")

Comercio <- ggplot(Politicas_FC,
                   aes(x = Municipio,
                       y = Distancia,
                       label = `Total de Casos`,
                       label1 = `Total de Óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           aes(fill = Municipio)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(paste(titulo, medida3)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

Comercio <- ggplotly(Comercio,
                     tooltip = c("x", "y", "label", "label1"))

saveRDS(MedidasPrevencao, file = "Fechamento de comércio.rds")

# 04 - Uso de Máscara:

medida4 <- "- Uso de Máscara"

Mascara <- PoliticasPublicas %>%
  select(Municipio, Distancia, Medidas, `Total de Casos`, `Total de Óbitos`) %>%
  filter(Medidas == "Uso de Máscara")

Mascara <- ggplot(Mascara,
                  aes(x = Municipio,
                      y = Distancia,
                      label = `Total de Casos`,
                      label1 = `Total de Óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           aes(fill = Municipio)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(paste(titulo, medida4)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

Mascara <- ggplotly(Mascara,
                    tooltip = c("x", "y", "label", "label1"))

saveRDS(MedidasPrevencao, file = "Uso de Máscara.rds")




