# Politicas Publicas - Estado do Rio de Janeiro

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


## Leitura dos Dados - Politicas Publicas nos municipios do RJ 

url <- "https://github.com/JuliaHellenFerreira/Covid-19-UFF/blob/master/EventosCOVIDMunicipiosUFs.xlsx?raw=true"
destfile <- "EventosCOVIDMunicipiosUFs.xlsx"
curl::curl_download(url, destfile)
PoliticasRJ <- read_excel(destfile)

## Importacao - Casos confirmados e obitos do RJ

Link = "https://sites.google.com/site/portaldadosuffcontracovid/home"
File = "GET.UFF.READ.RData"
download.file(url = paste0(Link,"/",File), destfile = File)
load(File) 
GET.UFF.READ(c("CASOS.CONFIRMADOS.RJ.R","OBITOS.RJ.R"))

## Data da primeira medida de cada municipio

###################### Medidas de cada municipio ########################

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

####################### Funcao para separar por medidas #############################

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

## Data do primeiro caso confirmado:

################## Funcao para encontrar o primeiro caso confirmado ###############################

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


####################### Filtrando os municipios que ire usar ##########################################

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

## Número de obitos de cada município

N_Obitos <- function(x){
  Munic <- c("Rio de Janeiro/RJ", "Niterói/RJ", "São Gonçalo/RJ",
           "Mesquita/RJ", "Belford Roxo/RJ", "Volta Redonda/RJ",
           "Itaboraí/RJ", "São João de Meriti/RJ", "Duque de Caxias/RJ",
           "Nova Iguaçu/RJ")
  obitos <- data.frame(Municipio = c(NULL),
                       Obitos = c(NULL))
  for (i in 1:length(Munic)){
    Muni <- Munic[i]
    total <- sum(as.numeric(OBITOS.RJ[Muni,]))
    obitos[i,1] <- rownames(x)[i]
    obitos[i,2] <- total
  }
  colnames(obitos)<- c("Municipios", "Obitos")
  return(obitos)
}

Total_Obitos <- N_Obitos(OBITOS.RJ)

## Criando um dataframe com os casos confirmados e a data da primeira medida

PoliticasPublicas <- inner_join(municipio_rj, RJ_PrimeiroCaso, by = "Municipio")

## Distancia (em dias) ate o primeiro caso confirmado

PoliticasPublicas$Distancia <- (as.Date(PoliticasPublicas$Dia_Decreto) - as.Date(PoliticasPublicas$Primeiro_Caso))
PoliticasPublicas$Distancia <- as.numeric(PoliticasPublicas$Distancia)
View(PoliticasPublicas)

## Visualização dos gráficos

##################################### Mensagem Interativa ############################



###################################### Gráficos por medidas ##########################

Politicas_MedidasPrevencao <- PoliticasPublicas %>%
  select(Municipio, Distancia, Medidas) %>%
  filter(Medidas == "Medidas de Prevenção")

PoliticasGrafico_MP <- plot_ly(Politicas_MedidasPrevencao, 
                            x = Politicas_MedidasPrevencao$Municipio,
                            y = Politicas_MedidasPrevencao$Distancia,
                            type = "bar",
                            marker = list(color = c("blue", "pink","yellow","green",
                                                    "violetred1","orange","red","yellowgreen",
                                                    "snow","tan1")))
PoliticasGrafico_MP <- PoliticasGrafico_MP %>% 
  layout(title = "Comparação dos tempos de reação à epidemia",
                      xaxis = list(title = "Municípios"),
                      yaxis = list(title = "Distância (em dias) até 1º caso confirmado"))
PoliticasGrafico_MP




