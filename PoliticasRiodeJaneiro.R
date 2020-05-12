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
  filter(Municipio == "Itaboraí" | 
           Municipio == "Volta Redonda" |
           Municipio == "Niterói" |
           Municipio == "Rio de Janeiro" |
           Municipio == "São João de Meriti" |
           Municipio == "Mesquita" |
           Municipio == "Nova Iguaçu" |
           Municipio == "Duque de Caxias" |
           Municipio == "São Gonçalo" |
           Municipio == "Belford Roxo",
         Classificacao != "Outros") %>%
  arrange(Inicio)

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
  Mun <- c("Rio de Janeiro", "Niterói", "São Gonçalo",
           "Mesquita", "Belford Roxo", "Volta Redonda",
           "Itaboraí", "São João de Meriti", "Duque de Caxias",
           "Nova Iguaçu")
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
      loc <- which(Medidas$Municipio == Muni &
                     Medidas$Classificacao == Medi)
      if (length(loc) != 0 ){
        i = i + 1
        lin <- loc[1]
        Medida[i,1] <- x[lin,1]
        Medida[i,2] <- x[lin,2]
        Medida[i,3] <- x[lin,3]
      }
    }  
  }
  colnames(Medida) <- c("Municipio", "Dia do Decreto", "Medidas")
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
  colnames(ocor) <- c("Municipio", "Ocorrencia")
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



## Criando um dataframe com os casos confirmados e a data da primeira medida



## Distancia (em dias) ate o primeiro caso confirmado

DADOS$Distancia <- with(DADOS, as.Date(DATA_FIM, "%d/%m/%Y") - as.Date(DATA_INICIO, "%d/%m/%Y"))
DADOS

# Grafico - Em andamento 

Municipios <- plot_ly(Politicas_RiodeJaneiro, 
                      x = "Municipio",
                      y = "Distancia",
                      type = "bar")
