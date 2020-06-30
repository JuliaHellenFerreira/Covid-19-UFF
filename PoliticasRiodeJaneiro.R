# GET UFF contra o COVID-19

# Politicas Publicas - Estado do Rio de Janeiro
###### Este script tem como objetivo calcular a distancia, em dias,
###### do primeiro caso confirmado ate um determinado decreto de 
###### cada municipio do estado do Rio de Janeiro.


## Pacotes ##

pacotes <- c("magrittr","knitr","tidyr","dplyr", "lubridate","plotly",
             "devtools", "readxl", "stringr", "magick")
for(pacote in pacotes){
  if(!is.element(pacote,installed.packages())){install.packages(pacote)}
}

library(lubridate)
library(magrittr)
library(knitr)
library(dplyr)
library(plotly)
library(devtools)
library(readxl)
library(ggplot2)
library(tidyr)
library(stringr)
library(magick)


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

# Medidas de cada munícipio:

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
  Med <- c("Trabalho", "Educação", "Transporte", "Prevenção",
          "Equipamento de Proteção", "Cultura", "Isolamento Social",
          "Hospital", "Economia", "Flexibilização", "Profissional da Saúde",
          "Comunicação", "Emergência de Saúde Pública", "Declaração de Calamidade Pública",
          "Estado de Emergência", "Comércio", "Outros"
           )
  Mun <- c("Rio de Janeiro/RJ", "Niterói/RJ", "São Gonçalo/RJ",
           "Mesquita/RJ", "Belford Roxo/RJ", "Volta Redonda/RJ",
           "Itaboraí/RJ", "São João de Meriti/RJ", "Duque de Caxias/RJ",
           "Nova Iguaçu/RJ")
  linhas <- nrow(x)
  colunas <- ncol(x)
  Medida <- data.frame(Município = c(NULL),
                       Ocorrência = c(NULL),
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
  colnames(Medida) <- c("Município", "Publicação do Decreto", "Medidas")
  return(Medida)
}

municipio_rj <- PrimeiraMedida(Medidas)

## Data do primeiro caso confirmado ##

PrimeiroCaso <- function(x){
  linhas <- nrow(x)
  colunas <- ncol(x)
  ocor <- data.frame(Município = c(NULL),
                     Ocorrência = c(NULL))
  for (i in 1:linhas){
    for (j in  2:colunas){
      if (x[i,j] != 0){
        ocor[i,1] = rownames(x)[i]
        ocor [i,2] = colnames(x)[j]
        break
      }
    }
  }
  colnames(ocor) <- c("Município", "1º Caso de COVID-19")
  return(ocor)
}

# Filtrando os municipios que irei usar:

CASOS.CONFIRMADOS.RJ$Municipio = rownames(CASOS.CONFIRMADOS.RJ)
Municipios_PrimeiroCaso <- PrimeiroCaso(CASOS.CONFIRMADOS.RJ)
RJ_PrimeiroCaso <- Municipios_PrimeiroCaso %>%
  filter(Município == "Itaboraí/RJ" | 
           Município == "Volta Redonda/RJ" |
           Município == "Niterói/RJ" |
           Município == "Rio de Janeiro/RJ" |
           Município == "São João de Meriti/RJ" |
           Município == "Mesquita/RJ" |
           Município == "Nova Iguaçu/RJ" |
           Município == "Duque de Caxias/RJ" |
           Município == "São Gonçalo/RJ" |
           Município == "Belford Roxo/RJ")

# Criando um data.frame com todos os dado ##

PoliticasPublicas <- inner_join(municipio_rj, RJ_PrimeiroCaso,
                                by = "Município")

# Numero de casos confirmados até o dia do decreto:

CasosConfirmados <- function(x){
  Municip <- as.list(x$Município)
  Medid <- as.list(x$Medidas)
  Dias <- as.list(x$`Publicação do Decreto`)
  CasosRJ <- data.frame(Município = c(NULL),
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
  colnames(CasosRJ) <- c("Município","Medidas","Casos")
  return(CasosRJ)
}

Total_Casos <- CasosConfirmados(PoliticasPublicas)

# Numero de óbitos até o dia do decreto:

Obitos <- function(x){
  Municip <- as.list(x$Município)
  Medid <- as.list(x$Medidas)
  Dias <- as.list(x$`Publicação do Decreto`)
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

## Acrescentando o total de casos confirmados e obitos:

PoliticasPublicas$`Casos Confirmados` = Total_Casos$Casos
PoliticasPublicas$`Número de óbitos` = Total_Obitos$Óbitos

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
  colnames(ocor) <- c("Município", "Registro do 1º dia com óbitos")
  return(ocor)
}

Dia_Obito <- PrimeiroObito(OBITOS.RJ)
Dia_Obito <- Dia_Obito %>% 
  select(Município, `Registro do 1º dia com óbitos`) %>% 
  filter(Município == "Itaboraí/RJ" | 
           Município == "Volta Redonda/RJ" |
           Município == "Niterói/RJ" |
           Município == "Rio de Janeiro/RJ" |
           Município == "São João de Meriti/RJ" |
           Município == "Mesquita/RJ" |
           Município == "Nova Iguaçu/RJ" |
           Município == "Duque de Caxias/RJ" |
           Município == "São Gonçalo/RJ" |
           Município == "Belford Roxo/RJ")

PoliticasPublicas <- inner_join(PoliticasPublicas, Dia_Obito,
                          by = "Município")

# Distancia (em dias) ate o primeiro caso confirmado:

PoliticasPublicas$Distância0 <- (as.Date(PoliticasPublicas$`Publicação do Decreto`) - as.Date(PoliticasPublicas$`1º Caso de COVID-19`))
PoliticasPublicas$Distância0 <- as.numeric(PoliticasPublicas$Distância0)

## Acrescentando linhas de municípios que não publicaram decretos:

PoliticasPublicas <- PoliticasPublicas %>%
  select(Município, Distância0, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`) %>%
  filter(Medidas == "Trabalho" | 
        Medidas == "Prevenção" |
        Medidas == "Equipamento de Proteção" |
        Medidas == "Isolamento Social" |
        Medidas == "Hospital" | 
        Medidas == "Economia" |
        Medidas == "Flexibilização" |
        Medidas == "Emergência de Saúde Pública"
        )

# Trabalho:

PoliticasPublicas[nrow(PoliticasPublicas) + 5, "Município"] <- ""
PoliticasPublicas[38,1] <-"São João de Meriti/RJ"
PoliticasPublicas[38,3] <-"Trabalho"
PoliticasPublicas[39,1] <-"Nova Iguaçu/RJ"
PoliticasPublicas[39,3] <-"Trabalho"
PoliticasPublicas[40,1] <-"Duque de Caxias/RJ"
PoliticasPublicas[40,3] <-"Trabalho"
PoliticasPublicas[41,1] <-"São Gonçalo/RJ"
PoliticasPublicas[41,3] <-"Trabalho"
PoliticasPublicas[42,1] <-"Belford Roxo/RJ"
PoliticasPublicas[42,3] <-"Trabalho"

# Prevenção:

PoliticasPublicas[nrow(PoliticasPublicas) + 3, "Município"] <- ""
PoliticasPublicas[43,1] <-"Nova Iguaçu/RJ"
PoliticasPublicas[43,3] <-"Prevenção"
PoliticasPublicas[44,1] <-"Duque de Caxias/RJ"
PoliticasPublicas[44,3] <-"Prevenção"
PoliticasPublicas[45,1] <-"Belford Roxo/RJ"
PoliticasPublicas[45,3] <-"Prevenção"

# Equipamento de Proteção:

PoliticasPublicas[nrow(PoliticasPublicas) + 6, "Município"] <- ""
PoliticasPublicas[46,1] <-"Itaboraí/RJ"
PoliticasPublicas[46,3] <-"Equipamento de Proteção"
PoliticasPublicas[47,1] <-"Mesquita/RJ"
PoliticasPublicas[47,3] <-"Equipamento de Proteção"
PoliticasPublicas[48,1] <-"Nova Iguaçu/RJ"
PoliticasPublicas[48,3] <-"Equipamento de Proteção"
PoliticasPublicas[49,1] <-"Duque de Caxias/RJ"
PoliticasPublicas[49,3] <-"Equipamento de Proteção"
PoliticasPublicas[50,1] <-"São Gonçalo/RJ"
PoliticasPublicas[50,3] <-"Equipamento de Proteção"
PoliticasPublicas[51,1] <-"Belford Roxo/RJ"
PoliticasPublicas[51,3] <-"Equipamento de Proteção"

# Isolamento Social:

PoliticasPublicas[nrow(PoliticasPublicas) + 7, "Município"] <- ""
PoliticasPublicas[52,1] <-"Itaboraí/RJ"
PoliticasPublicas[52,3] <-"Isolamento Social"
PoliticasPublicas[53,1] <-"São João de Meriti/RJ"
PoliticasPublicas[53,3] <-"Isolamento Social"
PoliticasPublicas[54,1] <-"Mesquita/RJ"
PoliticasPublicas[54,3] <-"Isolamento Social"
PoliticasPublicas[55,1] <-"Nova Iguaçu/RJ"
PoliticasPublicas[55,3] <-"Isolamento Social"
PoliticasPublicas[56,1] <-"Duque de Caxias/RJ"
PoliticasPublicas[56,3] <-"Isolamento Social"
PoliticasPublicas[57,1] <-"São Gonçalo/RJ"
PoliticasPublicas[57,3] <-"Isolamento Social"
PoliticasPublicas[58,1] <-"Belford Roxo/RJ"
PoliticasPublicas[58,3] <-"Isolamento Social"

# Hospital:

PoliticasPublicas[nrow(PoliticasPublicas) + 6, "Município"] <- ""
PoliticasPublicas[59,1] <-"Itaboraí/RJ"
PoliticasPublicas[59,3] <-"Hospital"
PoliticasPublicas[60,1] <-"São João de Merti/RJ"
PoliticasPublicas[60,3] <-"Hospital"
PoliticasPublicas[61,1] <-"Mesquita/RJ"
PoliticasPublicas[61,3] <-"Hospital"
PoliticasPublicas[62,1] <-"Nova Iguaçu/RJ"
PoliticasPublicas[62,3] <-"Hospital"
PoliticasPublicas[63,1] <-"Duque de Caxias/RJ"
PoliticasPublicas[63,3] <-"Hospital"
PoliticasPublicas[64,1] <-"Belford Roxo/RJ"
PoliticasPublicas[64,3] <-"Hospital"

# Economia:

PoliticasPublicas[nrow(PoliticasPublicas) + 5, "Município"] <- ""
PoliticasPublicas[65,1] <-"São João de Meriti/RJ"
PoliticasPublicas[65,3] <-"Economia"
PoliticasPublicas[66,1] <-"Mesquita/RJ"
PoliticasPublicas[66,3] <-"Economia"
PoliticasPublicas[67,1] <-"Nova Iguaçu/RJ"
PoliticasPublicas[67,3] <-"Economia"
PoliticasPublicas[68,1] <-"Duque de Caxias/RJ"
PoliticasPublicas[68,3] <-"Economia"
PoliticasPublicas[69,1] <-"Belford Roxo/RJ"
PoliticasPublicas[69,3] <-"Economia"

# Flexibilização:

PoliticasPublicas[nrow(PoliticasPublicas) + 7, "Município"] <- ""
PoliticasPublicas[70,1] <-"Itaboraí/RJ"
PoliticasPublicas[70,3] <-"Flexibilização"
PoliticasPublicas[71,1] <-"São João de Meriti/RJ"
PoliticasPublicas[71,3] <-"Flexibilização"
PoliticasPublicas[72,1] <-"Mesquita/RJ"
PoliticasPublicas[72,3] <-"Flexibilização"
PoliticasPublicas[73,1] <-"Nova iguaçu/RJ"
PoliticasPublicas[73,3] <-"Flexibilização"
PoliticasPublicas[74,1] <-"Duque de Caxias/RJ"
PoliticasPublicas[74,3] <-"Flexibilização"
PoliticasPublicas[75,1] <-"São Gonçalo/RJ"
PoliticasPublicas[75,3] <-"Flexibilização"
PoliticasPublicas[76,1] <-"Belford Roxo/RJ"
PoliticasPublicas[76,3] <-"Flexibilização"

# Emergência de Saúde Pública

PoliticasPublicas[nrow(PoliticasPublicas) + 4, "Município"] <- ""
PoliticasPublicas[77,1] <-"Volta Redonda/RJ"
PoliticasPublicas[77,3] <-"Emergência de Saúde Pública"
PoliticasPublicas[78,1] <-"São João de Meriti/RJ"
PoliticasPublicas[78,3] <-"Emergência de Saúde Pública"
PoliticasPublicas[79,1] <-"Duque de Caxias/RJ"
PoliticasPublicas[79,3] <-"Emergência de Saúde Pública"
PoliticasPublicas[80,1] <-"Belford Roxo/RJ"
PoliticasPublicas[80,3] <-"Emergência de Saúde Pública"

# Ranking:

Trabalho <- PoliticasPublicas %>% 
  select(Município, everything()) %>% 
  filter(Medidas == "Trabalho") %>% 
  arrange(Distância0)

Prevenção <- PoliticasPublicas %>% 
  select(Município, everything()) %>% 
  filter(Medidas == "Prevenção") %>% 
  arrange(Distância0)

Equipamento <- PoliticasPublicas %>% 
  select(Município, everything()) %>% 
  filter(Medidas == "Equipamento de Proteção") %>% 
  arrange(Distância0)

Isolamento <- PoliticasPublicas %>% 
  select(Município, everything()) %>% 
  filter(Medidas == "Isolamento Social") %>% 
  arrange(Distância0)

Hospital <- PoliticasPublicas %>% 
  select(Município, everything()) %>% 
  filter(Medidas == "Hospital") %>% 
  arrange(Distância0)

Economia <- PoliticasPublicas %>% 
  select(Município, everything()) %>% 
  filter(Medidas == "Economia") %>% 
  arrange(Distância0)

Flexibilização <- PoliticasPublicas %>% 
  select(Município, everything()) %>% 
  filter(Medidas == "Flexibilização") %>% 
  arrange(Distância0)

Emergência <- PoliticasPublicas %>% 
  select(Município, everything()) %>% 
  filter(Medidas == "Emergência de Saúde Pública") %>% 
  arrange(Distância0)

PoliticasPublicas <- bind_rows(Trabalho, Prevenção, Equipamento, Isolamento, 
                       Hospital, Economia, Flexibilização, Emergência)

Rank <- as.data.frame(paste0(rep(1:10,8),"º"))
colnames(Rank) <- "Rank"
PoliticasPublicas$`Rank` <- Rank$Rank

# Retirando o "/RJ" dos nomes:

PoliticasPublicas$Município <-  str_sub(PoliticasPublicas$Município, end = -4)

## Visualização dos gráficos ##
### Gráficos por medidas:

titulo = "Respostas Políticas"

image <- image_fill(image_read("logo_get_uff_covid.png"), 'none')
raster <- as.raster(image)

########################### 01 - Trabalho ################################

Politicas_Trabalho <- PoliticasPublicas %>%
  select(Município, Distância0, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`, Rank) %>%
  filter(Medidas == "Trabalho")

# Colocando no formato data:

Politicas_Trabalho$`Publicação do Decreto` <- format(Politicas_Trabalho$`Publicação do Decreto`,
                                                             "%d/%m/%Y")

Politicas_Trabalho$`1º Caso de COVID-19`<- as.Date(Politicas_Trabalho$`1º Caso de COVID-19`)
Politicas_Trabalho$`1º Caso de COVID-19`<- format(Politicas_Trabalho$`1º Caso de COVID-19`,
                                                          "%d/%m/%Y")

Politicas_Trabalho$`Registro do 1º dia com óbitos` <- as.Date(Politicas_Trabalho$`Registro do 1º dia com óbitos`)
Politicas_Trabalho$`Registro do 1º dia com óbitos` <- format(Politicas_Trabalho$`Registro do 1º dia com óbitos`,
                                                                     "%d/%m/%Y")

# Reclassificando para colocar em ordem crescente:

Politicas_Trabalho = arrange(Politicas_Trabalho,
                                     Distância0)

Politicas_Trabalho <- Politicas_Trabalho %>% 
  mutate_all(replace_na, 0)

Politicas_Trabalho$Município = factor(Politicas_Trabalho$Município,
                                              levels = Politicas_Trabalho$Município)

Politicas_Trabalho$Distância0 = as.integer(Politicas_Trabalho$Distância0)

# Gráfico no ggplot:

Politicas_Trabalho$Distância = as.character(Politicas_Trabalho[,2])

Trabalho <- ggplot(Politicas_Trabalho,
                           aes(x = Município,
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
                   "maroon", "yellowgreen"),
           aes(fill = Município)) +
  xlab("") +
  ylab("Distância (em dias) para a confirmação do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

Trabalho <- ggplotly(Trabalho,
                             tooltip = c("x","label", "label1",
                                         "label2","label3","label4",
                                         "label5", "label6"))
# Acrescentando LOGO:

Trabalho <- Trabalho %>% 
  layout(images = list(list(source = raster2uri(raster),
                            xref = "paper",
                            yref = "paper",
                            x= 0,
                            y= 0.25,
                            sizex = 0.4,
                            sizey = 0.4,
                            opacity = 0.8
  )
  )
  )

# Salvando gráfico:

saveRDS(Trabalho, file = "Trabalho.rds")

############################ 02 - Prevenção ##############################

Politicas_Prevenção <- PoliticasPublicas %>%
  select(Município, Distância0, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`, Rank) %>%
  filter(Medidas == "Prevenção")

# Colocando no formato data:

Politicas_Prevenção$`Publicação do Decreto` <- format(Politicas_Prevenção$`Publicação do Decreto`,
                                              "%d/%m/%Y")

Politicas_Prevenção$`1º Caso de COVID-19`<- as.Date(Politicas_Prevenção$`1º Caso de COVID-19`)
Politicas_Prevenção$`1º Caso de COVID-19`<- format(Politicas_Prevenção$`1º Caso de COVID-19`,
                                            "%d/%m/%Y")

Politicas_Prevenção$`Registro do 1º dia com óbitos` <- as.Date(Politicas_Prevenção$`Registro do 1º dia com óbitos`)
Politicas_Prevenção$`Registro do 1º dia com óbitos` <- format(Politicas_Prevenção$`Registro do 1º dia com óbitos`,
                                                                     "%d/%m/%Y")

# Reclassificando para colocar em ordem crescente:

Politicas_Prevenção = arrange(Politicas_Prevenção, Distância0)

Politicas_Prevenção <- Politicas_Prevenção %>% 
  mutate_all(replace_na, 0)

Politicas_Prevenção$Município = factor(Politicas_Prevenção$Município,
                                levels = Politicas_Prevenção$Município)

Politicas_Prevenção$Distância0 = as.integer(Politicas_Prevenção$Distância0)

# Gráfico no ggplot:

Politicas_Prevenção$Distância = as.character(Politicas_Prevenção[,2])

Prevenção <- ggplot(Politicas_Prevenção,
                            aes(x = Município,
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
           fill = c("royalblue2", "maroon",
                    "seagreen", "peru",
                    "seashell4", "orange1",
                    "rosybrown1","tomato",
                    "maroon", "yellowgreen"),
           aes(fill = Município)) +
  xlab("") +
  ylab("Distância (em dias) para a confirmação do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size=10),
        legend.position = "none")

# Tornando o gráfico interativo:

Prevenção <- ggplotly(Prevenção,
                              tooltip = c("x","label", "label1",
                                          "label2","label3","label4",
                                          "label5", "label6"))
# Acrescentando LOGO:

Prevenção <- Prevenção %>% 
  layout(images = list(list(source = raster2uri(raster),
                            xref = "paper",
                            yref = "paper",
                            x= 0,
                            y= 0.25,
                            sizex = 0.4,
                            sizey = 0.4,
                            opacity = 0.8
  )
  )
  )

# Salvando gráfico:

saveRDS(Prevenção, file = "Prevenção.rds")

######################### 03 - Equipamento de Proteção ###################

Politicas_Equipamento <- PoliticasPublicas %>%
  select(Município, Distância0, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`, Rank) %>%
  filter(Medidas == "Equipamento de Proteção")

# Colocando no formato data:

Politicas_Equipamento$`Publicação do Decreto` <- format(Politicas_Equipamento$`Publicação do Decreto`,
                                               "%d/%m/%Y")

Politicas_Equipamento$`1º Caso de COVID-19`<- as.Date(Politicas_Equipamento$`1º Caso de COVID-19`)
Politicas_Equipamento$`1º Caso de COVID-19`<- format(Politicas_Equipamento$`1º Caso de COVID-19`,
                                            "%d/%m/%Y")

Politicas_Equipamento$`Registro do 1º dia com óbitos` <- as.Date(Politicas_Equipamento$`Registro do 1º dia com óbitos`)
Politicas_Equipamento$`Registro do 1º dia com óbitos` <- format(Politicas_Equipamento$`Registro do 1º dia com óbitos`,
                                                       "%d/%m/%Y")

# Reclassificando para colocar em ordem crescente:

Politicas_Equipamento = arrange(Politicas_Equipamento, Distância0)

Politicas_Equipamento <- Politicas_Equipamento %>% 
  mutate_all(replace_na, 0)

Politicas_Equipamento$Município = factor(Politicas_Equipamento$Município,
                                levels = Politicas_Equipamento$Município)

Politicas_Equipamento$Distância0 = as.integer(Politicas_Equipamento$Distância0)

# Gráfico no ggplot:

Politicas_Equipamento$Distância = as.character(Politicas_Equipamento[,2])

Equipamento <- ggplot(Politicas_Equipamento,
                   aes(x = Município,
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
           fill = c("peru", "rosybrown1",
                    "orange1", "seashell4",
                    "orange1","seagreen",
                    "mediumaquamarine","tomato",
                    "maroon", "yellowgreen"),
           aes(fill = Municipio)) +
  xlab("") +
  ylab("Distância (em dias) para a confirmação do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

Equipamento <- ggplotly(Equipamento,
                     tooltip = c("x","label", "label1",
                                 "label2","label3","label4",
                                 "label5","label6"))
# Acrescentando LOGO:

Equipamento <- Equipamento %>% 
  layout(images = list(list(source = raster2uri(raster),
                            xref = "paper",
                            yref = "paper",
                            x= 0,
                            y= 0.25,
                            sizex = 0.4,
                            sizey = 0.4,
                            opacity = 0.8
  )
  )
  )


# Salvando gráfico:

saveRDS(Equipamento, file = "Equipamento.rds")

########################### 04 - Isolamento Social ########################

Politicas_Isolamento <- PoliticasPublicas %>%
  select(Município, Distância0, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`, Rank) %>%
  filter(Medidas == "Isolamento Social")

# Colocando no formato data:

Politicas_Isolamento$`Publicação do Decreto` <- format(Politicas_Isolamento$`Publicação do Decreto`,
                                          "%d/%m/%Y")

Politicas_Isolamento$`1º Caso de COVID-19`<- as.Date(Politicas_Isolamento$`1º Caso de COVID-19`)
Politicas_Isolamento$`1º Caso de COVID-19`<- format(Politicas_Isolamento$`1º Caso de COVID-19`,
                                       "%d/%m/%Y")

Politicas_Isolamento$`Registro do 1º dia com óbitos` <- as.Date(Politicas_Isolamento$`Registro do 1º dia com óbitos`)
Politicas_Isolamento$`Registro do 1º dia com óbitos` <- format(Politicas_Isolamento$`Registro do 1º dia com óbitos`,
                                                  "%d/%m/%Y")

# Reclassificando para colocar em ordem crescente:

Politicas_Isolamento = arrange(Politicas_Isolamento, Distância0)

Politicas_Isolamento <- Politicas_Isolamento %>% 
  mutate_all(replace_na, 0)

Politicas_Isolamento$Município = factor(Politicas_Isolamento$Município,
                                levels = Politicas_Isolamento$Município)

Politicas_Isolamento$Distância0 = as.integer(Politicas_Isolamento$Distância0)

# Gráfico no ggplot:

Politicas_Isolamento$Distância = as.character(Politicas_Isolamento[,2])

Isolamento <- ggplot(Politicas_Isolamento,
                  aes(x = Município,
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
           fill = c("peru", "orange1",
                    "seashell4", "seashell4",
                    "orange1","seagreen",
                    "mediumaquamarine","tomato",
                    "maroon", "yellowgreen"),
           aes(fill = Municipio)) +
  xlab("") +
  ylab("Distância (em dias) para a confirmação do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

Isolamento <- ggplotly(Isolamento,
                    tooltip = c("x","label", "label1",
                                "label2","label3","label4",
                                "label5", "label6"))
# Acrescentando LOGO:

Isolamento <- Isolamento %>% 
  layout(images = list(list(source = raster2uri(raster),
                            xref = "paper",
                            yref = "paper",
                            x= 0,
                            y= 0.25,
                            sizex = 0.4,
                            sizey = 0.4,
                            opacity = 0.8
  )
  )
  )


# Salvando gráfico:

saveRDS(Isolamento, file = "Isolamento.rds")

########################### 05 - Hospital ###############################

Politicas_Hospital <- PoliticasPublicas %>%
  select(Município, Distância0, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`, Rank) %>%
  filter(Medidas == "Hospital")

# Colocando no formato data:

Politicas_Hospital$`Publicação do Decreto` <- format(Politicas_Hospital$`Publicação do Decreto`,
                                                     "%d/%m/%Y")

Politicas_Hospital$`1º Caso de COVID-19`<- as.Date(Politicas_Hospital$`1º Caso de COVID-19`)
Politicas_Hospital$`1º Caso de COVID-19`<- format(Politicas_Hospital$`1º Caso de COVID-19`,
                                                  "%d/%m/%Y")

Politicas_Hospital$`Registro do 1º dia com óbitos` <- as.Date(Politicas_Hospital$`Registro do 1º dia com óbitos`)
Politicas_Hospital$`Registro do 1º dia com óbitos` <- format(Politicas_Hospital$`Registro do 1º dia com óbitos`,
                                                             "%d/%m/%Y")

# Reclassificando para colocar em ordem crescente:

Politicas_Hospital = arrange(Politicas_Hospital, Distância0)

Politicas_Hospital <- Politicas_Hospital %>% 
  mutate_all(replace_na, 0)

Politicas_Hospital$Município = factor(Politicas_Hospital$Município,
                                      levels = Politicas_Hospital$Município)

Politicas_Hospital$Distância0 = as.integer(Politicas_Hospital$Distância0)

# Gráfico no ggplot:

Politicas_Hospital$Distância = as.character(Politicas_Hospital[,2])

Hospital <- ggplot(Politicas_Hospital,
                   aes(x = Município,
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
           fill = c("peru", "seashell4",
                    "maroon", "orange1",
                    "orange1","seagreen",
                    "mediumaquamarine","tomato",
                    "maroon", "yellowgreen"),
           aes(fill = Municipio)) +
  xlab("") +
  ylab("Distância (em dias) para a confirmação do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

Hospital <- ggplotly(Hospital,
                     tooltip = c("x","label", "label1",
                                 "label2","label3","label4",
                                 "label5", "label6"))
# Acrescentando LOGO:

Hospital <- Hospital %>% 
  layout(images = list(list(source = raster2uri(raster),
                            xref = "paper",
                            yref = "paper",
                            x= 0,
                            y= 0.25,
                            sizex = 0.4,
                            sizey = 0.4,
                            opacity = 0.8
  )
  )
  )


# Salvando gráfico:

saveRDS(Hospital, file = "Hospital.rds")


########################### 06 - Economia ########################

Politicas_Economia <- PoliticasPublicas %>%
  select(Município, Distância0, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`, Rank) %>%
  filter(Medidas == "Economia")

# Colocando no formato data:

Politicas_Economia$`Publicação do Decreto` <- format(Politicas_Economia$`Publicação do Decreto`,
                                          "%d/%m/%Y")

Politicas_Economia$`1º Caso de COVID-19`<- as.Date(Politicas_Economia$`1º Caso de COVID-19`)
Politicas_Economia$`1º Caso de COVID-19`<- format(Politicas_Economia$`1º Caso de COVID-19`,
                                       "%d/%m/%Y")

Politicas_Economia$`Registro do 1º dia com óbitos` <- as.Date(Politicas_Economia$`Registro do 1º dia com óbitos`)
Politicas_Economia$`Registro do 1º dia com óbitos` <- format(Politicas_Economia$`Registro do 1º dia com óbitos`,
                                                  "%d/%m/%Y")

# Reclassificando para colocar em ordem crescente:

Politicas_Economia = arrange(Politicas_Economia, Distância0)

Politicas_Economia <- Politicas_Economia %>% 
  mutate_all(replace_na, 0)

Politicas_Economia$Município = factor(Politicas_Economia$Município,
                           levels = Politicas_Economia$Município)

Politicas_Economia$Distância0 = as.integer(Politicas_Economia$Distância0)

# Gráfico no ggplot:

Politicas_Economia$Distância = as.character(Politicas_Economia[,2])

Economia <- ggplot(Politicas_Economia,
                  aes(x = Município,
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
           fill = c("seagreen", "peru",
                    "seashell4", "orange1",
                    "maroon","seagreen",
                    "mediumaquamarine","tomato",
                    "maroon", "yellowgreen"),
           aes(fill = Municipio)) +
  xlab("") +
  ylab("Distância (em dias) para a confirmação do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

Economia <- ggplotly(Economia,
                    tooltip = c("x","label", "label1",
                                "label2","label3","label4",
                                "label5", "label6"))
# Acrescentando LOGO:

Economia <- Economia %>% 
  layout(images = list(list(source = raster2uri(raster),
                            xref = "paper",
                            yref = "paper",
                            x= 0,
                            y= 0.25,
                            sizex = 0.4,
                            sizey = 0.4,
                            opacity = 0.8
  )
  )
  )

# Salvando gráfico:

saveRDS(Economia, file = "Economia.rds")

############################ 07 - Flexibilização ###############################

Politicas_Flexibilização <- PoliticasPublicas %>%
  select(Município, Distância0, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`, Rank) %>%
  filter(Medidas == "Flexibilização")

# Colocando no formato data:

Politicas_Flexibilização$`Publicação do Decreto` <- format(Politicas_Flexibilização$`Publicação do Decreto`,
                                          "%d/%m/%Y")

Politicas_Flexibilização$`1º Caso de COVID-19`<- as.Date(Politicas_Flexibilização$`1º Caso de COVID-19`)
Politicas_Flexibilização$`1º Caso de COVID-19`<- format(Politicas_Flexibilização$`1º Caso de COVID-19`,
                                       "%d/%m/%Y")

Politicas_Flexibilização$`Registro do 1º dia com óbitos` <- as.Date(Politicas_Flexibilização$`Registro do 1º dia com óbitos`)
Politicas_Flexibilização$`Registro do 1º dia com óbitos` <- format(Politicas_Flexibilização$`Registro do 1º dia com óbitos`,
                                                  "%d/%m/%Y")

# Reclassificando para colocar em ordem crescente:

Politicas_Flexibilização = arrange(Politicas_Flexibilização, Distância0)

Politicas_Flexibilização <- Politicas_Flexibilização %>% 
  mutate_all(replace_na, 0)

Politicas_Flexibilização$Município = factor(Politicas_Flexibilização$Município,
                           levels = Politicas_Flexibilização$Município)

Politicas_Flexibilização$Distância0 = as.integer(Politicas_Flexibilização$Distância0)

# Gráfico no ggplot:

Politicas_Flexibilização$Distância = as.character(Politicas_Flexibilização[,2])

Flexibilização <- ggplot(Politicas_Flexibilização,
                  aes(x = Município,
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
           fill = c("orange1", "seashell4",
                    "peru", "seashell4",
                    "orange1","seagreen",
                    "mediumaquamarine","tomato",
                    "maroon", "yellowgreen"),
           aes(fill = Municipio)) +
  xlab("") +
  ylab("Distância (em dias) para a confirmação do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

Flexibilização <- ggplotly(Flexibilização,
                    tooltip = c("x","label", "label1",
                                "label2","label3","label4",
                                "label5", "label6"))

# Acrescentando LOGO:

Flexibilização <- Flexibilização %>% 
  layout(images = list(list(source = raster2uri(raster),
                            xref = "paper",
                            yref = "paper",
                            x= 0,
                            y= 0.25,
                            sizex = 0.4,
                            sizey = 0.4,
                            opacity = 0.8
  )
  )
  )

# Salvando gráfico:

saveRDS(Flexibilização, file = "Flexibilização.rds")

############################ 08 - Emergência de Saúde Pública ###############################

Politicas_Emergência <- PoliticasPublicas %>%
  select(Município, Distância0, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`, Rank) %>%
  filter(Medidas == "Emergência de Saúde Pública")

# Colocando no formato data:

Politicas_Emergência$`Publicação do Decreto` <- format(Politicas_Emergência$`Publicação do Decreto`,
                                          "%d/%m/%Y")

Politicas_Emergência$`1º Caso de COVID-19`<- as.Date(Politicas_Emergência$`1º Caso de COVID-19`)
Politicas_Emergência$`1º Caso de COVID-19`<- format(Politicas_Emergência$`1º Caso de COVID-19`,
                                       "%d/%m/%Y")

Politicas_Emergência$`Registro do 1º dia com óbitos` <- as.Date(Politicas_Emergência$`Registro do 1º dia com óbitos`)
Politicas_Emergência$`Registro do 1º dia com óbitos` <- format(Politicas_Emergência$`Registro do 1º dia com óbitos`,
                                                  "%d/%m/%Y")

# Reclassificando para colocar em ordem crescente:

Politicas_Emergência = arrange(Politicas_Emergência, Distância0)

Politicas_Emergência <- Politicas_Emergência %>% 
  mutate_all(replace_na, 0)

Politicas_Emergência$Município = factor(Politicas_Emergência$Município,
                           levels = Politicas_Emergência$Município)

Politicas_Emergência$Distância0 = as.integer(Politicas_Emergência$Distância0)

# Gráfico no ggplot:

Politicas_Emergência$Distância = as.character(Politicas_Emergência[,2])

Emergência <- ggplot(Politicas_Emergência,
                  aes(x = Município,
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
           fill = c("maroon", "seagreen",
                    "royalblue2", "royalblue2",
                    "mediumaquamarine", "orange1",
                    "seashell4","tomato",
                    "maroon", "yellowgreen"),
           aes(fill = Municipio)) +
  xlab("") +
  ylab("Distância (em dias) para a confirmação do primeiro caso") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

Emergência <- ggplotly(Emergência,
                    tooltip = c("x","label", "label1",
                                "label2","label3","label4",
                                "label5", "label6"))
# Acrescentando LOGO:

Emergência <- Emergência %>% 
  layout(images = list(list(source = raster2uri(raster),
                            xref = "paper",
                            yref = "paper",
                            x= 0,
                            y= 0.25,
                            sizex = 0.4,
                            sizey = 0.4,
                            opacity = 0.8
  )
  )
  )

# Salvando gráfico:

saveRDS(Emergência, file = "Emergência.rds")

