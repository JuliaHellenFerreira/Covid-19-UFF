# GET UFF contra o COVID-19

# Politicas Publicas - Estado do Rio de Janeiro
###### Este script tem como objetivo calcular a distancia, em dias,
###### do primeiro caso confirmado ate um determinado decreto de 
###### cada municipio do estado do Rio de Janeiro.


## Pacotes ##

pacotes <- c("magrittr","knitr","tidyr","dplyr", "lubridate","plotly",
             "devtools", "readxl", "stringr")
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

PoliticasPublicas$Distância <- (as.Date(PoliticasPublicas$`Publicação do Decreto`) - as.Date(PoliticasPublicas$`1º Caso de COVID-19`))
PoliticasPublicas$Distância <- as.numeric(PoliticasPublicas$Distância)

## Ranking:

Posicao <- function(v){
linhas <- nrow(v)
Prevenção <- data.frame(NULL)
Emergência <- data.frame(NULL)
Comércio <- data.frame(NULL)
Máscara <- data.frame(NULL)
nada <-  as.vector(NULL)
j = 1
l = 1
m = 1
u = 1
e = 1
for (i in 1: linhas){
  if(v$Medidas[i] == "Medidas de Prevenção"){
    Prevenção[j,1] <- v$Município[i]
    Prevenção[j,2] <- v$`Publicação do Decreto`[i]
    Prevenção[j,3] <- v$Medidas[i]
    Prevenção[j,4] <- v$`1º Caso de COVID-19`[i]
    Prevenção[j,5] <- v$`Casos Confirmados`[i]
    Prevenção[j,6] <- v$`Registro do 1º dia com óbitos`[i]
    Prevenção[j,7] <- v$`Número de óbitos`[i]
    Prevenção[j,8] <- v$Distância[i]
    j = j + 1
  }
  if (v$Medidas[i] == "Situação de Emergência"){
    Emergência[l,1] <- v$Município[i]
    Emergência[l,2] <- v$`Publicação do Decreto`[i]
    Emergência[l,3] <- v$Medidas[i]
    Emergência[l,4] <- v$`1º Caso de COVID-19`[i]
    Emergência[l,5] <- v$`Casos Confirmados`[i]
    Emergência[l,6] <- v$`Registro do 1º dia com óbitos`[i]
    Emergência[l,7] <- v$`Número de óbitos`[i]
    Emergência[l,8] <- v$Distância[i]
    l = l + 1
  }
  if (v$Medidas[i] == "Fechamento de comércio"){
    Comércio[m,1] <- v$Município[i]
    Comércio[m,2] <- v$`Publicação do Decreto`[i]
    Comércio[m,3] <- v$Medidas[i]
    Comércio[m,4] <- v$`1º Caso de COVID-19`[i]
    Comércio[m,5] <- v$`Casos Confirmados`[i]
    Comércio[m,6] <- v$`Registro do 1º dia com óbitos`[i]
    Comércio[m,7] <- v$`Número de óbitos`[i]
    Comércio[m,8] <- v$Distância[i]
    m = m + 1
  }
  if (v$Medidas[i] == "Uso de Máscara"){
    Máscara[u,1] <- v$Município[i]
    Máscara[u,2] <- v$`Publicação do Decreto`[i]
    Máscara[u,3] <- v$Medidas[i]
    Máscara[u,4] <- v$`1º Caso de COVID-19`[i]
    Máscara[u,5] <- v$`Casos Confirmados`[i]
    Máscara[u,6] <- v$`Registro do 1º dia com óbitos`[i]
    Máscara[u,7] <- v$`Número de óbitos`[i]
    Máscara[u,8] <- v$Distância[i]
    u = u + 1
  }
  else{
    nada[e] <- v$`Casos Confirmados`[i]
    e = e + 1
  }
}
Prevenção = Prevenção %>% arrange(V8)
Emergência = Emergência %>% arrange(V8)
Comércio = Comércio %>% arrange(V8)
Máscara = Máscara %>% arrange(V8)
Politicas <- bind_rows(Prevenção, Emergência, Comércio, Máscara)
Politicas = as.data.frame(Politicas)
colnames(Politicas) <- c("Município", "Publicação do Decreto",
                         "Medidas", "1º Caso de COVID-19","Casos Confirmados",
                         "Registro do 1º dia com óbitos", "Número de óbitos",
                         "Distância")
return(Politicas) 

}

## Acrescentando linhas de municípios que não publicaram decretos:

PoliticasPublicas <- PoliticasPublicas %>%
  select(Município, Distância, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`) %>%
  filter(Medidas == "Medidas de Prevenção" | 
        Medidas == "Situação de Emergência" |
        Medidas == "Fechamento de comércio" |
        Medidas == "Uso de Máscara")

# Medidas de Prevenção:

PoliticasPublicas[nrow(PoliticasPublicas) + 5, "Município"] <- ""
PoliticasPublicas[17,1] <-"São João de Meriti/RJ"
PoliticasPublicas[17,3] <-"Medidas de Prevenção"
PoliticasPublicas[18,1] <-"Nova Iguaçu/RJ"
PoliticasPublicas[18,3] <-"Medidas de Prevenção"
PoliticasPublicas[19,1] <-"Duque de Caxias/RJ"
PoliticasPublicas[19,3] <-"Medidas de Prevenção"
PoliticasPublicas[20,1] <-"São Gonçalo/RJ"
PoliticasPublicas[20,3] <-"Medidas de Prevenção"
PoliticasPublicas[21,1] <-"Belford Roxo/RJ"
PoliticasPublicas[21,3] <-"Medidas de Prevenção"

# Situação de Emergência

PoliticasPublicas[nrow(PoliticasPublicas) + 7, "Município"] <- ""
PoliticasPublicas[22,1] <-"Volta Redonda/RJ"
PoliticasPublicas[22,3] <-"Situação de Emergência"
PoliticasPublicas[23,1] <-"Rio de Janeiro/RJ"
PoliticasPublicas[23,3] <-"Situação de Emergência"
PoliticasPublicas[24,1] <-"São João de Meriti/RJ"
PoliticasPublicas[24,3] <-"Situação de Emergência"
PoliticasPublicas[25,1] <-"Nova Iguaçu/RJ"
PoliticasPublicas[25,3] <-"Situação de Emergência"
PoliticasPublicas[26,1] <-"Duque de Caxias/RJJ"
PoliticasPublicas[26,3] <-"Situação de Emergência"
PoliticasPublicas[27,1] <-"São Gonçalo/RJ"
PoliticasPublicas[27,3] <-"Situação de Emergência"
PoliticasPublicas[28,1] <-"Belford Roxo/RJ"
PoliticasPublicas[28,3] <-"Situação de Emergência"

# Fechamento de comércio:

PoliticasPublicas[nrow(PoliticasPublicas) + 5, "Município"] <- ""
PoliticasPublicas[29,1] <-"Rio de Janeiro/RJ"
PoliticasPublicas[29,3] <-"Fechamento de comércio"
PoliticasPublicas[30,1] <-"Nova Iguaçu/RJ"
PoliticasPublicas[30,3] <-"Fechamento de comércio"
PoliticasPublicas[31,1] <-"Duque de Caxias/RJ"
PoliticasPublicas[31,3] <-"Fechamento de comércio"
PoliticasPublicas[32,1] <-"São Gonçalo/RJ"
PoliticasPublicas[32,3] <-"Fechamento de comércio"
PoliticasPublicas[33,1] <-"Belford Roxo/RJ"
PoliticasPublicas[33,3] <-"Fechamento de comércio"

# Uso de Máscara:

PoliticasPublicas[nrow(PoliticasPublicas) + 7, "Município"] <- ""
PoliticasPublicas[34,1] <-"Itaboraí/RJ"
PoliticasPublicas[34,3] <-"Uso de Máscara"
PoliticasPublicas[35,1] <-"Rio de Janeiro/RJ"
PoliticasPublicas[35,3] <-"Uso de Máscara"
PoliticasPublicas[36,1] <-"Mesquita/RJ"
PoliticasPublicas[36,3] <-"Uso de Máscara"
PoliticasPublicas[37,1] <-"Nova Iguaçu/RJ"
PoliticasPublicas[37,3] <-"Uso de Máscara"
PoliticasPublicas[38,1] <-"Duque de Caxias/RJ"
PoliticasPublicas[38,3] <-"Uso de Máscara"
PoliticasPublicas[39,1] <-"São Gonçalo/RJ"
PoliticasPublicas[39,3] <-"Uso de Máscara"
PoliticasPublicas[40,1] <-"Belford Roxo/RJ"
PoliticasPublicas[40,3] <-"Uso de Máscara"

# Acrescentando o rank no data.frame:

PoliticasPublicas <- Posicao(PoliticasPublicas)
Rank <- as.data.frame(paste0(rep(1:10,4),"º"))
colnames(Rank) <- "Rank"
PoliticasPublicas$`Rank` <- Rank$Rank

# Retirando o "/RJ" dos nomes:

PoliticasPublicas$Município <-  str_sub(PoliticasPublicas$Município, end = -4)

## Visualização dos gráficos ##
### Gráficos por medidas:

titulo = "Respostas Políticas"

######################### 01 - Medidas de Prevencao: ###############

Politicas_MedidasPrevencao <- PoliticasPublicas %>%
  select(Município, Distância, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`, Rank) %>%
  filter(Medidas == "Medidas de Prevenção")

# Colocando no formato data:

Politicas_MedidasPrevencao$`Publicação do Decreto` <- format(Politicas_MedidasPrevencao$`Publicação do Decreto`,
                                                             "%d/%m/%Y")

Politicas_MedidasPrevencao$`1º Caso de COVID-19`<- as.Date(Politicas_MedidasPrevencao$`1º Caso de COVID-19`)
Politicas_MedidasPrevencao$`1º Caso de COVID-19`<- format(Politicas_MedidasPrevencao$`1º Caso de COVID-19`,
                                                          "%d/%m/%Y")

Politicas_MedidasPrevencao$`Registro do 1º dia com óbitos` <- as.Date(Politicas_MedidasPrevencao$`Registro do 1º dia com óbitos`)
Politicas_MedidasPrevencao$`Registro do 1º dia com óbitos` <- format(Politicas_MedidasPrevencao$`Registro do 1º dia com óbitos`,
                                                                     "%d/%m/%Y")

# Reclassificando para colocar em ordem crescente:

Politicas_MedidasPrevencao = arrange(Politicas_MedidasPrevencao,
                                     Distância)

Politicas_MedidasPrevencao <- Politicas_MedidasPrevencao %>% 
  mutate_all(replace_na, 0)

Politicas_MedidasPrevencao$Município = factor(Politicas_MedidasPrevencao$Município,
                                              levels = Politicas_MedidasPrevencao$Município)

Politicas_MedidasPrevencao$Distância = as.integer(Politicas_MedidasPrevencao$Distância)

# Gráfico no ggplot:

MedidasPrevencao <- ggplot(Politicas_MedidasPrevencao,
                           aes(x = Município,
                               y = Distância,
                               label = Rank,
                               label1 = `Publicação do Decreto`,
                               label2 = `1º Caso de COVID-19`,
                               label3 = `Casos Confirmados`,
                               label4 = `Registro do 1º dia com óbitos`,
                               label5 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           fill = c("seagreen","peru",
                   "seashell4","royalblue2",
                   "orange1","rosybrown1",
                   "mediumaquamarine","tomato",
                   "maroon", "yellowgreen"),
           aes(fill = Município)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

MedidasPrevencao <- ggplotly(MedidasPrevencao,
                             tooltip = c("x", "y","label", "label1",
                                         "label2","label3","label4", "label5"))
# Salvando gráfico:

saveRDS(MedidasPrevencao, file = "Medidas de Prevenção.rds")

####################### 02 - Situacao de Emergencia: ###################

Politicas_SE <- PoliticasPublicas %>%
  select(Município, Distância, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`, Rank) %>%
  filter(Medidas == "Situação de Emergência")
Politicas_SE$Município[8] <- "Duque de Caxias"

# Colocando no formato data:

Politicas_SE$`Publicação do Decreto` <- format(Politicas_SE$`Publicação do Decreto`,
                                              "%d/%m/%Y")

Politicas_SE$`1º Caso de COVID-19`<- as.Date(Politicas_SE$`1º Caso de COVID-19`)
Politicas_SE$`1º Caso de COVID-19`<- format(Politicas_SE$`1º Caso de COVID-19`,
                                            "%d/%m/%Y")

Politicas_SE$`Registro do 1º dia com óbitos` <- as.Date(Politicas_SE$`Registro do 1º dia com óbitos`)
Politicas_SE$`Registro do 1º dia com óbitos` <- format(Politicas_SE$`Registro do 1º dia com óbitos`,
                                                                     "%d/%m/%Y")

# Reclassificando para colocar em ordem crescente:

Politicas_SE = arrange(Politicas_SE, Distância)

Politicas_SE <- Politicas_SE %>% 
  mutate_all(replace_na, 0)

Politicas_SE$Município = factor(Politicas_SE$Município,
                                levels = Politicas_SE$Município)

Politicas_SE$Distância = as.integer(Politicas_SE$Distância)

# Gráfico no ggplot:

SituacaoEmegencia <- ggplot(Politicas_SE,
                            aes(x = Município,
                                y = Distância,
                                label = Rank,
                                label1 = `Publicação do Decreto`,
                                label2 = `1º Caso de COVID-19`,
                                label3 = `Casos Confirmados`,
                                label4 = `Registro do 1º dia com óbitos`,
                                label5 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           fill = c("seagreen","peru",
                    "orange1","peru",
                    "royalblue2","seagreen",
                    "mediumaquamarine","tomato",
                    "maroon", "yellowgreen"),
           aes(fill = Município)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size=10),
        legend.position = "none")

# Tornando o gráfico interativo:

SituacaoEmegencia <- ggplotly(SituacaoEmegencia,
                              tooltip = c("x", "y","label", "label1",
                                          "label2","label3","label4", "label5"))
# Salvando gráfico:

saveRDS(SituacaoEmegencia, file = "Situação de Emergência.rds")

######################### 03 - Fechamento de comércio: ################

Politicas_FC <- PoliticasPublicas %>%
  select(Município, Distância, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`, Rank) %>%
  filter(Medidas == "Fechamento de comércio")

# Colocando no formato data:

Politicas_FC$`Publicação do Decreto` <- format(Politicas_FC$`Publicação do Decreto`,
                                               "%d/%m/%Y")

Politicas_FC$`1º Caso de COVID-19`<- as.Date(Politicas_FC$`1º Caso de COVID-19`)
Politicas_FC$`1º Caso de COVID-19`<- format(Politicas_FC$`1º Caso de COVID-19`,
                                            "%d/%m/%Y")

Politicas_FC$`Registro do 1º dia com óbitos` <- as.Date(Politicas_FC$`Registro do 1º dia com óbitos`)
Politicas_FC$`Registro do 1º dia com óbitos` <- format(Politicas_FC$`Registro do 1º dia com óbitos`,
                                                       "%d/%m/%Y")

# Reclassificando para colocar em ordem crescente:

Politicas_FC = arrange(Politicas_FC, Distância)

Politicas_FC <- Politicas_FC %>% 
  mutate_all(replace_na, 0)

Politicas_FC$Município = factor(Politicas_FC$Município,
                                levels = Politicas_FC$Município)

Politicas_FC$Distância = as.integer(Politicas_FC$Distância)

# Gráfico no ggplot:

Comercio <- ggplot(Politicas_FC,
                   aes(x = Município,
                       y = Distância,
                       label = Rank,
                       label1 = `Publicação do Decreto`,
                       label2 = `1º Caso de COVID-19`,
                       label3 = `Casos Confirmados`,
                       label4 = `Registro do 1º dia com óbitos`,
                       label5 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           fill = c("seagreen","rosybrown1",
                    "peru","seashell4",
                    "orange1","seagreen",
                    "mediumaquamarine","tomato",
                    "maroon", "yellowgreen"),
           aes(fill = Municipio)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

Comercio <- ggplotly(Comercio,
                     tooltip = c("x", "y","label", "label1",
                                 "label2","label3","label4", "label5"))
# Salvando gráfico:

saveRDS(Comercio, file = "Fechamento de comércio.rds")

########################### 04 - Uso de Máscara: ########################

Mascara <- PoliticasPublicas %>%
  select(Município, Distância, Medidas,`Publicação do Decreto`,
         `1º Caso de COVID-19`,`Casos Confirmados`,
         `Registro do 1º dia com óbitos`,`Número de óbitos`, Rank) %>%
  filter(Medidas == "Uso de Máscara")

# Colocando no formato data:

Mascara$`Publicação do Decreto` <- format(Mascara$`Publicação do Decreto`,
                                          "%d/%m/%Y")

Mascara$`1º Caso de COVID-19`<- as.Date(Mascara$`1º Caso de COVID-19`)
Mascara$`1º Caso de COVID-19`<- format(Mascara$`1º Caso de COVID-19`,
                                       "%d/%m/%Y")

Mascara$`Registro do 1º dia com óbitos` <- as.Date(Mascara$`Registro do 1º dia com óbitos`)
Mascara$`Registro do 1º dia com óbitos` <- format(Mascara$`Registro do 1º dia com óbitos`,
                                                  "%d/%m/%Y")

# Reclassificando para colocar em ordem crescente:

Mascara = arrange(Mascara, Distância)

Mascara <- Mascara %>% 
  mutate_all(replace_na, 0)

Mascara$Município = factor(Mascara$Município,
                                levels = Mascara$Município)

Mascara$Distância = as.integer(Mascara$Distância)

# Gráfico no ggplot:

Mascara <- ggplot(Mascara,
                  aes(x = Município,
                      y = Distância,
                      label = Rank,
                      label1 = `Publicação do Decreto`,
                      label2 = `1º Caso de COVID-19`,
                      label3 = `Casos Confirmados`,
                      label4 = `Registro do 1º dia com óbitos`,
                      label5 = `Número de óbitos`)) +
  geom_bar(stat = "identity",
           col = "black",
           fill = c("seashell4","rosybrown1",
                    "orange1","seashell4",
                    "orange1","seagreen",
                    "mediumaquamarine","tomato",
                    "maroon", "yellowgreen"),
           aes(fill = Municipio)) +
  xlab("") +
  ylab("Distância (em dias) até o 1º caso confirmado") +
  ggtitle(titulo) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x =element_text(face = "bold",size = 10),
        legend.position = "none")

# Tornando o gráfico interativo:

Mascara <- ggplotly(Mascara,
                    tooltip = c("x", "y","label", "label1",
                                "label2","label3","label4", "label5"))
# Salvando gráfico:

saveRDS(Mascara, file = "Uso de Máscara.rds")

