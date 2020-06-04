# Panorama - Brasil

pacotes <- c("tidyverse","ggimage","gganimate","png","RCurl","lubridate",
             "tmap","readxl","gridExtra", "ggrepel", "scales",
             "stringr", "ggthemes", "transformr", "gifski",  "RColorBrewer",
             "grid", "viridis", "hrbrthemes","devtools", "ggplot2", "dplyr")
for(pacote in pacotes){
  if(!is.element(pacote,installed.packages())){install.packages(pacote)}
}

#Carregando pacotes

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggimage)
library(gganimate)
library(png)
library(RCurl)
library(lubridate)
library(tmap)
devtools::install_github("rpradosiqueira/brazilmaps") # Não instalou
library(brazilmaps) 
library(readxl)
library(gridExtra)
library(ggrepel)
library(scales)
library(stringr)
library(ggthemes)
library(transformr)
library(gifski)
library(RColorBrewer)
library(grid)
library(viridis)
library(hrbrthemes)
devtools::install_github("fndemarqui/readCovid19")
library(readCovid19)
install.packages("pracma")
library(pracma) 
#------------------------------------------------------------------------------------------------------------------------------------------------
# GERA FUNÇÃO "GET.UFF.READ" QUE PERMITE BAIXAR DADOS A PARTIR DOS SCRIPTS DO PORTAL
#------------------------------------------------------------------------------------------------------------------------------------------------                                                  

LINK = "https://sites.google.com/site/portaldadosuffcontracovid/home"
FILE = "GET.UFF.READ.RData"
download.file(url = paste0(LINK,"/",FILE), destfile=FILE)
load(FILE)  
GET.UFF.READ("BR.UF.ALT.R")

#################### Número de casos por estado ##############

g1 = ggplot(BaseFinalUF) +
  geom_bar(aes(x = fct_reorder(Estados, CasosAcumulados), 
               y = CasosAcumulados), 
           stat = 'identity',
           width = 0.65) + 
  coord_flip() +
  theme_light() + 
  xlab('') + 
  ylab('') + 
  ggtitle('Casos Acumulados') + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=9)) +
  scale_y_continuous(name="", 
                     limits=c(0, max(BaseFinalUF$CasosAcumulados) * 1.2)) +
  geom_text(aes(x = fct_reorder(Estados, CasosAcumulados), 
                y = CasosAcumulados, 
                label = prettyNum(CasosAcumulados, big.mark = ".", decimal.mark = ","), 
                hjust = -0.1), 
            size = 3.5) +
  annotation_custom(rasterGrob(img), 
                    xmin= paste(BaseFinalUF$Estados[BaseFinalUF$CasosAcumulados == sort(BaseFinalUF$CasosAcumulados)[3]]), 
                    xmax= paste(BaseFinalUF$Estados[base_atual$Casos == sort(base_atual$Casos)[1]]), 
                    ymin= max(base_atual$Casos) * 0.8,
                    ymax= max(base_atual$Casos) * 1.2)

#Número de mortes por estado
g2 = ggplot(base_atual) +
  geom_bar(aes(x = fct_reorder(state, Casos), 
               y = Mortes), 
           stat = 'identity', 
           width = 0.65) + 
  coord_flip() +
  theme_light() + 
  xlab('') + 
  ylab('') + 
  ggtitle('Óbitos') + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        #        axis.title.y=element_blank(), 
        #        axis.text.y=element_blank(), 
        #        axis.ticks.y=element_blank(),
        plot.title = element_text(size=9)) +
  scale_y_continuous(name = "", 
                     limits = c(0, max(base_atual$Mortes) * 1.2)) +
  geom_text(aes(x = fct_reorder(state, Casos), 
                y = Mortes, 
                label = prettyNum(Mortes, big.mark = ".", decimal.mark = ","), 
                hjust = -0.1), 
            size = 3.5)+
  annotation_custom(rasterGrob(img), 
                    xmin= paste(base_atual$state[base_atual$Casos == sort(base_atual$Casos)[3]]), 
                    xmax= paste(base_atual$state[base_atual$Casos == sort(base_atual$Casos)[1]]),
                    ymin= max(base_atual$Mortes) * 0.8,
                    ymax= max(base_atual$Mortes) * 1.2)


base_topo = data.frame(x = fct_reorder(base_atual$state, base_atual$Casos), y = base_atual$letalidade)

#letalidade por estado
g3 = ggplot(base_atual) +
  geom_bar(aes(x = fct_reorder(state, Casos), 
               y = letalidade), 
           stat = 'identity', 
           fill = '#ffa600',  
           width = 0.14) + 
  coord_flip() +
  theme_light() + 
  xlab('') + 
  ylab('') + 
  ggtitle(paste('Letalidade - média UF:', prettyNum(round(mean(base_atual$letalidade), 2), big.mark = ".", decimal.mark = ",") , "%")) + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),                        
        plot.title = element_text(size=9), 
        plot.subtitle = element_text(size = 10, 
                                     face = 'bold')) +
  scale_y_continuous(name = "", limits = c(0, 25)) +
  geom_text(aes(x = fct_reorder(state, Casos),
                y = max(letalidade) + 10,
                label = paste(prettyNum(letalidade, big.mark = ".", decimal.mark = ","),"%"), 
                hjust = 1),
            size = 3.5) +
  geom_hline(yintercept = mean(base_atual$letalidade), 
             linetype = 'dashed', 
             color = 'black') +
  geom_point(data = base_topo, 
             aes(x = x, y = y), 
             color = '#ffa600', 
             size = 5)

#Número de casos por 100.000 habitantes
g4 = ggplot(base_atual) +
  geom_bar(aes(x = fct_reorder(state, tx_casos_mil), 
               y = tx_casos_mil),
           stat = 'identity', 
           width = 0.65, 
           fill = '#0094d8', 
           alpha = 0.8) + 
  coord_flip() +
  xlab('') + 
  theme_light() + 
  ylab('') + 
  scale_y_continuous(name="", 
                     limits=c(0,max(base_atual$tx_casos_mil) * 1.1)) +
  geom_text(aes(x = fct_reorder(state, tx_casos_mil), 
                y = tx_casos_mil, 
                label = prettyNum(tx_casos_mil, big.mark = ".", decimal.mark = ","), 
                hjust = -0.1), 
            size = 3.5) +
  ggtitle('Taxa de Casos por 100 mil hab.') +
  theme(axis.title.x = element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        plot.title = element_text(size=9)) +
  annotation_custom(rasterGrob(img), 
                    xmin= paste(base_atual$state[base_atual$tx_casos_mil == sort(base_atual$tx_casos_mil)[3]]), 
                    xmax= paste(base_atual$state[base_atual$tx_casos_mil == sort(base_atual$tx_casos_mil)[1]]),
                    ymin= max(base_atual$tx_casos_mil) * 0.8,
                    ymax= max(base_atual$tx_casos_mil)*1.1)

#Número de mortes por 100.000 habitantes
g5 = ggplot(base_atual) +
  geom_bar(aes(x = fct_reorder(state, tx_mort_mil), 
               y = tx_mort_mil), 
           stat = 'identity', 
           width = 0.65, 
           fill = '#ffa600', 
           alpha = 0.8) + 
  coord_flip() +
  xlab('') + 
  theme_light() + 
  ylab('') + 
  scale_y_continuous(name="", 
                     limits=c(0, max(base_atual$tx_mort_mil) * 1.1)) +
  geom_text(aes(x = fct_reorder(state, tx_mort_mil), 
                y = tx_mort_mil, 
                label = prettyNum(tx_mort_mil, big.mark = ".", decimal.mark = ","), 
                hjust = -0.1), 
            size = 3.5) +
  ggtitle('Taxa de Óbitos por 100 mil hab.') +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        plot.title = element_text(size=9))  +
  annotation_custom(rasterGrob(img), 
                    xmin= paste(base_atual$state[base_atual$tx_mort_mil == sort(base_atual$tx_mort_mil)[3]]), 
                    xmax= paste(base_atual$state[base_atual$tx_mort_mil == sort(base_atual$tx_mort_mil)[1]]),
                    ymin= max(base_atual$tx_mort_mil) * 0.8,
                    ymax= max(base_atual$tx_mort_mil) * 1.1)


grid.arrange(g1, g2, g3, ncol = 3)

grid.arrange(g4, g5, ncol = 2)





