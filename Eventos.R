# Linha do Tempo - Eventos

# Instalação:

install.packages("vistime")
library(vistime)
if(!require("devtools")) install.packages("devtools")
devtools::install_github("shosaco/vistime")


# Resultado:

Eventos <- data.frame(eventosCovid_1_)
vistime(Eventos, events="content", groups="group", title="Medidas de Prevenção")
