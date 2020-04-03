# DISTRIBUIÇÃO DOS TESTES RÁPIDOS POR UNIDADES DA FEDERAÇÃO
# Site: https://coronavirus.saude.gov.br/distribuicao-de-testes

# PACOTES

pacotes = c("data.table","dplyr", "rvest")
for(pacote in pacotes){
  if(!is.element(pacote,installed.packages())){install.packages(pacote)}
}
library("data.table")          
library("dplyr")
library("rvest")

# URL - DISTRIBUIÇÃO DOS TESTES RÁPIDOS
url = 'https://coronavirus.saude.gov.br/distribuicao-de-testes'

# CÓDIGO HTML DA URL
site = read_html(url)

# ELEMENTO HTML PARA COLETAR
info_teste_html = html_nodes(site,'tabela')

# CONVERTE O HTML PARA TEXTO
info_teste = html_text(info_teste_html)

# VISUALIZAÇÃO DO TEXTO
head(info_teste, 20)

# VISUALIZAÇÃO E CAPTURA DAS TABELAS
Distribuicao_teste = site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

# VISUALIZAÇÃO 
str(Distribuicao_teste)
head(Distribuicao_teste[[1]],10)

# TRANSFORMANDO EM DATAFRAME

ajuste = Distribuicao_teste[[1]]
Distribuicao_teste = as.data.frame(Distribuicao_teste)
colnames(Distribuicao_teste) = Distribuicao_teste[1,]
Distribuicao_teste = Distribuicao_teste[-1,]
rownames(Distribuicao_teste)<-seq(1,28)
View(Distribuicao_teste)


