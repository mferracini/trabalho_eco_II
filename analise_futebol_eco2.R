#verificar se os pacotes usados estao instalados
#chama o pacote para analise dos dados (modelos lineares generalizados)
require(glm2)
require(stats)
require(dplyr)

#chama pacote para dados inflados em zero
require(pscl)

#chama o pacote para analise de dados em painel
require(plm)

#set work directories
#se quiser setar um novo diretorio
setwd("/home/babu/Documents/Mateus/mestrado/Econometria II/trabalho/dados_scraper")

meu_dir<-getwd()

#le o CSV com os dados
urlfile<-'https://raw.githubusercontent.com/mferracini/trabalho_eco_II/master/base_final_jogos.csv'
base_dados<-read.csv(urlfile)

View(base_dados)



#modelo de contagem para numero de gols



gols_mandante<- glm(placar_mandante ~ (mandante_posicao_inicio_rodada - Visitante_posicao_inicio_rodada) + clássico, data = base_dados , family = poisson())
summary(gols_mandante)

gols_visitante <- glm(placar_visitante ~ (mandante_posicao_inicio_rodada - Visitante_posicao_inicio_rodada) + clássico, data = base_dados , family = poisson())
summary(gols_visitante)


