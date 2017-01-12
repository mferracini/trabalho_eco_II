# Bibliotecas utilizadas
biblio = c("glm2", "stats", "dplyr", #chama o pacote para analise dos dados (modelos lineares generalizados)
           "pscl",                   #chama pacote para dados inflados em zero
           "plm")                    #chama o pacote para analise de dados em painel

# Verificar se os pacotes usados estao instalados, instala e os carrega
for (i in biblio){
  if (!require(i, character.only = TRUE)) {
    warning(paste("Instalando: ", i))
    install.packages(i)
    }
  library(i, character.only = TRUE)
}

# Limpa as variaveis e a tela
rm(list = ls())
cat("\014")

#set work directories
#se quiser setar um novo diretorio
#setwd("/home/babu/Documents/Mateus/mestrado/Econometria II/trabalho/dados_scraper")

#meu_dir<-getwd()

# Le o CSV com os dados
urlfile<-'https://raw.githubusercontent.com/mferracini/trabalho_eco_II/master/base_final_jogos.csv'
base_dados<-read.csv(urlfile)

View(base_dados)



## Modelo de contagem para numero de gols

# Diferenca entre a posicao do mandante e do visitante
base_dados$diff_posicao_mandante  =  base_dados$mandante_posicao_inicio_rodada - base_dados$Visitante_posicao_inicio_rodada
base_dados$diff_posicao_visitante = -base_dados$diff_posicao_mandante

# Modelos
gols_mandante<- glm(placarm_tn ~ diff_posicao_mandante + clássico + pagante, data = base_dados , family = poisson())
summary(gols_mandante)

gols_visitante <- glm(placarv_tn ~ diff_posicao_visitante + clássico + pagante, data = base_dados , family = poisson())
summary(gols_visitante)



# Modelo de dados em painel para renda e público

base_dados = subset(base_dados, !is.na(pagante))
base_dados$tempo = 100*base_dados$ano + 2*base_dados$rodada

# Renda Liquida
# Efeito Aleatório
regVolR = plm(log(renda_bruta) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada + clássico , data = base_dados, index = c("clubem", "tempo"), model = "random", na.action = na.omit)
summary(regVolR)

# Efeito fixo
regVolW = plm(log(renda_bruta) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada + clássico, data = base_dados,index = c("clubem", "tempo"), model = "within")
summary(regVolW)

# Pagante
# Efeito Aleatório
regVolR = plm(log(pagante) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada + clássico, data = base_dados, index = c("clubem", "tempo"), model = "random", na.action = na.omit)
summary(regVolR)

# Efeito fixo
regVolW = plm(log(pagante) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada + clássico, data = base_dados,index = c("clubem", "tempo"), model = "within")
summary(regVolW)


## 
