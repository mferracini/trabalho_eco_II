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

#graficos para visualizar a distribuição de gols
grf_gols_mandante <- hist(base_dados$placarm_tn)
grf_gols_visitante <- hist(base_dados$placarv_tn)
grf_gols_partida <- hist(base_dados$gs_partida)

# Modelos de contagem para numero de gols
#poisson
gols_mandante<- glm(placarm_tn ~ diff_posicao_mandante + clássico + pagante, data = base_dados , family = poisson())
summary(gols_mandante)

gols_visitante <- glm(placarv_tn ~ diff_posicao_visitante + clássico + pagante, data = base_dados , family = poisson())
summary(gols_visitante)

#modelos inflado em zero

gols_mandante_infl<- zeroinfl(placarm_tn ~ diff_posicao_mandante , data = base_dados)
summary(gols_mandante_infl)

gols_visitante_infl <- zeroinfl(placarv_tn ~ diff_posicao_visitante , data = base_dados)
summary(gols_visitante_infl)

#binomial negativa
gols_mandante_nb<- glm.nb(placarm_tn ~ diff_posicao_mandante , data = base_dados)
summary(gols_mandante_nb)

gols_visitante_nb <- glm.nb(placarv_tn ~ diff_posicao_visitante , data = base_dados)
summary(gols_visitante_nb)



# Modelo de dados em painel para renda e público
base_dados$ticket_medio = base_dados$renda_bruta / base_dados$pagante
base_dados$tempo = 100*base_dados$ano + 2*base_dados$rodada
base_dados$vs = as.factor(paste(base_dados$clubem,base_dados$clubev))
base_dados$rod_time = as.factor(paste(base_dados$clubem,base_dados$rodada))
base_dados_painel = subset(base_dados, !is.na(pagante))


# Pagante
# Efeito Aleatório
#regVolR = plm(log(pagante) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada + clássico, data = base_dados, index = c("clubem", "tempo"), model = "random", na.action = na.omit)
#summary(regVolR)

# Efeito fixo
#regVolW = plm(log(pagante) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada + clássico, data = base_dados,index = c("clubem", "tempo"), model = "within")
#summary(regVolW)

# Ocupacao
# Efeito Aleatório
#regVolR = plm(log(ingresso_vendido) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada + clássico, data = base_dados, index = c("clubem", "tempo"), model = "random", na.action = na.omit)
#summary(regVolR)

# Efeito fixo
#regVolW = plm(log(ingresso_vendido) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada + clássico, data = base_dados,index = c("clubem", "tempo"), model = "within")
#summary(regVolW)

## O Jogo como individuo
base_dados_painel$ticket_quadrado = base_dados_painel$ticket_medio^2
#log renda
# Efeito Aleatório
regVolR_ren = plm(log(renda_bruta) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada  + jg_fds + ingresso_vendido + ticket_quadrado, data = base_dados_painel, index = c("vs", "ano"), model = "random", na.action = na.omit)
summary(regVolR_ren)

# Efeito fixo
regVolW_ren = plm(log(renda_bruta) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada + jg_fds + ingresso_vendido  + ticket_quadrado, data = base_dados_painel,index = c("vs", "tempo"), model = "within")
summary(regVolW_ren)
summary(fixef(regVolW_ren))
#teste de hausman
h_test_ren <- phtest(regVolW_ren,regVolR_ren)

#log publico
# Efeito Aleatório
regVolR_pgt = plm(log(pagante) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada  + jg_fds + ingresso_vendido + log(ticket_medio), data = base_dados_painel, index = c("vs", "ano"), model = "random", na.action = na.omit)
summary(regVolR_pgt)

# Efeito fixo
regVolW_pgt = plm(log(pagante) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada + jg_fds + ingresso_vendido + ticket_quadrado, data = base_dados_painel,index = c("vs", "tempo"), model = "within")
summary(regVolW_pgt)
summary(fixef(regVolW_pgt))
#teste de hausman
h_test_pgt <- phtest(regVolW_pgt,regVolR_pgt)


##  probit/logit ordenado para calcular probabilidades de resultado
# cria uma variavel q serve como proxy de poder ofensivo e defensivo
base_dados$pwr_ofe = base_dados$man_gf_ini_rod - base_dados$vis_gt_ini_rod
base_dados$pwr_def = base_dados$man_mean_gt_ini_r - base_dados$vis_mean_gf_ini_r
base_dados$pwr_net = base_dados$pwr_ofe - base_dados$pwr_def

##tabela com resultados descritivos

table(base_dados$resultado_time_mandante)


logit_ord <- polr(resultado_time_mandante ~ diff_posicao_mandante + pwr_net+ clássico , data = base_dados, Hess=TRUE)
summary(logit_ord)
  #cria tabela com os coeficientes
(tabela_coeficientes <- coef(summary(logit_ord)))
  #calcula op_valo
p_valor <- pnorm(abs(tabela_coeficientes[, "t value"]), lower.tail = FALSE) * 2
  #concatena tabelas
(tabela_final <- cbind(tabela_coeficientes, "p value" = p_valor))
 #calcula o odds ratio e o IC
OR_CI <- exp(cbind(OR = coef(logit_ord), ci = confint(logit_ord)))
OR_CI

dados_aux <- data.frame(
  diff_posicao_mandante = rep(-19:19, 20),
  clássico = rep(0:1 , each = 390),
  pwr_net = rep(seq(from = -4, to = 4, length.out = 100),39))
View(dados_aux)

previsao <- cbind(dados_aux, predict(logit_ord, dados_aux, type = "probs"))
head(previsao)
View(previsao)



