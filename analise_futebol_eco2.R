# Bibliotecas utilizadas
biblio = c("glm2", "stats", "dplyr", #chama o pacote para analise dos dados (modelos lineares generalizados)
           "pscl",                   #chama pacote para dados inflados em zero
           "plm",                    #chama o pacote para analise de dados em painel
           "stargazer")              # pacote de tabelas

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


### Funcao para os Graficos ###
graf.comp = function(modelo){
  x = modelo$model[[1]] 
  y = as.numeric(x - modelo$residuals) 
  nome = deparse(substitute(modelo))
  dir.create("./graficos", showWarnings = FALSE)
  pdf(paste("./graficos/", nome,".pdf", sep = ""),width = 8, height=5)
  plot(x,y,
       main = "",
       xlab = "Observado",
       ylab = "Estimado")
  lines(c(1,20),c(1,20),
        col = "red")
  dev.off()
}


# Le o CSV com os dados
urlfile<-'https://raw.githubusercontent.com/mferracini/trabalho_eco_II/master/base_final_jogos.csv'
base_dados<-read.csv(urlfile)
pib_url <-'https://raw.githubusercontent.com/mferracini/trabalho_eco_II/master/PIBMunicipal_2010_2013.csv'
View(base_dados)

# Adiciona a tabela o PIB Munipal:
pib = read.csv(pib_url, strip.white=TRUE)

pibCap2012 = vector(mode = "numeric", length = length(base_dados$cidade))
for (i in 1:length(base_dados$cidade)){
  a = grepl(base_dados$cidade[i], pib$unidade)
  #base_dados$pib[i] = pib$capita2013[a]
  if (any(a)){
    pibCap2012[i] = pib$capita2012[a]
  } else {
    pibCap2012[i] = NA
  }
}
base_dados =cbind(base_dados,pibCap2012)

# cria uma variavel q serve como proxy de poder ofensivo e defensivo
base_dados$pwr_ofe = base_dados$man_gf_ini_rod - base_dados$vis_gt_ini_rod
base_dados$pwr_def = base_dados$man_mean_gt_ini_r - base_dados$vis_mean_gf_ini_r
base_dados$pwr_net = base_dados$pwr_ofe - base_dados$pwr_def

# Adiciona o resultado dos tres ultimos jogos somados
tres = function(timep){
  n = nrow(base_dados)
  resultado = rep(0, n)
  for (i in n:1){
    if (base_dados$rodada[i] < 3){
      resultado[i] = NA
    }else{
      time = timep[i]
      cont = 0
      j = i - 1
      while (cont != 3 & j != 1){
        timem = base_dados$clubem[j]
        timev = base_dados$clubev[j]
        if (timem == time){
          cont = cont + 1
          resultado[i] = resultado[i] + as.numeric(base_dados$fator_resultado[j])
        }
        if (timev == time){
          cont = cont + 1
          resultado[i] = resultado[i] - as.numeric(base_dados$fator_resultado[j])
        }
        j = j - 1
      }
    }
  }
  return(resultado)
}
#base_dados$result_3ult_m = resultado
base_dados$result_3ult_m = tres(base_dados$clubem)
base_dados$result_3ult_v = tres(base_dados$clubev)


# Corrigindo posicao mandante inicio da rodada de 0 para 20
base_dados$mandante_posicao_inicio_rodada[base_dados$mandante_posicao_inicio_rodada == 0] = 20
base_dados$Visitante_posicao_inicio_rodada[base_dados$Visitante_posicao_inicio_rodada == 0] = 20



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
gols_mandante<- glm(placarm_tn ~ diff_posicao_mandante + pwr_net+ clássico + result_3ult_m , data = base_dados , family = poisson())
summary(gols_mandante)
graf.comp(gols_mandante)

gols_visitante <- glm(placarv_tn ~ diff_posicao_mandante + pwr_net+ clássico + result_3ult_m, data = base_dados , family = poisson())
summary(gols_visitante)
graf.comp(gols_visitante)

#modelos inflado em zero

gols_mandante_infl<- zeroinfl(placarm_tn ~ diff_posicao_mandante + pwr_net+ clássico + result_3ult_m , data = base_dados)
summary(gols_mandante_infl)

ajustado = as.numeric(gols_mandante_infl$model[[1]] - gols_mandante_infl$residuals) 
plot(ajustado, gols_mandante_infl$model[[1]])
lines(c(1,20),c(1,20),col = "red")

gols_visitante_infl <- zeroinfl(placarv_tn ~ diff_posicao_mandante + pwr_net+ clássico, data = base_dados)
summary(gols_visitante_infl)

#binomial negativa
gols_mandante_nb<- glm.nb(placarm_tn ~ diff_posicao_mandante + pwr_net+ clássico + result_3ult_m , data = base_dados)
summary(gols_mandante_nb)

ajustado = as.numeric(gols_mandante_nb$model[[1]] - gols_mandante_nb$residuals) 
plot(ajustado, gols_mandante_nb$model[[1]])
lines(c(1,20),c(1,20),col = "red")

gols_visitante_nb <- glm.nb(placarv_tn ~ diff_posicao_mandante + pwr_net+ clássico + result_3ult_m , data = base_dados)
summary(gols_visitante_nb)

# Tabelas do modelo de contagem
# Poisson
dir.create("./tabelas", showWarnings = FALSE)
sink(file = "./tabelas/cont_poisson.tex")
stargazer(gols_mandante, gols_visitante, title="Contagem: Poisson",
          align=TRUE, dep.var.labels=c("Gols Mandante","Gols Visitante"),
          covariate.labels=c("Diferença de posições","Clássico","Pagantes"), 
          omit.stat=c(), no.space=TRUE)
sink()

# Inflado
sink(file = "./tabelas/cont_inflado.tex")
stargazer(gols_mandante_infl, gols_visitante_infl, title="Contagem: Poisson Inflado de Zeros",
          align=TRUE, dep.var.labels=c("Gols Mandante","Gols Visitante"),
          covariate.labels=c("Diferença de posições","Clássico","Pagantes"), 
          omit.stat=c(), no.space=TRUE)
sink()

# Binomial
sink(file = "./tabelas/cont_binomial.tex")
stargazer(gols_mandante_nb, gols_visitante_nb, title="Contagem: Binomial Negativa",
          align=TRUE, dep.var.labels=c("Gols Mandante","Gols Visitante"),
          covariate.labels=c("Diferença de posições","Clássico","Pagantes"), 
          omit.stat=c(), no.space=TRUE)
sink()

# Todos
dir.create("./tabelas", showWarnings = FALSE)
sink(file = "./tabelas/contagem.tex")
stargazer(gols_mandante, gols_visitante,
          gols_mandante_nb, gols_visitante_nb,
          title="Contagem",
          align=FALSE, dep.var.labels=c("Gols Mandante","Gols Visitante"),
          covariate.labels=c("Diferença de posições","Clássico","Pagante"), 
          omit.stat=c(), no.space=TRUE)
sink()

# Modelo de dados em painel para renda e público
base_dados$ticket_medio = base_dados$renda_bruta / base_dados$pagante
base_dados$tempo = 100*base_dados$ano + 2*base_dados$rodada
base_dados$vs = as.factor(paste(base_dados$clubem,base_dados$clubev))
base_dados$rod_time = as.factor(paste(base_dados$clubem,base_dados$rodada))
base_dados_painel = subset(base_dados, !is.na(pagante))
base_dados_painel = subset(base_dados_painel, !is.na(pibCap2012))

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
base_dados_painel$ticket_log = log(base_dados_painel$ticket_medio)
base_dados_painel$logPibCap2012 = log(base_dados_painel$pibCap2012)

#log renda
# Efeito Aleatório
ren_rand = plm(log(renda_bruta) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada  + jg_fds + ingresso_vendido + ticket_log + log(pibCap2012) + rodada + result_3ult_m, data = base_dados_painel, index = c("vs", "ano"), model = "random")
summary(ren_rand)
graf.comp(ren_rand)

ajustado = as.numeric(ren_rand$model[[1]] - ren_rand$residuals) 
plot(ajustado, ren_rand$model[[1]])
lines(c(1,20),c(1,20),col = "red")

# Efeito fixo
ren_fix = plm(log(renda_bruta) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada  + jg_fds + ingresso_vendido + ticket_log + log(pibCap2012) + rodada + result_3ult_m, data = base_dados_painel,index = c("vs", "ano"), model = "within")
summary(ren_fix)
summary(fixef(ren_fix))
graf.comp(ren_fix)

#teste de hausman
h_test_ren <- phtest(ren_fix,ren_rand)

#log publico
# Efeito Aleatório
pgt_rand = plm(log(pagante) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada  + jg_fds + ticket_log + log(pibCap2012) + rodada + result_3ult_m, data = base_dados_painel, index = c("vs", "ano"), model = "random")
summary(pgt_rand)
graf.comp(pgt_rand)

# Efeito fixo
pgt_fix = plm(log(pagante) ~ mandante_posicao_inicio_rodada + Visitante_posicao_inicio_rodada  + jg_fds  + ticket_log + log(pibCap2012) + rodada + result_3ult_m, data = base_dados_painel,index = c("vs", "ano"), model = "within")
summary(pgt_fix)
summary(fixef(pgt_fix))
graf.comp(pgt_fix)

#teste de hausman
h_test_pgt <- phtest(pgt_fix,pgt_rand)

sink(file = "./tabelas/painel.tex")
stargazer(pgt_rand, pgt_fix,
          title="Painel",
          align=FALSE, dep.var.labels=c("log(Pagante)"),
          covariate.labels=c("Pos.: Mandante Ini. da Rod.", "Pos.: Visitante Ini. da Rod.", "Final de Semana", "log(Ticket)", "PIB Municipal per Cap.", "Rodada", "Resultado 3 últimos"), 
          omit.stat=c(), no.space=TRUE)
sink()


##  probit/logit ordenado para calcular probabilidades de resultado

##tabela com resultados descritivos

table(base_dados$fator_resultado)


probit_ord <- polr(as.ordered(fator_resultado) ~ diff_posicao_mandante + pwr_net+ clássico + result_3ult_m, data = base_dados, method="probit", Hess=TRUE)
summary(probit_ord)
  #cria tabela com os coeficientes
(tabela_coeficientes <- coef(summary(probit_ord)))
  #calcula op_valo
p_valor <- pnorm(abs(tabela_coeficientes[, "t value"]), lower.tail = FALSE) * 2
  #concatena tabelas
(tabela_final <- cbind(tabela_coeficientes, "p value" = p_valor))
 
#calcula o odds ratio e o IC
#OR_CI <- exp(cbind(OR = coef(logit_ord), ci = confint(logit_ord)))
#OR_CI ---- OR apenas para logit

dados_aux <- data.frame(
  diff_posicao_mandante = rep(-19:19, 20*7),
  clássico = rep(0:1 , each = 390*7),
  pwr_net = rep(seq(from = -4, to = 4, length.out = 100),39*7),
  result_3ult_m = rep(-3:3, each = 390))
View(dados_aux)

previsao_base <- cbind(base_dados, predict(probit_ord, base_dados, type = "probs"))
head(previsao_base)
View(previsao_base)

previsao_aux <- cbind(dados_aux, predict(probit_ord, dados_aux, type = "probs"))
head(previsao_aux)
View(previsao_aux)

# Tabela
sink(file = "./tabelas/probit.tex")
stargazer(probit_ord,
          title="Probit Ordenado",
          align=FALSE, dep.var.labels=c("Resultado do Jogo"),
          covariate.labels=c("Diferença de Posição", "Poder Ofen. e Def.","Clássico", "Resultado 3 últ."), 
          omit.stat=c(), no.space=TRUE)
sink()


# Matriz de Confusao
estimado = probit_ord$fitted.values
observado = probit_ord$model[,1]

previsto = vector(mode = "numeric", length = length(observado))
for (i in 1:length(observado)){
  previsto[i] = which.max(as.numeric(estimado[i,]))-2
}
previsto = as.factor(previsto)

# Matriz de Confusao para vitoria
estimado = probit_ord$fitted.values
observado = probit_ord$model[,1]

previsto = vector(mode = "numeric", length = length(observado))
for (i in 1:length(observado)){
  previsto[i] = as.numeric(as.numeric(estimado[i,3])>=0.5)
}
previsto = as.factor(previsto)
conf1 = table(observado,previsto)

c = as.numeric(estimado[,1])<=as.numeric(estimado[,2])
table(observado,c)

sink(file = "./tabelas/probit.tex")
stargazer(probit_ord,
          title="Probit Ordenado",
          align=FALSE, dep.var.labels=c("Resultado do Jogo"),
          covariate.labels=c("Diferença de Posição", "Poder Ofen. e Def.","Clássico", "Resultado 3 últ."), 
          omit.stat=c(), no.space=TRUE)
sink()


