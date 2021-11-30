
if(!require(evd)){
  install.packages("evd")
  library(evd)
}

if(!require(evdbayes)){
  install.packages("evdbayes")
  library(evdbayes)
}

if(!require(coda)){
  install.packages("coda")
  library(coda)
}


if(!require(ggplot2)){
  install.packages("ggplot2")
  library(coda)
}

if(!require(openxlsx)){
  install.packages("openxlsx")
  library(openxlsx)
}


if(!require(readxl)){
  install.packages("readxl")
  library(openxlsx)
}


## carregar os dados de precipitação máxima de São João da Boa Vista #####

SJBV<-read.table("dados_max_Anual_SJBVista.txt",header=T)

## carregar os dados de precipitação máxima de Lavras, para obtre info a priori #####
lavras=readxl::read_xlsx("maximos.xlsx",col_names = TRUE)



### grafico de dispersão de precipitação de são joão da boa vista ###
g3=ggplot(data=SJBV[1:30,1:2], aes(x=Ano[1:30], y=Max_ano[1:30])) +    
  geom_point(size=3, fill="white") +        
  xlab("Ano") + ylab("Precipitação Máxima") 

g3+theme_bw() + theme(axis.title = element_text(size = 15),axis.text = element_text(size=13),panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#ajuste dos dados de lavras por maxima verossimilhanca ####

ml=fgev(lavras$prec[1:29])

# obtencao de níveis de retorno e intervalo de confianca para prec de Lavras

rl10=fgev(lavras$prec[1:29],prob = 1/10)
cf10=confint(rl10)

rl100=fgev(lavras$prec[1:29],prob = 1/100)
cf100=confint(rl100)

rl200=fgev(lavras$prec[1:29],prob = 1/200)
cf200=confint(rl200)



#################################################################
##---------------obtencao das diferencas de quantis para priori gama 1  ("diferenca direta" de quantis)--################# 
###################################

LI_q10=cf10[1,1]
LI_q100=cf100[1,1]
LI_q200=cf200[1,1]

LS_q10=cf10[1,2]
LS_q100=cf100[1,2]
LS_q200=cf200[1,2]

df_LI_qp2=LI_q100-LI_q10

df_LI_qp3=LI_q200-LI_q100

df_LS_qp2=LS_q100-LS_q10

df_LS_qp3=LS_q200-LS_q100

####-------para carregar a rotina auxiliar (.R) que contem a funcao que retorno os parametros da gama a partir dos quantis  -------#### 

source("gamma.parms.from.quantiles.R")

## Hiperparametros

par_gam1_qp1=gamma.parms.from.quantiles(c(LI_q10,LS_q10), p=c(0.025,0.975))

a1_gam1=par_gam1_qp1$shape
b1_gam1=par_gam1_qp1$scale

par_gam1_qp2=gamma.parms.from.quantiles(c(df_LI_qp2,df_LS_qp2), p=c(0.025,0.975))

a2_gam1=par_gam1_qp2$shape
b2_gam1=par_gam1_qp2$scale

par_gam1_qp3=gamma.parms.from.quantiles(c(1,df_LS_qp3), p=c(0.025,0.975))

a3_gam1=par_gam1_qp3$shape
b3_gam1=par_gam1_qp3$scale



#################################################################
##-----obtencao das diferen?as de quantis para priori gama 2  ("diferen?a entre o lim superior (quantil pi) e o lim superior (quantil pi+1)--################# 
###################################

df2_LI_qp2=LI_q100-LS_q10

df2_LI_qp3=LI_q200-LS_q100

df2_LS_qp2=LS_q100-LI_q10

df2_LS_qp3=LS_q200-LI_q100

####-------para carregar a rotina auxiliar (.R) que cont?m a funcao que retorno os parametros da gama a partir dos quantis  -------#### 

source("gamma.parms.from.quantiles.R")

## Hiperparametros

par_gam2_qp1=gamma.parms.from.quantiles(c(LI_q10,LS_q10), p=c(0.025,0.975))

a1_gam2=par_gam1_qp1$shape
b1_gam2=par_gam1_qp1$scale

par_gam2_qp2=gamma.parms.from.quantiles(c(1,df2_LS_qp2), p=c(0.025,0.975))

a2_gam2=par_gam2_qp2$shape
b2_gam2=par_gam2_qp2$scale

par_gam2_qp3=gamma.parms.from.quantiles(c(1,df2_LS_qp3), p=c(0.025,0.975))

a3_gam2=par_gam2_qp3$shape
b3_gam2=par_gam2_qp3$scale

#### Funcoes auxiliares  - DIC

dic=function(poste,dados){
  
  #param.mcmc <- as.mcmc(poste)
  
  post_mcmc=as.data.frame(poste)
  
  thetaBayes<-sapply(as.list(post_mcmc),mean)
  
  llhd<- function(theta2) gevlik2(theta2,dados) 
  
  llhd_post=c(rep(0,length(nrow(poste))))
  
  for (g in 1:nrow(poste)) {llhd_post[g]=llhd(poste[g,1:3])}
  
  pDIC <- 2*(llhd(thetaBayes) - mean(llhd_post))
  dic <- -2*llhd(thetaBayes) + 2*pDIC
  return(dic)
}    


#### Funcoes auxiliares  - nivel de retorno


func_rl=function(poste,prob=c(1/10,1/50,1/100,1/200)){
  
  k<-length(poste[,1]) 
  loc <- numeric(k) 
  escala <- numeric(k) 
  forma <- numeric(k) 
  np<-length(prob)
  
  #para alterar tempos de retorno, basta mudar os valores
  # denomidador 
  np<-length(prob) 
  q1 <- numeric(k) 
  q2 <- numeric(k)
  q3 <- numeric(k)
  q4 <- numeric(k)
  for (i in 1:k)
  { 
    loc[i] <- poste[i,1] 
    escala[i] <- poste[i,2] 
    forma[i] <- poste[i,3] 
    q1[i]<-(loc[i]+((escala[i]/forma[i])*(((-log(1-prob[1]))^ 
                                             (-forma[i]))-1))) 
    q2[i]<-(loc[i]+((escala[i]/forma[i])*(((-log(1-prob[2]))^ 
                                             (-forma[i]))-1))) 
    q3[i]<-(loc[i]+((escala[i]/forma[i])*(((-log(1-prob[3]))^ 
                                             (-forma[i]))-1)))
    q4[i]<-(loc[i]+((escala[i]/forma[i])*(((-log(1-prob[4]))^ 
                                             (-forma[i]))-1)))
  } 
  
  nit<-length(q1)  
  qo<-matrix(0,nit,4)  
  qo[,1]<-q1 
  qo[,2]<-q2 
  qo[,3]<-q3
  qo[,4]<-q4
  ####################################
  #     Analise dos niveis de retorno
  ###################################
  nivel_ret_mcmc<- as.mcmc(qo)
  return(nivel_ret_mcmc)
}

#######################################################################  
####-----------POSTERIOR WITH prior gama 1 - differences of quantiles #####################
######################################################################

n.size.chain=5500
burn.in=500
thi.n=1

a_gam1=c(a1_gam1,a2_gam1,a3_gam1)
b_gam1=c(b1_gam1,b2_gam1,b3_gam1)

pn_gam1<-prior.quant(prob=c(0.1,0.01,0.005),shape=a_gam1, scale=b_gam1)

psd0<- ar.choice(init=c(65,12,0.1), prior=pn_gam1, lh = "gev", n = 1000, psd=rep(.01, 3), data=SJBV$Max_ano[1:30])


##OBTENCAO DA POSTERIORI GAMA 1
  poste_gam1<-posterior(n.size.chain, init = c(65,12,0.1), prior = pn_gam1, lh = "gev",data=SJBV$Max_ano[1:30], psd = psd0$psd,burn=burn.in,thin=thi.n)
  
  #####niveis de retorno 
  rl_ga1=func_rl(poste = poste_gam1,prob =c(1/5,1/10,1/15,1/20))
  
  post_ga1=cbind(poste_gam1,rl_ga1) # posterior parameters and return levels
  
  #criterios de diagnostico 
  g_ga1=as.data.frame(geweke.diag(post_ga1)[1])$z  # GEweke
  r_ga1=raftery.diag(post_ga1)$resmatrix[,4]     #raftery e lewis
  h_ga1=heidel.diag(post_ga1)[,3]    #heidelberger e welch

#dataframe reunindo os resultados dos criterios de convergencia
grh_gama1=cbind(g_ga1,r_ga1,h_ga1)

#dataframe reunindo os resultados da posteriori
smpt_gama1=cbind(summary(as.mcmc(post_ga1))$statistics[,1],summary(as.mcmc(post_ga1))$statistics[,2],HPDinterval(as.mcmc(post_ga1)),grh_gama1)  ## media e mediana E HPD a posteriori

#salvar os resultados da posteriori em arquivo excel, opcional  ####
write.xlsx(smpt_gama1,"gama1.xlsx",row.names=T)

## DIC do modelo
dic_gama1=dic(poste_gam1,SJBV$Max_ano[1:30])

#calcular o viés
vies_ga1=smpt_gama1[1:7]-c(locat,sca,sha,quant)

#salvar o intervalo HPD 
hpdn=HPDinterval(rl_ga1);hpdn

#calcular a amplitude media 
amp_hpd1=mean(hpdn[1,2]-hpdn[1,1],hpdn[2,2]-hpdn[2,1],hpdn[3,2]-hpdn[3,1],hpdn[4,2]-hpdn[4,1])

# valores observados de precipitação maxima em Sao JOao da Boa Vista
yob5=max(SJBV$Max_ano[31:36])
yob10=max(SJBV$Max_ano[31:41])
yob15=max(SJBV$Max_ano[31:46])
yob20=max(SJBV$Max_ano[31:50])

obs1=c(yob5,yob10,yob15,yob20)

### niveis de retorno
quant1=mean(rl_ga1[,1])
quant2=mean(rl_ga1[,2])
quant3=mean(rl_ga1[,3])
quant4=mean(rl_ga1[,4])

pred_bayes=c(quant1,quant2,quant3,quant4);pred_bayes

## Erro medio de predicao 
Emp_1_gama= abs((obs1-pred_bayes)/obs1)
round(mean(Emp_1_gama)*100,2)


#######################################################################  
####-----------POSTERIOR WITH prior gama 2 - differences of quantiles #####################
######################################################################

n.size.chain=5500
burn.in=500
thi.n=1

a_gam2=c(a1_gam2,a2_gam2,a3_gam2)
b_gam2=c(b1_gam2,b2_gam2,b3_gam2)

pn_gam2<-prior.quant(prob=c(0.1,0.01,0.005),shape=a_gam2, scale=b_gam2)

psd0<- ar.choice(init=c(65,12,0.1), prior=pn_gam2, lh = "gev", n = 1000, psd=rep(.01, 3), data=SJBV$Max_ano[1:30])

##OBTENCAO DA POSTERIORI GAMA 2
poste_gam2<-posterior(n.size.chain, init = c(65,12,0.1), prior = pn_gam2, lh = "gev",data=SJBV$Max_ano[1:30], psd = psd0$psd,burn=burn.in,thin=thi.n)

#####niveis de retorno 
rl_ga2=func_rl(poste = poste_gam2,prob =c(1/5,1/10,1/15,1/20))

post_ga2=cbind(poste_gam2,rl_ga2) # posterior parameters and return levels

#criterios de diagnostico 
g_ga2=as.data.frame(geweke.diag(post_ga2)[1])$z  # GEweke
r_ga2=raftery.diag(post_ga2)$resmatrix[,4]     #raftery e lewis
h_ga2=heidel.diag(post_ga2)[,3]    #heidelberger e welch

#dataframe reunindo os resultados dos criterios de convergencia
grh_gama2=cbind(g_ga2,r_ga2,h_ga2)

#dataframe reunindo os resultados da posteriori
smpt_gama2=cbind(summary(as.mcmc(post_ga2))$statistics[,1],summary(as.mcmc(post_ga2))$statistics[,2],HPDinterval(as.mcmc(post_ga2)),grh_gama2)  ## media e mediana E HPD a posteriori

#salvar os resultados da posteriori em arquivo excel, opcional  ####
write.xlsx(smpt_gama2,"gama2.xlsx",row.names=T)

## DIC do modelo
dic_gama2=dic(poste_gam2,SJBV$Max_ano[1:30])

#calcular o viés
vies_ga2=smpt_gama2[1:7]-c(locat,sca,sha,quant)

#salvar o intervalo HPD 
hpdg2=HPDinterval(rl_ga2);hpdg2

#calcular a amplitude media 
amp_hpd2=mean(hpdg2[1,2]-hpdg2[1,1],hpdg2[2,2]-hpdg2[2,1],hpdg2[3,2]-hpdg2[3,1],hpdg2[4,2]-hpdg2[4,1])

# valores observados de precipitação maxima em Sao JOao da Boa Vista
yob5=max(SJBV$Max_ano[31:36])
yob10=max(SJBV$Max_ano[31:41])
yob15=max(SJBV$Max_ano[31:46])
yob20=max(SJBV$Max_ano[31:50])

obs1=c(yob5,yob10,yob15,yob20)

### niveis de retorno
ga2_quant1=mean(rl_ga2[,1])
ga2_quant2=mean(rl_ga2[,2])
ga2_quant3=mean(rl_ga2[,3])
ga2_quant4=mean(rl_ga2[,4])

ga2_pred_bayes=c(ga2_quant1,ga2_quant2,ga2_quant3,ga2_quant4);ga2_pred_bayes

## Erro medio de predicao 
Emp_2_gama= abs((obs1-ga2_pred_bayes)/obs1)
round(mean(Emp_2_gama)*100,2)

####=========== normal - não info =================####

n.size.chain=5500
burn.in=500
thi.n=1

vmed <-c(0,0,0)  ## Hiperparametros para a media
mvar <- diag(c(10000000, 1000000, 100)) ## Hiperparametros para matriz de variancia

p_naoinf <- prior.norm(mean = vmed , cov = mvar)  # priori

psd0<- ar.choice(init=c(65,12,0.1), p_naoinf, lh = "gev", n = 1000, psd=rep(.01, 3), data=SJBV$Max_ano[1:30])

poste_ni<-posterior(n.size.chain, init = c(65,12,0.1), prior = p_naoinf, lh = "gev",data=SJBV$Max_ano[1:30], psd = psd0$psd,burn=burn.in,thin=thi.n)

rl_ni=func_rl(poste = poste_ni,prob =c(1/5,1/10,1/15,1/20))

post_ni=cbind(poste_ni,rl_ni) # posterior parameters and return levels

g_ni=as.data.frame(geweke.diag(post_ni)[1])$z
r_ni=raftery.diag(post_ni)$resmatrix[,4]
h_ni=heidel.diag(post_ni)[,3]

grh_ni=cbind(g_ni,r_ni,h_ni)
smpt_ni=cbind(summary(as.mcmc(post_ni))$statistics[,1],summary(as.mcmc(post_ni))$statistics[,2],HPDinterval(as.mcmc(post_ni)),grh_ni)  ## media e mediana E HPD a posteriori

write.xlsx(smpt_ni,"ni.xlsx",row.names=T)

dic_ni=dic(poste_ni,SJBV$Max_ano[1:30])

hpdn=HPDinterval(rl_ni);hpdn

amp_hpd_ni=mean(hpdn[1,2]-hpdn[1,1],hpdn[2,2]-hpdn[2,1],hpdn[3,2]-hpdn[3,1],hpdn[4,2]-hpdn[4,1])

yob5=max(SJBV$Max_ano[31:36])
yob10=max(SJBV$Max_ano[31:41])
yob15=max(SJBV$Max_ano[31:46])
yob20=max(SJBV$Max_ano[31:50])

obs1=c(yob5,yob10,yob15,yob20)

quant1=mean(rl_ni[,1])
quant2=mean(rl_ni[,2])
quant3=mean(rl_ni[,3])
quant4=mean(rl_ni[,4])

pred_bayes_ni=c(quant1,quant2,quant3,quant4);pred_bayes_ni

Emp_1_ni= abs((obs1-pred_bayes_ni)/obs1)
round(mean(Emp_1_ni)*100,2)

####=========== normal - var1 =================####

n.size.chain=5500
burn.in=500
thi.n=1

lavras

#(n.size.chain-burn.in)/thi.n
vmed1 <-c(ml$estimate[1],log(ml$estimate[2]),ml$estimate[3])  ## Hiperparametros para a media
mvar_i1 <- round(ml$var.cov,4) ## Hiperparametros para matriz de variancia

p_inf1 <- prior.norm(mean = vmed1 , cov = mvar_i1)  # priori

psd_i1<- ar.choice(init=c(60,20,0.1), p_inf1, lh = "gev", n = 1000, psd=rep(.01, 3), data=SJBV$Max_ano[1:30])

poste_i1<-posterior(n.size.chain, init =c(60,20,0.1), prior = p_inf1, lh = "gev",data =SJBV$Max_ano[1:30], psd=psd_i1$psd,burn=burn.in,thin=thi.n)

rl_i1=func_rl(poste_i1,prob =c(1/10,1/50,1/100,1/200))

post_i1=cbind(poste_i1,rl_i1) # posterior parameters and return levels

g_i1=as.data.frame(geweke.diag(post_i1)[1])$z
r_i1=raftery.diag(post_i1)$resmatrix[,4]
h_i1=heidel.diag(post_i1)[,3]

#saveRDS(post,paste("cadeias_gama",k,".RDS",sep=""))

grh_i1=cbind(g_i1,r_i1,h_i1)
smpt_i1=cbind(summary(as.mcmc(post_i1))$statistics[,1],summary(as.mcmc(post_i1))$statistics[,2],HPDinterval(as.mcmc(post_i1)),grh_i1)  ## media e mediana E HPD a posteriori

write.xlsx(smpt_i1,"i1.xlsx",row.names=T)

dic_i1=dic(poste_i1,SJBV$Max_ano[1:30])

hpdn=HPDinterval(rl_i1);hpdn

amp_hpd_i1=mean(hpdn[1,2]-hpdn[1,1],hpdn[2,2]-hpdn[2,1],hpdn[3,2]-hpdn[3,1],hpdn[4,2]-hpdn[4,1])

yob5=max(SJBV$Max_ano[31:36])
yob10=max(SJBV$Max_ano[31:41])
yob15=max(SJBV$Max_ano[31:46])
yob20=max(SJBV$Max_ano[31:50])

obs1=c(yob5,yob10,yob15,yob20)

quant1=mean(rl_i1[,1])
quant2=mean(rl_i1[,2])
quant3=mean(rl_i1[,3])
quant4=mean(rl_i1[,4])

pred_bayes_i1=c(quant1,quant2,quant3,quant4);pred_bayes_i1

Emp_1_i1= abs((obs1-pred_bayes_i1)/obs1)
round(mean(Emp_1_i1)*100,2)

####=========== normal - var4 =================####

n.size.chain=5500
burn.in=500
thi.n=1

vmed4 <-c(ml$estimate[1],log(ml$estimate[2]),ml$estimate[3])  ## Hiperparametros para a media
mvar_i4 <- 4*round(ml$var.cov,4) ## Hiperparametros para matriz de variancia

p_inf4 <- prior.norm(mean = vmed4 , cov = mvar_i4)  # priori

psd_i4<- ar.choice(init=c(60,20,0.1), p_inf4, lh = "gev", n = 1000, psd=rep(.01, 3), data=SJBV$Max_ano[1:30])

poste_i4<-posterior(n.size.chain, init =c(60,20,0.1), prior = p_inf4, lh = "gev",data =SJBV$Max_ano[1:30], psd=psd_i4$psd,burn=burn.in,thin=thi.n)

rl_i4=func_rl(poste_i4,prob =c(1/10,1/50,1/100,1/200))

post_i4=cbind(poste_i4,rl_i4) # posterior parameters and return levels

g_i4=as.data.frame(geweke.diag(post_i4)[1])$z
r_i4=raftery.diag(post_i4)$resmatrix[,4]
h_i4=heidel.diag(post_i4)[,3]

grh_i4=cbind(g_i4,r_i4,h_i4)
smpt_i4=cbind(summary(as.mcmc(post_i4))$statistics[,1],summary(as.mcmc(post_i4))$statistics[,2],HPDinterval(as.mcmc(post_i4)),grh_i4)  ## media e mediana E HPD a posteriori

write.xlsx(smpt_i4,"i4.xlsx",row.names=T)

dic_i4=dic(poste_i4,SJBV$Max_ano[1:30])

hpdn=HPDinterval(rl_i4);hpdn

amp_hpd_i4=mean(hpdn[1,2]-hpdn[1,1],hpdn[2,2]-hpdn[2,1],hpdn[3,2]-hpdn[3,1],hpdn[4,2]-hpdn[4,1])

yob5=max(SJBV$Max_ano[31:36])
yob10=max(SJBV$Max_ano[31:41])
yob15=max(SJBV$Max_ano[31:46])
yob20=max(SJBV$Max_ano[31:50])

obs1=c(yob5,yob10,yob15,yob20)

quant1=mean(rl_i4[,1])
quant2=mean(rl_i4[,2])
quant3=mean(rl_i4[,3])
quant4=mean(rl_i4[,4])

pred_bayes_i4=c(quant1,quant2,quant3,quant4);pred_bayes_i4

Emp_1_i4= abs((obs1-pred_bayes_i4)/obs1)
round(mean(Emp_1_i4)*100,2)

amp_hpd1
amp_hpd2
amp_hpd_ni
amp_hpd_i1
amp_hpd_i4


dic_gama1
dic_gama2
dic_ni
dic_i1
dic_i4


round(mean(Emp_1_gama)*100,2)
round(mean(Emp_2_gama)*100,2)
round(mean(Emp_1_ni)*100,2)
round(mean(Emp_1_i1)*100,2)
round(mean(Emp_1_i4)*100,2)
