
if(!require(coda)){
  install.packages("coda")
  library(coda)
}
if(!require(evd)){
  install.packages("evd")
  library(evd)
}

if(!require(evdbayes)){
  install.packages("evdbayes")
  library(evdbayes)
}

if(!require(openxlsx)){
  install.packages("openxlsx")
  library(openxlsx)
}

############################################################################
######----------- trocar ar.choice por ar.choice2

source("ar.choice2.R")

#################################################################
##---------------obtencao das diferen?as de quantis para priori gama 1  ("diferen?a direta" de quantis) GISEle e Thais--################# 
###################################

Median_q10=37.13
Median_q50=38.23
Median_q100=38.54

LI_q10=36.59
LI_q50=37.49
LI_q100=37.7

LS_q10=37.73
LS_q50=39.16
LS_q100=39.62

df_median_qp2=Median_q50-Median_q10

df_median_qp3=Median_q100-Median_q50

df_LI_qp2=LI_q50-LI_q10

df_LI_qp3=LI_q100-LI_q50

df_LS_qp2=LS_q50-LS_q10

df_LS_qp3=LS_q100-LS_q50

####-------para carregar a rotina auxiliar (.R) que cont?m a funcao que retorno os parametros da gama a partir dos quantis  -------#### 

source("gamma.parms.from.quantiles.R")

## Hiperparametros

par_gam1_qp1=gamma.parms.from.quantiles(c(LI_q10,LS_q10), p=c(0.025,0.975))

a1_gam1=par_gam1_qp1$shape
b1_gam1=par_gam1_qp1$scale

par_gam1_qp2=gamma.parms.from.quantiles(c(df_LI_qp2,df_LS_qp2), p=c(0.025,0.975))

a2_gam1=par_gam1_qp2$shape
b2_gam1=par_gam1_qp2$scale

par_gam1_qp3=gamma.parms.from.quantiles(c(0.01,df_LS_qp3), p=c(0.025,0.975))

a3_gam1=par_gam1_qp3$shape
b3_gam1=par_gam1_qp3$scale


#################################################################
##-----obtencao das diferen?as de quantis para priori gama 2  ("diferen?a entre o lim superior (quantil pi) e o lim superior (quantil pi+1)--################# 
###################################

df2_LI_qp2=LI_q50-LS_q10

df2_LI_qp3=LI_q100-LS_q50

df2_LS_qp2=LS_q50-LI_q10

df2_LS_qp3=LS_q100-LI_q50

####-------para carregar a rotina auxiliar (.R) que cont?m a funcao que retorno os parametros da gama a partir dos quantis  -------#### 

source("gamma.parms.from.quantiles.R")


## Hiperparametros

par_gam2_qp1=gamma.parms.from.quantiles(c(LI_q10,LS_q10), p=c(0.025,0.975))

a1_gam2=par_gam1_qp1$shape
b1_gam2=par_gam1_qp1$scale

par_gam2_qp2=gamma.parms.from.quantiles(c(0.01,df2_LS_qp2), p=c(0.025,0.975))

a2_gam2=par_gam2_qp2$shape
b2_gam2=par_gam2_qp2$scale

par_gam2_qp3=gamma.parms.from.quantiles(c(0.01,df2_LS_qp3), p=c(0.025,0.975))

a3_gam2=par_gam2_qp3$shape
b3_gam2=par_gam2_qp3$scale

#### Fun??es auxiliares  - DIC, n?vel de retorno e criterios de diagostico

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

test_diag=function(diagns,nchain,burn,th,rf,hl,ge){
  if(!rf){
    n.size.chain=nchain+nchain
    burn.in=burn+burn
    thi.n=th+th 
    list(diag.ns=diagns,n.chain=n.size.chain,bur.n=burn.in,th.in=thi.n)
  }
  else{
    if(!hl){
      n.size.chain=nchain+nchain
      burn.in=burn+burn
      thi.n=th+th
      list(diag.ns=diagns,n.chain=n.size.chain,bur.n=burn.in,th.in=thi.n)
    }else{
      if(!ge){
        n.size.chain=nchain+nchain
        burn.in=burn+burn
        thi.n=th+th
        list(diag.ns=diagns,n.chain=n.size.chain,bur.n=burn.in,th.in=thi.n)
      }else{
        diagns <- T; list(diag.ns=diagns,n.chain=n.size.chain,bur.n=burn.in,th.in=thi.n)
      }
    }
  }
  
}


n.simu=1000  #numero de simulacoes

k=1

while(k <=n.simu) {
  
  n=30  
  locat=34
  sca=2
  sha=-0.4
  
  sim_max=evd::rgev(n,locat,sca,sha)
  quant=evd::qgev(c(1/10,1/50,1/100,1/200),loc = locat,scale = sca,shape = sha,lower.tail = F)
  
#######################################################################  
####-----------prior gama 1 - differences of quantiles #####################
######################################################################
  
  n.size.chain=5500
  burn.in=500
  thi.n=1
  
  #(n.size.chain-burn.in)/thi.n
  
  a_gam1=c(a1_gam1,a2_gam1,a3_gam1)
  b_gam1=c(b1_gam1,b2_gam1,b3_gam1)
  
  pn_gam1<-prior.quant(prob=c(0.1,0.02,0.01),shape=a_gam1, scale=b_gam1)
  
  psd0<- ar.choice2(init=c(32,1.5,0.01), prior=pn_gam1, lh = "gev", n = 1000, psd=rep(.01, 3), data=sim_max)
  
  diagns=F
  
  while (!diagns) {
    
    if(n.size.chain>1500000){break}
    
    poste_gam1<-posterior(n.size.chain, init = c(32,1.5,0.01), prior = pn_gam1, lh = "gev",data=sim_max, psd = psd0$psd,burn=burn.in,thin=thi.n)
    
    rl_ga1=func_rl(poste = poste_gam1,prob =c(1/10,1/50,1/100,1/200))
    
    post_ga1=cbind(poste_gam1,rl_ga1) # posterior parameters and return levels
    
    g_ga1=as.data.frame(geweke.diag(post_ga1)[1])$z
    r_ga1=raftery.diag(post_ga1)$resmatrix[,4]
    h_ga1=heidel.diag(post_ga1)[,3]
    
    Rf_ga1=all(r_ga1[]<=1.99) 
    Hl_ga1=all(h_ga1[]>0.05)
    Ge_ga1=all(abs(g_ga1[])<=1.96)
  
    diagnostics=test_diag(diagns = diagns,n.size.chain,burn.in,thi.n,Rf_ga1,Hl_ga1,Ge_ga1)
    
    diagns=diagnostics$diag.ns
    n.size.chain=diagnostics$n.chain
    burn.in=diagnostics$bur.n
    thi.n=diagnostics$th.in
      
              }  
    
  if(n.size.chain>1500000){next}
  
  #saveRDS(post,paste("cadeias_gama",k,".RDS",sep=""))
  
  grh_gama1=cbind(g_ga1,r_ga1,h_ga1)
  smpt_gama1=cbind(summary(as.mcmc(post_ga1))$statistics[,1],summary(as.mcmc(post_ga1))$quantiles[,3],HPDinterval(as.mcmc(post_ga1)))  ## media e mediana E HPD a posteriori
  dic_gama1=dic(poste_gam1,sim_max)
  vies_ga1=smpt_gama1[1:7]-c(locat,sca,sha,quant)

  #######################################################################  
  ####-----------prior gama 2 - differences of quantiles #####################
  ######################################################################
  
  n.size.chain=5500
  burn.in=500
  thi.n=1
  
  #(n.size.chain-burn.in)/thi.n
  
  a_gam2=c(a1_gam2,a2_gam2,a3_gam2)
  b_gam2=c(b1_gam2,b2_gam2,b3_gam2)
  
  pn_gam2<-prior.quant(prob=c(0.1,0.02,0.01),shape=a_gam2, scale=b_gam2)
  
  psd0<- ar.choice2(init=c(32,1.5,0.01), prior=pn_gam2, lh = "gev", n = 1000, psd=rep(.01, 3), data=sim_max)
  
  diagns=F
  
  while (!diagns) {
    
    if(n.size.chain>1500000){break}
    
    poste_gam2<-posterior(n.size.chain, init = c(32,1.5,0.01), prior = pn_gam2, lh = "gev",data=sim_max, psd = psd0$psd,burn=burn.in,thin=thi.n)
    
    rl_ga2=func_rl(poste = poste_gam2,prob =c(1/10,1/50,1/100,1/200))
    
    post_ga2=cbind(poste_gam2,rl_ga2) # posterior parameters and return levels

    g_ga2=as.data.frame(geweke.diag(post_ga2)[1])$z
    r_ga2=raftery.diag(post_ga2)$resmatrix[,4]
    h_ga2=heidel.diag(post_ga2)[,3]
    
    Rf_ga2=all(r_ga2[]<=1.99) 
    Hl_ga2=all(h_ga2[]>0.05)
    Ge_ga2=all(abs(g_ga2[])<=1.96)
    
    diagnostics=test_diag(diagns = diagns,n.size.chain,burn.in,thi.n,Rf_ga2,Hl_ga2,Ge_ga2)
    
    diagns=diagnostics$diag.ns
    n.size.chain=diagnostics$n.chain
    burn.in=diagnostics$bur.n
    thi.n=diagnostics$th.in
    
  }  
  
  if(n.size.chain>1500000){next}
  
  #saveRDS(post,paste("cadeias_gama",k,".RDS",sep=""))
  
  grh_gama2=cbind(g_ga2,r_ga2,h_ga2)
  smpt_gama2=cbind(summary(as.mcmc(post_ga2))$statistics[,1],summary(as.mcmc(post_ga2))$quantiles[,3],HPDinterval(as.mcmc(post_ga2)))  ## media e mediana E HPD a posteriori
  dic_gama2=dic(poste_gam2,sim_max)
  vies_ga2=smpt_gama2[1:7]-c(locat,sca,sha,quant)
  
  
  ###########################################################################################################
  #--------------- priori normal multivariada nao informativa -----------------------
  ###################################################################################################
  
  
  n.size.chain=5500
  burn.in=500
  thi.n=1
  
  vmed <-c(0,0,0)  ## Hiperparametros para a media
  mvar <- diag(c(10000, 10000, 100)) ## Hiperparametros para matriz de variancia
  
  p_naoinf <- prior.norm(mean = vmed , cov = mvar)  # priori
  
  psd0<- ar.choice2(init=c(32,1.5,0.01), p_naoinf, lh = "gev", n = 1000, psd=rep(.01, 3), data=sim_max)
  
  diagns=F
  
  while (!diagns) {
    
    if(n.size.chain>1500000){break}
    
    poste_ni<-posterior(n.size.chain, init =c(32,1.5,0.01), prior = p_naoinf, lh = "gev",data =sim_max, psd=psd0$psd,burn=burn.in,thin=thi.n)
    
    rl_ni=func_rl(poste_ni,prob =c(1/10,1/50,1/100,1/200))
    
    post_ni=cbind(poste_ni,rl_ni) # posterior parameters and return levels
    
    g_ni=as.data.frame(geweke.diag(post_ni)[1])$z
    r_ni=raftery.diag(post_ni)$resmatrix[,4]
    h_ni=heidel.diag(post_ni)[,3]
    
    Rf_ni=all(r_ni[]<=1.99)
    Hl_ni=all(h_ni[]>0.05)
    Ge_ni=all(abs(g_ni[])<=1.96)
    
    diagnostics=test_diag(diagns = diagns,n.size.chain,burn.in,thi.n,Rf_ni,Hl_ni,Ge_ni)
    
    diagns=diagnostics$diag.ns
    n.size.chain=diagnostics$n.chain
    burn.in=diagnostics$bur.n
    thi.n=diagnostics$th.in
    
  }  
  
  if(n.size.chain>1500000){next}
  
  #saveRDS(post_ni,paste("cadeias_norm_ni",k,".RDS",sep=""))
  
  grh_ni=cbind(g_ni,r_ni,h_ni)
  smpt_ni=cbind(summary(as.mcmc(post_ni))$statistics[,1],summary(as.mcmc(post_ni))$quantiles[,3],HPDinterval(as.mcmc(post_ni))) 
  dic_ni=dic(poste_ni,sim_max)
  vies_ni=smpt_ni[1:7]-c(locat,sca,sha,quant)
  

  ###########################################################################################################
  #--------------- priori normal multivariada com matriz de var.cov multip por 1 -----------------------
  ###################################################################################################
  
  n.size.chain=5500
  burn.in=500
  thi.n=1
  
  vmed <-c(34,log(2),-0.4)  ## Hiperparametros para a media
  mvar_i1 <- round(diag(c(0.1553,0.07893,0.015748),3),5) ## Hiperparametros para matriz de variancia
  
  p_inf1 <- prior.norm(mean = vmed , cov = mvar_i1)  # priori
  
  psd_i1<- ar.choice2(init=c(32,1.5,0.01), p_inf1, lh = "gev", n = 1000, psd=rep(.01, 3), data=sim_max)
  
  diagns=F
  
  while (!diagns) {
    
    if(n.size.chain>1500000){break}
    
    poste_i1<-posterior(n.size.chain, init =c(32,1.5,0.01), prior = p_inf1, lh = "gev",data =sim_max, psd=psd_i1$psd,burn=burn.in,thin=thi.n)
    
    rl_i1=func_rl(poste_i1,prob =c(1/10,1/50,1/100,1/200))
    
    post_i1=cbind(poste_i1,rl_i1) # posterior parameters and return levels
    
    g_i1=as.data.frame(geweke.diag(post_i1)[1])$z
    r_i1=raftery.diag(post_i1)$resmatrix[,4]
    h_i1=heidel.diag(post_i1)[,3]
    
    Rf_i1=all(r_i1[]<=1.99)
    Hl_i1=all(h_i1[]>0.05)
    Ge_i1=all(abs(g_i1[])<=1.96)
    
    diagnostics=test_diag(diagns = diagns,n.size.chain,burn.in,thi.n,Rf_i1,Hl_i1,Ge_i1)
    
    diagns=diagnostics$diag.ns
    n.size.chain=diagnostics$n.chain
    burn.in=diagnostics$bur.n
    thi.n=diagnostics$th.in
  }  
  
  if(n.size.chain>1500000){next}
  #saveRDS(post_i1,paste("cadeias_norm_i1",k,".RDS",sep=""))
  
  grh_i1=cbind(g_i1,r_i1,h_i1)
  smpt_i1=cbind(summary(as.mcmc(post_i1))$statistics[,1],summary(as.mcmc(post_i1))$quantiles[,3],HPDinterval(as.mcmc(post_i1))) 
  dic_i1=dic(poste_i1,sim_max)
  vies_i1=smpt_i1[1:7]-c(locat,sca,sha,quant)
  
  ###########################################################################################################
  #--------------- priori normal multivariada com matriz de var.cov multip por 4 -----------------------
  ###################################################################################################
  
  n.size.chain=5500
  burn.in=500
  thi.n=1
  
  vmed <-c(34,log(2),-0.4)  ## Hiperparametros para a media
  mvar_i4 <- round(4*diag(c(0.1553,0.07893,0.015748),3),5) ## Hiperparametros para matriz de variancia
  
  p_inf4 <- prior.norm(mean = vmed , cov = mvar_i4)  # priori
  
  psd_i4<- ar.choice2(init=c(30,1.5,0.01), p_inf4, lh = "gev", n = 1000, psd=rep(.01, 3), data=sim_max)
  
  diagns=F
  
  while (!diagns) {
    
    if(n.size.chain>1500000){break}
    
    poste_i4<-posterior(n.size.chain, init =c(30,1.5,0.01), prior = p_inf4, lh = "gev",data =sim_max, psd=psd_i4$psd,burn=burn.in,thin=thi.n)
    
    rl_i4=func_rl(poste_i4,prob =c(1/10,1/50,1/100,1/200))
    
    post_i4=cbind(poste_i4,rl_i4) # posterior parameters and return levels
    
    g_i4=as.data.frame(geweke.diag(post_i4)[1])$z
    r_i4=raftery.diag(post_i4)$resmatrix[,4]
    h_i4=heidel.diag(post_i4)[,3]
    
    Rf_i4=all(r_i4[]<=1.99)
    Hl_i4=all(h_i4[]>0.05)
    Ge_i4=all(abs(g_i4[])<=1.96)
    
    diagnostics=test_diag(diagns = diagns,n.size.chain,burn.in,thi.n,Rf_i4,Hl_i4,Ge_i4)
    
    diagns=diagnostics$diag.ns
    n.size.chain=diagnostics$n.chain
    burn.in=diagnostics$bur.n
    thi.n=diagnostics$th.in
  }  
  
  if(n.size.chain>1500000){next}
  #saveRDS(post_i4,paste("cadeias_norm_i4",k,".RDS",sep=""))
  
  grh_i4=cbind(g_i4,r_i4,h_i4)
  smpt_i4=cbind(summary(as.mcmc(post_i4))$statistics[,1],summary(as.mcmc(post_i4))$quantiles[,3],HPDinterval(as.mcmc(post_i4))) 
  dic_i4=dic(poste_i4,sim_max)
  vies_i4=smpt_i4[1:7]-c(locat,sca,sha,quant)
  ######################################################################################################
  ###---------------------- reunir e salvar os resultados -----------------------------------##########
  ######################################################################################################
  resumo_sim=data.frame(matrix(c(t(smpt_gama1),vies_ga1,dic_gama1,t(smpt_gama2),vies_ga2,dic_gama2,t(smpt_ni),vies_ni,dic_ni,t(smpt_i1),vies_i1,dic_i1,t(smpt_i4),vies_i4,dic_i4),1,(36*5)))
  resumo_converge_sim=data.frame(matrix(c(t(grh_gama1),t(grh_gama2),t(grh_ni),t(grh_i1),t(grh_i4)),1,21*5))
  
  saveRDS(resumo_sim,paste("res_fit_sim",k,".RDS",sep=""))  #salva os resultados como um objeto R (no caso, um data frame)
  saveRDS(resumo_converge_sim,paste("res_diag_sim",k,".RDS",sep=""))  #salva os resultados de criterios convergencia 
  k=k+1
}


########## para carregar os resultados de estimativas de todas as simulacoes 

#n.simu=500

read_res=function(i){
  results=readRDS(paste("res_fit_sim",i,".RDS",sep=""))
  return(results)
}

dt=data.frame()
for (i in 1:n.simu) {
  dt[i,1:180]<-read_res(i)
}

head(dt)

nome_ga1=rep(paste0("ga1."),36)
nome_ga2=rep(paste0("ga2."),36)
nome_ni=rep(paste0("ni."),36)
nome_i1=rep(paste0("i1."),36)
nome_i4=rep(paste0("i4."),36)
nome_para=c("mu.med","mu.median","mu.li","mu.ls","sig.med","sig.median","sig.li","sig.ls","xi.med","xi.median","xi.li","xi.ls","t10.med","t10.median","t10.li","t10.ls","t50.med","t50.median","t50.li","t50.ls","t100.med","t100.median","t100.li","t100.ls","t200.med","t200.median","t200.li","t200.ls","vs.mu","vs.sig","vs.xi","vs.t10","vs.t50","vs.t100","vs.t200","dic")

nam_ga1=character(36)
for(k in 1:36){
  nam_ga1[k]=paste0(nome_ga1[k],nome_para[k])
}

nam_ga2=character(36)
for(k in 1:36){
  nam_ga2[k]=paste0(nome_ga2[k],nome_para[k])
}

nam_ni=character(36)
for(k in 1:36){
  nam_ni[k]=paste0(nome_ni[k],nome_para[k])
}

nam_i1=character(36)
for(k in 1:36){
  nam_i1[k]=paste0(nome_i1[k],nome_para[k])
}

nam_i4=character(36)
for(k in 1:36){
  nam_i4[k]=paste0(nome_i4[k],nome_para[k])
}

colnames(dt)<-c(nam_ga1,nam_ga2,nam_ni,nam_i1,nam_i4)

dt  ##  para imprimir os resultados com as estimativas dos parametros e niveis de retorno


########## para carregar os resultados dos criterios de convergencia de todas as simulacoes

read_conv=function(i){
  conver=readRDS(paste("res_diag_sim",i,".RDS",sep=""))
  return(conver)
}

dt2=data.frame()
for (i in 1:n.simu) {
  dt2[i,1:105]<-read_conv(i)
}

head(dt2)

nome_ga1=rep(paste0("ga1."),21)
nome_ga2=rep(paste0("ga2."),21)
nome_ni=rep(paste0("ni."),21)
nome_i1=rep(paste0("i1."),21)
nome_i4=rep(paste0("i4."),21)

nome_crit=c("mu.gew","mu.raf","mu.hei","sig.gew","sig.raf","sig.hei","xi.gew","xi.raf","xi.hei","t10.gew","t10.raf","t10.hei","t50.gew","t50.raf","t50.hei","t100.gew","t100.raf","t100.hei","t200.gew","t200.raf","t200.hei")

crit_ga1=character(21)
for(k in 1:21){
  crit_ga1[k]=paste0(nome_ga1[k],nome_crit[k])
}

crit_ga2=character(21)
for(k in 1:21){
  crit_ga2[k]=paste0(nome_ga2[k],nome_crit[k])
}

crit_ni=character(21)
for(k in 1:21){
  crit_ni[k]=paste0(nome_ni[k],nome_crit[k])
}

crit_i1=character(21)
for(k in 1:21){
  crit_i1[k]=paste0(nome_i1[k],nome_crit[k])
}

crit_i4=character(21)
for(k in 1:21){
  crit_i4[k]=paste0(nome_i4[k],nome_crit[k])
}

colnames(dt2)<-c(crit_ga1,crit_ga2,crit_ni,crit_i1,crit_i4)

dt2  # para imprimir os resultados de convergencia

saveRDS(dt,"resultados.RDS")

resumo_simulacao=readRDS("resultados.RDS")

media_tudo=sapply(as.list(resumo_simulacao),mean)

media_tudo2=data.frame(matrix((media_tudo),5,36,byrow = T),row.names = c("Gama1","Gama2","Naoinfo","Info1","Info4"))
colnames(media_tudo2)<-nome_para

options(OutDec=",")

write.xlsx(round(media_tudo2,4),"valormedio_simu_todos.xlsx",row.names=T)

vies.medio=list(vies.medio.ga1=as.numeric(media_tudo2[which(row.names(media_tudo2)=="Gama1"),29:35]),vies.medio.ga2=as.numeric(media_tudo2[which(row.names(media_tudo2)=="Gama2"),29:35]),vies.medio.ni=as.numeric(media_tudo2[which(row.names(media_tudo2)=="Naoinfo"),29:35]),vies.medio.i1=as.numeric(media_tudo2[which(row.names(media_tudo2)=="Info1"),29:35]),vies.medio.i4=as.numeric(media_tudo2[which(row.names(media_tudo2)=="Info4"),29:35]))
vies.medio.rel=list(vies.medio.rel.ga1=abs(as.numeric(media_tudo2[which(row.names(media_tudo2)=="Gama1"),29:35]))*100/c(locat,sca,sha,quant),vies.medio.rel.ga2=abs(as.numeric(media_tudo2[which(row.names(media_tudo2)=="Gama2"),29:35]))*100/c(locat,sca,sha,quant),vies.medio.rel.ni=abs(as.numeric(media_tudo2[which(row.names(media_tudo2)=="Naoinfo"),29:35]))*100/c(locat,sca,sha,quant),vies.medio.rel.i1=abs(as.numeric(media_tudo2[which(row.names(media_tudo2)=="Info1"),29:35]))*100/c(locat,sca,sha,quant),vies.medio.rel.i4=abs(as.numeric(media_tudo2[which(row.names(media_tudo2)=="Info4"),29:35]))*100/c(locat,sca,sha,quant))
erro.quad.medio=list(erro.quad.medio.ga1=(as.numeric(media_tudo2[which(row.names(media_tudo2)=="Gama1"),29:35]))^2,erro.quad.medio.ga2=(as.numeric(media_tudo2[which(row.names(media_tudo2)=="Gama2"),29:35]))^2,erro.quad.medio.ni=(as.numeric(media_tudo2[which(row.names(media_tudo2)=="Naoinfo"),29:35]))^2,erro.quad.medio.i1=(as.numeric(media_tudo2[which(row.names(media_tudo2)=="Info1"),29:35]))^2,erro.quad.medio.i4=(as.numeric(media_tudo2[which(row.names(media_tudo2)=="Info4"),29:35]))^2)

options(OutDec=",")

medidas_vies=data.frame(vies.medio,vies.medio.rel,erro.quad.medio,row.names =c("mu","sigma","xi","t10","t50","t100","t200"))

write.xlsx(round(medidas_vies,4),"vies_todos.xlsx",row.names=T)





