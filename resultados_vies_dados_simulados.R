
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

if(!require(plyr)){
  install.packages("plyr")
  library(plyr)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}


if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

library(patchwork)

if(!require(ggpubr)){
  install.packages("ggpubr")
  library(ggpubr)
}


########## para carregar os resultados de estimativas de todas as simulacoes 

n.simu=1000

read_res=function(i){
  results=readRDS(paste("res_fit_sim",i,".RDS",sep=""))
  return(results)
}

dt=data.frame()
for (i in 1:n.simu) {
  dt[i,1:180]<-read_res(i)
}


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
  ##  para imprimir os resultados com as estimativas dos parametros e niveis de retorno


########## para carregar os resultados dos criterios de convergencia de todas as simulacoes

read_conv=function(i){
  conver=readRDS(paste("res_diag_sim",i,".RDS",sep=""))
  return(conver)
}

dt2=data.frame()
for (i in 1:n.simu) {
  dt2[i,1:105]<-read_conv(i)
}


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
  # para imprimir os resultados de convergencia

saveRDS(dt,"resultados.RDS")


resumo_simulacao=readRDS("resultados.RDS")

paste0("resumo_simulacao","2")

r1=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados.RDS"))
r2=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (1).RDS"))
r3=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (2).RDS"))
r4=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (3).RDS"))
r5=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (4).RDS"))
r6=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (5).RDS"))
r7=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (6).RDS"))
r8=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (7).RDS"))
r9=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (8).RDS"))
r10=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (9).RDS"))
r11=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (10).RDS"))
r12=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (11).RDS"))
r13=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (12).RDS"))
r14=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (13).RDS"))
r15=readRDS(paste0("C:/Users/mathe/OneDrive/Projeto_mestrado/rotina/","resultados (14).RDS"))


medidas_vies=function(r2,prior,tamanho="30"){

media_2=sapply(as.list(r2),mean)

media_r2=data.frame(matrix((media_2),5,36,byrow = T),row.names = c("Gama1","Gama2","Naoinfo","Info1","Info4"))

nome_ga1=rep(paste0("ga1."),36)
nome_ga2=rep(paste0("ga2."),36)
nome_ni=rep(paste0("ni."),36)
nome_i1=rep(paste0("i1."),36)
nome_i4=rep(paste0("i4."),36)
nome_para=c("mu.med","mu.median","mu.li","mu.ls","sig.med","sig.median","sig.li","sig.ls","xi.med","xi.median","xi.li","xi.ls","t10.med","t10.median","t10.li","t10.ls","t50.med","t50.median","t50.li","t50.ls","t100.med","t100.median","t100.li","t100.ls","t200.med","t200.median","t200.li","t200.ls","vs.mu","vs.sig","vs.xi","vs.t10","vs.t50","vs.t100","vs.t200","dic")

colnames(media_r2)<-nome_para

#options(OutDec=",")

#write.xlsx(round(media_r1,4),"valormedio_simu_todos.xlsx",row.names=T)
locat=NA
sca=NA
sha=NA

if(prior=="p1"){
locat=70
sca=30
sha=0.4}

if(prior=="p2"){
  locat=70
  sca=25
  sha=0.05}

if(prior=="p3"){
  locat=34
  sca=2
  sha=-0.4}

if(prior=="p4"){
  locat=6.78
  sca=0.561
  sha=-0.2}

if(prior=="p5"){
  locat=1200
  sca=340
  sha=0.1}

quant=evd::qgev(c(1/10,1/50,1/100,1/200),loc = locat,scale = sca,shape = sha,lower.tail = F)

vies.medio=list(vies.medio.ga1=as.numeric(media_r2[which(row.names(media_r2)=="Gama1"),29:35]),vies.medio.ga2=as.numeric(media_r2[which(row.names(media_r2)=="Gama2"),29:35]),vies.medio.ni=as.numeric(media_r2[which(row.names(media_r2)=="Naoinfo"),29:35]),vies.medio.i1=as.numeric(media_r2[which(row.names(media_r2)=="Info1"),29:35]),vies.medio.i4=as.numeric(media_r2[which(row.names(media_r2)=="Info4"),29:35]))
vies.medio.rel=list(vies.medio.rel.ga1=abs(as.numeric(media_r2[which(row.names(media_r2)=="Gama1"),29:35])*100/c(locat,sca,sha,quant)),vies.medio.rel.ga2=abs(as.numeric(media_r2[which(row.names(media_r2)=="Gama2"),29:35])*100/c(locat,sca,sha,quant)),vies.medio.rel.ni=abs(as.numeric(media_r2[which(row.names(media_r2)=="Naoinfo"),29:35])*100/c(locat,sca,sha,quant)),vies.medio.rel.i1=abs(as.numeric(media_r2[which(row.names(media_r2)=="Info1"),29:35])*100/c(locat,sca,sha,quant)),vies.medio.rel.i4=abs(as.numeric(media_r2[which(row.names(media_r2)=="Info4"),29:35])*100/c(locat,sca,sha,quant)))
erro.quad.medio=list(erro.quad.medio.ga1=(as.numeric(media_r2[which(row.names(media_r2)=="Gama1"),29:35]))^2,erro.quad.medio.ga2=(as.numeric(media_r2[which(row.names(media_r2)=="Gama2"),29:35]))^2,erro.quad.medio.ni=(as.numeric(media_r2[which(row.names(media_r2)=="Naoinfo"),29:35]))^2,erro.quad.medio.i1=(as.numeric(media_r2[which(row.names(media_r2)=="Info1"),29:35]))^2,erro.quad.medio.i4=(as.numeric(media_r2[which(row.names(media_r2)=="Info4"),29:35]))^2)
medidas_vies1=data.frame(vies.medio,vies.medio.rel,erro.quad.medio, param=c("mu","sigma","xi","t10","t50","t100","t200"),n=tamanho)
return(medidas_vies1)
}

vies_medio=function(r1,r2,r3,prior,rl=FALSE){
  v1=medidas_vies(r1,prior,tamanho = "30")
  v2=medidas_vies(r2,prior,tamanho = "60")
  v3=medidas_vies(r3,prior,tamanho = "120")
  
  todos2=v1%>%full_join(v2)%>%full_join(v3)
  
  params_p2=todos2[c(which(todos2$param=="mu"),which(todos2$param=="sigma"),which(todos2$param=="xi")),]
  rlevels_p2=todos2[c(which(todos2$param=="t10"),which(todos2$param=="t50"),which(todos2$param=="t100"),which(todos2$param=="t200")),]
  
  if(rl){
  vies_m2=data.frame(vies.m=c(rlevels_p2$vies.medio.rel.ga1,rlevels_p2$vies.medio.rel.ga2,rlevels_p2$vies.medio.rel.ni,rlevels_p2$vies.medio.rel.i1,rlevels_p2$vies.medio.rel.i4),prioris=c(rep("ga1",12),rep("ga2",12),rep("ni",12),rep("i1",12),rep("i4",12)),n=c(rep(c(30,60,120),20)),tr=c(rep(c(rep("10",3),rep("50",3),rep("100",3),rep("200",3)),5)))
  vies_m2$tr<-factor(vies_m2$tr,levels = c("10","50","100","200"))
  }else{
  vies_m2=data.frame(vies.m=c(params_p2$vies.medio.rel.ga1,params_p2$vies.medio.rel.ga2,params_p2$vies.medio.rel.ni,params_p2$vies.medio.rel.i1,params_p2$vies.medio.rel.i4),prioris=c(rep("ga1",9),rep("ga2",9),rep("ni",9),rep("i1",9),rep("i4",9)),n=c(rep(c(30,60,120),15)),par=c(rep(c(rep("mu",3),rep("sigma",3),rep("xi",3)),5)))
  
  vies_m2$par<-factor(vies_m2$par,levels = c("mu","sigma","xi"))
  }
  vies_m2$prioris<-factor(vies_m2$prioris,levels = c("ga1","ga2","ni","i1","i4"))
  #vies_m2$n<-factor(vies_m2$n,levels = c("30","60","120"))
  
  return(vies_m2)
}

mean(priori2$vies.m)

mean(priori2$vies.m[which(priori2$prioris=="i4"&priori2$par=="mu"&priori2$n=="30")])

priori1=vies_medio(r1,r2,r3,"p1",rl=FALSE)
#priori1$modelo="priori1"
priori2=vies_medio(r4,r5,r6,"p2",rl=FALSE)
#priori2$modelo="priori2"
priori3=vies_medio(r7,r8,r9,"p3",rl=FALSE)
#priori3$modelo="priori3"
priori4=vies_medio(r10,r11,r12,"p4",rl=FALSE)
#priori4$modelo="priori4"
priori5=vies_medio(r13,r14,r15,"p5",rl=FALSE)
#priori5$modelo="priori5"
# 


# priori1$n<-as.integer(priori1$n)
# priori2$n<-as.integer(priori2$n)
# priori3$n<-as.integer(priori3$n)
# priori4$n<-as.integer(priori4$n)
# priori5$n<-as.integer(priori5$n)

#  long <- reshape2::melt(priori, id.vars = c("n", "prioris",vari))
  #long <- reshape2::melt(priori1, id.vars = c("Tamanho", "prioris","paramet","modelo"))
  
  # devtools::install_github("zeehio/facetscales")
  # library(tidyverse)
  # library(facetscales)
  # #Define scales
  # scales_y <- list(
  #   `mu` = scale_y_continuous(limits = c(-10, 20)),
  #   `sigma` = scale_y_continuous(limits = c(-10, 20)),
  #   `xi` = scale_y_continuous(limits = c(-0.52, 0.53))
  # )
  
  source("test_change_scales.R")
# 
#     # Plot
#     don %>%
#       ggplot( aes(x=n, y=vies.m, group=prioris, color=prioris)) +
#       geom_line()+geom_point()
#     
    #priori1$n=ordered(priori1$n, levels=c(30, 60, 120))
    
    
    graf_linha_final=function(priori,para="mu"){
      don1 <- priori %>% 
        filter(par %in% c(para))
      g1= ggplot(don1,aes(x=n, y=vies.m,group=prioris,color=prioris,linetype=prioris,shape=prioris))+
      geom_line(aes(color=prioris,linetype=prioris),size=1.2) +
      theme_bw()+
      #theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 15),axis.text.x = element_text(size=12))+
      theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 12),axis.text.x = element_blank())+
      #geom_point(aes(x=n,shape=prioris),size=1.6) +
      scale_x_continuous(breaks = c(30, 60, 120)) + #breaks = c(30, 60, 120),labels = c("30", "60", "120"))+  
      #scale_linetype(labels=c("10 anos","50 anos","100 anos","200 anos"))+
      #scale_shape_manual(values=c(0,1,2,3),labels=c("10 anos","50 anos","100 anos","200 anos")) +
      #scale_y_log10(breaks = scales::log_breaks(n = 5),labels = scales::number_format(accuracy = 0.01,decimal.mark = ','))+
      labs(x = NULL,   #"Tamanho da amostra (n)",
      y = NULL)+  ## "VAMRP")+   #,linetype="Modelos",shape="Modelos",color="Modelos",size=2) +
      scale_color_manual(name="Modelos",labels = c("ga1"=expression("M"[Gama1]), 
                                    "ga2"=expression("M"[Gama2]), 
                                    "ni"=expression("M"[NTNI]), 
                                    "i1"=expression("M"[NTV1]), 
                                    "i4"=expression("M"[NTV4])), values = c("ga1"="blue", 
                                    "ga2"="green", 
                                    "ni"="red", 
                                    "i1"="brown", 
                                    "i4"="orange")) +
      scale_shape_discrete(name="Modelos",labels = c("ga1"=expression("M"[Gama1]), 
                                                     "ga2"=expression("M"[Gama2]), 
                                                     "ni"=expression("M"[NTNI]), 
                                                     "i1"=expression("M"[NTV1]), 
                                                     "i4"=expression("M"[NTV4]))) +
      scale_linetype_discrete(name="Modelos",labels = c("ga1"=expression("M"[Gama1]), 
                                                        "ga2"=expression("M"[Gama2]), 
                                                        "ni"=expression("M"[NTNI]), 
                                                        "i1"=expression("M"[NTV1]), 
                                                        "i4"=expression("M"[NTV4]))) +
      guides(color = guide_legend(order=1),
             linetype = guide_legend(order=1),shape=guide_legend(order = 1)) # + wrap_elements(grid::textGrob('a'))
    
    }

    graf_linha_final5=function(priori,para="mu"){
      don1 <- priori %>% 
        filter(par %in% c(para))
      g1= ggplot(don1,aes(x=n, y=vies.m,group=prioris,color=prioris,linetype=prioris,shape=prioris))+
        geom_line(aes(color=prioris,linetype=prioris),size=1.2) + 
        theme_bw()+
        theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 12),axis.text.x = element_text(size=12))+
        #theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 15),axis.text.x = element_blank())+
        #geom_point(aes(x=n,shape=prioris),size=1.6) +
        scale_x_continuous(breaks = c(30, 60, 120)) + #breaks = c(30, 60, 120),labels = c("30", "60", "120"))+  
        #scale_linetype(labels=c("10 anos","50 anos","100 anos","200 anos"))+
        #scale_shape_manual(values=c(0,1,2,3),labels=c("10 anos","50 anos","100 anos","200 anos")) +
        #scale_y_log10()+
        labs(x = NULL,   #"Tamanho da amostra (n)",
             y = NULL)+  ## "VAMRP")+   #,linetype="Modelos",shape="Modelos",color="Modelos",size=2) +
        scale_color_manual(name="Modelos",labels = c("ga1"=expression("M"[Gama1]), 
                                                     "ga2"=expression("M"[Gama2]), 
                                                     "ni"=expression("M"[NTNI]), 
                                                     "i1"=expression("M"[NTV1]), 
                                                     "i4"=expression("M"[NTV4])), values = c("ga1"="blue", 
                                                                                             "ga2"="green", 
                                                                                             "ni"="red", 
                                                                                             "i1"="brown", 
                                                                                             "i4"="orange")) +
        scale_shape_discrete(name="Modelos",labels = c("ga1"=expression("M"[Gama1]), 
                                                       "ga2"=expression("M"[Gama2]), 
                                                       "ni"=expression("M"[NTNI]), 
                                                       "i1"=expression("M"[NTV1]), 
                                                       "i4"=expression("M"[NTV4]))) +
        scale_linetype_discrete(name="Modelos",labels = c("ga1"=expression("M"[Gama1]), 
                                                          "ga2"=expression("M"[Gama2]), 
                                                          "ni"=expression("M"[NTNI]), 
                                                          "i1"=expression("M"[NTV1]), 
                                                          "i4"=expression("M"[NTV4]))) +
        guides(color = guide_legend(order=1),
               linetype = guide_legend(order=1),shape=guide_legend(order = 1)) # + wrap_elements(grid::textGrob('a'))
      
    }
    
    #vm=wrap_elements(text_grob(expression("VAMRP em escala log "[10]),rot = 90))
    
    vm=wrap_elements(text_grob(expression("VAMRP (%)"),rot = 90))
    
    a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))
    
    tam=wrap_elements(text_grob("Tamanho amostral"))
         
grafs_p=list(j, k,l,plot_spacer(),
  
gra1_mu=graf_linha_final(priori1,"mu"),
gra1_sigma=graf_linha_final(priori1,"sigma"),
gra1_xi=graf_linha_final(priori1,"xi"),a,

gra2_mu=graf_linha_final(priori2,"mu"),
gra2_sigma=graf_linha_final(priori2,"sigma"),
gra2_xi=graf_linha_final(priori2,"xi"),b,

gra3_mu=graf_linha_final(priori3,"mu"),
gra3_sigma=graf_linha_final(priori3,"sigma"),
gra3_xi=graf_linha_final(priori3,"xi"),c,

gra4_mu=graf_linha_final(priori4,"mu"),
gra4_sigma=graf_linha_final(priori4,"sigma"),
gra4_xi=graf_linha_final(priori4,"xi"),d,

gra5_mu=graf_linha_final5(priori5,"mu"),
gra5_sigma=graf_linha_final5(priori5,"sigma"),
gra5_xi=graf_linha_final5(priori5,"xi"),e,
plot_spacer(),tam,plot_spacer()
)

w1=wrap_plots(grafs_p,guides = 'collect', byrow=TRUE,nrow=7,ncol=4,heights =c(2,8,8,8,8,8) ,widths = c(8,8,8,1))&theme(legend.text = element_text(size = 15),legend.title =element_text(size=15),legend.text.align = 0,legend.position='bottom')  

graf_v_par=(vm|w1) +plot_layout(widths = c(0.3,10))

ggsave(filename = "graf_v_par2.pdf", graf_v_par,
       width = 10, height = 8, dpi = 500, units = "in", device='pdf')

##########################################################################
############# N?veis de retorno ######################

priori1=vies_medio(r1,r2,r3,"p1",rl=TRUE)
#priori1$modelo="priori1"
priori2=vies_medio(r4,r5,r6,"p2",rl=TRUE)
#priori2$modelo="priori2"
priori3=vies_medio(r7,r8,r9,"p3",rl=TRUE)
#priori3$modelo="priori3"
priori4=vies_medio(r10,r11,r12,"p4",rl=TRUE)
#priori4$modelo="priori4"
priori5=vies_medio(r13,r14,r15,"p5",rl=TRUE)
#priori5$modelo="priori5"

priori1$n<-as.integer(priori1$n)
priori2$n<-as.integer(priori2$n)
priori3$n<-as.integer(priori3$n)
priori4$n<-as.integer(priori4$n)
priori5$n<-as.integer(priori5$n)


graf_linha_final=function(priori,para="10"){
  don1 <- priori %>% 
    filter(tr %in% c(para))
  g1=ggplot(don1,aes(x=n, y=vies.m,group=prioris,color=prioris,linetype=prioris,shape=prioris))+
    geom_line(aes(color=prioris,linetype=prioris),size=1.2) + 
    theme_bw()+
    #theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 15),axis.text.x = element_text(size=12))+
    theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 12),axis.text.x = element_blank())+
    #geom_point(aes(x=n,shape=prioris),size=1.6) +
    scale_x_continuous(breaks = c(30, 60, 120)) + #breaks = c(30, 60, 120),labels = c("30", "60", "120"))+  
    scale_y_continuous(labels=scales::number_format(accuracy = 0.01,decimal.mark = ','))+
    #scale_linetype(labels=c("10 anos","50 anos","100 anos","200 anos"))+
    #scale_shape_manual(values=c(0,1,2,3),labels=c("10 anos","50 anos","100 anos","200 anos")) +
    #scale_y_log10(labels = scales::number_format(accuracy = 0.01,decimal.mark = ','))+
    labs(x = NULL,   #"Tamanho da amostra (n)",
         y = NULL)+  ## "VAMRP")+   #,linetype="Modelos",shape="Modelos",color="Modelos",size=2) +
    scale_color_manual(name="Modelos",labels = c("ga1"=expression("M"[Gama1]), 
                                                 "ga2"=expression("M"[Gama2]), 
                                                 "ni"=expression("M"[NTNI]), 
                                                 "i1"=expression("M"[NTV1]), 
                                                 "i4"=expression("M"[NTV4])), values = c("ga1"="blue", 
                                                                                         "ga2"="green", 
                                                                                         "ni"="red", 
                                                                                         "i1"="brown", 
                                                                                         "i4"="orange")) +
    scale_shape_discrete(name="Modelos",labels = c("ga1"=expression("M"[Gama1]), 
                                                   "ga2"=expression("M"[Gama2]), 
                                                   "ni"=expression("M"[NTNI]), 
                                                   "i1"=expression("M"[NTV1]), 
                                                   "i4"=expression("M"[NTV4]))) +
    scale_linetype_discrete(name="Modelos",labels = c("ga1"=expression("M"[Gama1]), 
                                                      "ga2"=expression("M"[Gama2]), 
                                                      "ni"=expression("M"[NTNI]), 
                                                      "i1"=expression("M"[NTV1]), 
                                                      "i4"=expression("M"[NTV4]))) +
    guides(color = guide_legend(order=1),
           linetype = guide_legend(order=1),shape=guide_legend(order = 1)) # + wrap_elements(grid::textGrob('a'))
  
}

graf_linha_final5=function(priori,para="mu"){
  don1 <- priori %>% 
    filter(tr %in% c(para))
  g1=ggplot(don1,aes(x=n, y=vies.m,group=prioris,color=prioris,linetype=prioris,shape=prioris))+
    geom_line(aes(color=prioris,linetype=prioris),size=1.2) + 
    theme_bw()+
    theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 12),axis.text.x = element_text(size=12))+
    #theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 15),axis.text.x = element_blank())+
    #geom_point(aes(x=n,shape=prioris),size=1.6) +
    scale_x_continuous(breaks = c(30, 60, 120)) + #breaks = c(30, 60, 120),labels = c("30", "60", "120"))+  
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01,decimal.mark = ','))+
    #scale_linetype(labels=c("10 anos","50 anos","100 anos","200 anos"))+
    
    #scale_shape_manual(values=c(0,1,2,3),labels=c("10 anos","50 anos","100 anos","200 anos")) +
    #scale_y_log10(breaks = scales::log_breaks(n = 5),labels = scales::number_format(accuracy = 0.01,decimal.mark = ','))+
    #scale_y_log10(labels = scales::number_format(accuracy = 0.01,decimal.mark = ','))+
    labs(x = NULL,   #"Tamanho da amostra (n)",
         y = NULL)+  ## "VAMRP")+   #,linetype="Modelos",shape="Modelos",color="Modelos",size=2) +
    scale_color_manual(name="Modelos",labels = c("ga1"=expression("M"[Gama1]), 
                                                 "ga2"=expression("M"[Gama2]), 
                                                 "ni"=expression("M"[NTNI]), 
                                                 "i1"=expression("M"[NTV1]), 
                                                 "i4"=expression("M"[NTV4])), values = c("ga1"="blue", 
                                                                                         "ga2"="green", 
                                                                                         "ni"="red", 
                                                                                         "i1"="brown", 
                                                                                         "i4"="orange")) +
    scale_shape_discrete(name="Modelos",labels = c("ga1"=expression("M"[Gama1]), 
                                                   "ga2"=expression("M"[Gama2]), 
                                                   "ni"=expression("M"[NTNI]), 
                                                   "i1"=expression("M"[NTV1]), 
                                                   "i4"=expression("M"[NTV4]))) +
    scale_linetype_discrete(name="Modelos",labels = c("ga1"=expression("M"[Gama1]), 
                                                      "ga2"=expression("M"[Gama2]), 
                                                      "ni"=expression("M"[NTNI]), 
                                                      "i1"=expression("M"[NTV1]), 
                                                      "i4"=expression("M"[NTV4]))) +
    guides(color = guide_legend(order=1),
           linetype = guide_legend(order=1),shape=guide_legend(order = 1)) # + wrap_elements(grid::textGrob('a'))
  
}

vm=wrap_elements(text_grob("VAMRP (%)",rot = 90))

a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 15));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 15));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 15))

tam=wrap_elements(text_grob("Tamanho amostral"))

o=wrap_elements(text_grob('10 anos',face= "bold",size=12))
p=wrap_elements(text_grob('50 anos',face= "bold",size=12))
q=wrap_elements(text_grob('100 anos',face= "bold",size=12))
r=wrap_elements(text_grob('200 anos',face= "bold",size=12))

grafs2=list(a,b,plot_spacer(),
           
           gra1_10=graf_linha_final(priori1,"10"),gra2_10=graf_linha_final(priori2,"10"),o,
           gra1_50=graf_linha_final(priori1,"50"),gra2_50=graf_linha_final(priori2,"50"),p,
           gra1_100=graf_linha_final(priori1,"100"),gra2_100=graf_linha_final(priori2,"100"),q,
           gra1_200=graf_linha_final5(priori1,"200"),gra2_200=graf_linha_final5(priori2,"200"),r,
           tam,tam,plot_spacer()
           # gra2_10=graf_linha_final5(priori2,"10"),o,
           # gra2_50=graf_linha_final5(priori2,"50"),p,
           # gra2_100=graf_linha_final5(priori2,"100"),q,
           # gra2_200=graf_linha_final5(priori2,"200"),r
           # 
           # gra3_10=graf_linha_final(priori3,"10"),
           # gra3_50=graf_linha_final(priori3,"50"),
           # gra3_100=graf_linha_final(priori3,"100"),
           # gra3_200=graf_linha_final(priori3,"200"),c,
           # 
           # gra4_10=graf_linha_final(priori4,"10"),
           # gra4_50=graf_linha_final(priori4,"50"),
           # gra4_100=graf_linha_final(priori4,"100"),
           # gra4_200=graf_linha_final(priori4,"200"),d,           
           # 
           # gra5_10=graf_linha_final5(priori5,"10"),
           # gra5_50=graf_linha_final5(priori5,"50"),
           # gra5_100=graf_linha_final5(priori5,"100"),
           # gra2_200=graf_linha_final5(priori5,"200"),e
)

wrl1=wrap_plots(grafs2,guides = 'collect', byrow=TRUE,nrow=6,ncol=3,heights =c(2,7,7,7,7,2) ,widths = c(10,10,2))&theme(legend.text = element_text(size = 15),legend.title =element_text(size=15),legend.text.align = 0,legend.position='bottom')  


graf_v_rl_ab=(vm|wrl1) +plot_layout(widths = c(0.2,10))


#w1=(w1/tam)+plot_layout(heights = c(10,0.3))

# graf_v_rl1=(vm|w1) +plot_layout(widths = c(0.2,10))&theme(legend.text = element_text(size = 15),legend.title =element_text(size=15),legend.text.align = 0,legend.position='top')

ggsave(filename = "graf_v_rl_ab_v3.pdf",graf_v_rl_ab,
       width = 10, height = 10, dpi = 500, units = "in", device='pdf')



grafs2cde=list(
           
           # gra1_10=graf_linha_final(priori1,"10"),
           # gra1_50=graf_linha_final(priori1,"50"),
           # gra1_100=graf_linha_final(priori1,"100"),
           # gra1_200=graf_linha_final(priori1,"200"),a,
           # 
           # gra2_10=graf_linha_final(priori2,"10"),
           # gra2_50=graf_linha_final(priori2,"50"),
           # gra2_100=graf_linha_final(priori2,"100"),
           # gra2_200=graf_linha_final(priori2,"200"),b
           
           a,b,c,plot_spacer(),
           
           gra3_10=graf_linha_final(priori3,"10"),gra4_10=graf_linha_final(priori4,"10"),gra5_10=graf_linha_final(priori5,"10"),o,
           gra3_50=graf_linha_final(priori3,"50"),gra4_50=graf_linha_final(priori4,"50"),gra5_50=graf_linha_final(priori5,"50"),p,
           gra3_100=graf_linha_final(priori3,"100"),gra4_100=graf_linha_final(priori4,"100"),gra5_50=graf_linha_final(priori5,"50"),q,
           gra3_200=graf_linha_final5(priori3,"200"),gra4_200=graf_linha_final5(priori4,"200"),gra5_50=graf_linha_final5(priori5,"50"),r,
           tam,tam,tam,plot_spacer()
           
           # 
           # gra3_10=graf_linha_final(priori3,"10"),
           # gra3_50=graf_linha_final(priori3,"50"),
           # gra3_100=graf_linha_final(priori3,"100"),
           # gra3_200=graf_linha_final(priori3,"200"),c,
           # 
           # gra4_10=graf_linha_final(priori4,"10"),
           # gra4_50=graf_linha_final(priori4,"50"),
           # gra4_100=graf_linha_final(priori4,"100"),
           # gra4_200=graf_linha_final(priori4,"200"),d,
           # 
           # gra5_10=graf_linha_final5(priori5,"10"),
           # gra5_50=graf_linha_final5(priori5,"50"),
           # gra5_100=graf_linha_final5(priori5,"100"),
           # gra2_200=graf_linha_final5(priori5,"200"),e
)


wrl2=wrap_plots(grafs2cde,guides = 'collect', byrow=TRUE,nrow=6,ncol=4,heights =c(2,7,7,7,7,2),widths = c(10,10,10,2))&theme(legend.text = element_text(size = 15),legend.title =element_text(size=15),legend.text.align = 0,legend.position='bottom')#  &theme(legend.text = element_text(size = 15),legend.title =element_text(size=15),legend.text.align = 0,legend.position='right')  


graf_v_rl_cde=(vm|wrl2) +plot_layout(widths = c(0.2,10))


#w1=(w1/tam)+plot_layout(heights = c(10,0.3))

# graf_v_rl1=(vm|w1) +plot_layout(widths = c(0.2,10))&theme(legend.text = element_text(size = 15),legend.title =element_text(size=15),legend.text.align = 0,legend.position='top')

ggsave(filename = "graf_v_rl_cde_v2.pdf",graf_v_rl_cde,
       width = 14, height = 6, dpi = 500, units = "in", device='pdf')

#w2=(w2/tam)+plot_layout(heights = c(10,0.3))

# graf_v_rl2=(vm|w2) +plot_layout(widths = c(0.2,10))&theme(legend.text = element_text(size = 15),legend.title =element_text(size=15),legend.text.align = 0,legend.position='top')


ggsave(filename = "graf_v_rl_cde2.pdf",w2,
       width = 14, height = 6, dpi = 500, units = "in", device='pdf')


#histograms

his=function(prior){

if(prior=="p1"){
  locat=70
  sca=30
  sha=0.4
  r1=data.frame(x=rgev(10000,loc = locat,scale = sca,shape = sha))
  hist1= ggplot(r1,aes(x=x))+geom_histogram(aes(y =..density..),color = "white", fill = "gray")+
    theme_classic(base_size = 18)+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_density(col=2)+
    labs(title ="a")+
    xlab("Máximos")+
    ylab("Densidade")+
    scale_x_continuous(breaks = seq(from = 30,to = 200,by = 20), limits = c(30,200))
  
  }

if(prior=="p2"){
  locat=70
  sca=25
  sha=0.05
  r1=data.frame(x=rgev(10000,loc = locat,scale = sca,shape = sha))
  min(r1)
  max(r1)
  hist1= ggplot(r1,aes(x=x))+geom_histogram(aes(y =..density..),color = "white", fill = "gray")+
    theme_classic(base_size = 18)+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_density(col=2)+
    labs(title ="b")+
    xlab("Máximos")+
    ylab("Densidade")+
    scale_x_continuous(breaks = seq(from = 30,to = 200,by = 20), limits = c(30,200))
  }

if(prior=="p3"){
  locat=34
  sca=2
  sha=-0.4
  r1=data.frame(x=rgev(10000,loc = locat,scale = sca,shape = sha))
  hist1= ggplot(r1,aes(x=x))+geom_histogram(aes(y =..density..),color = "white", fill = "gray")+
    theme_classic(base_size = 18)+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_density(col=2)+
    labs(title ="c")+
    xlab("Máximos")+
    ylab("Densidade")+
    scale_x_continuous(breaks = seq(from = 28,to = 39,by = 1), limits = c(28,39))
  }

if(prior=="p4"){
  locat=6.78
  sca=0.561
  sha=-0.2
  r1=data.frame(x=rgev(1000,loc = locat,scale = sca,shape = sha))
  
  hist1= ggplot(r1,aes(x=x))+geom_histogram(aes(y =..density..),color = "white", fill = "gray")+
    theme_classic(base_size = 18)+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_density(col=2)+
    labs(title ="d")+
    xlab("Máximos")+
    ylab("Densidade")+
    scale_x_continuous(breaks = seq(from = 5.2,to = 8.6,by = 0.6), limits = c(5.2,8.6))
  }

if(prior=="p5"){
  locat=1200
  sca=340
  sha=0.1
  r1=data.frame(x=rgev(1000,loc = locat,scale = sca,shape = sha))
 
  hist1= ggplot(r1,aes(x=x))+geom_histogram(aes(y =..density..),color = "white", fill = "gray")+
    theme_classic(base_size = 18)+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_density(col=2)+
    labs(title ="e")+
    xlab("Máximos")+
    ylab("Densidade")+
    scale_x_continuous(breaks = seq(from = 800,to = 2700,by = 400), limits = c(500,3000))
}
  
hist1  
  }


his1=his("p1")
his2=his("p2")
his3=his("p3")
his4=his("p4")
his5=his("p5")

hists=wrap_plots(his1,plot_spacer(),his2,his3,plot_spacer(),his4, byrow=TRUE,nrow=3,ncol=3,heights=c(1,1),widths = c(10,1,10))

plot_e=(plot_spacer()+his5+plot_spacer())+plot_layout(heights=c(1),widths = c(6,10,6))

hists/plot_e+plot_layout(heights = c(5,2))

design <- "A#B
           #C#
           D#E"
           
?area          

hist2=wrap_plots(A = his1,B = his2,C= his3,D = his4,E = his5, design = design)


hist3=(his1 | plot_spacer()| his2)/(plot_spacer()|his3| plot_spacer())/(his4| plot_spacer()|his5) 

ggsave(filename = "hist4.pdf", hist4,
       width = 14, height = 6, dpi = 500, units = "in", device='pdf')


library(ggplot2)

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) + geom_bar(aes(gear)) + facet_wrap(~cyl)

layout <- c(
  area(1, 1,1,1),
  area(1, 2, 2,2),
  area(3, 3, 3,3),
  area(1, 2,1,2),
  area(2, 2, 2,2)
)

design <- "AAAAAAAAA#BBBBBBBBB"
w1=wrap_plots(A = his1, B = his2, design = design)

design <- "A"
w2=wrap_plots(A = his3, design = design)

hist4=w1/(plot_spacer()+w2+plot_spacer())/w3

design <- "AAAAAAAAA#BBBBBBBBB"
w3=wrap_plots(A = his4, B = his5, design = design)

design <- "#BBB
           CCC#"
w2=wrap_plots(B = his2, C = plot_spacer(), design = design)


w1|w2

# Show the layout to make sure it looks as it should
plot(layout)

# Apply it to a patchwork
p1 + p2 + p3 + plot_layout(design = layout)



#######################################################################################
#############-------------Boxplot--------------#############################


bxmu1=box_para(todos_par1,"mu",yin =-10,ysu=15)

bxmu2=box_para(todos(r4,r5,r6),"mu",yin =-10,ysu=10)

bxmu3=box_para(todos(r7,r8,r9),"mu",yin =-1,ysu=0.7)

bxmu4=box_para(todos(r10,r11,r12),"mu",yin=-0.2,ysu=0.3)

bxmu5=box_para(todos(r13,r14,r15),"mu",yin =-100,ysu=150)

#vies=wrap_elements(text_grob("Vi?s",size=20,rot = 90))

vies=wrap_elements(text_grob("Viés",size=13))

# a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))
# 
# tam=wrap_elements(text_grob("Tamanho amostral"))


a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))

tam=wrap_elements(text_grob("Tamanho amostral"))

ta1=wrap_elements(text_grob('n=30',face= "bold",size=15))

ta2=wrap_elements(text_grob('n=60',face= "bold",size=15))

ta3=wrap_elements(text_grob('n=120',face= "bold",size=15))


boxs1=list(
  (ta1+ta2+ta3),plot_spacer(),
  bxmu1,a,
  bxmu2,b,
  bxmu3,c,
  bxmu4,d,
  bxmu5,e,
  vies,plot_spacer()
  
)

w1=wrap_plots(boxs1,guides = 'collect', byrow=TRUE,nrow=7,ncol=2,heights =c(2,9,9,9,9,9,2) ,widths = c(9.5,0.3))&theme(legend.text = element_text(size = 15),legend.title =element_text(size=15),legend.position='bottom')  


ggsave(filename = "box_v_mu1.pdf", w1,
       width = 10, height = 10, dpi = 500, units = "in", device='pdf')



#######################################################
##-------boxplot sigma-----------##################
##################################

box_para=function(todos_par1,para="sigma",yin=-1,ysu=1){
  don1 <- todos_par1 %>% 
    filter(paramet %in% c(para))
    bx0 <- ggplot(don1,aes(y=vies,x=prioris, color=prioris)) +  theme_bw()+
    theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 13), axis.text.x = element_text(size=13))+
    #theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 15),axis.text.x = element_blank())+
    #geom_point(aes(x=n,shape=prioris),size=1.6) +
    #scale_x_continuous(breaks = c(30, 60, 120)) + #breaks = c(30, 60, 120),labels = c("30", "60", "120"))+  
    #theme(axis.text.y =element_blank(), strip.text.y = element_blank())+
    ylab(NULL)+
    xlab(NULL)+
    geom_boxplot(position=position_dodge(1)) +
    #coord_cartesian(ylim = c(yin, ysu))+
    #scale_x_discrete(breaks = c(30, 60, 120),expand=c(0.2,0))+
    geom_hline(yintercept = 0,linetype="dashed") +
    scale_x_discrete(name="",
                     labels = c("ga1"=expression("M"[Gama1]),
                                "ga2"=expression("M"[Gama2]),
                                "ni"=expression("M"[NTNI]),
                                "i1"=expression("M"[NTV1]),
                                "i4"=expression("M"[NTV4])))+ 
    scale_color_manual(name="Modelos",
                       labels = c("ga1"=expression("M"[Gama1]),
                                  "ga2"=expression("M"[Gama2]),
                                  "ni"=expression("M"[NTNI]),
                                  "i1"=expression("M"[NTV1]),
                                  "i4"=expression("M"[NTV4])),
                       values = c("ga1"="blue", 
                                  "ga2"="green", 
                                  "ni"="red", 
                                  "i1"="brown", 
                                  "i4"="orange"))+  
    facet_wrap(.~Tamanho)+
    coord_flip(ylim = c(yin, ysu))
  #+
  # facet_wrap_custom(.~Tamanho, scales = "free", scale_overrides = list(
  #   scale_override(1, scale_y_continuous(limits = c(-0.7, 1.2))),
  #   scale_override(2, scale_y_continuous(limits = c(-0.7, 1.2))),
  #   scale_override(3, scale_y_continuous(limits = c(-0.7, 1.2)))))

  return(bx0)
}

todos=function(r1,r2,r3){
  v1=vies_simu(r1,"30")
  
  v2=vies_simu(r2,"60")
  
  v3=vies_simu(r3,"120")
  
  todos_1=v1$vies_params%>%full_join(v2$vies_params)%>%full_join(v3$vies_params)
  
  v_par=function(v){
    v$prioris<-factor(v$prioris,levels = c("ga1","ga2","ni","i1","i4"))    
    v$paramet<-factor(v$paramet,levels = c("mu","sigma","xi"))
    v$Tamanho<-factor(v$Tamanho,levels = c("30","60","120"))
    return(v)
  }
  
  todos_par1=v_par(todos_1)
  return(todos_par1)
}

bxsigma1=box_para(todos_par1,"sigma",yin =-10,ysu=15)

bxsigma2=box_para(todos(r4,r5,r6),"sigma",yin =-10,ysu=10)

bxsigma3=box_para(todos(r7,r8,r9),"sigma",yin =-1,ysu=0.7)

bxsigma4=box_para(todos(r10,r11,r12),"sigma",yin=-0.2,ysu=0.3)

bxsigma5=box_para(todos(r13,r14,r15),"sigma",yin =-100,ysu=150)

#vies=wrap_elements(text_grob("Vi?s",size=20,rot = 90))

vies=wrap_elements(text_grob("Viés",size=13))

# a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))
# 
# tam=wrap_elements(text_grob("Tamanho amostral"))


a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))

tam=wrap_elements(text_grob("Tamanho amostral"))

ta1=wrap_elements(text_grob('n=30',face= "bold",size=15))

ta2=wrap_elements(text_grob('n=60',face= "bold",size=15))

ta3=wrap_elements(text_grob('n=120',face= "bold",size=15))


boxs2=list(
           (ta1+ta2+ta3),plot_spacer(),
           bxsigma1,a,
           bxsigma2,b,
           bxsigma3,c,
           bxsigma4,d,
           bxsigma5,e,
           vies,plot_spacer()
           
)

w3=wrap_plots(boxs2,guides = 'collect', byrow=TRUE,nrow=7,ncol=2,heights =c(2,9,9,9,9,9,2) ,widths = c(9.5,0.3))&theme(legend.text = element_text(size = 15),legend.title =element_text(size=15),legend.position='none')  


ggsave(filename = "box_v_sigma1.pdf", w3,
       width = 10, height = 10, dpi = 500, units = "in", device='pdf')


#######################################################
##-------boxplot xi-----------##################
##################################



box_para=function(todos_par1,para="sigma",yin=-1,ysu=1){
  don1 <- todos_par1 %>% 
    filter(paramet %in% c(para))
  bx0 <- ggplot(don1,aes(y=vies,x=prioris, color=prioris)) +  theme_bw()+
    theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 13), axis.text.x = element_text(size=13))+
    #theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 15),axis.text.x = element_blank())+
    #geom_point(aes(x=n,shape=prioris),size=1.6) +
    #scale_x_continuous(breaks = c(30, 60, 120)) + #breaks = c(30, 60, 120),labels = c("30", "60", "120"))+  
    #theme(axis.text.y =element_blank(), strip.text.y = element_blank())+
    ylab(NULL)+
    xlab(NULL)+
    geom_boxplot(position=position_dodge(1)) +
    #coord_cartesian(ylim = c(yin, ysu))+
    #scale_x_discrete(breaks = c(30, 60, 120),expand=c(0.2,0))+
    geom_hline(yintercept = 0,linetype="dashed") +
    scale_x_discrete(name="",
                     labels = c("ga1"=expression("M"[Gama1]),
                                "ga2"=expression("M"[Gama2]),
                                "ni"=expression("M"[NTNI]),
                                "i1"=expression("M"[NTV1]),
                                "i4"=expression("M"[NTV4])))+ 
    scale_color_manual(name="Modelos",
                       labels = c("ga1"=expression("M"[Gama1]),
                                  "ga2"=expression("M"[Gama2]),
                                  "ni"=expression("M"[NTNI]),
                                  "i1"=expression("M"[NTV1]),
                                  "i4"=expression("M"[NTV4])),
                       values = c("ga1"="blue", 
                                  "ga2"="green", 
                                  "ni"="red", 
                                  "i1"="brown", 
                                  "i4"="orange"))+  
    facet_wrap(.~Tamanho)+
    coord_flip(ylim = c(yin, ysu))
  #+
  # facet_wrap_custom(.~Tamanho, scales = "free", scale_overrides = list(
  #   scale_override(1, scale_y_continuous(limits = c(-0.7, 1.2))),
  #   scale_override(2, scale_y_continuous(limits = c(-0.7, 1.2))),
  #   scale_override(3, scale_y_continuous(limits = c(-0.7, 1.2)))))
  
  return(bx0)
}

todos=function(r1,r2,r3){
  v1=vies_simu(r1,"30")
  
  v2=vies_simu(r2,"60")
  
  v3=vies_simu(r3,"120")
  
  todos_1=v1$vies_params%>%full_join(v2$vies_params)%>%full_join(v3$vies_params)
  
  v_par=function(v){
    v$prioris<-factor(v$prioris,levels = c("ga1","ga2","ni","i1","i4"))    
    v$paramet<-factor(v$paramet,levels = c("mu","sigma","xi"))
    v$Tamanho<-factor(v$Tamanho,levels = c("30","60","120"))
    return(v)
  }
  
  todos_par1=v_par(todos_1)
  return(todos_par1)
}

bxxi1=box_para(todos_par1,"xi",yin =-0.3,ysu=0.3)

bxxi2=box_para(todos(r4,r5,r6),"xi",yin =-0.3,ysu=0.3)

bxxi3=box_para(todos(r7,r8,r9),"xi",yin =-0.3,ysu=0.3)

bxxi4=box_para(todos(r10,r11,r12),"xi",yin=-0.3,ysu=0.3)

bxxi5=box_para(todos(r13,r14,r15),"xi",yin =-0.3,ysu=.3)

#vies=wrap_elements(text_grob("Vi?s",size=20,rot = 90))

vies=wrap_elements(text_grob("Viés",size=13))

# a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))
# 
# tam=wrap_elements(text_grob("Tamanho amostral"))


a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))

tam=wrap_elements(text_grob("Tamanho amostral"))

ta1=wrap_elements(text_grob('n=30',face= "bold",size=15))

ta2=wrap_elements(text_grob('n=60',face= "bold",size=15))

ta3=wrap_elements(text_grob('n=120',face= "bold",size=15))


boxs5=list(
  (ta1+ta2+ta3),plot_spacer(),
  bxxi1,a,
  bxxi2,b,
  bxxi3,c,
  bxxi4,d,
  bxxi5,e,
  vies,plot_spacer()
)

w5=wrap_plots(boxs5,guides = 'collect', byrow=TRUE,nrow=7,ncol=2,heights =c(2,9,9,9,9,9,2) ,widths = c(9.5,0.3))&theme(legend.text = element_text(size = 15),legend.title =element_text(size=15),legend.position='none')  


ggsave(filename = "box_v_xi1.pdf", w5,
       width = 10, height = 10, dpi = 500, units = "in", device='pdf')









#######################################################
##-------boxplot tr 10 anos-----------##################
##################################



todos=function(r1,r2,r3){
  v1=vies_simu(r1,"30")
  
  v2=vies_simu(r2,"60")
  
  v3=vies_simu(r3,"120")
  
  todos_1=v1$vies_returlev%>%full_join(v2$vies_returlev)%>%full_join(v3$vies_returlev)
  
  v_par=function(v){
    v$prioris<-factor(v$prioris,levels = c("ga1","ga2","ni","i1","i4"))    
    v$paramet<-factor(v$paramet,levels = c("NR10","NR50","NR100","NR200"))
    v$Tamanho<-factor(v$Tamanho,levels = c("30","60","120"))
    return(v)
  }
  
  todos_par1=v_par(todos_1)
  return(todos_par1)
}

box_para=function(todos_par1,para="NR10",yin=-1000,ysu=1000){
  don1 <- todos_par1 %>% 
    filter(paramet %in% c(para))
  
  bx0 <- ggplot(don1,aes(y=vies,x=prioris, color=prioris)) +  theme_bw()+
    theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 13),axis.text.x = element_blank())+
    #theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 15),axis.text.x = element_blank())+
    #geom_point(aes(x=n,shape=prioris),size=1.6) +
    #scale_x_continuous(breaks = c(30, 60, 120)) + #breaks = c(30, 60, 120),labels = c("30", "60", "120"))+  
    #theme(axis.text.y =element_blank(), strip.text.y = element_blank())+
    ylab(NULL)+
    xlab(NULL)+
    geom_boxplot(position=position_dodge(1)) +
    #coord_cartesian(ylim = c(yin, ysu))+
    #scale_x_discrete(breaks = c(30, 60, 120),expand=c(0.2,0))+
    geom_hline(yintercept = 0,linetype="dashed") +

       scale_x_discrete(name="",
                        labels = c("ga1"=expression("M"[Gama1]),
                                   "ga2"=expression("M"[Gama2]),
                                   "ni"=expression("M"[NTNI]),
                                   "i1"=expression("M"[NTV1]),
                                   "i4"=expression("M"[NTV4])))+ 
    scale_color_manual(name="Modelos",
                   labels = c("ga1"=expression("M"[Gama1]),
                              "ga2"=expression("M"[Gama2]),
                              "ni"=expression("M"[NTNI]),
                              "i1"=expression("M"[NTV1]),
                              "i4"=expression("M"[NTV4])),
    values = c("ga1"="blue", 
               "ga2"="green", 
               "ni"="red", 
               "i1"="brown", 
               "i4"="orange"))+  
    facet_wrap(.~Tamanho)+
    coord_flip(ylim = c(yin, ysu))
  #+
    # facet_wrap_custom(.~Tamanho, scales = "free", scale_overrides = list(
    #   scale_override(1, scale_y_continuous(limits = c(-0.7, 1.2))),
    #   scale_override(2, scale_y_continuous(limits = c(-0.7, 1.2))),
    #   scale_override(3, scale_y_continuous(limits = c(-0.7, 1.2)))))
  return(bx0)
}


box_lab_x=function(todos_par1,para="NR10",yin=-1000,ysu=1000){
  don1 <- todos_par1 %>% 
    filter(paramet %in% c(para))
  
  bx0 <- ggplot(don1,aes(y=vies,x=prioris, color=prioris)) +  theme_bw()+
    theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 13),axis.text.x = element_text(size=13))+
    #theme(strip.text.x = element_blank(),axis.text.y =element_text(size = 15),axis.text.x = element_blank())+
    #geom_point(aes(x=n,shape=prioris),size=1.6) +
    #scale_x_continuous(breaks = c(30, 60, 120)) + #breaks = c(30, 60, 120),labels = c("30", "60", "120"))+  
    #theme(axis.text.y =element_blank(), strip.text.y = element_blank())+
    ylab(NULL)+
    xlab(NULL)+
    geom_boxplot(position=position_dodge(1)) +
    #coord_cartesian(ylim = c(yin, ysu))+
    #scale_x_discrete(breaks = c(30, 60, 120),expand=c(0.2,0))+
    geom_hline(yintercept = 0,linetype="dashed") +
    
    scale_x_discrete(name="",
                     labels = c("ga1"=expression("M"[Gama1]),
                                "ga2"=expression("M"[Gama2]),
                                "ni"=expression("M"[NTNI]),
                                "i1"=expression("M"[NTV1]),
                                "i4"=expression("M"[NTV4])))+ 
    scale_color_manual(name="Modelos",
                       labels = c("ga1"=expression("M"[Gama1]),
                                  "ga2"=expression("M"[Gama2]),
                                  "ni"=expression("M"[NTNI]),
                                  "i1"=expression("M"[NTV1]),
                                  "i4"=expression("M"[NTV4])),
                       values = c("ga1"="blue", 
                                  "ga2"="green", 
                                  "ni"="red", 
                                  "i1"="brown", 
                                  "i4"="orange"))+  
    facet_wrap(.~Tamanho)+
    coord_flip(ylim = c(yin, ysu))
  #+
  # facet_wrap_custom(.~Tamanho, scales = "free", scale_overrides = list(
  #   scale_override(1, scale_y_continuous(limits = c(-0.7, 1.2))),
  #   scale_override(2, scale_y_continuous(limits = c(-0.7, 1.2))),
  #   scale_override(3, scale_y_continuous(limits = c(-0.7, 1.2)))))
  return(bx0)
}



bxr1=box_lab_x(todos(r1,r2,r3),"NR10",yin = -75,ysu =80)

bxr2=box_lab_x(todos(r1,r2,r3),"NR50",yin = -130,ysu =500)

bxr3=box_lab_x(todos(r1,r2,r3),"NR100",yin = -130,ysu =700)

bxr4=box_lab_x(todos(r1,r2,r3),"NR200",yin = -130,ysu =1500)

# 
# box=(bxr1/bxr2/bxr3/bxr4/bxr5)
# 
# w5=wrap_plots(box,guides = 'collect', byrow=TRUE)&theme(legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='bottom')  
# 
# box_v_rl1=(vies|w5) +plot_layout(widths = c(0.5,10))
# 
# 
# ggsave(filename = "box_v_rl1.pdf", box_v_rl1,
#        width = 10, height = 10, dpi = 500, units = "in", device='pdf')



#vies=wrap_elements(text_grob("Vi?s",rot = 90))
vies=wrap_elements(text_grob("Viés",size=13))

# a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))
# 
# tam=wrap_elements(text_grob("Tamanho amostral"))

ta1=wrap_elements(text_grob('n=30',face= "bold",size=15))

ta2=wrap_elements(text_grob('n=60',face= "bold",size=15))

ta3=wrap_elements(text_grob('n=120',face= "bold",size=15))

o=wrap_elements(text_grob('10 anos',face= "bold",size=10))
p=wrap_elements(text_grob('50 anos',face= "bold",size=10))
q=wrap_elements(text_grob('100 anos',face= "bold",size=10))
r=wrap_elements(text_grob('200 anos',face= "bold",size=10))

#tre1<-wrap_elements(text_grob('TR-10 anos',face= "bold",size=15))

boxs3=list(
           (ta1+ta2+ta3),plot_spacer(),
           bxr1,o,
           bxr2,p,
           bxr3,q,
           bxr4,r,
           vies,plot_spacer()
           
)

w5=wrap_plots(boxs3,guides = 'collect', byrow=TRUE,nrow=6,ncol=2,heights =c(2,9,9,9,9,2.2) ,widths = c(9,0.7))&theme(legend.text.align = 0,legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='none')  

#box_v_rl1=(w5/vies) +plot_layout( byrow=TRUE,heights = c(10,0.5))

ggsave(filename = "box_v_cen_a.pdf", w5,width=10, height=10, dpi=500, units="in", device='pdf')


#################################################
#####-----cenario b----------############
###########################

bxr1=box_lab_x(todos(r4,r5,r6),"NR10",yin = -30,ysu =30)

bxr2=box_lab_x(todos(r4,r5,r6),"NR50",yin = -30,ysu =100)

bxr3=box_lab_x(todos(r4,r5,r6),"NR100",yin = -40,ysu =100)

bxr4=box_lab_x(todos(r4,r5,r6),"NR200",yin = -40,ysu =150)

# 
# box=(bxr1/bxr2/bxr3/bxr4/bxr5)
# 
# w5=wrap_plots(box,guides = 'collect', byrow=TRUE)&theme(legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='bottom')  
# 
# box_v_rl1=(vies|w5) +plot_layout(widths = c(0.5,10))
# 
# 
# ggsave(filename = "box_v_rl1.pdf", box_v_rl1,
#        width = 10, height = 10, dpi = 500, units = "in", device='pdf')



#vies=wrap_elements(text_grob("Vi?s",rot = 90))
vies=wrap_elements(text_grob("Viés",size=13))

# a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))
# 
# tam=wrap_elements(text_grob("Tamanho amostral"))

ta1=wrap_elements(text_grob('n=30',face= "bold",size=15))

ta2=wrap_elements(text_grob('n=60',face= "bold",size=15))

ta3=wrap_elements(text_grob('n=120',face= "bold",size=15))

o=wrap_elements(text_grob('10 anos',face= "bold",size=10))
p=wrap_elements(text_grob('50 anos',face= "bold",size=10))
q=wrap_elements(text_grob('100 anos',face= "bold",size=10))
r=wrap_elements(text_grob('200 anos',face= "bold",size=10))

#tre1<-wrap_elements(text_grob('TR-10 anos',face= "bold",size=15))

boxs3=list(
  (ta1+ta2+ta3),plot_spacer(),
  bxr1,o,
  bxr2,p,
  bxr3,q,
  bxr4,r,
  vies,plot_spacer()
  
)

w7=wrap_plots(boxs3,guides = 'collect', byrow=TRUE,nrow=6,ncol=2,heights =c(2,9,9,9,9,2.2) ,widths = c(9,0.7))&theme(legend.text.align = 0,legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='none')  

#box_v_rl1=(w5/vies) +plot_layout( byrow=TRUE,heights = c(10,0.5))

ggsave(filename = "box_v_cen_b.pdf", w7,width=10, height=10, dpi=500, units="in", device='pdf')



#################################################
#####-----cenario c----------############
###########################

bxr1=box_lab_x(todos(r7,r8,r9),"NR10",yin = -0.8,ysu =1)

bxr2=box_lab_x(todos(r7,r8,r9),"NR50",yin = -0.8,ysu =1)

bxr3=box_lab_x(todos(r7,r8,r9),"NR100",yin = -0.8,ysu =1)

bxr4=box_lab_x(todos(r7,r8,r9),"NR200",yin = -0.8,ysu =1.5)

# 
# box=(bxr1/bxr2/bxr3/bxr4/bxr5)
# 
# w5=wrap_plots(box,guides = 'collect', byrow=TRUE)&theme(legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='bottom')  
# 
# box_v_rl1=(vies|w5) +plot_layout(widths = c(0.5,10))
# 
# 
# ggsave(filename = "box_v_rl1.pdf", box_v_rl1,
#        width = 10, height = 10, dpi = 500, units = "in", device='pdf')



#vies=wrap_elements(text_grob("Vi?s",rot = 90))
vies=wrap_elements(text_grob("Viés",size=13))

# a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))
# 
# tam=wrap_elements(text_grob("Tamanho amostral"))

ta1=wrap_elements(text_grob('n=30',face= "bold",size=15))

ta2=wrap_elements(text_grob('n=60',face= "bold",size=15))

ta3=wrap_elements(text_grob('n=120',face= "bold",size=15))

o=wrap_elements(text_grob('10 anos',face= "bold",size=10))
p=wrap_elements(text_grob('50 anos',face= "bold",size=10))
q=wrap_elements(text_grob('100 anos',face= "bold",size=10))
r=wrap_elements(text_grob('200 anos',face= "bold",size=10))

#tre1<-wrap_elements(text_grob('TR-10 anos',face= "bold",size=15))

boxs3=list(
  (ta1+ta2+ta3),plot_spacer(),
  bxr1,o,
  bxr2,p,
  bxr3,q,
  bxr4,r,
  vies,plot_spacer()
  
)

w8=wrap_plots(boxs3,guides = 'collect', byrow=TRUE,nrow=6,ncol=2,heights =c(2,9,9,9,9,2.2) ,widths = c(9,0.7))&theme(legend.text.align = 0,legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='none')  

#box_v_rl1=(w5/vies) +plot_layout( byrow=TRUE,heights = c(10,0.5))

ggsave(filename = "box_v_cen_c.pdf", w8,width=10, height=10, dpi=500, units="in", device='pdf')



#################################################
#####-----cenario d----------############
###########################

bxr1=box_lab_x(todos(r10,r11,r12),"NR10",yin = -0.4,ysu =0.5)

bxr2=box_lab_x(todos(r10,r11,r12),"NR50",yin = -0.4,ysu =0.5)

bxr3=box_lab_x(todos(r10,r11,r12),"NR100",yin = -0.4,ysu =0.7)

bxr4=box_lab_x(todos(r10,r11,r12),"NR200",yin = -0.4,ysu =0.7)

# 
# box=(bxr1/bxr2/bxr3/bxr4/bxr5)
# 
# w5=wrap_plots(box,guides = 'collect', byrow=TRUE)&theme(legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='bottom')  
# 
# box_v_rl1=(vies|w5) +plot_layout(widths = c(0.5,10))
# 
# 
# ggsave(filename = "box_v_rl1.pdf", box_v_rl1,
#        width = 10, height = 10, dpi = 500, units = "in", device='pdf')



#vies=wrap_elements(text_grob("Vi?s",rot = 90))
vies=wrap_elements(text_grob("Viés",size=13))

# a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))
# 
# tam=wrap_elements(text_grob("Tamanho amostral"))

ta1=wrap_elements(text_grob('n=30',face= "bold",size=15))

ta2=wrap_elements(text_grob('n=60',face= "bold",size=15))

ta3=wrap_elements(text_grob('n=120',face= "bold",size=15))

o=wrap_elements(text_grob('10 anos',face= "bold",size=10))
p=wrap_elements(text_grob('50 anos',face= "bold",size=10))
q=wrap_elements(text_grob('100 anos',face= "bold",size=10))
r=wrap_elements(text_grob('200 anos',face= "bold",size=10))

#tre1<-wrap_elements(text_grob('TR-10 anos',face= "bold",size=15))

boxs3=list(
  (ta1+ta2+ta3),plot_spacer(),
  bxr1,o,
  bxr2,p,
  bxr3,q,
  bxr4,r,
  vies,plot_spacer()
  
)

w10=wrap_plots(boxs3,guides = 'collect', byrow=TRUE,nrow=6,ncol=2,heights =c(2,9,9,9,9,2.2) ,widths = c(9,0.7))&theme(legend.text.align = 0,legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='none')  

#box_v_rl1=(w5/vies) +plot_layout( byrow=TRUE,heights = c(10,0.5))

ggsave(filename = "box_v_cen_d.pdf", w10,width=10, height=10, dpi=500, units="in", device='pdf')




#################################################
#####-----cenario e----------############
###########################

bxr1=box_lab_x(todos(r13,r14,r15),"NR10",yin = -300,ysu =500)

bxr2=box_lab_x(todos(r13,r14,r15),"NR50",yin = -500,ysu =600)

bxr3=box_lab_x(todos(r13,r14,r15),"NR100",yin = -700,ysu= 1550)

bxr4=box_lab_x(todos(r13,r14,r15),"NR200",yin = -900,ysu =2500)

# 
# box=(bxr1/bxr2/bxr3/bxr4/bxr5)
# 
# w5=wrap_plots(box,guides = 'collect', byrow=TRUE)&theme(legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='bottom')  
# 
# box_v_rl1=(vies|w5) +plot_layout(widths = c(0.5,10))
# 
# 
# ggsave(filename = "box_v_rl1.pdf", box_v_rl1,
#        width = 10, height = 10, dpi = 500, units = "in", device='pdf')



#vies=wrap_elements(text_grob("Vi?s",rot = 90))
vies=wrap_elements(text_grob("Viés",size=13))

# a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))
# 
# tam=wrap_elements(text_grob("Tamanho amostral"))

ta1=wrap_elements(text_grob('n=30',face= "bold",size=15))

ta2=wrap_elements(text_grob('n=60',face= "bold",size=15))

ta3=wrap_elements(text_grob('n=120',face= "bold",size=15))

o=wrap_elements(text_grob('10 anos',face= "bold",size=10))
p=wrap_elements(text_grob('50 anos',face= "bold",size=10))
q=wrap_elements(text_grob('100 anos',face= "bold",size=10))
r=wrap_elements(text_grob('200 anos',face= "bold",size=10))

#tre1<-wrap_elements(text_grob('TR-10 anos',face= "bold",size=15))

boxs3=list(
  (ta1+ta2+ta3),plot_spacer(),
  bxr1,o,
  bxr2,p,
  bxr3,q,
  bxr4,r,
  vies,plot_spacer()
  
)

w11=wrap_plots(boxs3,guides = 'collect', byrow=TRUE,nrow=6,ncol=2,heights =c(2,9,9,9,9,2.2) ,widths = c(9,0.7))&theme(legend.text.align = 0,legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='none')  

#box_v_rl1=(w5/vies) +plot_layout( byrow=TRUE,heights = c(10,0.5))

ggsave(filename = "box_v_cen_e.pdf", w11,width=10, height=10, dpi=500, units="in", device='pdf')



#################################################
#####-----TR 100 anos ----------############
###########################


box_para=function(todos_par1,para="NR10"){
  don1 <- todos_par1 %>% 
    filter(paramet %in% c(para))
  
  bx0 <- ggplot(don1,aes(y=vies, color=prioris)) + 
    theme(axis.text.x =element_blank(), strip.text.x = element_blank())+
    geom_hline(yintercept = 0,linetype="dashed") +
    ylab(NULL)+
    geom_boxplot() +
    scale_color_manual(name="Modelos", 
                       labels = c("ga1"=expression("M"[Gama1]), 
                                  "ga2"=expression("M"[Gama2]), 
                                  "ni"=expression("M"[NTNI]), 
                                  "i1"=expression("M"[NTV1]), 
                                  "i4"=expression("M"[NTV4])), 
                       values = c("ga1"="blue", 
                                  "ga2"="green", 
                                  "ni"="red", 
                                  "i1"="brown", 
                                  "i4"="orange"))  +
    facet_wrap_custom(.~Tamanho, scales = "free", scale_overrides = list(
      scale_override(1, scale_y_continuous(limits = c(-500, 2000))),
      scale_override(2, scale_y_continuous(limits = c(-500, 2000))),
      scale_override(3, scale_y_continuous(limits = c(-500, 2000)))))
  return(bx0)
}


bxrii1=box_para(todos(r1,r2,r3),"NR100")

bxrii2=box_para(todos(r4,r5,r6),"NR100")

bxrii3=box_para(todos(r7,r8,r9),"NR100")

bxrii4=box_para(todos(r10,r11,r12),"NR100")

bxrii5=box_para(todos(r13,r14,r15),"NR100")

vies=wrap_elements(text_grob("Vi?s",rot = 90))

a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))

tam=wrap_elements(text_grob("Tamanho amostral"))

ta1=wrap_elements(text_grob('n=30',face= "bold",size=15))

ta2=wrap_elements(text_grob('n=60',face= "bold",size=15))

ta3=wrap_elements(text_grob('n=120',face= "bold",size=15))

o=wrap_elements(text_grob('10 anos',face= "bold",size=15))
p=wrap_elements(text_grob('50 anos',face= "bold",size=15))
q=wrap_elements(text_grob('100 anos',face= "bold",size=15))
r=wrap_elements(text_grob('200 anos',face= "bold",size=15))

tre4<-wrap_elements(text_grob('TR-100 anos',face= "bold",size=20))

boxs5=list(tre4, plot_spacer(),
           (ta1+ta2+ta3),plot_spacer(),
           bxrii1,a,
           bxrii2,b,
           bxrii3,c,
           bxrii4,d,
           bxrii5,e
           
)

w7=wrap_plots(boxs5,guides = 'collect', byrow=TRUE,nrow=7,ncol=2,heights =c(2,2,8,8,8,8,8) ,widths = c(9.5,0.3))&theme(legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='bottom')  

box_v_rl3=(vies|w7) +plot_layout(widths = c(0.5,10))

ggsave(filename = "box_v_rl3.pdf", box_v_rl3,
       width = 10, height = 10, dpi = 500, units = "in", device='pdf')




#################################################
#####-----TR 200 anos ----------############
###########################


box_para=function(todos_par1,para="NR10"){
  don1 <- todos_par1 %>% 
    filter(paramet %in% c(para))
  
  bx0 <- ggplot(don1,aes(y=vies, color=prioris)) + 
    theme(axis.text.x =element_blank(), strip.text.x = element_blank())+
    geom_hline(yintercept = 0,linetype="dashed") +
    ylab(NULL)+
    geom_boxplot() +
    scale_color_manual(name="Modelos", 
                       labels = c("ga1"=expression("M"[Gama1]), 
                                  "ga2"=expression("M"[Gama2]), 
                                  "ni"=expression("M"[NTNI]), 
                                  "i1"=expression("M"[NTV1]), 
                                  "i4"=expression("M"[NTV4])), 
                       values = c("ga1"="blue", 
                                  "ga2"="green", 
                                  "ni"="red", 
                                  "i1"="brown", 
                                  "i4"="orange"))  +
    facet_wrap_custom(.~Tamanho, scales = "free", scale_overrides = list(
      scale_override(1, scale_y_continuous(limits = c(-100, 1000))),
      scale_override(2, scale_y_continuous(limits = c(-100, 1000))),
      scale_override(3, scale_y_continuous(limits = c(-100, 1000)))))
  return(bx0)
}


bxriii1=box_para(todos(r1,r2,r3),"NR200")

bxriii2=box_para(todos(r4,r5,r6),"NR200")

bxriii3=box_para(todos(r7,r8,r9),"NR200")

bxriii4=box_para(todos(r10,r11,r12),"NR200")

bxriii5=box_para(todos(r13,r14,r15),"NR200")

vies=wrap_elements(text_grob("Vi?s",rot = 90))

a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))

tam=wrap_elements(text_grob("Tamanho amostral"))

ta1=wrap_elements(text_grob('n=30',face= "bold",size=15))

ta2=wrap_elements(text_grob('n=60',face= "bold",size=15))

ta3=wrap_elements(text_grob('n=120',face= "bold",size=15))

o=wrap_elements(text_grob('10 anos',face= "bold",size=15))
p=wrap_elements(text_grob('50 anos',face= "bold",size=15))
q=wrap_elements(text_grob('100 anos',face= "bold",size=15))
r=wrap_elements(text_grob('200 anos',face= "bold",size=15))

tre5<-wrap_elements(text_grob('TR-200 anos',face= "bold",size=20))

boxs6=list(tre5, plot_spacer(),
           (ta1+ta2+ta3),plot_spacer(),
           bxrii1,a,
           bxrii2,b,
           bxrii3,c,
           bxrii4,d,
           bxrii5,e
           
)

w8=wrap_plots(boxs6,guides = 'collect', byrow=TRUE,nrow=7,ncol=2,heights =c(2,2,8,8,8,8,8) ,widths = c(9.5,0.3))&theme(legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='bottom')  

box_v_rl4=(vies|w8) +plot_layout(widths = c(0.5,10))

ggsave(filename = "box_v_rl4.pdf", box_v_rl4,
       width = 10, height = 10, dpi = 500, units = "in", device='pdf')




#######################################################
##-------boxplot diferentes tipos-----------##################
##################################


todos=function(r1,r2,r3){
  v1=vies_simu(r1,"30")
  
  v2=vies_simu(r2,"60")
  
  v3=vies_simu(r3,"120")
  
  todos_1=v1$vies_returlev%>%full_join(v2$vies_returlev)%>%full_join(v3$vies_returlev)
  
  v_par=function(v){
    v$prioris<-factor(v$prioris,levels = c("ga1","ga2","ni","i1","i4"))    
    v$paramet<-factor(v$paramet,levels = c("NR10","NR50","NR100","NR200"))
    v$Tamanho<-factor(v$Tamanho,levels = c("30","60","120"))
    return(v)
  }
  
  todos_par1=v_par(todos_1)
  return(todos_par1)
}

?quantile

f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95),type = 1)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

q <- function(x) {
  subset(x, quantile(x, 0.95) < x)
}


box_para=function(todos_par1,para="NR10"){
  don1 <- todos_par1 %>% 
    filter(paramet %in% c(para))
  
  bx0 <- ggplot(don1,aes(x=prioris,y=vies, color=prioris)) + 
    theme(axis.text.x =element_blank(), strip.text.x = element_blank())+
    geom_hline(yintercept = 0,linetype="dashed") +
    ylab(NULL)+
    #geom_boxplot() +
    stat_summary(fun.data = f, geom="boxplot", 
                 position=position_dodge(1))+
    stat_summary(aes(color=prioris),fun.y = q, geom="point", 
                 position=position_dodge(1))+
    scale_color_manual(name="Modelos", 
                       labels = c("ga1"=expression("M"[Gama1]), 
                                  "ga2"=expression("M"[Gama2]), 
                                  "ni"=expression("M"[NTNI]), 
                                  "i1"=expression("M"[NTV1]), 
                                  "i4"=expression("M"[NTV4])), 
                       values = c("ga1"="blue", 
                                  "ga2"="green", 
                                  "ni"="red", 
                                  "i1"="brown", 
                                  "i4"="orange"))  +
    facet_wrap_custom(.~Tamanho, scales = "free", scale_overrides = list(
      scale_override(1, scale_y_continuous(limits = c(-0.7, 1.2))),
      scale_override(2, scale_y_continuous(limits = c(-0.7, 1.2))),
      scale_override(3, scale_y_continuous(limits = c(-0.7, 1.2)))))
  return(bx0)
}

bxr1=box_para(todos(r1,r2,r3),"NR10")

bxr2=box_para(todos(r4,r5,r6),"NR10")

bxr3=box_para(todos(r7,r8,r9),"NR10")

bxr4=box_para(todos(r10,r11,r12),"NR10")

bxr5=box_para(todos(r13,r14,r15),"NR10")

vies=wrap_elements(text_grob("Vi?s",rot = 90))

a=wrap_elements(text_grob('a',face= "bold",size=15)); b =wrap_elements(text_grob('b',face= "bold",size=15));c = wrap_elements(text_grob('c',face= "bold",size=15)); d = wrap_elements(text_grob('d',face= "bold",size=15));e= wrap_elements(text_grob('e',face= "bold",size=15));j=wrap_elements(text_grob(expression(mu),face = "bold",size = 20));k=wrap_elements(text_grob(expression(sigma),face = "bold",size = 20));l=wrap_elements(text_grob(expression(xi),face = "bold",size = 20))

tam=wrap_elements(text_grob("Tamanho amostral"))

ta1=wrap_elements(text_grob('n=30',face= "bold",size=15))

ta2=wrap_elements(text_grob('n=60',face= "bold",size=15))

ta3=wrap_elements(text_grob('n=120',face= "bold",size=15))

o=wrap_elements(text_grob('10 anos',face= "bold",size=15))
p=wrap_elements(text_grob('50 anos',face= "bold",size=15))
q=wrap_elements(text_grob('100 anos',face= "bold",size=15))
r=wrap_elements(text_grob('200 anos',face= "bold",size=15))

tre1<-wrap_elements(text_grob('TR-10 anos',face= "bold",size=20))

boxs3=list(tre1, plot_spacer(),
           (ta1+ta2+ta3),plot_spacer(),
           bxr1,a,
           bxr2,b,
           bxr3,c,
           bxr4,d,
           bxr5,e
           
)

w5=wrap_plots(boxs3,guides = 'collect', byrow=TRUE,nrow=7,ncol=2,heights =c(2,2,8,8,8,8,8) ,widths = c(9.5,0.3))&theme(legend.text = element_text(size = 15),legend.title =element_text(size=20),legend.position='bottom')  

box_v_rl1=(vies|w5) +plot_layout(widths = c(0.5,10))

ggsave(filename = "box_v_rl1.pdf", box_v_rl1,
       width = 10, height = 10, dpi = 500, units = "in", device='pdf')




##########################
####--------Boxplot mu ajustado


vies_simu=function(r2,tamanho="30"){
  
  vies_r2=matrix(c(r2$ga1.vs.mu,r2$ga1.vs.sig,r2$ga1.vs.xi,r2$ga1.vs.t10,r2$ga1.vs.t50,r2$ga1.vs.t100,r2$ga1.vs.t200,r2$ga2.vs.mu,r2$ga2.vs.sig,r2$ga2.vs.xi,r2$ga2.vs.t10,r2$ga2.vs.t50,r2$ga2.vs.t100,r2$ga2.vs.t200,r2$ni.vs.mu,r2$ni.vs.sig,r2$ni.vs.xi,r2$ni.vs.t10,r2$ni.vs.t50,r2$ni.vs.t100,r2$ni.vs.t200,r2$i1.vs.mu,r2$i1.vs.sig,r2$i1.vs.xi,r2$i1.vs.t10,r2$i1.vs.t50,r2$i1.vs.t100,r2$i1.vs.t200,r2$i4.vs.mu,r2$i4.vs.sig,r2$i4.vs.xi,r2$i4.vs.t10,r2$i4.vs.t50,r2$i4.vs.t100,r2$i4.vs.t200),1000*7*5,1,byrow = T)  #,row.names = c("Gama1","Gama2","Naoinfo","Info1","Info4"))
  
  vies_r2_dt=data.frame(vies=vies_r2,prioris=c(rep("ga1",7000),rep("ga2",7000),rep("ni",7000),rep("i1",7000),rep("i4",7000)),paramet=c(rep(c(rep("mu",1000),rep("sigma",1000),rep("xi",1000),rep("NR10",1000),rep("NR50",1000),rep("NR100",1000),rep("NR200",1000)),5)),Tamanho=tamanho)
  
  vies_params=vies_r2_dt[c(which(vies_r2_dt$paramet=="mu"),which(vies_r2_dt$paramet=="sigma"),which(vies_r2_dt$paramet=="xi")),]
  vies_returlev=vies_r2_dt[c(which(vies_r2_dt$paramet=="NR10"),which(vies_r2_dt$paramet=="NR50"),which(vies_r2_dt$paramet=="NR100"),which(vies_r2_dt$paramet=="NR200")),]
  
  list(vies_params=vies_params,vies_returlev=vies_returlev)     
}

# v1=vies_params$vies[which(vies_params$paramet=="mu"&vies_params$prioris=="ga1")]
# 
# quantile(v1,probs = c(0.05,0.25,0.5,0.75,0.95))
# 
# all.equal(vies_r2_dt$vies[1:1000],vies_params$vies[1:1000])

v1=vies_simu(r1,"30")

v2=vies_simu(r2,"60")

v3=vies_simu(r3,"120")

todos_1=v1$vies_params%>%full_join(v2$vies_params)%>%full_join(v3$vies_params)

v_par=function(v){
  v$prioris<-factor(v$prioris,levels = c("ga1","ga2","ni","i1","i4"))    
  v$paramet<-factor(v$paramet,levels = c("mu","sigma","xi"))
  v$Tamanho<-factor(v$Tamanho,levels = c("30","60","120"))
  return(v)
}

todos_par1=v_par(todos_1)

?quantile

f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95),type = 1)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

q <- function(x) {
  subset(x, quantile(x, 0.95) < x)
}


box_para=function(todos_par1,para="mu"){
  don1 <- todos_par1 %>% 
    filter(paramet %in% c(para))
  bx0 <- ggplot(don1,aes(x=prioris,y=vies, color=prioris)) + 
    theme(axis.text.x =element_blank(), strip.text.x = element_blank())+
    geom_hline(yintercept = 0,linetype="dashed") +
    ylab(NULL)+
    #geom_boxplot() +
    stat_summary(fun.data = f, geom="boxplot", 
                 position=position_dodge(1))+
    stat_summary(aes(color=prioris),fun = q, geom="point", 
                 position=position_dodge(1))+
    scale_color_manual(name="Modelos", 
                       labels = c("ga1"=expression("M"[Gama1]), 
                                  "ga2"=expression("M"[Gama2]), 
                                  "ni"=expression("M"[NTNI]), 
                                  "i1"=expression("M"[NTV1]), 
                                  "i4"=expression("M"[NTV4])), 
                       values = c("ga1"="blue", 
                                  "ga2"="green", 
                                  "ni"="red", 
                                  "i1"="brown", 
                                  "i4"="orange"))  +
    facet_wrap_custom(.~Tamanho, scales = "free", scale_overrides = list(
      scale_override(1, scale_y_continuous(limits = c(-12, 20))),
      scale_override(2, scale_y_continuous(limits = c(-12, 20))),
      scale_override(3, scale_y_continuous(limits = c(-12, 20)))))
  # scale_override(2, scale_y_continuous(limits = c(-10, 13))),
  # scale_override(3, scale_y_continuous(limits = c(-1, 0.8))),
  # scale_override(4, scale_y_continuous(limits = c(-0.3, 0.3))),
  # scale_override(5, scale_y_continuous(limits = c(-120, 190)))))
  #facet_wrap(~Tamanho, scale="fixed")
  return(bx0)
}

todos=function(r1,r2,r3){
  v1=vies_simu(r1,"30")
  
  v2=vies_simu(r2,"60")
  
  v3=vies_simu(r3,"120")
  
  todos_1=v1$vies_params%>%full_join(v2$vies_params)%>%full_join(v3$vies_params)
  
  v_par=function(v){
    v$prioris<-factor(v$prioris,levels = c("ga1","ga2","ni","i1","i4"))    
    v$paramet<-factor(v$paramet,levels = c("mu","sigma","xi"))
    v$Tamanho<-factor(v$Tamanho,levels = c("30","60","120"))
    return(v)
  }
  
  todos_par1=v_par(todos_1)
  return(todos_par1)
}

bxmu1=box_para(todos_par1,"mu")

adjbox(don1$vies)

don1 <- todos(r4,r5,r6) %>% filter(paramet %in% c("mu")&Tamanho %in% c("30")&prioris %in% c("ga1"))

bxmu2=box_para(todos(r4,r5,r6),"mu")

bxmu3=box_para(todos(r7,r8,r9),"mu")

bxmu4=box_para(todos(r10,r11,r12),"mu")

bxmu5=box_para(todos(r13,r14,r15),"mu")




 v22=vies_params$vies[which(vies_params$paramet=="mu"&vies_params$prioris=="ga2")]
 v11=vies_params$vies[which(vies_params$paramet=="mu"&vies_params$prioris=="ga1")]
 quantile(v11,probs = c(0.05,0.25,0.5,0.75,0.95),type = 1)
summary(v11)




quantile(xt1$vies[which(xt1$prioris=="ga1")])

xt1=todos(r1,r2,r3) %>% filter(paramet %in% c("mu")&Tamanho %in% c("30")) %>% group_by(prioris)

my_boxplot.stats(xt1$vies[which(xt1$prioris=="ga1")])

x=xt1$vies[which(xt1$prioris=="ga1")]

x=df %>% group_by(fact)

x=x$val

m

my_boxplot.stats <-function(x){
  quantiles <-quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95))
  labels <-names(quantile(x))
  #replacing the upper whisker to geom_boxplot
  quantiles[5] <-boxplot.stats(x)$stats[5]
  res <-data.frame(rbind(quantiles))
  names(res) <-labels
  res$out <-boxplot.stats(x)$out
  return(res)
}


?boxplot.stats
library(dplyr)
todos(r1,r2,r3) %>% filter(paramet %in% c("mu")&Tamanho %in% c("30")) %>% group_by(prioris) %>% do(my_boxplot.stats(.$vies)) %>% 
  ggplot(aes(x=prioris, y=out, ymin = `0%`, lower = `25%`, middle = `50%`,
             upper = `75%`,  ymax = `100%`)) +
  geom_boxplot(stat = "identity") + geom_point()




library(plyr)
library(ggplot2)

set.seed(4)
df <- data.frame(fact = sample(letters[1:2], 12, replace = TRUE),
                 val  = c(1:10, 100, 101))
df


library(dplyr)
df %>% group_by(fact) %>% do(my_boxplot.stats(.$val)) %>% 
  ggplot(aes(x=fact, y=out, ymin = `0%`, lower = `25%`, middle = `50%`,
             upper = `75%`,  ymax = `100%`)) +
  geom_boxplot(stat = "identity") + geom_point()


min1=quantile(v11,type=9)[1]
q1=quantile(v11,type=9)[2]
me=quantile(v11,type=9)[3]
q3=quantile(v11,type=9)[4]
max1=quantile(v11,type=9)[5]

z<- boxplot(1:10)

length(z$stats)

#look at the outbut an assign yout data to stats
z$stats<- quantile(v11)

#use bxp to plot, via add you can combine all three
bxp(z)

ggplot()+geom_boxplot(aes(x=1, y = v11, lower = q1, upper = q3, middle = me, ymin=min1, ymax=max1))

# 
# bx1 <- ggplot(long, aes(x = n, y = value, group = interaction(n,prioris), color = prioris))+
#       #ggplot(todos_par1,aes(x=Tamanho,y=vies, color=prioris)) 
#       geom_hline(yintercept = 0,linetype="dashed") +
#       #theme(strip.text.x =element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank())+
#       theme(strip.text.x = element_text(size=12, color="black",face="bold.italic"), axis.ticks.x = element_blank())+
#       # theme_tufte()+
#       # geom_tufteboxplot(median.type = "line", 
#       #                   whisker.type = 'line',
#       #                   hoffset = 0, width = 3)+
#       
#       ylab(" ")+
#       xlab(NULL)+
#       geom_boxplot() +
#       scale_color_manual(name="Modelos", 
#                          labels = c("ga1"=expression("M"[Gama1]), 
#                                     "ga2"=expression("M"[Gama2]), 
#                                     "ni"=expression("M"[NTNI]), 
#                                     "i1"=expression("M"[NTV1]), 
#                                     "i4"=expression("M"[NTV4])), 
#                          values = c("ga1"="blue", 
#                                     "ga2"="green", 
#                                     "ni"="red", 
#                                     "i1"="brown", 
#                                     "i4"="orange")) +
#       facet_wrap_custom(.~paramet, labeller =label_parsed, scales = "free", ncol = 3, scale_overrides = list(
#         scale_override(1, scale_y_continuous(limits = c(-10, 15))),
#         scale_override(2, scale_y_continuous(limits = c(-10, 15))),
#         scale_override(3, scale_y_continuous(limits = c(-0.5, 0.5)))
#       ))
#   
  