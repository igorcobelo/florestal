dest<-function(x,A,an,am,E=0.1,p=0.05,c=1,rn=F,pt=T,prot=NULL,ampl=5,...){

  #para que o arquivo docx seja nomeado com o mesmo nome do input
  nm <-deparse(substitute(x))

  #para que seja feito o ggplot
  x<-as.data.frame(x)

  #Grafico de distribuicao diametrica por estrato
  if(pt==T){

    diam<-ggplot(x, aes(x=x[,6], colour=x[,1])) +
      geom_histogram( binwidth=ampl,fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      theme_bw(16)+
      theme(axis.text.y = element_text(size=10),legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12)) +
      scale_x_continuous(breaks = seq(0, max(x[,6]), ampl)) +
      xlab("Classe Diametrica (cm)") +
      ylab("Frequencia") +
      facet_wrap( ~ x[,1])

  }else{
    diam<-ggplot(x, aes(x=x[,6], colour=x[,1])) +
      geom_histogram( binwidth=ampl,fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      theme_bw(16)+
      theme(axis.text.y = element_text(size=10),legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12)) +
      scale_x_continuous(breaks = seq(0, max(x[,6]), ampl)) +
      xlab("Diameter Class (cm)") +
      ylab("Frequency") +
      facet_wrap( ~ x[,1])
  }

  #volume/parcela

  for(i in 1:max(x[,2])){
    vv<-c(sum(subset(x[,ncol(x)],x[,2]==i),na.rm = T))
  }

  vv<-as.data.frame(vv)

  for(i in 1:max(x[,2])){
    vv[i]<-c(sum(subset(x[,ncol(x)],x[,2]==i),na.rm = T))
  }
  vv<-as.data.frame(vv)


  #Encontrar quantas unid. primarias ha em cada secundaria
  for(i in 1:max(x[,1])){
    ss<-c(length(unique(subset(x[,2], x[,1]==i))))
  }
  ss<-as.data.frame(ss)

  for(i in 1:max(x[,1])){
    ss[i]<-c(length(unique(subset(x[,2], x[,1]==i))))
  }
  ss<-as.data.frame(ss)

  #Tabela de volume/parcela
  rep<-rep(c(1:length(ss)),ss)

  if(pt==T){
    vp<-data.table(c(rep,"Media"), c(1:max(x[,2]),""), c(vv,sum(vv)/max(x[,2])), c(vv/am, (sum(vv)/max(x[,2]))/am), c(vv*A/am, (sum(vv)/max(x[,2]))*A/am))
    colnames(vp)[1]<-"Unid. Primaria"
    colnames(vp)[2]<-"Unid. Secundaria"
    colnames(vp)[3]<-"Volume/unid. secundaria (m3)"
    colnames(vp)[4]<-"Volume/hectare (m3)"
    colnames(vp)[5]<-"Volume/area total (m3)"

  }else{
    vp<-data.table(c(rep,"Mean"), c(1:max(x[,2]),""), c(vv,sum(vv)/max(x[,2])), c(vv/am, (sum(vv)/max(x[,2]))/am), c(vv*A/am, (sum(vv)/max(x[,2]))*A/am))
    colnames(vp)[1]<-"Primary unit"
    colnames(vp)[2]<-"Secondary unit"
    colnames(vp)[3]<-"Volume/secondary unit (m3)"
    colnames(vp)[4]<-"Volume/hectare (m3)"
    colnames(vp)[5]<-"Volume/total area (m3)"

  }

  vp2<-data.table(vp)

  vp2[,3]<-as.numeric(unlist(vp2[,3]))
  vp2[,4]<-as.numeric(unlist(vp2[,4]))
  vp2[,5]<-as.numeric(unlist(vp2[,5]))

  vp2[,3]<-format(round(vp2[,3],4),nsmall=4)
  vp2[,4]<-format(round(vp2[,4],4),nsmall=4)
  vp2[,5]<-format(round(vp2[,5],4),nsmall=4)


  vopa <- flextable(vp2)
  vopa <- align(vopa, align = "center")
  vopa <- align_text_col(vopa, align = "center")
  vopa<-autofit(vopa)

  #soma de volumes por estrato

  vp<-data.frame(vp)

  ph2<-as.data.frame(vp)
  ph2[,3]<-as.numeric(ph2[,3])


  for(i in ph2[1:nrow(ph2)-1,1]){
    tt<-c(sum(subset(ph2[,3], ph2[,1]==i)))
  }
  tt<-as.data.frame(tt)

  for(i in ph2[1:nrow(ph2)-1,1]){
    tt[i]<-c(sum(subset(ph2[,3], ph2[,1]==i)))
  }
  tt<-as.data.frame(tt)

  tt[,1]<-NULL


  # Tabela de volume por unidade primaria

  if(pt==T){
    vol.estrat<-data.table(c(1:length(ss),"Media"), c(tt,sum(tt)/length(tt)), c(tt/am,(sum(tt)/length(tt))/am),c(tt*A/am,(sum(tt)/length(tt))*A/am))
    colnames(vol.estrat)[1]<-"Unid. Primaria"
    colnames(vol.estrat)[2]<-"Volume/unid. primaria (m3)"
    colnames(vol.estrat)[3]<-"Volume/ha (m3)"
    colnames(vol.estrat)[4]<-"Volume/area total (m3)"

    vol.estrat$`Volume/unid. primaria (m3)`<-as.numeric(vol.estrat$`Volume/unid. primaria (m3)`)
    vol.estrat$`Volume/unid. primaria (m3)`<-format(round(vol.estrat$`Volume/unid. primaria (m3)`,4),nsmall=4)
    vol.estrat$`Volume/ha (m3)`<-as.numeric(vol.estrat$`Volume/ha (m3)`)
    vol.estrat$`Volume/ha (m3)`<-format(round(vol.estrat$`Volume/ha (m3)`,4),nsmall=4)
    vol.estrat$`Volume/area total (m3)`<-as.numeric(vol.estrat$`Volume/area total (m3)`)
    vol.estrat$`Volume/area total (m3)`<-format(round(vol.estrat$`Volume/area total (m3)`,4),nsmall=4)


  }else{
    vol.estrat<-data.table(c(1:length(ss),"Mean"), c(tt,sum(tt)/length(tt)), c(tt/a,(sum(tt)/length(tt))/a),c(tt*A/a,(sum(tt)/length(tt))*A/a))
    colnames(vol.estrat)[1]<-"Primary unit"
    colnames(vol.estrat)[2]<-"Volume/primary unit (m3)"
    colnames(vol.estrat)[3]<-"Volume/ha (m3)"
    colnames(vol.estrat)[4]<-"Volume/total area (m3)"

    vol.estrat$`Volume/primary unit (m3)`<-as.numeric(vol.estrat$`Volume/primary unit (m3)`)
    vol.estrat$`Volume/primary unit (m3)`<-format(round(vol.estrat$`Volume/primary unit (m3)`,4),nsmall=4)
    vol.estrat$`Volume/ha (m3)`<-as.numeric(vol.estrat$`Volume/ha (m3)`)
    vol.estrat$`Volume/ha (m3)`<-format(round(vol.estrat$`Volume/ha (m3)`,4),nsmall=4)
    vol.estrat$`Volume/total area (m3)`<-as.numeric(vol.estrat$`Volume/total area (m3)`)
    vol.estrat$`Volume/total area (m3)`<-format(round(vol.estrat$`Volume/total area (m3)`,4),nsmall=4)


  }

  vol.estrat2<-as.data.frame(vol.estrat)

  vol.estrat2[,2]<-as.numeric(unlist(vol.estrat2[,2]))
  vol.estrat2[,3]<-as.numeric(unlist(vol.estrat2[,3]))
  vol.estrat2[,4]<-as.numeric(unlist(vol.estrat2[,4]))

  vol.estrat2[,2]<-format(round(vol.estrat2[,2],4),nsmall=4)
  vol.estrat2[,3]<-format(round(vol.estrat2[,3],4),nsmall=4)
  vol.estrat2[,4]<-format(round(vol.estrat2[,4],4),nsmall=4)

  vol.unid <- flextable(vol.estrat2)
  vol.unid  <- align(vol.unid , align = "center")
  vol.unid  <- align_text_col(vol.unid , align = "center")
  vol.unid <-autofit(vol.unid)


  #Dados iniciais

  y<-sum(ph2[1:nrow(ph2)-1,3])/sum(ss)
  n<-ncol(ss)
  m<-sum(ss)/ncol(ss)


  N=A/an
  M=(A/am)/N


  #Anova

  x[,1]<-as.factor(x[,1])

  modelo.anova <- lm(x[,ncol(x)] ~ x[,1], data= x)
  anova<-anova(modelo.anova)
  mq<-as.data.frame(anova$`Mean Sq`)
  s2e<-(mq[1,]-mq[2,])/m
  s2d<-mq[2,]
  s2t<-sum(s2e,s2d)

  ftab<-qf (0.95, df1 = anova$Df[1], df2 = anova$Df[2])

  fn<-1-(n/N) # p/ unidade

  fm<-1-(m/M) # p/ unidade secundaria

  if(fn<0.98 & fm<0.98){
    s2y<-(fn*(s2e/n))+(fm*(s2d/(n*m)))
  }

  if(fn<0.98 & fm>=0.98){
    s2y<-(fn*(s2e/n))+(s2d/(n*m))
  }

  if(fn>=0.98 & fm<0.98){
    s2y<-((s2e/n))+(fm*(s2d/(n*m)))
  }

  if(fn>=0.98 & fm>=0.98){
    s2y<-((s2e/n))+(s2d/(n*m))
  }

  CV<-sqrt(s2t)/y*100

  sy<-sqrt(s2y)


  invt<-qt(1-p/2, df=(m*n)-1)

  invtn<-qt(1-p/2, df=(n-1))

  #Erro de amostragem
  eabs<-sy*invt
  erel<-eabs/y*100

  if(fn<0.98){
    #intensidade amostral FINITA
    nn<-(invtn^2*(s2e+s2d/m))/((E*y)^2+(1/N)*invtn^2*(s2e+s2d/M))

    if(pt==F){
      pop<-"(Finite pop.)"
    }

    if(pt==T){
    pop<-"(Pop. finita)"
}

    if(rn==T){
      invtn<-qt(1-p/2, df=(nn-1))
      nn<-(invtn^2*(s2e+s2d/m))/((E*y)^2+(1/N)*invtn^2*(s2e+s2d/M))

    }
  }

  if(fn>=0.98){
    #intensidade amostral INFINITA
    nn<-(invtn^2*(s2e+s2d/m))/(E*y)^2

    if(pt==F){
    pop<-"(Infinite pop.)"
    }

    if(pt==T){
    pop<-"(Pop. infinita)"
    }

    if(rn==T){
      invtn<-qt(1-p/2, df=(nn-1))
      nn<-(invtn^2*(s2e+s2d/m))/(E*y)^2
    }
  }

  mm<-sqrt(c*(s2d/s2e))


  #Estimativa do volume total da populacao
  Y<-y*N*M

  #Intervalo de Confianca
  ICparmax<-y+eabs
  ICparmin<-y-eabs

  IChecmax<-ICparmax/am
  IChecmin<-ICparmin/am

  ICfaixamax<-IChecmax*an
  ICfaixamin<-IChecmin*an

  ICtotmax<-ICparmax*A/am
  ICtotmin<-ICparmin*A/am


  if(pt==F){
    df <- data.table(Parameters=c("Mean", "Mean variance",
                                  "Mean standard error", "Total population volume",
                                  "Tabulated t value",
                                  "Absolute sampling error",
                                  "Relative sampling error",
                                  "Required error", "Significance level",
                                  "Coefficient of variation",
                                  "Sampled secondary units (m)", "Sampling intensity (m)",
                                  "Sampled primary units (n)","Sampling intensity (n)",
                                  "Lower CI per secondary unit",
                                  "Upper CI per secondary unit",
                                  "Lower CI per primary unit",
                                  "Upper CI per primary unit",
                                  "Lower CI per hectare",
                                  "Upper CI per hectare",
                                  "Lower CI per total area",
                                  "Upper CI per total area"),
                     Estimates=c(y, s2y,sy, Y, invt, eabs, erel, E*100,p*100,CV,m,ceiling(mm),n,ceiling(nn),ICparmin,
                                   ICparmax,ICfaixamin,ICfaixamax,IChecmin,IChecmax,ICtotmin,ICtotmax),
                     Unit=c("m3/secondary unit", "m3/secondary unit","m3/secondary unit","m3/total area",
                               "","m3/secondary unit","%", "%", "%","%","Secondary units/primary","Secondary units/primary","Primary units","Primary units","m3/secondary unit","m3/secondary unit",
                               "m3/primary unit","m3/primary unit", "m3/hectare","m3/hectare","m3/total area","m3/total area"))
  }


  if(pt==T){
  df <- data.table(Parametros=c("Media", "Variancia da media",
                                "Erro padrao da media", "Volume total da populacao",
                                "Valor de t tabelado",
                                "Erro de amostragem absoluto",
                                "Erro de amostragem relativo",
                                "Erro requerido", "Nivel de significancia",
                                "Coeficiente de variacao",
                                "Unid. secundarias amostradas (m)", "Intensidade amostral (m)",
                                "Unid. primarias amostradas (n)","Intensidade amostral (n)",
                                "IC inferior por unid. secundaria",
                                "IC superior por unid. secundaria",
                                "IC inferior por unid. primaria",
                                "IC superior por unid. primaria",
                                "IC inferior por hectare",
                                "IC superior por hectare",
                                "IC inferior para area total",
                                "IC superior para area total"),
                   Estimativas=c(y, s2y,sy, Y, invt, eabs, erel, E*100,p*100,CV,m,ceiling(mm),n,ceiling(nn),ICparmin,
                                 ICparmax,ICfaixamin,ICfaixamax,IChecmin,IChecmax,ICtotmin,ICtotmax),
                   Unidade=c("m3/unid. secundaria", "m3/unid. secundaria","m3/unid. secundaria","m3/area total",
                             "","m3/unid. secundaria","%", "%", "%","%","Unid. secundarias/primaria","Unid. secundarias/primaria","Unid. primarias","Unid. primarias","m3/unid. secundaria","m3/unid. secundaria",
                             "m3/unid. primaria","m3/unid. primaria", "m3/hectare","m3/hectare","m3/area total","m3/area total"))
}

  df[,2]<-format(round(df[,2],4),nsmall=4,scientific = F)

  par <- flextable(df)
  par <- align(par, align = "center")
  par <- align_text_col(par, align = "center")
  par<-autofit(par)


  if(n>=nn & m>=mm){


    if(pt==T){
    cat("\n------------------------------------------------------------------------------------\n")
    cat("A intensidade amostral satisfaz o erro requerido de", E*100,"%, para um nivel de significancia de",p*100,"%.")
    cat("\nPortanto, nao e necessario amostrar mais unidades secundarias nem primarias.\n")
    cat("------------------------------------------------------------------------------------")
    }

    if(pt==F){
      cat("\n------------------------------------------------------------------------------------\n")
      cat("The sampling intensity satisfies the required error of", E*100,"%, to a significance level of",p*100,"%.")
      cat("\nTherefore, it is not necessary to sample more secondary or primary units.\n")
      cat("------------------------------------------------------------------------------------")
    }

  }

  if(n<nn & m<mm){

    if(pt==T){
    cat("\n------------------------------------------------------------------------------------\n")
    cat("A intensidade amostral nao satisfaz o erro requerido de", E*100,"%, para um nivel de significancia de",p*100,"%.")
    cat("\nPortanto, e necessario amostrar mais",ceiling(nn-sum(ss)),"unidade(s) primaria(s), com mais",ceiling(mm-sum(ss)),"unidades secundarias cada.\n")
    cat("------------------------------------------------------------------------------------")
    }else{
      cat("\n------------------------------------------------------------------------------------\n")
      cat("The sample intensity does not satisfy the required error of", E*100,"%, to a significance level of",p*100,"%.")
      cat("\nTherefore, it is necessary to sample",ceiling(nn-sum(ss)),"more primary unit(s), with",ceiling(mm-sum(ss))," more secondary units each.\n")
      cat("------------------------------------------------------------------------------------")
    }



  }

  if(n<nn & m>=mm){


    if(pt==T){
    cat("\n------------------------------------------------------------------------------------\n")
    cat("A intensidade amostral nao satisfaz o erro requerido de", E*100,"%, para um nivel de significancia de",p*100,"%, apenas para as unidades primarias.")
    cat("\nPortanto, e necessario amostrar mais",ceiling(nn-n),"unidade(s) primaria(s).\n")
    cat("------------------------------------------------------------------------------------")
    }else{
      cat("\n------------------------------------------------------------------------------------\n")
      cat("The sample intensity does not satisfy the required error of", E*100,"%, to a significance level of",p*100,"%, for primary units only.")
      cat("\nTherefore, it is necessary to sample",ceiling(nn-n),"more primary unit(s).\n")
      cat("------------------------------------------------------------------------------------")
    }

  }

  if(n>=nn & m<mm){


    if(pt==T){
    cat("\n------------------------------------------------------------------------------------\n")
    cat("A intensidade amostral nao satisfaz o erro requerido de", E*100,"%, para um nivel de significancia de",p*100,"%, apenas para as unidades secundarias.")
    cat("\nPortanto, e necessario amostrar mais",ceiling(mm-m),"unidade(s) secundaria(s) em cada unidade primaria.\n")
    cat("------------------------------------------------------------------------------------")
    }else{
      cat("\n------------------------------------------------------------------------------------\n")
      cat("The sample intensity does not satisfy the required error of", E*100,"%, to a significance level of",p*100,"%, for secondary units only.")
      cat("\nTherefore, it is necessary to sample",ceiling(mm-m),"more secondary unit(s) in each primary unit.\n")
      cat("------------------------------------------------------------------------------------")
    }

  }

  if(anova$`F value`[1]>ftab){

    if(pt==T){
    cat("------------------------------------------------------------------------------------")
    cat("\nHa diferenca significativa entre as medias das unidades primarias.\n")
    cat("------------------------------------------------------------------------------------")
    }else{
      cat("------------------------------------------------------------------------------------")
      cat("\nThere is significant difference between primary unit means.\n")
      cat("------------------------------------------------------------------------------------")
    }

  }

  if(anova$`F value`[1]<ftab){

    if(pt==T){
    cat("------------------------------------------------------------------------------------")
    cat("\nNao ha diferenca significativa entre as medias das unidades primarias.\n")
    cat("------------------------------------------------------------------------------------")
    }else{
      cat("------------------------------------------------------------------------------------")
      cat("\nThere is not significant difference between primary unit means.\n")
      cat("------------------------------------------------------------------------------------")
    }

  }

  #Analise fitossociologica

  #Ajeitar dados para grafico e tabela

  x[,1]<-as.numeric(x[,1])

  `Unid. Primaria`<-x[,1]
  Especie<-x[,4]
  parcela<-x[,2]
  d<-x[,6]



  fito <- data.table(`Unid. Primaria`=`Unid. Primaria`,Especie=Especie, parcela=parcela, d=d)

  fito$gi<-pi*d^2/40000 #coluna com area seccional por individuo

  fito<-as.data.frame(fito)

  #quantidade de individuos por especies (n)

  for(i in fito[,2]){
    for(j in 1:max(fito[,1])){
      qt<-c(length(subset(fito[,2], fito[,2]==i & fito[,1]==j)))
    }
  }

  qt<-as.data.frame(qt)

  for(i in fito[,2]){
    for(j in 1:max(fito[,1])){
      qt[i,j]<-c(length(subset(fito[,2], fito[,2]==i & fito[,1]==j)))
    }
  }

  qt<-as.data.frame(qt)
  qt<-qt[-1,]
  qt2<-data.frame(n = unlist(qt,use.names = F))


  #quantidade de parcelas em que as especies estao presentes (UA)

  for(i in fito[,2]){
    for(j in 1:max(fito[,1])){
      sp<-c(length(unique(subset(fito[,3], fito[,2]==i & fito[,1]==j))))
    }
  }
  sp<-as.data.frame(sp)

  for(i in fito[,2]){
    for(j in 1:max(fito[,1])){
      sp[i,j]<-c(length(unique(subset(fito[,3], fito[,2]==i & fito[,1]==j))))
    }
  }
  sp<-as.data.frame(sp)
  sp<-sp[-1,]
  sp2<-data.frame(UA = unlist(sp,use.names = F))

  #area basal por especie

  for(i in fito[,2]){
    for(j in 1:max(fito[,1])){
      g<-c(sum(subset(fito[,5], fito[,2]==i & fito[,1]==j)))
    }
  }
  g<-as.data.frame(g)

  for(i in fito[,2]){
    for(j in 1:max(fito[,1])){
      g[i,j]<-c(sum(subset(fito[,5], fito[,2]==i & fito[,1]==j)))
    }
  }
  g<-as.data.frame(g)
  g<-g[-1,]
  g2<-data.frame(g = unlist(g,use.names = F))



  #criacao da tabela de parametros fitossociologicos

  dtt<-data.table(qt2,g2,sp2)
  dtt$especie<-rep(rownames(qt), ncol(qt))
  dtt$estrato<-rep(1:ncol(qt),each=nrow(qt))

  dtt<-dtt[!(dtt$n==0),]

  colnames(dtt)[1]<-"n"
  colnames(dtt)[2]<-"G (m2)"
  colnames(dtt)[3]<-"UA"
  colnames(dtt)[4]<-"Especie"
  colnames(dtt)[5]<-"Estrato"


  dtt$`DA (n/ha)`<-dtt$n/A #coluna de Densidade Absoluta (DA)

  #soma de DA pra cada estrato
  dtt<-dtt[order(dtt[,5]),]


  dtt<-as.data.frame(dtt)
  for(i in 1:max(dtt[,5])){
    sumda<-c(sum(subset(dtt[,6], dtt[,5]==1)))
  }
  sumda<-as.data.frame(sumda)

  for(i in 1:max(dtt[,5])){
    sumda[i]<-c(sum(subset(dtt[,6], dtt[,5]==i)))
  }
  sumda<-as.data.frame(sumda)

  sumda<-as.numeric(sumda)


  for(i in 1:max(dtt[,5])){

    sumdac<-with(dtt, ifelse(dtt$Estrato==i, sumda[i], ""))

  }

  sumdac<-as.data.frame(sumdac)

  for(i in 1:max(dtt[,5])){

    sumdac[i]<-with(dtt, ifelse(dtt$Estrato==i, sumda[i], ""))

  }

  sumdac<-as.data.frame(sumdac)


  sumdac2<-data.frame(sumdac = unlist(sumdac,use.names = T))



  sumdac2<-sumdac2[!(sumdac2$sumdac==""),]

  sumdac2<-as.matrix(sumdac2)
  sumdac2<-as.numeric(sumdac2)

  dtt$sumdac<-sumdac2


  dtt$`DR (%)`<-dtt$`DA (n/ha)`/dtt$sumdac*100 #coluna Densidade Relativa
  dtt$`DoA (G/ha)`<-dtt$`G (m2)`/A #coluna Dominancia Absoluta (DoA)

  #Soma de DoA por estrato

  for(i in 1:max(dtt[,5])){
    sumdoa<-c(sum(subset(dtt[,8], dtt[,5]==i)))
  }
  sumdoa<-as.data.frame(sumdoa)

  for(i in 1:max(dtt[,5])){
    sumdoa[i]<-c(sum(subset(dtt[,8], dtt[,5]==i)))
  }
  sumdoa<-as.data.frame(sumdoa)

  sumdoa<-as.numeric(sumdoa)


  for(i in 1:max(dtt[,5])){

    sumdoac<-with(dtt, ifelse(dtt$Estrato==i, sumdoa[i], ""))

  }

  sumdoac<-as.data.frame(sumdoac)

  for(i in 1:max(dtt[,5])){

    sumdoac[i]<-with(dtt, ifelse(dtt$Estrato==i, sumdoa[i], ""))

  }

  sumdoac<-as.data.frame(sumdoac)


  sumdoac2<-data.frame(sumdoac = unlist(sumdoac,use.names = T))



  sumdoac2<-sumdoac2[!(sumdoac2$sumdoac==""),]

  sumdoac2<-as.matrix(sumdoac2)
  sumdoac2<-as.numeric(sumdoac2)

  dtt$sumdoac<-sumdoac2


  dtt$`DoR (%)`<-dtt$`DoA (G/ha)`/dtt$sumdoac*100 #coluna de Dominancia Relativa (DoR)


  #parcelas/estrato
  for(i in 1:max(x[,1])){
    maxn<-c(length(unique(subset(x[,2], x[,1]==i))))
  }
  maxn<-as.data.frame(maxn)

  for(i in 1:max(x[,1])){
    maxn[i]<-c(length(unique(subset(x[,2], x[,1]==i))))
  }
  maxn<-as.data.frame(maxn)


  m<-as.numeric(maxn)


  for(i in 1:max(dtt[,5])){

    test<-with(dtt, ifelse(dtt$Estrato==i, m[i], ""))

  }

  test<-as.data.frame(test)

  for(i in 1:max(dtt[,5])){

    test[i]<-with(dtt, ifelse(dtt$Estrato==i, m[i], ""))

  }

  test<-as.data.frame(test)


  test2<-data.frame(test = unlist(test,use.names = F))

  test2<-test2[!(test2$test==""),]

  test2<-as.matrix(test2)
  test2<-as.numeric(test2)

  dtt$maxn<-test2

  dtt$`FA (%)`<-dtt$UA/dtt$maxn*100 #coluna Frequencia Absoluta (FA)


  #Soma de FA

  for(i in 1:max(dtt[,5])){
    sumfa<-c(sum(subset(dtt[,10], dtt[,5]==i)))
  }
  sumfa<-as.data.frame(sumfa)

  for(i in 1:max(dtt[,5])){
    sumfa[i]<-c(sum(subset(dtt[,10], dtt[,5]==i)))
  }
  sumfa<-as.data.frame(sumfa)

  sumfa<-as.numeric(sumfa)


  for(i in 1:max(dtt[,5])){

    sumfac<-with(dtt, ifelse(dtt$Estrato==i, sumfa[i], ""))

  }

  sumfac<-as.data.frame(sumfac)

  for(i in 1:max(dtt[,5])){

    sumfac[i]<-with(dtt, ifelse(dtt$Estrato==i, sumfa[i], ""))

  }

  sumfac<-as.data.frame(sumfac)


  sumfac2<-data.frame(sumfac = unlist(sumfac,use.names = T))



  sumfac2<-sumfac2[!(sumfac2$sumfac==""),]

  sumfac2<-as.data.frame(sumfac2)

  sumfac2<-as.matrix(sumfac2)
  sumfac2<-as.numeric(sumfac2)

  dtt$sumfac<-sumfac2

  dtt$`FR (%)`<-dtt$`FA (%)`/dtt$sumfac*100 #coluna FR
  dtt$`IVI (%)`<-dtt$`DR (%)`+dtt$`DoR (%)`+dtt$`FR (%)` #coluna IVI

  dtt2<-dtt[,c(5,4,1,2,3,6,8,9,11,13,15,16)] #ordenar as colunas

  dtt2<-dtt2[order(dtt2$`IVI (%)`, decreasing = T),] #ordenar por IVI
  dtt2<-dtt2[order(dtt2$Estrato),] #ordenar por Estrato

  colnames(dtt2)[1]<-"Unid. Primaria"

  #nomear as colunas em ingles
  if(pt==F){
    colnames(dtt2)[1]<-"Primary Unit"
    colnames(dtt2)[2]<-"Specie"
    colnames(dtt2)[3]<-"n"
    colnames(dtt2)[4]<-"G (m2)"
    colnames(dtt2)[5]<-"SU"
    colnames(dtt2)[6]<-"AD (n/ha)"
    colnames(dtt2)[7]<-"RD (%)"
    colnames(dtt2)[8]<-"ADo (G/ha)"
    colnames(dtt2)[9]<-"RDo (%)"
    colnames(dtt2)[10]<-"AF (%)"
    colnames(dtt2)[11]<-"RF (%)"
    colnames(dtt2)[12]<-"IVI (%)"

  }

  dtt3<-as.data.frame(dtt2)

  dtt3[,1]<-format(round(dtt3[,1],0),nsmall=0)
  dtt3[,3]<-format(round(dtt3[,3],0),nsmall=0)
  dtt3[,4]<-format(round(dtt3[,4],4),nsmall=4)
  dtt3[,5]<-format(round(dtt3[,5],0),nsmall=0)
  dtt3[,6]<-format(round(dtt3[,6],2),nsmall=2)
  dtt3[,7]<-format(round(dtt3[,7],2),nsmall=2)
  dtt3[,8]<-format(round(dtt3[,8],2),nsmall=2)
  dtt3[,9]<-format(round(dtt3[,9],2),nsmall=2)
  dtt3[,10]<-format(round(dtt3[,10],2),nsmall=2)
  dtt3[,11]<-format(round(dtt3[,11],2),nsmall=2)
  dtt3[,12]<-format(round(dtt3[,12],2),nsmall=2)




  #transformar em flextable
  fitot <- flextable(dtt3)
  fitot<-autofit(fitot)
  fitot <- align(fitot, align = "center", part="all")
  fitot<-italic(fitot,j=2)



  #Gr?fico fito


  if(pt==T){

    data <- dtt2[c(1, 2, 7, 9, 11)] %>%
      gather(Parametros, b, -`Unid. Primaria`, -Especie) %>%
      mutate(Parametros = case_when(
        grepl('^DR', Parametros) ~ 'Densidade Relativa (%)',
        grepl('^DoR', Parametros) ~ 'Dominancia Relativa (%)',
        grepl('^FR', Parametros) ~ 'Frequencia Relativa (%)',
        TRUE ~ NA_character_
      ))

    gg2<-ggplot(data, aes(reorder(Especie,b), b, fill = Parametros)) +
      geom_col(alpha = 0.8) +
      scale_fill_brewer(palette = "Dark2") +
      theme_bw(16)  +
      coord_flip() +
      xlab("Especies") + ylab("Indice de Valor de Importancia (%)") +
      labs(fill = "Parametros") +
      theme(axis.text.y = element_text(face = "italic",size=8), legend.title=element_blank(),legend.justification = "center" ,legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12),
            legend.position="bottom",legend.direction = "horizontal")+
      facet_wrap(~ data[,1])


    p2 <- gg2 + theme(legend.position = "none")
    le1 <- cowplot::get_legend(gg2)
    gg3<-cowplot::plot_grid(p2, le1,nrow = 2,rel_heights = c(1, 0.2))


  }else{
    data <- dtt2[c(1, 2, 7, 9, 11)] %>%
      gather(Parameters, b, -`Primary Unit`, -Specie) %>%
      mutate(Parameters = case_when(
        grepl('^RDo', Parameters) ~ 'Relative Dominance (%)',
        grepl('^RD', Parameters) ~ 'Relative Density (%)',
        grepl('^RF', Parameters) ~ 'Relative Frequency (%)',
        TRUE ~ NA_character_
      ))

    gg2<-ggplot(data, aes(reorder(Specie,b), b, fill = Parameters)) +
      geom_col(alpha = 0.8) +
      scale_fill_brewer(palette = "Dark2") +
      theme_bw(16)  +
      coord_flip() +
      xlab("Species") + ylab("Importance Value Index (%)") +
      labs(fill = "Parameters") +
      theme(axis.text.y = element_text(face = "italic",size=8), legend.title=element_blank(),legend.justification = "center" ,legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12),
            legend.position="bottom",legend.direction = "horizontal")+
      facet_wrap(~ data[,1])

    p2 <- gg2 + theme(legend.position = "none")
    le1 <- cowplot::get_legend(gg2)
    gg3<-cowplot::plot_grid(p2, le1,nrow = 2,rel_heights = c(1, 0.2))

  }


  #Tabela de volume por especie

  x<-as.data.frame(x)
  x[,ncol(x)]<-as.numeric(x[,ncol(x)])



  for(i in x[,4]){
    vvol<-c(sum(subset(x[,ncol(x)], x[,4]==i)))
  }
  vvol<-as.data.frame(vvol)

  for(i in x[,4]){
    tryCatch({
      vvol[i]<-c(sum(subset(x[,ncol(x)], x[,4]==i)))
    }, error=function(e){})
  }
  vvol<-as.data.frame(vvol)
  vvol[,1]<-NULL
  vvol<-as.data.frame(t(vvol))
  vvol[,2]<-rownames(vvol)


  if(pt==T){
    colnames(x)[1]<-"Unid. Primaria"
    colnames(x)[2]<-"Parcela"
    colnames(x)[3]<-"Individuo"
    colnames(x)[4]<-"Especie"
    colnames(x)[5]<-"Altura (m)"
    colnames(x)[6]<-"Diametro (cm)"
    colnames(x)[7]<-"Volume (m3)"
  }else{
    colnames(x)[1]<-"Primary Unit"
    colnames(x)[2]<-"Plot"
    colnames(x)[3]<-"Individual"
    colnames(x)[4]<-"Specie"
    colnames(x)[5]<-"Height (m)"
    colnames(x)[6]<-"Diameter (cm)"
    colnames(x)[7]<-"Volume (m3)"
  }


  x2<-as.data.frame(x)

  x2[,1]<-format(round(x2[,1],0),nsmall=0)
  x2[,2]<-format(round(x2[,2],0),nsmall=0)
  x2[,3]<-format(round(x2[,3],0),nsmall=0)
  x2[,5]<-format(round(x2[,5],2),nsmall=2)
  x2[,6]<-format(round(x2[,6],2),nsmall=2)
  x2[,7]<-format(round(x2[,7],4),nsmall=4)

  x3 <- flextable(x2)
  x3 <- autofit(x3)
  x3 <- align(x3, align = "center", part="all")

  #tabela de volume/especie se nao houver o argumento prot

  if(missing(prot)) {
    if(pt==T){
      vt<-data.table(c(vvol[,2],"Total"), c(vvol[,1],sum(vvol[,1])), c(vvol[,1]/a, sum(vvol[,1])/a), c(vvol[,1]*A/a, sum(vvol[,1])*A/a))
      colnames(vt)[1]<-"Especie"
      colnames(vt)[2]<-"Volume amostrado (m3)"
      colnames(vt)[3]<-"Volume/hectare (m3)"
      colnames(vt)[4]<-"Volume/area total (m3)"

      vt$`Volume amostrado (m3)`<-as.numeric(vt$`Volume amostrado (m3)`)
      vt$`Volume amostrado (m3)`<-format(round(vt$`Volume amostrado (m3)`,4),nsmall=4)
      vt$`Volume/hectare (m3)`<-as.numeric(vt$`Volume/hectare (m3)`)
      vt$`Volume/hectare (m3)`<-format(round(vt$`Volume/hectare (m3)`,4),nsmall=4)
      vt$`Volume/area total (m3)`<-as.numeric(vt$`Volume/area total (m3)`)
      vt$`Volume/area total (m3)`<-format(round(vt$`Volume/area total (m3)`,4),nsmall=4)


    }else{
      vt<-data.table(c(vvol[,2],"Total"), c(vvol[,1],sum(vvol[,1])), c(vvol[,1]/a, sum(vvol[,1])/a), c(vvol[,1]*A/a, sum(vvol[,1])*A/a))
      colnames(vt)[1]<-"Specie"
      colnames(vt)[2]<-"Sampled volume (m3)"
      colnames(vt)[3]<-"Volume/hectare (m3)"
      colnames(vt)[4]<-"Volume/total area (m3)"

      vt$`Sampled volume (m3)`<-as.numeric(vt$`Sampled volume (m3)`)
      vt$`Sampled volume (m3)`<-format(round(vt$`Sampled volume (m3)`,4),nsmall=4)
      vt$`Volume/hectare (m3)`<-as.numeric(vt$`Volume/hectare (m3)`)
      vt$`Volume/hectare (m3)`<-format(round(vt$`Volume/hectare (m3)`,4),nsmall=4)
      vt$`Volume/total area (m3)`<-as.numeric(vt$`Volume/total area (m3)`)
      vt$`Volume/total area (m3)`<-format(round(vt$`Volume/total area (m3)`,4),nsmall=4)
    }

    vt2<-as.data.frame(vt)

    vt2[,2]<-as.numeric(vt2[,2])
    vt2[,3]<-as.numeric(vt2[,3])
    vt2[,4]<-as.numeric(vt2[,4])


    vt2[,2]<-format(round(vt2[,2],4),nsmall=4)
    vt2[,3]<-format(round(vt2[,3],4),nsmall=4)
    vt2[,4]<-format(round(vt2[,4],4),nsmall=4)

    vtt<-as.data.frame(vt2)
    vtt <- flextable(vtt)
    vtt <- autofit(vtt)
    vtt <- align(vtt, align = "center", part="all")
    vtt<-italic(vtt,j=1,i=2:nrow(vt)-1)




    #criar docx sem argumento prot


    if(pt==T){
      doc <- read_docx() %>%

        body_add_par("Tabela 1. Parametros da amostragem em dois estagios.", style = "centered") %>%
        body_add_flextable(par) %>% #tabela de parametros volume
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_gg(diam,style="centered", height=4,width=6) %>% #distribuicao diametrica
        body_add_par("Figura 1. Distribuicao diametrica por unidade primaria.", style = "centered") %>%
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_par("Tabela 2. Volume lenhoso por parcela.", style = "centered") %>%
        body_add_flextable(vopa) %>% #volume/parcela
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 3. Volume lenhoso por unidade primaria.", style = "centered") %>%
        body_add_flextable(vol.unid) %>% #volume/estrato
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 4. Volume lenhoso por especie.", style = "centered") %>%
        body_add_flextable(vtt) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 5. Parametros fitossociologicos por unidade primaria, em que: n = quantidade de individuos amostrados; G = area basal; UA = quantidade de unidades amostrais; DA (n/ha) = Densidade absoluta; DR (%) = Densidade relativa; DoA (G/ha) = Dominancia Absoluta; DoR (%) = Dominancia Relativa; FA (%) = Frequencia absoluta; FR (%) = Frequencia Relativa; IVI (%) = Indice de Valor de Importancia.", style = "centered") %>%
        body_add_flextable(fitot) %>% #parametros fito
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_gg(gg3,style="centered", height=6,width=6)%>%#grafico fito
        body_add_par("Figura 2. Indice de Valor de Importancia por especie e por unidade primaria (soma de densidade relativa, dominancia relativa e frequencia relativa).", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 6. Volume lenhoso individual.", style = "centered") %>%
        body_add_flextable(x3)%>%
        body_end_section_landscape()



    }else{

      doc <- read_docx() %>%
        body_add_par("Table 1. Two-stage sampling parameters.", style = "centered") %>%
        body_add_flextable(par) %>% #tabela de parametros volume
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_gg(diam,style="centered", height=4,width=6) %>% #distribuicao diametrica
        body_add_par("Figure 1. Diameter distribution by primary unit.", style = "centered") %>%
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_par("Table 2. Woody volume by plot.", style = "centered") %>%
        body_add_flextable(vopa) %>% #volume/parcela
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 3. Woody volume by primary unit.", style = "centered") %>%
        body_add_flextable(vol.unid) %>% #volume/estrato
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 4. Woody volume by specie", style = "centered") %>%
        body_add_flextable(vtt) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 5. Phytosociological parameters by primary unit, where: n = number of sampled individuals; G = basal area; SU = number of sample units; AD (n/ha) = absolute density; RD (%) = relative density; ADo (G/ha) = absolute dominance; RDo (%) = relative dominance; AF (%) = absolute frequency; RF (%) = relative frequency; IVI (%) = Importance Value Index.", style = "centered") %>%
        body_add_flextable(fitot) %>% #parametros fito
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_gg(gg3,style="centered", height=6,width=6)%>%#grafico fito
        body_add_par("Figure 2. Importance Value Index by specie and by primary unit (sum of relative density, relative dominance and relative frequency).", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 6. Individual woody volume.", style = "centered") %>%
        body_add_flextable(x3)%>%
        body_end_section_landscape()


    }



  } else {

    #tabela volume com argumento prot

    for(i in prot){
      pp<-c(sum(subset(x[,ncol(x)],x[,4]==i),na.rm = T))
    }

    pp<-as.data.frame(pp)

    for(i in prot){
      pp[i]<-c(sum(subset(x[,ncol(x)],x[,4]==i),na.rm = T))
    }

    pp<-as.data.frame(pp[,2:ncol(pp)])

    if(pt==T){
      vt<-data.table(Especie=c(vvol[,2],"Total"), `Volume amostrado (m3)`=c(vvol[,1],sum(vvol[,1])), `Volume/hectare (m3)`=c(vvol[,1]/a, sum(vvol[,1])/a), `Volume/area total (m3)`=c(vvol[,1]*A/a, sum(vvol[,1])*A/a))

      vt$`Volume amostrado (m3)`<-as.numeric(vt$`Volume amostrado (m3)`)
      vt$`Volume amostrado (m3)`<-format(round(vt$`Volume amostrado (m3)`,4),nsmall=4)
      vt$`Volume/hectare (m3)`<-as.numeric(vt$`Volume/hectare (m3)`)
      vt$`Volume/hectare (m3)`<-format(round(vt$`Volume/hectare (m3)`,4),nsmall=4)
      vt$`Volume/area total (m3)`<-as.numeric(vt$`Volume/area total (m3)`)
      vt$`Volume/area total (m3)`<-format(round(vt$`Volume/area total (m3)`,4),nsmall=4)

      ph<-data.table(Especie=c(prot,"Total protegido","Total desprotegido"), `Volume amostrado (m3)`=c(pp,sum(pp),sum(x[,ncol(x)])-sum(pp)), `Volume/hectare (m3)`=c(pp/a, sum(pp)/a, (sum(x[,ncol(x)])-sum(pp))/a), `Volume/area total (m3)`=c(pp*A/a, sum(pp)*A/a, (sum(x[,ncol(x)])-sum(pp))*A/a))

      ph$`Volume amostrado (m3)`<-as.numeric(ph$`Volume amostrado (m3)`)
      ph$`Volume amostrado (m3)`<-format(round(ph$`Volume amostrado (m3)`,4),nsmall=4)
      ph$`Volume/hectare (m3)`<-as.numeric(ph$`Volume/hectare (m3)`)
      ph$`Volume/hectare (m3)`<-format(round(ph$`Volume/hectare (m3)`,4),nsmall=4)
      ph$`Volume/area total (m3)`<-as.numeric(ph$`Volume/area total (m3)`)
      ph$`Volume/area total (m3)`<-format(round(ph$`Volume/area total (m3)`,4),nsmall=4)
    }else{
      vt<-data.table(Specie=c(vvol[,2],"Total"), `Sampled volume (m3)`=c(vvol[,1],sum(vvol[,1])), `Volume/hectare (m3)`=c(vvol[,1]/a, sum(vvol[,1])/a), `Volume/total area (m3)`=c(vvol[,1]*A/a, sum(vvol[,1])*A/a))

      vt$`Sampled volume (m3)`<-as.numeric(vt$`Sampled volume (m3)`)
      vt$`Sampled volume (m3)`<-format(round(vt$`Sampled volume (m3)`,4),nsmall=4)
      vt$`Volume/hectare (m3)`<-as.numeric(vt$`Volume/hectare (m3)`)
      vt$`Volume/hectare (m3)`<-format(round(vt$`Volume/hectare (m3)`,4),nsmall=4)
      vt$`Volume/total area (m3)`<-as.numeric(vt$`Volume/total area (m3)`)
      vt$`Volume/total area (m3)`<-format(round(vt$`Volume/total area (m3)`,4),nsmall=4)

      ph<-data.table(Specie=c(prot,"Total protected","Total unprotected"), `Sampled volume (m3)`=c(pp,sum(pp),sum(x[,ncol(x)])-sum(pp)), `Volume/hectare (m3)`=c(pp/a, sum(pp)/a, (sum(x[,ncol(x)])-sum(pp))/a), `Volume/total area (m3)`=c(pp*A/a, sum(pp)*A/a, ((sum(x[,ncol(x)]))-(sum(pp)))*A/a))

      ph$`Sampled volume (m3)`<-as.numeric(ph$`Sampled volume (m3)`)
      ph$`Sampled volume (m3)`<-format(round(ph$`Sampled volume (m3)`,4),nsmall=4)
      ph$`Volume/hectare (m3)`<-as.numeric(ph$`Volume/hectare (m3)`)
      ph$`Volume/hectare (m3)`<-format(round(ph$`Volume/hectare (m3)`,4),nsmall=4)
      ph$`Volume/total area (m3)`<-as.numeric(ph$`Volume/total area (m3)`)
      ph$`Volume/total area (m3)`<-format(round(ph$`Volume/total area (m3)`,4),nsmall=4)

    }

    vt2<-as.data.frame(vt)

    vt2[,2]<-as.numeric(vt2[,2])
    vt2[,3]<-as.numeric(vt2[,3])
    vt2[,4]<-as.numeric(vt2[,4])


    vt2[,2]<-format(round(vt2[,2],4),nsmall=4)
    vt2[,3]<-format(round(vt2[,3],4),nsmall=4)
    vt2[,4]<-format(round(vt2[,4],4),nsmall=4)

    vtt<-as.data.frame(vt2)
    vtt <- flextable(vtt)
    vtt <- autofit(vtt)
    vtt <- align(vtt, align = "center", part="all")
    vtt<-italic(vtt,j=1,i=2:nrow(vt)-1)


    ph2<-as.data.frame(ph)

    ph2[,2]<-as.numeric( ph2[,2])
    ph2[,3]<-as.numeric( ph2[,3])
    ph2[,4]<-as.numeric( ph2[,4])


    ph2[,2]<-format(round(ph2[,2],4),nsmall=4)
    ph2[,3]<-format(round(ph2[,3],4),nsmall=4)
    ph2[,4]<-format(round(ph2[,4],4),nsmall=4)

    phi<-as.data.frame(ph2)
    phi <- flextable(phi)
    phi <- autofit(phi)
    phi <- align(phi, align = "center", part="all")
    phi<-italic(phi,j=1,i=c(1:length(prot)))

    #criar docx com argumento prot


    if(pt==T){
      doc <- read_docx() %>%

        body_add_par("Tabela 1. Parametros da amostragem em dois estagios.", style = "centered") %>%
        body_add_flextable(par) %>% #tabela de parametros volume
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_gg(diam, style="centered", height=4,width=6) %>% #distribuicao diametrica
        body_add_par("Figura 1. Distribuicao diametrica por unidade primaria.", style = "centered") %>%
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_par("Tabela 2. Volume lenhoso por parcela.", style = "centered") %>%
        body_add_flextable(vopa) %>% #volume/parcela
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 3. Volume lenhoso por unidade primaria.", style = "centered") %>%
        body_add_flextable(vol.unid) %>% #volume/estrato
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 4. Volume lenhoso por especie.", style = "centered") %>%
        body_add_flextable(vtt) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 5. Volume lenhoso por especie protegida.", style = "centered") %>%
        body_add_flextable(phi) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 6. Parametros fitossociologicos por unidade primaria, em que: n = quantidade de individuos amostrados; G = area basal; UA = quantidade de unidades amostrais; DA (n/ha) = Densidade absoluta; DR (%) = Densidade relativa; DoA (G/ha) = Dominancia Absoluta; DoR (%) = Dominancia Relativa; FA (%) = Frequencia absoluta; FR (%) = Frequencia Relativa; IVI (%) = Indice de Valor de Importancia.", style = "centered") %>%
        body_add_flextable(fitot) %>% #parametros fito
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_gg(gg3,style="centered", height=6,width=6)%>%#grafico fito
        body_add_par("Figura 2. Indice de Valor de Importancia por especie e por unidade primaria (soma de densidade relativa, dominancia relativa e frequencia relativa).", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 7. Volume lenhoso individual.", style = "centered") %>%
        body_add_flextable(x3)%>%
        body_end_section_landscape()

    }else{

      doc <- read_docx() %>%
        body_add_par("Table 1. Two-stage sampling parameters.", style = "centered") %>%
        body_add_flextable(par) %>% #tabela de parametros volume
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_gg(diam,style="centered", height=4,width=6) %>% #distribuicao diametrica
        body_add_par("Figure 1. Diameter distribution by primary unit.", style = "centered") %>%
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_par("Table 2. Woody volume by plot.", style = "centered") %>%
        body_add_flextable(vopa) %>% #volume/parcela
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 3. Woody volume by primary unit.", style = "centered") %>%
        body_add_flextable(vol.unid) %>% #volume/estrato
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 4. Woody volume by specie", style = "centered") %>%
        body_add_flextable(vtt) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 5. Woody volume by protected specie", style = "centered") %>%
        body_add_flextable(phi) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 6. Phytosociological parameters by primary unit, where: n = number of sampled individuals; G = basal area; SU = number of sample units; AD (n/ha) = absolute density; RD (%) = relative density; ADo (G/ha) = absolute dominance; RDo (%) = relative dominance; AF (%) = absolute frequency; RF (%) = relative frequency; IVI (%) = Importance Value Index.", style = "centered") %>%
        body_add_flextable(fitot) %>% #parametros fito
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_gg(gg3,style="centered", height=6,width=6)%>%#grafico fito
        body_add_par("Figure 2. Importance Value Index by specie and by primary unit (sum of relative density, relative dominance and relative frequency).", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 7. Individual woody volume.", style = "centered") %>%
        body_add_flextable(x3)%>%
        body_end_section_landscape()


    }



  }

  if(pt==T){
    fileout <- tempfile(fileext = ".docx")
    fileout <- paste(getwd(),"/Inventario Florestal - ",nm,".docx",sep="")
    print(doc, target = fileout)
  }else{
    fileout <- tempfile(fileext = ".docx")
    fileout <- paste(getwd(),"/Forest Inventory - ",nm,".docx",sep="")
    print(doc, target = fileout)
  }


  if(missing(prot)){
    return(list(x3,gg3,fitot,vtt,vol.unid,vopa,diam,par))
  }else{
    return(list(x3,gg3,fitot,phi,vtt,vol.unid,vopa,diam,par))
  }
}
