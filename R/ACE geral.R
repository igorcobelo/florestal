
ace<-function(x,a,aj,E=0.1,p=0.05,prot=NULL,ampl=5,prop=F,rn=F,spivi=15,ci=2,un=F,pt=T,...){


  ##para que o arquivo docx seja nomeado com o mesmo nome do input
  nm <-deparse(substitute(x))

  #para que seja feito o ggplot
  x<-as.data.frame(x)

#Grafico de distribuicao diametrica por estrato
  if(pt==T){
  diam<-ggplot(x, aes(x=x[,6], colour=x[,1])) +
    geom_histogram(binwidth=ampl,fill="#69b3a2", color="#e9ecef", alpha=0.9) +
    theme_bw(16)+
    theme(axis.text.y = element_text(size=10),legend.text=element_text(size=10),
          axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
          axis.title.y=element_text(size=12)) +
    scale_x_continuous(breaks = seq(0, max(x[,6])+2, ampl)) +
    xlab("Classe Diametrica (cm)") +
    ylab("Frequencia") +
    facet_wrap(~ x[,1])

  }else{
    diam<-ggplot(x, aes(x=x[,6], colour=x[,1])) +
      geom_histogram( binwidth=ampl,fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      theme_bw(16)+
      theme(axis.text.y = element_text(size=10),legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12)) +
      scale_x_continuous(breaks = seq(0, max(x[,6])+2, ampl)) +
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

  #Encontrar quantas parcelas h? em cada estrato
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
A<-sum(aj)

  if(pt==T){
  vp<-data.table(c(rep,"Media"), c(1:max(x[,2]),""), c(vv,sum(vv)/max(x[,2])), c(vv/a, (sum(vv)/max(x[,2]))/a), c(vv*A/a, (sum(vv)/max(x[,2]))*A/a))
  colnames(vp)[1]<-"Estrato"
  colnames(vp)[2]<-"Parcela"
  colnames(vp)[3]<-"Volume/parcela (m3)"
  colnames(vp)[4]<-"Volume/hectare (m3)"
  colnames(vp)[5]<-"Volume/area total (m3)"

  }else{
    vp<-data.table(c(rep,"Mean"), c(1:max(x[,2]),""), c(vv,sum(vv)/max(x[,2])), c(vv/a, (sum(vv)/max(x[,2]))/a), c(vv*A/a, (sum(vv)/max(x[,2]))*A/a))
    colnames(vp)[1]<-"Stratum"
    colnames(vp)[2]<-"Plot"
    colnames(vp)[3]<-"Volume/plot (m3)"
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

  #Vari?ncia por estrato

  vp<-data.frame(vp)

  ph2<-as.data.frame(vp)
  ph2[,3]<-as.numeric(ph2[,3])


  for(i in ph2[1:nrow(ph2)-1,1]){
    var<-c(var(subset(ph2[,3], ph2[,1]==i)))
  }
  var<-as.data.frame(var)

  for(i in ph2[1:nrow(ph2)-1,1]){
    var[i]<-c(var(subset(ph2[,3], ph2[,1]==i)))
  }
  var<-as.data.frame(var)

  var[,1]<-NULL

  #soma de volumes por estrato
  for(i in ph2[1:nrow(ph2)-1,1]){
    tt<-c(sum(subset(ph2[,3], ph2[,1]==i)))
  }
  tt<-as.data.frame(tt)

  for(i in ph2[1:nrow(ph2)-1,1]){
    tt[i]<-c(sum(subset(ph2[,3], ph2[,1]==i)))
  }
  tt<-as.data.frame(tt)

  tt[,1]<-NULL


  # Dados iniciais
  A<-sum(aj) #area total
  N<-A/a #parcelas que cabem na area total
  estrat<-aj/a #parcelas que cabem em cada estrato
  P<-estrat/N #P
  yi<-tt/ss #media por estrato
  y<-sum(P*yi) #media geral

  s<-sqrt(var) #variancia geral
  erroabsreq<-y*E #erro requerido absoluto



  #t tabelado
  invt<-qt(1-p/2, df=sum(ss)-1)

  #Fator de corre??o (finita ou infinita)
  f<-1-sum(ss)/N

  if(prop==F){
    #P/ ALOCA??O ?TIMA

    if(f<0.98){
      #intensidade amostral FINITA
      n<-(invt^2*(sum(P*s)^2))/(erroabsreq^2 + (invt^2*sum(P*s)/N))

      if(rn==T){
        invt<-qt(1-p/2, df=n-1)
        n<-(invt^2*(sum(P*s)^2))/(erroabsreq^2 + (invt^2*sum(P*s)/N))
      }
    }

    if(f>=0.98){
      #intensidade amostral INFINITA
      n<-(invt^2*(sum(P*s)^2))/(erroabsreq^2)

      if(rn==T){
        invt<-qt(1-p/2, df=n-1)
        n<-(invt^2*(sum(P*s)^2))/(erroabsreq^2)
      }
    }

    #aloca??o ?tima dos estratos
    nj<-(((P*s)/sum(P*s))*n)
  }

  if(prop==T){
    #P/ ALOCA??O PROPORCIONAL

    if(f<0.98){
      #intensidade amostral FINITA
      n<-(invt^2*sum(P*var))/(erroabsreq^2 + (invt^2*sum(P*var)/N))

      if(rn==T){
      invt<-qt(1-p/2, df=n-1)
      n<-(invt^2*sum(P*var))/(erroabsreq^2 + (invt^2*sum(P*var)/N))
      }
    }

    if(f>=0.98){
      #intensidade amostral INFINITA
      n<-(invt^2*sum(P*var))/(erroabsreq^2)

      if(rn==T){
      invt<-qt(1-p/2, df=n-1)
      n<-(invt^2*sum(P*var))/(erroabsreq^2)
      }
    }

    #aloca??o proporcional dos estratos
    nj<-as.data.frame(P*n)

  }

#Criacao de "pop"
if(f<0.98){
  if(pt==T){
  pop<-"(Pop. finita)"
  }else{
    pop<-"(Finite pop.)"
  }
}

if(f>=0.98){
  if(pt==T){
    pop<-"(Pop. infinita)"
  }else{
    pop<-"(Infinite pop.)"
  }
}



  #tabela auxiliar

  jj<-as.data.frame(nj) #nj esta mais abaixo
  jjp<-t(data.frame(jj))

  tabaux<-data.table(c(1:max(x[,1]),"Total"), c(ss,sum(ss)), c(estrat,sum(estrat)), c(P,sum(P)), c(var,""),c(s,""),c(P*var,sum(P*var)),c(P*s,sum(P*s)), c(jjp, sum(jjp)))

  if(pt==T){
    colnames(tabaux)[1]<-"Estrato"
    if(prop==T){
      colnames(tabaux)[9]<-"Alocacao proporcional"
    }else{
      colnames(tabaux)[9]<-"Alocacao otima"
    }

  }else{
    colnames(tabaux)[1]<-"Stratum"
    if(prop==T){
      colnames(tabaux)[9]<-"Proportional allocation"
    }else{
      colnames(tabaux)[9]<-"Optimal allocation"
    }
  }


  colnames(tabaux)[2]<-"nj"
  colnames(tabaux)[3]<-"Nj"
  colnames(tabaux)[4]<-"Pj"
  colnames(tabaux)[5]<-"S2j"
  colnames(tabaux)[6]<-"Sj"
  colnames(tabaux)[7]<-"PjS2j"
  colnames(tabaux)[8]<-"PjSj"

  tabaux$nj<-as.numeric(tabaux$nj)
  tabaux$Nj<-as.numeric(tabaux$Nj)
  tabaux$Pj<-as.numeric(tabaux$Pj)
  tabaux$Sj<-as.numeric(tabaux$Sj)
  tabaux$S2j<-as.numeric(tabaux$S2j)
  tabaux$PjS2j<-as.numeric(tabaux$PjS2j)
  tabaux$PjSj<-as.numeric(tabaux$PjSj)


  tabaux$nj<-format(round(tabaux$nj,0),nsmall=0)
  tabaux$Nj<-format(round(tabaux$Nj,0),nsmall=0)
  tabaux$Pj<-format(round(tabaux$Pj,3),nsmall=3)
  tabaux$Sj<-format(round(tabaux$Sj,3),nsmall=3)
  tabaux$S2j<-format(round(tabaux$S2j,3),nsmall=3)
  tabaux$PjS2j<-format(round(tabaux$PjS2j,3),nsmall=3)
  tabaux$PjSj<-format(round(tabaux$PjSj,3),nsmall=3)

  tabaux[nrow(tabaux),c(5,6)]<-""

  tabaux <- flextable(tabaux)
  tabaux  <- align(tabaux , align = "center")
  tabaux  <- align_text_col(tabaux , align = "center")
  tabaux <-autofit(tabaux)

  #Variancia da media estratificada

  if(f<0.98){
    #P/ pop. finita
    s2y<-(((sum(P*s))^2)/sum(ss))-(sum(P*var)/N)
  }

  if(f>=0.98){
    #P/pop. infinita
    s2y<-(sum(p*s)^2/n)
  }

  #erro-padrao da media estratificada
  sy<-sqrt(s2y)


  #Erro de amostragem
  eabs<-sy*invt
  erel<-eabs/y*100

  #Estimativa do volume total da populacao
  Y<-y*N

  #Intervalo de Confianca
  ICparmax<-y+eabs
  ICparmin<-y-eabs

  IChecmax<-ICparmax/a
  IChecmin<-ICparmin/a

  ICtotmax<-ICparmax*A/a
  ICtotmin<-ICparmin*A/a

  CV<-sum(P*s)/y*100


  # Tabela de volume por estrato

  if(pt==T){
  vol.estrat<-data.table(c(1:length(ss),"Media"), c(tt,sum(tt)/length(tt)), c(tt/a,(sum(tt)/length(tt))/a),c(tt*A/a,(sum(tt)/length(tt))*A/a))
  colnames(vol.estrat)[1]<-"Estrato"
  colnames(vol.estrat)[2]<-"Volume amostrado/estrato (m3)"
  colnames(vol.estrat)[3]<-"Volume/ha (m3)"
  colnames(vol.estrat)[4]<-"Volume/area total (m3)"

  vol.estrat$`Volume amostrado/estrato (m3)`<-as.numeric(vol.estrat$`Volume amostrado/estrato (m3)`)
  vol.estrat$`Volume amostrado/estrato (m3)`<-format(round(vol.estrat$`Volume amostrado/estrato (m3)`,4),nsmall=4)
  vol.estrat$`Volume/ha (m3)`<-as.numeric(vol.estrat$`Volume/ha (m3)`)
  vol.estrat$`Volume/ha (m3)`<-format(round(vol.estrat$`Volume/ha (m3)`,4),nsmall=4)
  vol.estrat$`Volume/area total (m3)`<-as.numeric(vol.estrat$`Volume/area total (m3)`)
  vol.estrat$`Volume/area total (m3)`<-format(round(vol.estrat$`Volume/area total (m3)`,4),nsmall=4)


  }else{
    vol.estrat<-data.table(c(1:length(ss),"Mean"), c(tt,sum(tt)/length(tt)), c(tt/a,(sum(tt)/length(tt))/a),c(tt*A/a,(sum(tt)/length(tt))*A/a))
    colnames(vol.estrat)[1]<-"Stratum"
    colnames(vol.estrat)[2]<-"Sampled volume/stratum (m3)"
    colnames(vol.estrat)[3]<-"Volume/ha (m3)"
    colnames(vol.estrat)[4]<-"Volume/total area (m3)"

    vol.estrat$`Sampled volume/stratum (m3)`<-as.numeric(vol.estrat$`Sampled volume/stratum (m3)`)
    vol.estrat$`Sampled volume/stratum (m3)`<-format(round(vol.estrat$`Sampled volume/stratum (m3)`,4),nsmall=4)
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

  vol.estrat2 <- flextable(vol.estrat2)
  vol.estrat2  <- align(vol.estrat2 , align = "center")
  vol.estrat2  <- align_text_col(vol.estrat2 , align = "center")
  vol.estrat2 <-autofit(vol.estrat2)


#Parametros estatisticos

if(pt==T){
  df <- data.table(Parametros=c("Media estratificada", "Variancia da media estratificada",
                                "Erro padrao da media estratificada", "Volume total da populacao",
                                "Valor de t tabelado",
                                "Erro de amostragem absoluto",
                                "Erro de amostragem relativo",
                                "Erro requerido", "Nivel de significancia",
                                "Coeficiente de variacao", "Fator de correcao",
                                "Parcelas amostradas", "Intensidade amostral",
                                "IC inferior por parcela",
                                "IC superior por parcela",
                                "IC inferior por hectare",
                                "IC superior por hectare",
                                "IC inferior para area total",
                                "IC superior para area total"),
                   Estimativas=c(y, s2y,sy, Y, invt, eabs, erel, E*100,p*100,CV,f,sum(ss),n,ICparmin,
                                 ICparmax,IChecmin,IChecmax,ICtotmin,ICtotmax),
                   Unidade=c("m3/estrato", "m3/estrato","m3/estrato","m3/area total",
                             "","m3/estrato","%", "%", "%","%", pop,"Parcelas","Parcelas","m3/parcela","m3/parcela",
                             "m3/hectare","m3/hectare","m3/area total","m3/area total"))
}else{
  df <- data.table(Parameters=c("Stratified mean", "Stratified mean variance",
                                "Stratified mean standard error", "Total population volume",
                                "Tabulated t value",
                                "Absolute sampling error",
                                "Relative sampling error",
                                "Required error", "Significance level",
                                "Coefficient of variation", "Correction factor",
                                "Sampled plots", "Sampling intensity",
                                "Lower CI per plot",
                                "Upper CI per plot",
                                "Lower CI per hectare",
                                "Upper CI per hectare",
                                "Lower CI for total area",
                                "Upper CI for total area"),
                   Estimates=c(y, s2y,sy, Y, invt, eabs, erel, E*100,p*100,CV,f,sum(ss),n,ICparmin,
                                 ICparmax,IChecmin,IChecmax,ICtotmin,ICtotmax),
                   Unit=c("m3/stratum", "m3/stratum","m3/stratum","m3/total area",
                             "","m3/stratum","%", "%", "%","%", pop,"Plots","Plots","m3/plot","m3/plot",
                             "m3/hectare","m3/hectare","m3/total area","m3/total area"))

}


  df[,2]<-format(round(df[,2],4),nsmall=4)

  par <- flextable(df)
  par <- align(par, align = "center")
  par <- align_text_col(par, align = "center")
  par<-autofit(par)


  #Para mostrar no console

  if(sum(ss)>=n){
    if(pt==T){
    cat("\n------------------------------------------------------------------------------------\n")
    cat("A intensidade amostral satisfaz o erro requerido de", E*100,"%, para um nivel de significancia de",p*100,"%.")
    cat("\n Portanto, nao e necessario amostrar mais parcelas.\n")
    cat("------------------------------------------------------------------------------------")
    }else{
      cat("\n------------------------------------------------------------------------------------\n")
      cat("The sampling intensity satisfies the required error of", E*100,"%, to a significance level of",p*100,"%.")
      cat("\nTherefore, it is not necessary to sample more plots.\n")
      cat("------------------------------------------------------------------------------------")

}
  }

  if(sum(ss)<n){
    if(pt==T){
    cat("\n------------------------------------------------------------------------------------\n")
    cat("A intensidade amostral nao satisfaz o erro requerido de", E*100,"%, para um nivel de significancia de",p*100,"%.")
    cat("\n Portanto, e necessario amostrar mais",ceiling(n-sum(ss)),"parcelas.\n")
    cat("------------------------------------------------------------------------------------")
    }else{
      cat("\n------------------------------------------------------------------------------------\n")
      cat("The sample intensity does not satisfy the required error of", E*100,"%, to a significance level of",p*100,"%.")
      cat("\nTherefore, it is necessary to sample",ceiling(n-sum(ss)),"more plots.\n")
      cat("------------------------------------------------------------------------------------")

    }
  }


  #Anova
  x[,1]<-as.factor(x[,1])

  modelo.anova <- lm(x[,ncol(x)] ~ x[,1], data= x)
  anova<-anova(modelo.anova)
  ftab<-qf (0.95, df1 = anova$Df[1], df2 = anova$Df[2])

  if(anova$`F value`[1]>ftab){

    if(pt==T){

    cat("------------------------------------------------------------------------------------")
    cat("\nHa diferenca significativa entre as medias dos estratos.\n")
    cat("------------------------------------------------------------------------------------")
    }else{
      cat("------------------------------------------------------------------------------------")
      cat("\nThere is significant difference between strata means.\n")
      cat("------------------------------------------------------------------------------------")

    }
  }

  if(anova$`F value`[1]<ftab){

    if(pt==T){
    cat("------------------------------------------------------------------------------------")
    cat("\nNao ha diferenca significativa entre as medias dos estratos.\n")
    cat("------------------------------------------------------------------------------------")
    }else{
      cat("------------------------------------------------------------------------------------")
      cat("\nThere is not significant difference between strata means.\n")
      cat("------------------------------------------------------------------------------------")

    }
  }


#Analise fitossociologica

#Ajeitar dados para grafico e tabela

  x[,1] <- as.numeric(x[,1])
  
  Estrato<-x[,1]
  Especie<-x[,4]
  parcela<-x[,2]
  d<-x[,5]
  
  
  
  fito <- data.table(Estrato=Estrato,Especie=Especie, parcela=parcela, d=d)
  
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
    sumdoa<-c(sum(subset(dtt[,9], dtt[,5]==i)))
  }
  sumdoa<-as.data.frame(sumdoa)
  
  for(i in 1:max(dtt[,5])){
    sumdoa[i]<-c(sum(subset(dtt[,9], dtt[,5]==i)))
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
  
  for(i in 1:max(fito$Estrato)){
    maxn<-c(length(unique(subset(fito$parcela, fito$Estrato==i))))
  }
  maxn<-as.data.frame(maxn)
  
  for(i in 1:max(fito$Estrato)){
    maxn[i]<-c(length(unique(subset(fito$parcela, fito$Estrato==i))))
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
    sumfa<-c(sum(subset(dtt[,13], dtt[,5]==i)))
  }
  sumfa<-as.data.frame(sumfa)
  
  for(i in 1:max(dtt[,5])){
    sumfa[i]<-c(sum(subset(dtt[,13], dtt[,5]==i)))
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
  
  dtt_g<- dtt2[1:spivi,] #seleciona os maiores IVI com spivi
  
  dtt2<-dtt2[order(dtt2$Estrato),] #ordenar por Estrato
  
  #nomear as colunas em ingles
  if(pt==F){
    colnames(dtt2)[1]<-"Stratum"
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
  
  
  
  #PARA UMA ESPECIE APENAS:
  
if(un==T){
  
  if(pt==T){
    colnames(x)[1]<-"Estrato"
    colnames(x)[2]<-"Parcela"
    colnames(x)[3]<-"Individuo"
    colnames(x)[4]<-"Especie"
    colnames(x)[5]<-"Altura (m)"
    colnames(x)[6]<-"Diametro (cm)"
    colnames(x)[7]<-"Volume (m3)"
  }else{
    colnames(x)[1]<-"Stratum"
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
  x3<-italic(x3,j=4)
  
  
  if(pt==T){
    doc <- read_docx() %>%
      body_add_par("Tabela 1. Parametros da amostragem casual estratificada.", style = "centered") %>%
      body_add_flextable(par) %>% #tabela de parametros volume
      body_end_section_portrait() %>%
      
      body_add_break() %>%
      body_add_gg(diam, style="centered", height=4,width=6) %>% #distribuicao diametrica
      body_add_par("Figura 1. Distribuicao diametrica por estrato.", style = "centered") %>%
      body_end_section_portrait() %>%
      
      body_add_break() %>%
      body_add_par("Tabela 2. Volume lenhoso por parcela.", style = "centered") %>%
      body_add_flextable(vopa) %>% #volume/parcela
      body_end_section_landscape() %>%
      
      body_add_break() %>%
      body_add_par("Tabela 3. Volume lenhoso por estrato.", style = "centered") %>%
      body_add_flextable(vol.estrat2) %>% #volume/estrato
      body_end_section_landscape() %>%
      
      body_add_break() %>%
      body_add_par("Tabela 4. Alocacao das parcelas por estrato e tabela auxiliar para calculo dos parametros de amostragem.", style = "centered") %>%
      body_add_flextable(tabaux) %>% #tabela auxiliar
      body_end_section_landscape() %>%
      
      body_add_break() %>%
      body_add_par("Tabela 5. Volume lenhoso individual.", style = "centered") %>%
      body_add_flextable(x3) %>%
      body_end_section_landscape()
    
  }else{
    
    doc <- read_docx() %>%
      body_add_par("Table 1. Stratified casual sampling parameters.", style = "centered") %>%
      body_add_flextable(par) %>% #tabela de parametros volume
      body_end_section_portrait() %>%
      
      body_add_break() %>%
      body_add_gg(diam, style="centered", height=4,width=6) %>% #distribuicao diametrica
      body_add_par("Figura 1. Diameter distribution by stratum.", style = "centered") %>%
      body_end_section_portrait() %>%
      
      body_add_break() %>%
      body_add_par("Table 2. Woody volume by plot.", style = "centered") %>%
      body_add_flextable(vopa) %>% #volume/parcela
      body_end_section_landscape() %>%
      
      body_add_break() %>%
      body_add_par("Table 3. Woody volume by stratum.", style = "centered") %>%
      body_add_flextable(vol.estrat2) %>% #volume/estrato
      body_end_section_landscape() %>%
      
      body_add_break() %>%
      body_add_par("Table 4. Allocation of plots by stratum and auxiliary table for calculation of sampling parameters.", style = "centered") %>%
      body_add_flextable(tabaux) %>% #tabela auxiliar
      body_end_section_landscape() %>%
     
      body_add_break() %>%
      body_add_par("Table 5. Individual woody volume.", style = "centered") %>%
      body_add_flextable(x3) %>%
      body_end_section_landscape()
    
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
  
  if(pt==T){
    return(list(`vol individual`=x3,
                `distribuicao diam`=diam,
                `tabela aux`=tabaux,
                `volume por estrato`=vol.estrat2,
                `volume por parcela`=vopa,
                `parametros vol`=par))
  }else{
    
    return(list(`individual vol`=x3,
                `diam distribuction`=diam,
                `aux table`=tabaux,
                `volume by stratum`=vol.estrat2,
                `volume by plot`=vopa,
                `vol parameters`=par))     
  }
  
  
}else{
  
  #Para mais de uma especie:
  
  
  
  #Grafico fito
  
  
  if(pt==T){
    
    data <- dtt_g[c(1, 2, 7, 9, 11)] %>%
      tidyr::gather(Parametros, b, -Estrato, -Especie) %>%
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
      xlab("Especies\n") + ylab("\nIndice de Valor de Importancia (%)") +
      labs(fill = "Parametros") +
      theme(axis.text.y = element_text(face = "italic",size=8), legend.title=element_blank(),legend.justification = "center" ,legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12),
            legend.position="bottom",legend.direction = "horizontal")+
      facet_wrap( ~ data[,1])
    
    p2 <- gg2 + theme(legend.position = "none")
    le1 <- cowplot::get_legend(gg2)
    gg3<-cowplot::plot_grid(p2, le1,nrow = 2,rel_heights = c(1, 0.2))
    
    
  }else{
    data <- dtt_g[c(1, 2, 7, 9, 11)] %>%
      gather(Parameters, b, -Estrato, -Especie) %>%
      mutate(Parameters = case_when(
        grepl('^DoR', Parameters) ~ 'Relative Dominance (%)',
        grepl('^DR', Parameters) ~ 'Relative Density (%)',
        grepl('^FR', Parameters) ~ 'Relative Frequency (%)',
        TRUE ~ NA_character_
      ))
    
    gg2<-ggplot(data, aes(reorder(Especie,b), b, fill = Parameters)) +
      geom_col(alpha = 0.8) +
      scale_fill_brewer(palette = "Dark2") +
      theme_bw(16)  +
      coord_flip() +
      xlab("Species\n") + ylab("\nImportance Value Index (%)") +
      labs(fill = "Parameters") +
      theme(axis.text.y = element_text(face = "italic",size=8), legend.title=element_blank(),legend.justification = "center" ,legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12),
            legend.position="bottom",legend.direction = "horizontal")+
      facet_wrap( ~ data[,1])
    
    
    p2 <- gg2 + theme(legend.position = "none")
    le1 <- cowplot::get_legend(gg2)
    gg3<-cowplot::plot_grid(p2, le1,nrow = 2,rel_heights = c(1, 0.2))
  }
    
#CURVA ESPECIES-AREA
    
    freqsp<-as.data.frame.matrix(table(x$Plot, x$Specie))
    rep<-data.frame(rep)
    rep$site.totals<-apply(freqsp,1,sum)
    accum <- accumresult(freqsp, y=rep, scale='site.totals', method='exact', conditioned=TRUE)
    

    #grafico no docx
  

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


    if(pt==T){
    colnames(x)[1]<-"Estrato"
    colnames(x)[2]<-"Parcela"
    colnames(x)[3]<-"Individuo"
    colnames(x)[4]<-"Especie"
    colnames(x)[5]<-"Altura (m)"
    colnames(x)[6]<-"Diametro (cm)"
    colnames(x)[7]<-"Volume (m3)"
    }else{
      colnames(x)[1]<-"Stratum"
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
    x3<-italic(x3,j=4)
    

    #criar docx sem argumento prot


    if(pt==T){
      doc <- read_docx() %>%
      body_add_par("Tabela 1. Parametros da amostragem casual estratificada.", style = "centered") %>%
      body_add_flextable(par) %>% #tabela de parametros volume
      body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_gg(diam, style="centered", height=3.4,width=6) %>% #distribuicao diametrica
        body_add_par("Figura 1. Distribuicao diametrica por estrato.", style = "centered") %>%
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_par("Tabela 2. Volume lenhoso por parcela.", style = "centered") %>%
        body_add_flextable(vopa) %>% #volume/parcela
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 3. Volume lenhoso por estrato.", style = "centered") %>%
        body_add_flextable(vol.estrat2) %>% #volume/estrato
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 4. Alocacao das parcelas por estrato e tabela auxiliar para calculo dos parametros de amostragem.", style = "centered") %>%
        body_add_flextable(tabaux) %>% #tabela auxiliar
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 5. Volume lenhoso por especie.", style = "centered") %>%
        body_add_flextable(vtt) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 6. Parametros fitossociologicos por estrato, em que: n = quantidade de individuos amostrados; G = area basal; UA = quantidade de unidades amostrais; DA (n/ha) = Densidade absoluta; DR (%) = Densidade relativa; DoA (G/ha) = Dominancia Absoluta; DoR (%) = Dominancia Relativa; FA (%) = Frequencia absoluta; FR (%) = Frequencia Relativa; IVI (%) = Indice de Valor de Importancia.", style = "centered") %>%
        body_add_flextable(fitot) %>% #parametros fito
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_gg(gg3,style="centered", height=3.5,width=6)%>%#grafico fito
        body_add_par("Figura 2. Indice de Valor de Importancia por especie e por estrato (soma de densidade relativa, dominancia relativa e frequencia relativa).", style = "centered") %>%
        body_end_section_landscape() %>%
        
        body_add_break() %>%
        body_add_vg(code = accumcomp(freqsp, y=rep, factor='rep', method='random', legend=F, conditioned=TRUE,
                                        xlab="Parcelas", ylab="Riqueza", ci=ci) ) %>%
        body_add_par("Figura 3. Curva de acumulacao de especies, com parcelas adicionadas em ordem aleatoria e 100 permutacoes, para cada estrato.", style = "centered") %>%
        body_end_section_landscape() %>%
        

        body_add_break() %>%
        body_add_par("Tabela 7. Volume lenhoso individual.", style = "centered") %>%
        body_add_flextable(x3) %>%
        body_end_section_landscape()

    }else{

      doc <- read_docx() %>%

        body_add_par("Table 1. Stratified casual sampling parameters.", style = "centered") %>%
        body_add_flextable(par) %>% #tabela de parametros volume
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_gg(diam, style="centered", height=4,width=6) %>% #distribuicao diametrica
        body_add_par("Figura 1. Diameter distribution by stratum.", style = "centered") %>%
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_par("Table 2. Woody volume by plot.", style = "centered") %>%
        body_add_flextable(vopa) %>% #volume/parcela
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 3. Woody volume by stratum.", style = "centered") %>%
        body_add_flextable(vol.estrat2) %>% #volume/estrato
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 4. Allocation of plots by stratum and auxiliary table for calculation of sampling parameters.", style = "centered") %>%
        body_add_flextable(tabaux) %>% #tabela auxiliar
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 5. Woody volume by specie.", style = "centered") %>%
        body_add_flextable(vtt) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 6. Phytosociological parameters by stratum, where: n = number of sampled individuals; G = basal area; SU = number of sample units; AD (n/ha) = absolute density; RD (%) = relative density; ADo (G/ha) = absolute dominance; RDo (%) = relative dominance; AF (%) = absolute frequency; RF (%) = relative frequency; IVI (%) = Importance Value Index.", style = "centered") %>%
        body_add_flextable(fitot) %>% #parametros fito
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_gg(gg3,style="centered", height=6,width=6)%>%#grafico fito
        body_add_par("Figure 2. Importance Value Index by specie and by stratum (sum of relative density, relative dominance and relative frequency).", style = "centered")%>%
        body_end_section_landscape() %>%
        
        body_add_break() %>%
        body_add_vg(code = accumcomp(freqsp, y=rep, factor='rep', method='random', legend=F, conditioned=TRUE,
                                          xlab="Plot", ylab="Richness", ci=ci) ) %>%
        body_add_par("Figure 3. Species accumulation curve, with plots added in random order and 100 permutations, for each stratum.", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 7. Individual woody volume.", style = "centered") %>%
        body_add_flextable(x3) %>%
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

      ph<-data.table(Specie=c(prot,"Total protected","Total unprotected"), `Sampled volume (m3)`=c(pp,sum(pp),sum(x[,ncol(x)])-sum(pp)), `Volume/hectare (m3)`=c(pp/a, sum(pp)/a, (sum(x[,ncol(x)])-sum(pp))/a), `Volume/total area (m3)`=c(pp*A/a, sum(pp)*A/a, ((sum(x[,ncol(x)]))-(sum(pp)))*A/a))

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

      ph<-data.table(Specie=c(prot,"Total protected","Total unprotected"), `Sampled volume (m3)`=c(pp,sum(pp),sum(x[,ncol(x)])-sum(pp)), `Volume/hectare (m3)`=c(pp/a, sum(pp)/a, (sum(x[,ncol(x)])-sum(pp))/a), `Volume/total area (m3)`=c(pp*A/a, sum(pp)*A/a, (sum(x[,ncol(x)]))-(sum(pp)*A/a)))

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
        body_add_par("Tabela 1. Parametros da amostragem casual estratificada.", style = "centered") %>%
        body_add_flextable(par) %>% #tabela de parametros volume
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_gg(diam, style="centered", height=4,width=6) %>% #distribuicao diametrica
        body_add_par("Figura 1. Distribuicao diametrica por estrato.", style = "centered") %>%
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_par("Tabela 2. Volume lenhoso por parcela.", style = "centered") %>%
        body_add_flextable(vopa) %>% #volume/parcela
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 3. Volume lenhoso por estrato.", style = "centered") %>%
        body_add_flextable(vol.estrat2) %>% #volume/estrato
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 4. Alocacao das parcelas por estrato e tabela auxiliar para calculo dos parametros de amostragem.", style = "centered") %>%
        body_add_flextable(tabaux) %>% #tabela auxiliar
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 5. Volume lenhoso por especie.", style = "centered") %>%
        body_add_flextable(vtt) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 6. Volume lenhoso por especie protegida.", style = "centered") %>%
        body_add_flextable(phi) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 7. Parametros fitossociologicos por estrato, em que: n = quantidade de individuos amostrados; G = area basal; UA = quantidade de unidades amostrais; DA (n/ha) = Densidade absoluta; DR (%) = Densidade relativa; DoA (G/ha) = Dominancia Absoluta; DoR (%) = Dominancia Relativa; FA (%) = Frequencia absoluta; FR (%) = Frequencia Relativa; IVI (%) = Indice de Valor de Importancia.", style = "centered") %>%
        body_add_flextable(fitot) %>% #parametros fito
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_gg(gg3,style="centered", height=6,width=6)%>%#grafico fito
        body_add_par("Figura 2. Indice de Valor de Importancia por especie e por estrato (soma de densidade relativa, dominancia relativa e frequencia relativa).", style = "centered") %>%
        body_end_section_landscape() %>%
        
        body_add_break() %>%
        body_add_vg(code = accumcomp(freqsp, y=rep, factor='rep', method='random', legend=F, conditioned=TRUE,
                                          xlab="Parcelas", ylab="Riqueza", ci=ci) ) %>%
        body_add_par("Figura 3. Curva de acumulacao de especies, com parcelas adicionadas em ordem aleatoria e 100 permutacoes, para cada estrato.", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Tabela 8. Volume lenhoso individual.", style = "centered") %>%
        body_add_flextable(x3) %>%
        body_end_section_landscape()

    }else{

      doc <- read_docx() %>%
        body_add_par("Table 1. Stratified casual sampling parameters.", style = "centered") %>%
        body_add_flextable(par) %>% #tabela de parametros volume
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_gg(diam, style="centered", height=4,width=6) %>% #distribuicao diametrica
        body_add_par("Figura 1. Diameter distribution by stratum.", style = "centered") %>%
        body_end_section_portrait() %>%

        body_add_break() %>%
        body_add_par("Table 2. Woody volume by plot.", style = "centered") %>%
        body_add_flextable(vopa) %>% #volume/parcela
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 3. Woody volume by stratum.", style = "centered") %>%
        body_add_flextable(vol.estrat2) %>% #volume/estrato
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 4. Allocation of plots by stratum and auxiliary table for calculation of sampling parameters.", style = "centered") %>%
        body_add_flextable(tabaux) %>% #tabela auxiliar
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 5. Woody volume by specie.", style = "centered") %>%
        body_add_flextable(vtt) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 6. Woody volume by protected specie.", style = "centered") %>%
        body_add_flextable(phi) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 7. Phytosociological parameters by stratum, where: n = number of sampled individuals; G = basal area; SU = number of sample units; AD (n/ha) = absolute density; RD (%) = relative density; ADo (G/ha) = absolute dominance; RDo (%) = relative dominance; AF (%) = absolute frequency; RF (%) = relative frequency; IVI (%) = Importance Value Index.", style = "centered") %>%
        body_add_flextable(fitot) %>% #parametros fito
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_gg(gg3,style="centered", height=6,width=6)%>%#grafico fito
        body_add_par("Figure 2. Importance Value Index by specie and by stratum (sum of relative density, relative dominance and relative frequency).", style = "centered")%>%
        body_end_section_landscape() %>%
        
        body_add_break() %>%
        body_add_vg(code = accumcomp(freqsp, y=rep, factor='rep', method='random', legend=F, conditioned=TRUE,
                                          xlab="Plot", ylab="Richness", ci=ci)) %>%
        body_add_par("Figure 3. Species accumulation curve, with plots added in random order and 100 permutations, for each stratum.", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break() %>%
        body_add_par("Table 8. Individual woody volume.", style = "centered") %>%
        body_add_flextable(x3) %>%
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
    
    if(pt==T){
    return(list(`vol individual`=x3,
                `grafico ivi`=gg3,
                `parametros fito`=fitot,
                `volume por sp`=vtt,
                `distribuicao diam`=diam,
                `tabela aux`=tabaux,
                `volume por estrato`=vol.estrat2,
                `volume por parcela`=vopa,
                `parametros vol`=par))
    }else{
      return(list(`individual vol`=x3,
                  `ivi plot`=gg3,
                  `phyto parameters`=fitot,
                  `volume by sp`=vtt,
                  `diam distribuction`=diam,
                  `aux table`=tabaux,
                  `volume by stratum`=vol.estrat2,
                  `volume by plot`=vopa,
                  `vol parameters`=par))
    }
      
      
  }else{
    
    if(pt==T){
    return(list(`vol individual`=x3,
                `grafico ivi`=gg3,
                `parametros fito`=fitot,
                `spp prot`=phi,
                `volume por sp`=vtt,
                `distribuicao diam`=diam,
                `tabela aux`=tabaux,
                `volume por estrato`=vol.estrat2,
                `volume por parcela`=vopa,
                `parametros vol`=par))
      }else{
                  
        return(list(`individual vol`=x3,
                    `ivi plot`=gg3,
                    `phyto parameters`=fitot,
                    `prot spp`=phi,
                    `volume by sp`=vtt,
                    `diam distribuction`=diam,
                    `aux table`=tabaux,
                    `volume by stratum`=vol.estrat2,
                    `volume by plot`=vopa,
                    `vol parameters`=par))     
  }}
    
}
}
  
