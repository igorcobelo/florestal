ace<-function(x,a,aj,E=0.1,p=0.05,ampl=2,prot=NULL,prop=F,rn=F,spivi=15,un=F,pt=T,save=T){


  #para que o arquivo docx seja nomeado com o mesmo nome do input
  nm <-deparse(substitute(x))

  #para que seja feito o ggplot
  x<-as.data.frame(x)

#Grafico de distribuicao diametrica por estrato

    breaks<-list()
    d<-list()
    for(i in x[,1]){

      breaks[i] <- list(seq(from=min(x[x[,1]==i,6]),to=max(x[x[,1]==i,6]),by=ampl))

      for(j in breaks){
        d[i] <- list(cut(x[x[,1]==i,6],breaks=j,right=F,include.lowest = T))
        table <- lapply(d, FUN = table)
        data <- as.data.frame(do.call(cbind, table))

      }

    }

    colnames(data) <- 1:ncol(data)
    data <- mutate(data, d=rownames(data))

    data <- data %>% gather(Estrato, Freq,-d)

    data$d <- stri_replace_all_fixed(data$d,",", "<")
    data$d <- stri_replace_all_fixed(data$d,")", "")
    data$d <- stri_replace_all_fixed(data$d,"[", "")
    data$d <- stri_replace_all_fixed(data$d,"]", "")

    data$d <- factor(data$d,levels=unique(data$d))


    data$Estrato <- as.numeric(data$Estrato)


    sum<- data %>% group_by(Estrato) %>%
      summarise(sum=sum(Freq))
    data <- left_join(data,sum,by="Estrato",keep=F)

    if(pt==T){
    diam <- ggplot(data, aes(x=d, y=Freq)) +
      geom_bar(stat = "identity", width=0.5, fill="black",alpha=0.9)+
      theme_bw()+
      theme(strip.background=element_rect(fill="white"))+
      xlab("\nClasse Diametrica (cm)")+
      ylab("Quantidade de Individuos\n")+
      geom_text(aes(label = paste0(round((Freq*100)/sum,2),"%"),
                    y = Freq),
                position = position_dodge(width = 1),
                vjust = -0.5,color="black",size=2)+
      facet_wrap(~Estrato)


  }else{
    diam <- ggplot(data, aes(x=d, y=Freq)) +
      geom_bar(stat = "identity", width=0.5, fill="black",alpha=0.9)+
      theme_bw()+
      theme(strip.background=element_rect(fill="white"))+
      xlab("\nDiameter Class (cm)")+
      ylab("Number of Individuals\n")+
      geom_text(aes(label = paste0(round((Freq*100)/sum,2),"%"),
                    y = Freq),
                position = position_dodge(width = 1),
                vjust = -0.5,color="black",size=2)+
      facet_wrap(~Estrato)
}



  #volume/parcela

  for(i in 1:max(x[,2])){
    vv<-c(sum(subset(x[,ncol(x)],x[,2]==i),na.rm = T))
  }


  for(i in 1:max(x[,2])){
    vv[i]<-c(sum(subset(x[,ncol(x)],x[,2]==i),na.rm = T))
  }

  #Encontrar quantas parcelas ha em cada estrato
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
  vp<-data.table(rep, 1:max(x[,2]), vv)
  colnames(vp)[1]<-"Estrato"
  colnames(vp)[2]<-"Parcela"
  colnames(vp)[3]<-"Volume/Parcela (m3)"

  }else{
    vp<-data.table(rep, 1:max(x[,2]), vv)
    colnames(vp)[1]<-"Stratum"
    colnames(vp)[2]<-"Plot"
    colnames(vp)[3]<-"Volume/Plot (m3)"

  }

  vp2<-data.table(vp)

  vp2[,3]<-as.numeric(unlist(vp2[,3]))
  vp2[,3]<-format(round(vp2[,3],4),nsmall=4)


  vopa <- flextable(vp2)
  vopa <- align(vopa, align = "center")
  vopa <- align_text_col(vopa, align = "center")
  vopa<-autofit(vopa)

  #Variancia por estrato

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


  #soma de volumes por estrato
  tt<-as.numeric()
  for(i in ph2[1:nrow(ph2)-1,1]){
    tt[i]<-c(sum(subset(ph2[,3], ph2[,1]==i)))
  }

  # Dados iniciais
  A<-sum(aj) #area total
  N<-A/a #parcelas que cabem na area total
  estrat<-aj/a #parcelas que cabem em cada estrato
  P<-estrat/N #P
  yi<-tt/ss #media/parcela pra cada estrato
  y<-sum(P*yi) #media estratificada (m3/parcela)
  s<-sqrt(var) #variancia geral
  erroabsreq<-y*E #erro requerido absoluto




  #t tabelado
  invt<-qt(1-p/2, df=sum(ss)-1)

  #Fator de corre??o (finita ou infinita)
  f<-1-sum(ss)/N

  if(prop==FALSE){
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

      if(rn==TRUE){
        invt<-qt(1-p/2, df=n-1)
        n<-(invt^2*(sum(P*s)^2))/(erroabsreq^2)
      }
    }

    #aloca??o ?tima dos estratos
    nj<-(((P*s)/sum(P*s))*n)
  }

  if(prop==TRUE){
    #P/ ALOCA??O PROPORCIONAL

    if(f<0.98){
      #intensidade amostral FINITA
      n<-(invt^2*sum(P*var))/(erroabsreq^2 + (invt^2*sum(P*var)/N))

      if(rn==TRUE){
      invt<-qt(1-p/2, df=n-1)
      n<-(invt^2*sum(P*var))/(erroabsreq^2 + (invt^2*sum(P*var)/N))
      }
    }

    if(f>=0.98){
      #intensidade amostral INFINITA
      n<-(invt^2*sum(P*var))/(erroabsreq^2)

      if(rn==TRUE){
      invt<-qt(1-p/2, df=n-1)
      n<-(invt^2*sum(P*var))/(erroabsreq^2)
      }
    }

    #aloca??o proporcional dos estratos
    nj<-as.data.frame(P*n)

  }

#Criacao de "pop"
if(f<0.98){
  if(pt==TRUE){
  pop<-"(Pop. finita)"
  }else{
    pop<-"(Finite pop.)"
  }
}

if(f>=0.98){
  if(pt==TRUE){
    pop<-"(Pop. infinita)"
  }else{
    pop<-"(Infinite pop.)"
  }
}



  #tabela auxiliar

  jj<-as.data.frame(nj) #nj esta mais abaixo
  jjp<-t(data.frame(jj))

  tabaux<-data.table(c(1:max(x[,1]),"Total"), c(ss,sum(ss)), c(estrat,sum(estrat)), c(P,sum(P)), c(var,""),c(s,""),c(P*var,sum(P*var)),c(P*s,sum(P*s)), c(jjp, sum(jjp)))

  if(pt==TRUE){
    colnames(tabaux)[1]<-"Estrato"
    if(prop==TRUE){
      colnames(tabaux)[9]<-"Alocacao proporcional"
    }else{
      colnames(tabaux)[9]<-"Alocacao otima"
    }

  }else{
    colnames(tabaux)[1]<-"Stratum"
    if(prop==TRUE){
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

  tabaux2 <- as.data.frame(tabaux)

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
    s2y<-(sum(P*s)^2/sum(ss))
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


#Parametros estatisticos

if(pt==TRUE){
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
                   Unidade=c("m3/parcela", "m3/parcela","m3/parcela","m3/area total",
                             "","m3/parcela","%", "%", "%","%", pop,"Parcelas","Parcelas","m3/parcela","m3/parcela",
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
                   Unit=c("m3/plot", "m3/plot","m3/plot","m3/total area",
                             "","m3/plot","%", "%", "%","%", pop,"Plots","Plots","m3/plot","m3/plot",
                             "m3/hectare","m3/hectare","m3/total area","m3/total area"))

}


  df[,2]<-format(round(df[,2],4),nsmall=4)

  df <- as.data.frame(df)

  par <- flextable(df)
  par <- align(par, align = "center")
  par <- align_text_col(par, align = "center")
  par<-autofit(par)

  #Para mostrar no console

  if(sum(ss)>=n){
    if(pt==TRUE){
      message("\n--------------------------------------------------------------\n")
      message("A intensidade amostral satisfaz o erro requerido de ", E*100,"%, para um nivel de significancia de ",p*100,"%.")
    message("\n Portanto, nao e necessario amostrar mais parcelas.\n")
    message("--------------------------------------------------------------")
    }else{
      message("\n--------------------------------------------------------------\n")
      message("The sampling intensity satisfies the required error of ", E*100,"%, to a significance level of ",p*100,"%.")
      message("\nTherefore, it is not necessary to sample more plots.\n")
      message("--------------------------------------------------------------")

}
  }

  if(sum(ss)<n){
    if(pt==TRUE){
      message("\n--------------------------------------------------------------\n")
      message("A intensidade amostral nao satisfaz o erro requerido de ", E*100,"%, para um nivel de significancia de ",p*100,"%.")
    message("\n Portanto, e necessario amostrar mais ",ceiling(n-sum(ss))," parcelas.\n")
    message("--------------------------------------------------------------")
    }else{
      message("\n--------------------------------------------------------------\n")
      message("The sample intensity does not satisfy the required error of ", E*100,"%, to a significance level of ",p*100,"%.")
      message("\nTherefore, it is necessary to sample ",ceiling(n-sum(ss))," more plots.\n")
      message("--------------------------------------------------------------")

    }
  }


  #Anova
  x[,1]<-as.factor(x[,1])

  modelo.anova <- lm(x[,ncol(x)] ~ x[,1], data= x)
  anova<-anova(modelo.anova)
  ftab<-qf (0.95, df1 = anova$Df[1], df2 = anova$Df[2])

  res.aov <- aov(x[,ncol(x)] ~ x[,1], data= x)
  tukey<-TukeyHSD(res.aov)


  if(anova$`F value`[1]>ftab){

    if(pt==TRUE){

    message("\nHa diferenca significativa entre as medias dos estratos.\n")

    message("Teste de Tukey para diferenca significativa entre estratos, a 95% de confianca:")

    message(paste0(capture.output(tukey$`x[, 1]`), collapse="\n"))
    }else{
      message("\nThere is significant difference between strata means.\n")
      message("Tukey's test for significant differences between strata, with 95% confidence:")

      message(paste0(capture.output(tukey$`x[, 1]`), collapse="\n"))
    }
  }

  if(anova$`F value`[1]<ftab){

    if(pt==TRUE){
    message("\nNao ha diferenca significativa entre as medias dos estratos.\n")
    message("Teste de Tukey para diferenca significativa entre estratos, a 95% de confianca:")
    message(paste0(capture.output(tukey$`x[, 1]`), collapse="\n"))
    }else{
      message("\nThere is not significant difference between strata means.\n")
      print ("Tukey's test for significant differences between strata, with 95% confidence:")
      message(paste0(capture.output(tukey$`x[, 1]`), collapse="\n"))
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

  dtt0<-data.table(qt2,g2,sp2)
  dtt0$especie<-rep(rownames(qt), ncol(qt))
  dtt0$estrato<-rep(1:ncol(qt),each=nrow(qt))

  dtt<-dtt0[!(dtt0$n==0),]

  colnames(dtt)[1]<-"n"
  colnames(dtt)[2]<-"G (m2)"
  colnames(dtt)[3]<-"UA"
  colnames(dtt)[4]<-"Especie"
  colnames(dtt)[5]<-"Estrato"


  #VOLUME/SP ESTRATIFICADO EXTRAPOLANDO

  fitocomvol<-fito
  fitocomvol$Volume<-x[,7]

  for(i in fitocomvol[,2]){
    for(j in 1:max(fitocomvol[,1])){
      vvvv<-c(sum(subset(fitocomvol[,6], fitocomvol[,2]==i & fitocomvol[,1]==j)))
    }
  }
  vvvv<-as.data.frame(vvvv)

  for(i in fitocomvol[,2]){
    for(j in 1:max(fitocomvol[,1])){
      vvvv[i,j]<-c(sum(subset(fitocomvol[,6], fitocomvol[,2]==i & fitocomvol[,1]==j)))
    }
  }
  vvvv<-as.data.frame(vvvv)
  vvvv<-vvvv[-1,]
  vvvv2<-data.frame(vvvv = unlist(vvvv,use.names = F))

  volex<-data.table(vvvv2,dtt0)

  volex<-volex[!(volex$n==0),]

  yiv<-volex$vvvv/max(volex$UA)


  ll<-as.numeric()
  for(i in 1:max(volex$estrato)){
  ll[i]<-length(volex$estrato[volex$estrato==i])
  }

  volex$Pzao<-rep(P, ll)

  summ<-sum(yiv*volex$Pzao)*N #representa o volume total da pop

  vpestrat <- yiv*volex$Pzao #VOLUME/SP/PARCELA ESTRATIFICADO


  sparta <- data.table(Especie=volex$especie,`Volume/Parcela (m3)`=vpestrat)

  sparta2<-as.numeric()
  for(i in sparta$Especie){
    sparta2[i]<-sum(subset(sparta$`Volume/Parcela (m3)`, sparta$Especie==i))
  }
  sparta2<-as.data.frame(sparta2[order(-sparta2)])

  vpestrat_ha <- sparta2$`sparta2[order(-sparta2)]`/a #VOLUME/SP/HA ESTRATIFFICADO

  vpestrat_tot <- vpestrat_ha*A #VOLUME/SP TOTAL ESTRATIFICADO

  if(pt==TRUE){
  vesp <- data.table(Especie=c(rownames(sparta2), "Total"), `Volume/Parcela (m3)`=c(sparta2$`sparta2[order(-sparta2)]`, sum(sparta2$`sparta2[order(-sparta2)]`)), `Volume/ha (m3)`=c(vpestrat_ha,sum(vpestrat_ha)), `Volume/Area Total (m3)`=c(vpestrat_tot, sum(vpestrat_tot)))

  vesp$`Volume/Parcela (m3)`<-as.numeric(vesp$`Volume/Parcela (m3)`)
  vesp$`Volume/ha (m3)`<-as.numeric(vesp$`Volume/ha (m3)`)
  vesp$`Volume/Area Total (m3)`<-as.numeric(vesp$`Volume/Area Total (m3)`)

  vesp$`Volume/Parcela (m3)`<-format(round(vesp$`Volume/Parcela (m3)`,4),nsmall=4)
  vesp$`Volume/ha (m3)`<-format(round(vesp$`Volume/ha (m3)`,4),nsmall=4)
  vesp$`Volume/Area Total (m3)`<-format(round(vesp$`Volume/Area Total (m3)`,4),nsmall=4)
  }else{
    vesp <- data.table(Specie=c(rownames(sparta2), "Total"), `Volume/Plot (m3)`=c(sparta2$`sparta2[order(-sparta2)]`, sum(sparta2$`sparta2[order(-sparta2)]`)), `Volume/ha (m3)`=c(vpestrat_ha,sum(vpestrat_ha)), `Volume/Total Area (m3)`=c(vpestrat_tot, sum(vpestrat_tot)))

    vesp$`Volume/Plot (m3)`<-as.numeric(vesp$`Volume/Plot (m3)`)
    vesp$`Volume/ha (m3)`<-as.numeric(vesp$`Volume/ha (m3)`)
    vesp$`Volume/Total Area (m3)`<-as.numeric(vesp$`Volume/Total Area (m3)`)

    vesp$`Volume/Plot (m3)`<-format(round(vesp$`Volume/Plot (m3)`,4),nsmall=4)
    vesp$`Volume/ha (m3)`<-format(round(vesp$`Volume/ha (m3)`,4),nsmall=4)
    vesp$`Volume/Total Area (m3)`<-format(round(vesp$`Volume/Total Area (m3)`,4),nsmall=4)

  }

  vesp <- as.data.frame(vesp)

  vesp2 <- flextable(vesp)
  vesp2  <- align(vesp2 , align = "center")
  vesp2  <- align_text_col(vesp2 , align = "center")
  vesp2 <-autofit(vesp2)
  vesp2<-italic(vesp2,j=1,i=1:(nrow(vesp)-1))



  #TESTANDO NUMERO DE PARCELAS/ESTRATO

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


  result<-as.numeric()
  for(i in 1:max(dtt$Estrato)){
    result[i]<-nrow(subset(dtt, dtt$Estrato==i))
  }
  result<-as.matrix(result)
  maxn2<-as.matrix(maxn)
  par_est <- rep(maxn2,result)



  dtt$`DA (n/ha)`<-dtt$n/(par_est*a) #coluna de Densidade Absoluta (DA)

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
  dtt$`DoA (G/ha)`<-dtt$`G (m2)`/(par_est*a) #coluna Dominancia Absoluta (DoA)

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
  dtt$`IVI (%)`<- (dtt$`DR (%)`+dtt$`DoR (%)`+dtt$`FR (%)`)/3 #coluna IVI

  dtt2<-dtt[,c(5,4,1,2,3,6,8,9,11,13,15,16)] #ordenar as colunas

  dtt2<-dtt2[order(dtt2$`IVI (%)`, decreasing = T),] #ordenar por IVI

  dtt_g<- dtt2[1:spivi,] #seleciona os maiores IVI com spivi

  dtt2<-dtt2[order(dtt2$Estrato),] #ordenar por Estrato

  #nomear as colunas em ingles
  if(pt==FALSE){
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


  #NOVA TABELA (individuos)


  spind<-as.numeric()
  for(i in dtt3[,2]){

  spind[i] <- sum(subset(as.numeric(dtt3[,6]), dtt3[,2]==i))

  }

  spind<-spind[order(-spind)]
  spind<-as.data.frame(spind)



  if(pt==TRUE){
    inds<-data.table(Especie=c(rownames(spind), "Total"), `Ind./ha`= c(spind$spind, sum(spind$spind)), `Ind./Area Total`= c(spind$spind*A, sum(spind$spind*A)))
    inds<-as.data.frame(inds)

  }else{
    inds<-data.table(Specie=c(rownames(spind), "Total"), `Ind./ha`= c(spind$spind, sum(spind$spind)), `Ind./Total Area`= c(spind$spind*A, sum(spind$spind*A)))
    inds<-as.data.frame(inds)
  }

  inds2<-inds

  inds <- flextable(inds)
  inds <- autofit(inds)
  inds <- align(inds, align = "center", part="all")
  inds <- italic(inds,j=1,i=1:nrow(spind))



  #PARA UMA ESPECIE APENAS:

if(un==TRUE){

  if(pt==TRUE){
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


  if(pt==TRUE){
    doc <- read_docx() %>%
      body_add_par("Tabela 1. Parametros da amostragem casual estratificada.", style = "centered") %>%
      body_add_flextable(par) %>% #tabela de parametros volume
      body_end_section_landscape() %>%

      body_add_break(pos="on") %>%
      body_add_gg(diam, style="centered") %>% #distribuicao diametrica
      body_add_par("Figura 1. Distribuicao diametrica por estrato.", style = "centered") %>%
      body_end_section_landscape() %>%

      body_add_break(pos="on") %>%
      body_add_par("Tabela 2. Alocacao das parcelas por estrato e tabela auxiliar para calculo dos parametros de amostragem.", style = "centered") %>%
      body_add_flextable(tabaux) %>% #tabela auxiliar
      body_end_section_landscape() %>%

      body_add_break(pos="on") %>%
      body_add_par("Tabela 3. Volume lenhoso individual.", style = "centered") %>%
      body_add_flextable(x3) %>%
      body_end_section_landscape()

  }else{

    doc <- read_docx() %>%
      body_add_par("Table 1. Stratified casual sampling parameters.", style = "centered") %>%
      body_add_flextable(par) %>% #tabela de parametros volume
      body_end_section_landscape() %>%

      body_add_break(pos="on") %>%
      body_add_gg(diam, style="centered") %>% #distribuicao diametrica
      body_add_par("Figura 1. Diameter distribution by stratum.", style = "centered") %>%
      body_end_section_landscape() %>%

      body_add_break(pos="on") %>%
      body_add_par("Table 2. Allocation of plots by stratum and auxiliary table for calculation of sampling parameters.", style = "centered") %>%
      body_add_flextable(tabaux) %>% #tabela auxiliar
      body_end_section_landscape() %>%

      body_add_break(pos="on") %>%
      body_add_par("Table 3. Individual woody volume.", style = "centered") %>%
      body_add_flextable(x3) %>%
      body_end_section_landscape()

  }


  if(save==TRUE){

  if(pt==TRUE){
    fileout <- tempfile(pattern="InventarioFlorestal", fileext=".docx")
    print(doc, target = fileout)
  }else{
    fileout <- tempfile(pattern="ForestInventory", fileext=".docx")
    print(doc, target = fileout)
  }
  }

  if(pt==TRUE){
    return(list(`vol individual`=x2,
                `distribuicao diam`=diam,
                `tabela aux`=tabaux2,
                `parametros vol`=df))
  }else{

    return(list(`individual vol`=x2,
                `diam distribuction`=diam,
                `aux table`=tabaux2,
                `vol parameters`=df))
  }


}else{

  #Para mais de uma especie:



  #Grafico fito


  if(pt==TRUE){

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
      xlab("Especies\n") + ylab("\nIndice de Valor de Importancia") +
      labs(fill = "Parametros") +
      theme(axis.text.y = element_text(face = "italic",size=10), legend.title=element_blank(),legend.justification = "center" ,legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12),
            strip.background=element_rect(fill="white"),
            legend.position="bottom",legend.direction = "horizontal")+
      facet_wrap( ~ data[,1])+
      ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))


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
      xlab("Species\n") + ylab("\nImportance Value Index") +
      labs(fill = "Parameters") +
      theme(axis.text.y = element_text(face = "italic",size=10), legend.title=element_blank(),legend.justification = "center" ,legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12),
            strip.background=element_rect(fill="white"),
            legend.position="bottom",legend.direction = "horizontal")+
      facet_wrap( ~ data[,1])+
      ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))



    p2 <- gg2 + theme(legend.position = "none")
    le1 <- cowplot::get_legend(gg2)
    gg3<-cowplot::plot_grid(p2, le1,nrow = 2,rel_heights = c(1, 0.2))
  }

#CURVA ESPECIES-AREA

    freqsp<-as.data.frame.matrix(table(x[,2], x[,4]))
    rep<-data.frame(rep)

    freqsp$strat<-rep$rep


    sp2<-list()
    for(i in 1:max(freqsp$strat)){
      sp2 [i]<- list(accumresult(freqsp[freqsp$strat==i,], method = "exact",permutations=1000))
    }

    rr<-as.numeric()
    for(i in 1:length(sp2)){
      rr[i]<-list(sp2[[i]]$richness)
    }
    rr2<-data.frame(matrix(unlist(rr)))

    sts<-as.numeric()
    for(i in 1:length(sp2)){
      sts[i]<-list(sp2[[i]]$sites)
    }
    sts2<-data.frame(matrix(unlist(sts)))

    sdd<-as.numeric()
    for(i in 1:length(sp2)){
      sdd[i]<-list(sp2[[i]]$sd)
    }
    sdd2<-data.frame(matrix(unlist(sdd)))

    h<-data.frame(strat=rep$rep ,r=rr2,p=sts2, sd=sdd2)
    h$strat<-as.factor(h$strat)
    colnames(h)[2]<-"r"
    colnames(h)[3]<-"p"
    colnames(h)[4]<-"sd"

    if(pt==TRUE){
curve <- ggplot(h, aes(x=p, y=r, color=strat, fill=strat))+
      geom_line() +
      geom_ribbon(aes(ymin=r-sd, ymax=r+sd), alpha = 0.2,colour=NA)+
      theme_bw(16)+
      theme(axis.text.y = element_text(size=10),legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12), legend.title = element_text(size=12))+
      xlab("\nParcelas")+
      ylab("Riqueza\n")+
      labs(colour = "Estrato",fill="Estrato")+
      scale_x_continuous(breaks=seq(1, max(h$p), 2))


    }else{
      curve <- ggplot(h, aes(x=p, y=r, color=strat, fill=strat))+
        geom_line() +
        geom_ribbon(aes(ymin=r-sd, ymax=r+sd), alpha = 0.2,colour=NA)+
        theme_bw(16)+
        theme(axis.text.y = element_text(size=10),legend.text=element_text(size=10),
              axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
              axis.title.y=element_text(size=12), legend.title = element_text(size=12))+
        xlab("\nPlot")+
        ylab("Richness\n")+
        labs(colour = "Stratum", fill="Stratum")+
        scale_x_continuous(breaks=seq(1, max(h$p), 2))
    }





    if(pt==TRUE){
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


    if(pt==TRUE){
      doc <- read_docx() %>%
      body_add_par("Tabela 1. Parametros da amostragem casual estratificada.", style = "centered") %>%
      body_add_flextable(par) %>% #tabela de parametros volume
      body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_gg(diam, style="centered") %>% #distribuicao diametrica
        body_add_par("Figura 1. Distribuicao diametrica por estrato.", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Tabela 2. Alocacao das parcelas por estrato e tabela auxiliar para calculo dos parametros de amostragem.", style = "centered") %>%
        body_add_flextable(tabaux) %>% #tabela auxiliar
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Tabela 3. Volume lenhoso estratificado por especie.", style = "centered") %>%
        body_add_flextable(vesp2) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Tabela 4. Quantidade de individuos por especie.", style = "centered") %>%
        body_add_flextable(inds) %>% #ind/sp
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Tabela 5. Parametros fitossociologicos por estrato, em que: n = quantidade de individuos amostrados; G = area basal; UA = quantidade de unidades amostrais; DA (n/ha) = Densidade absoluta; DR (%) = Densidade relativa; DoA (G/ha) = Dominancia Absoluta; DoR (%) = Dominancia Relativa; FA (%) = Frequencia absoluta; FR (%) = Frequencia Relativa; IVI (%) = Indice de Valor de Importancia.", style = "centered") %>%
        body_add_flextable(fitot) %>% #parametros fito
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_gg(gg3,style="centered")%>%#grafico fito
        body_add_par("Figura 2. Indice de Valor de Importancia por especie e por estrato (soma de densidade relativa, dominancia relativa e frequencia relativa).", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_gg(curve,style="centered")%>%#grafico curva
        body_add_par("Figura 3. Curva de acumulacao de especies para cada estrato. Foi utilizado o metodo Bootstrap para estimar o numero total extrapolado de especies na area, com 1000 permutacoes. O sombreamento em volta da linha representa o intervalo de confianca de 95% a partir do desvio-padrao.", style = "centered") %>%
        body_end_section_landscape() %>%


        body_add_break(pos="on") %>%
        body_add_par("Tabela 6. Volume lenhoso individual.", style = "centered") %>%
        body_add_flextable(x3) %>%
        body_end_section_landscape()

    }else{

      doc <- read_docx() %>%

        body_add_par("Table 1. Stratified casual sampling parameters.", style = "centered") %>%
        body_add_flextable(par) %>% #tabela de parametros volume
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_gg(diam, style="centered") %>% #distribuicao diametrica
        body_add_par("Figure 1. Diameter distribution by stratum.", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 2. Allocation of plots by stratum and auxiliary table for calculation of sampling parameters.", style = "centered") %>%
        body_add_flextable(tabaux) %>% #tabela auxiliar
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 3. Stratified woody volume by specie.", style = "centered") %>%
        body_add_flextable(vesp2) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 4. Number of individuals by specie.", style = "centered") %>%
        body_add_flextable(inds) %>% #ind/sp
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 5. Phytosociological parameters by stratum, where: n = number of sampled individuals; G = basal area; SU = number of sample units; AD (n/ha) = absolute density; RD (%) = relative density; ADo (G/ha) = absolute dominance; RDo (%) = relative dominance; AF (%) = absolute frequency; RF (%) = relative frequency; IVI (%) = Importance Value Index.", style = "centered") %>%
        body_add_flextable(fitot) %>% #parametros fito
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_gg(gg3,style="centered")%>%#grafico fito
        body_add_par("Figure 2. Importance Value Index by specie and by stratum (sum of relative density, relative dominance and relative frequency).", style = "centered")%>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_gg(curve,style="centered")%>%#grafico curva
        body_add_par("Figure 3. Species accumulation curve for each stratum. The Bootstrap method was used to estimate the total extrapolated number of species in the area, with 1000 permutations. The shading around the line represents the 95% confidence interval from the standard deviation.", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 6. Individual woody volume.", style = "centered") %>%
        body_add_flextable(x3) %>%
        body_end_section_landscape()

    }

    if(save==TRUE){

    if(pt==TRUE){
      fileout <- tempfile(pattern="InventarioFlorestal", fileext=".docx")
      print(doc, target = fileout)
    }else{
      fileout <- tempfile(pattern="ForestInventory", fileext=".docx")
      print(doc, target = fileout)
    }
    }

if(!(is.null(prot))){
    #tabela volume com argumento prot


  pp<-as.numeric()
  if(pt==TRUE){
    for(i in prot){
      pp[i]<-subset(vesp$`Volume/Parcela (m3)`,vesp$Especie==i)
    }
  }else{
    for(i in prot){
      pp[i]<-subset(vesp$`Volume/Plot (m3)`,vesp$Specie==i)
    }
  }

    pp<-as.numeric(pp) #prot por parcela
    ppha<-pp/a #prot por ha
    pptot<-ppha*A

     if(pt==TRUE){

      vesp$`Volume/Parcela (m3)`<-as.numeric(vesp$`Volume/Parcela (m3)`)
      vesp$`Volume/ha (m3)`<-as.numeric(vesp$`Volume/ha (m3)`)
      vesp$`Volume/Area Total (m3)`<-as.numeric(vesp$`Volume/Area Total (m3)`)

      ph<-data.table(Especie=c(prot,"Total Protegido","Total Desprotegido"), `Volume/Parcela (m3)`=c(pp,sum(pp),sum(vesp$`Volume/Parcela (m3)`[-nrow(vesp)])-sum(pp)), `Volume/ha (m3)`=c(ppha, sum(ppha), (sum(vesp$`Volume/ha (m3)`[-nrow(vesp)])-sum(ppha))), `Volume/Area Total (m3)`=c(pptot, sum(pptot), (sum(vesp$`Volume/Area Total (m3)`[-nrow(vesp)])-sum(pptot))))

      ph$`Volume/Parcela (m3)`<-as.numeric(ph$`Volume/Parcela (m3)`)
      ph$`Volume/Parcela (m3)`<-format(round(ph$`Volume/Parcela (m3)`,4),nsmall=4)
      ph$`Volume/ha (m3)`<-as.numeric(ph$`Volume/ha (m3)`)
      ph$`Volume/ha (m3)`<-format(round(ph$`Volume/ha (m3)`,4),nsmall=4)
      ph$`Volume/Area Total (m3)`<-as.numeric(ph$`Volume/Area Total (m3)`)
      ph$`Volume/Area Total (m3)`<-format(round(ph$`Volume/Area Total (m3)`,4),nsmall=4)
    }else{

      vesp$`Volume/Plot (m3)`<-as.numeric(vesp$`Volume/Plot (m3)`)
      vesp$`Volume/ha (m3)`<-as.numeric(vesp$`Volume/ha (m3)`)
      vesp$`Volume/Total Area (m3)`<-as.numeric(vesp$`Volume/Total Area (m3)`)

      ph<-data.table(Especie=c(prot,"Total Protected","Total Unprotected"), `Volume/Plot (m3)`=c(pp,sum(pp),sum(vesp$`Volume/Plot (m3)`[-nrow(vesp)])-sum(pp)), `Volume/ha (m3)`=c(ppha, sum(ppha), (sum(vesp$`Volume/ha (m3)`[-nrow(vesp)])-sum(ppha))), `Volume/Total Area (m3)`=c(pptot, sum(pptot), (sum(vesp$`Volume/Total Area (m3)`[-nrow(vesp)])-sum(pptot))))

      ph$`Volume/Plot (m3)`<-as.numeric(ph$`Volume/Plot (m3)`)
      ph$`Volume/Plot (m3)`<-format(round(ph$`Volume/Plot (m3)`,4),nsmall=4)
      ph$`Volume/ha (m3)`<-as.numeric(ph$`Volume/ha (m3)`)
      ph$`Volume/ha (m3)`<-format(round(ph$`Volume/ha (m3)`,4),nsmall=4)
      ph$`Volume/Total Area (m3)`<-as.numeric(ph$`Volume/Total Area (m3)`)
      ph$`Volume/Total Area (m3)`<-format(round(ph$`Volume/Total Area (m3)`,4),nsmall=4)

    }


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


    if(pt==TRUE){
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

    #criar docx com argumento prot


    if(pt==TRUE){
      doc <- read_docx() %>%
        body_add_par("Tabela 1. Parametros da amostragem casual estratificada.", style = "centered") %>%
        body_add_flextable(par) %>% #tabela de parametros volume
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_gg(diam, style="centered") %>% #distribuicao diametrica
        body_add_par("Figura 1. Distribuicao diametrica por estrato.", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Tabela 2. Alocacao das parcelas por estrato e tabela auxiliar para calculo dos parametros de amostragem.", style = "centered") %>%
        body_add_flextable(tabaux) %>% #tabela auxiliar
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Tabela 3. Volume lenhoso estratificado por especie.", style = "centered") %>%
        body_add_flextable(vesp2) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Tabela 4. Quantidade de individuos por especie.", style = "centered") %>%
        body_add_flextable(inds) %>% #ind/sp
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Tabela 5. Volume lenhoso por especie protegida.", style = "centered") %>%
        body_add_flextable(phi) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Tabela 6. Parametros fitossociologicos por estrato, em que: n = quantidade de individuos amostrados; G = area basal; UA = quantidade de unidades amostrais; DA (n/ha) = Densidade absoluta; DR (%) = Densidade relativa; DoA (G/ha) = Dominancia Absoluta; DoR (%) = Dominancia Relativa; FA (%) = Frequencia absoluta; FR (%) = Frequencia Relativa; IVI (%) = Indice de Valor de Importancia.", style = "centered") %>%
        body_add_flextable(fitot) %>% #parametros fito
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_gg(gg3,style="centered")%>%#grafico fito
        body_add_par("Figura 2. Indice de Valor de Importancia por especie e por estrato (soma de densidade relativa, dominancia relativa e frequencia relativa).", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_gg(curve,style="centered")%>%#grafico curva
        body_add_par("Figura 3. Curva de acumulacao de especies para cada estrato. Foi utilizado o metodo Bootstrap para estimar o numero total extrapolado de especies na area, com 1000 permutacoes. O sombreamento em volta da linha representa o intervalo de confianca de 95% a partir do desvio-padrao.", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Tabela 7. Volume lenhoso individual.", style = "centered") %>%
        body_add_flextable(x3) %>%
        body_end_section_landscape()

    }else{

      doc <- read_docx() %>%
        body_add_par("Table 1. Stratified casual sampling parameters.", style = "centered") %>%
        body_add_flextable(par) %>% #tabela de parametros volume
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_gg(diam, style="centered") %>% #distribuicao diametrica
        body_add_par("Figura 1. Diameter distribution by stratum.", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 2. Allocation of plots by stratum and auxiliary table for calculation of sampling parameters.", style = "centered") %>%
        body_add_flextable(tabaux) %>% #tabela auxiliar
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 3. Stratified woody volume by specie.", style = "centered") %>%
        body_add_flextable(vesp2) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 4. Number of individuals by specie.", style = "centered") %>%
        body_add_flextable(inds) %>% #ind/sp
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 5. Woody volume by protected specie.", style = "centered") %>%
        body_add_flextable(phi) %>% #volume/sp
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 6. Phytosociological parameters by stratum, where: n = number of sampled individuals; G = basal area; SU = number of sample units; AD (n/ha) = absolute density; RD (%) = relative density; ADo (G/ha) = absolute dominance; RDo (%) = relative dominance; AF (%) = absolute frequency; RF (%) = relative frequency; IVI (%) = Importance Value Index.", style = "centered") %>%
        body_add_flextable(fitot) %>% #parametros fito
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_gg(gg3,style="centered")%>%#grafico fito
        body_add_par("Figure 2. Importance Value Index by specie and by stratum (sum of relative density, relative dominance and relative frequency).", style = "centered")%>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_gg(curve,style="centered")%>%#grafico curva
        body_add_par("Figure 3. Species accumulation curve for each stratum. The Bootstrap method was used to estimate the total extrapolated number of species in the area, with 1000 permutations. The shading around the line represents the 95% confidence interval from the standard deviation.", style = "centered") %>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 7. Individual woody volume.", style = "centered") %>%
        body_add_flextable(x3) %>%
        body_end_section_landscape()

    }




    if(save==TRUE){

  if(pt==TRUE){
    fileout <- tempfile(pattern="InventarioFlorestal", fileext=".docx")
    print(doc, target = fileout)
  }else{
    fileout <- tempfile(pattern="ForestInventory", fileext=".docx")
    print(doc, target = fileout)
  }
}
}

  if(missing(prot)){

    if(pt==TRUE){
    return(list(`vol individual`=x2,
                `curva especies`=curve,
                `grafico ivi`=gg3,
                `parametros fito`=dtt3,
                `ind por sp`=inds2,
                `volume por sp`=vesp,
                `distribuicao diam`=diam,
                `tabela aux`=tabaux2,
                `parametros vol`=df))
    }else{
      return(list(`individual vol`=x2,
                  `species curve`=curve,
                  `ivi plot`=gg3,
                  `phyto parameters`=dtt3,
                  `ind by sp`=inds2,
                  `volume by sp`=vesp,
                  `diam distribuction`=diam,
                  `aux table`=tabaux2,
                  `vol parameters`=df))
    }


  }else{

    if(pt==TRUE){
    return(list(`vol individual`=x2,
                `curva especies`=curve,
                `grafico ivi`=gg3,
                `parametros fito`=dtt3,
                `spp prot`=ph2,
                `ind por sp`=inds2,
                `volume por sp`=vesp,
                `distribuicao diam`=diam,
                `tabela aux`=tabaux2,
                `parametros vol`=df))
      }else{

        return(list(`individual vol`=x2,
                    `species curve`=curve,
                    `ivi plot`=gg3,
                    `phyto parameters`=dtt3,
                    `prot spp`=ph2,
                    `ind by sp`=inds2,
                    `volume by sp`=vesp,
                    `diam distribuction`=diam,
                    `aux table`=tabaux2,
                    `vol parameters`=df))
  }}

}
}
