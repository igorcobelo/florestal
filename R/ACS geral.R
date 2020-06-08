acs<-function(x,A,a,E=0.1,p=0.05,prot=NULL,ampl=5,rn=F,spivi=15,ci=2,un=F,pt=T,...){

nm <-deparse(substitute(x))

  max<-ceiling(max(x[,5],na.rm=T))

  #Distribuicao diametrica

  x<-as.data.frame(x)

  if(pt==T){

    diam<-ggplot(x, aes(x=x[,5])) +
      geom_histogram( binwidth=ampl ,fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      theme_bw(16)+
      theme(axis.text.y = element_text(size=10),legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12)) +
      scale_x_continuous(breaks = seq(0, max, ampl)) +
      xlab("Classe Diametrica (cm)") +
      ylab("Frequencia")

  }else{

    diam<-ggplot(x, aes(x=x[,5])) +
      geom_histogram(binwidth=ampl,fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      theme_bw(16)+
      theme(axis.text.y = element_text(size=10),legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12)) +
      scale_x_continuous(breaks = seq(0, max, ampl)) +
      xlab("Diameter Class (cm)") +
      ylab("Frequency")
  }


  #volume/parcela

  for(i in 1:max(x[,1], na.rm=T)){
    vv<-c(sum(subset(x[,ncol(x)],x[,1]==i),na.rm = T))
  }

  vv<-as.data.frame(vv)

  for(i in 1:max(x[,1], na.rm=T)){
    vv[i]<-c(sum(subset(x[,ncol(x)],x[,1]==i),na.rm = T))
  }
  vv<-as.data.frame(vv)

  if(pt==T){
  vopa<-data.table(Parcela=c(1:max(x[,1],na.rm=T), "Media"), `Volume amostrado (m3)`=c(vv,sum(vv,na.rm=T)/length(vv)), `Volume/hectare (m3)`=c(vv/a, (sum(vv,na.rm=T)/length(vv))/a), `Volume/area total (m3)`=c(vv*A/a, (sum(vv,na.rm=T)/length(vv))*A/a))
  vopa<-as.data.frame(vopa)
  }else{
  vopa<-data.table(Plot=c(1:max(x[,1], na.rm=T), "Mean"), `Sampled volume (m3)`=c(vv,sum(vv,na.rm=T)/length(vv)), `Volume/hectare (m3)`=c(vv/a, (sum(vv,na.rm=T)/length(vv))/a), `Volume/total area (m3)`=c(vv*A/a, (sum(vv,na.rm=T)/length(vv))*A/a))
  vopa<-as.data.frame(vopa)
  }


  vopa2<-data.table(vopa)

  vopa2[,2]<-as.numeric(unlist(vopa2[,2]))
  vopa2[,3]<-as.numeric(unlist(vopa2[,3]))
  vopa2[,4]<-as.numeric(unlist(vopa2[,4]))

  vopa2[,2]<-format(round(vopa2[,2],4),nsmall=4)
  vopa2[,3]<-format(round(vopa2[,3],4),nsmall=4)
  vopa2[,4]<-format(round(vopa2[,4],4),nsmall=4)


  vopa2 <- flextable(vopa2)
  vopa2 <- align(vopa2, align = "center")
  vopa2 <- align_text_col(vopa2, align = "center")
  vopa2<-autofit(vopa2)


  #parametros
  n<-ncol(vv)
  vv<-as.numeric(vv)
  y<-mean(vv)
  var<-var(vv)
  s<-sqrt(var)
  CV<-s/y*100
  N<-A/a
  invt<-qt(1-p/2, df=n-1)

  #Fator de corre??o (finita ou infinita)
  f<-1-n/N

  #Pop. infinita
  if(f>=0.98){
    s2y<- var/n

    nn<-(invt^2*CV^2)/(E*100)^2

    if(rn==T){
      invt<-qt(1-p/2, df=nn-1)
      nn<-(invt^2*CV^2)/(E*100)^2
    }else{
      nn<-(invt^2*CV^2)/(E*100)^2
    }

    if(pt==T){
      pop<-"(Pop. infinita)"
    }

    if(pt==F){
      pop<-"(Infinite pop.)"
    }
  }

  #Pop. finita
  if(f<0.98){
    s2y<- var/n*f
    nn <-(invt^2*CV^2)/((E*100)^2+(invt^2*CV^2/N))

    if(rn==F){
      nn <-(invt^2*CV^2)/((E*100)^2+(invt^2*CV^2/N))
    }

    if(rn==T){
      invt<-qt(1-p/2, df=nn-1)
      nn <-(invt^2*CV^2)/((E*100)^2+(invt^2*CV^2/N))
    }

    if(pt==T){
      pop<-"(Pop. finita)"
    }

    if(pt==F){
      pop<-"(Finite pop.)"
  }
  }

  #Continua parametros
  sy<-sqrt(s2y)
  eabs <- invt*sy
  erel <- (eabs/y)*100

  #Estimativa do volume total da populacao
  Y<-y*N

  #Intervalo de Confianca
  ICparmax<-y+eabs
  ICparmin<-y-eabs

  IChecmax<-ICparmax/a
  IChecmin<-ICparmin/a

  ICtotmax<-ICparmax*A/a
  ICtotmin<-ICparmin*A/a


  if(pt==T){
    df <- data.table(Parametros=c("Media", "Variancia da media",
                                  "Erro padrao da media", "Volume total da populacao",
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
                     Estimativas=c(y, s2y,sy, Y, invt, eabs, erel, E*100,p*100,CV,f,n,nn,ICparmin,
                                   ICparmax,IChecmin,IChecmax,ICtotmin,ICtotmax),
                     Unidade=c("m3/parcela", "m3/parcela","m3/parcela","m3/area total",
                               "","m3/parcela","%", "%", "%","%", pop,"Parcelas","Parcelas","m3/parcela","m3/parcela",
                               "m3/hectare","m3/hectare","m3/area total","m3/area total"))
  }else{
    df <- data.table(Parameters=c("Mean", "Mean variance",
                                  "Mean standard error", "Total population volume",
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
                     Estimates=c(y, s2y,sy, Y, invt, eabs, erel, E*100,p*100,CV,f,n,nn,ICparmin,
                                 ICparmax,IChecmin,IChecmax,ICtotmin,ICtotmax),
                     Unit=c("m3/plot", "m3/plot","m3/plot","m3/total area",
                            "","m3/plot","%", "%", "%","%", pop,"Plots","Plots","m3/plot","m3/plot",
                            "m3/hectare","m3/hectare","m3/total area","m3/total area"))
  }

  df[,2]<-format(round(df[,2],4),nsmall=4)


  if(n>=nn){

    if(pt==F){
      cat("\n------------------------------------------------------------------------------------\n")
      cat("The sampling intensity satisfies the required error of", E*100,"%, to a significance level of",p*100,"%.")
      cat("\nTherefore, it is not necessary to sample more plots.\n")
      cat("------------------------------------------------------------------------------------")
    }

    if(pt==T){
      cat("\n------------------------------------------------------------------------------------\n")
      cat("A intensidade amostral satisfaz o erro requerido de", E*100,"%, para um nivel de significancia de",p*100,"%.")
      cat("\nPortanto, nao e necessario amostrar mais parcelas.\n")
      cat("------------------------------------------------------------------------------------")
    }
  }

  if(n<nn){

    if(pt==F){
      cat("\n------------------------------------------------------------------------------------\n")
      cat("The sample intensity does not satisfy the required error of", E*100,"%, to a significance level of",p*100,"%.")
      cat("\nTherefore, it is necessary to sample more",ceiling(nn-n),"plots.\n")
      cat("------------------------------------------------------------------------------------")
    }

    if(pt==T){
      cat("\n------------------------------------------------------------------------------------\n")
      cat("A intensidade amostral nao satisfaz o erro requerido de", E*100,"%, para um nivel de significancia de",p*100,"%.")
      cat("\nPortanto, e necessario amostrar mais",ceiling(nn-n),"parcelas.\n")
      cat("------------------------------------------------------------------------------------")
    }
  }

  df<-as.data.frame(df)
  par <- flextable(df)
  par <- align(par, align = "center")
  par <- align_text_col(par, align = "center")
  par<-autofit(par)


  #FITO

  Especie<-x[,3]
  parcela<-x[,1]
  d<-x[,5]
  fito <- data.frame(Especie=Especie,parcela=parcela, d=d)

  fito$gi<-pi*d^2/40000

  fito<-as.data.frame(fito)

  for(i in fito[,1]){
    qt<-c(length(subset(fito[,1], fito[,1]==i)))
  }
  qt<-as.data.frame(qt)

  for(i in fito[,1]){
    tryCatch({
    qt[i]<-c(length(subset(fito[,1], fito[,1]==i)))
    }, error=function(e){})
  }

  qt<-as.data.frame(qt)
  qt[,1]<-NULL

  for(i in fito[,1]){
    sp<-c(length(unique(subset(fito[,2], fito[,1]==i))))
  }
  sp<-as.data.frame(sp)

  for(i in fito[,1]){
    tryCatch({
    sp[i]<-c(length(unique(subset(fito[,2], fito[,1]==i))))
  }, error=function(e){})
  }

  sp<-as.data.frame(sp)
  sp[,1]<-NULL

  for(i in fito[,1]){
    g<-c(sum(subset(fito[,4], fito[,1]==i)))
  }
  g<-as.data.frame(g)

  for(i in fito[,1]){
    tryCatch({
    g[i]<-c(sum(subset(fito[,4], fito[,1]==i)))
    },error=function(e){})
  }

  g<-as.data.frame(g)
  g[,1]<-NULL

  qtt<-as.data.frame(t(qt))
  spt<-as.data.frame(t(sp))
  gt<-as.data.frame(t(g))
  rnn<-as.data.frame(rownames(qtt))

  dtt<-data.table(rnn,qtt,gt,spt)

  if(pt==T){
    colnames(dtt)[1]<-"Especie"
    colnames(dtt)[2]<-"n"
    colnames(dtt)[3]<-"G (m2)"
    colnames(dtt)[4]<-"UA"

    dtt$`DA (n/ha)`<-dtt$n/A
    dtt$`DR (%)`<-dtt$`DA (n/ha)`/sum(dtt$`DA (n/ha)`)*100
    dtt$`DoA (G/ha)`<-dtt$`G (m2)`/A
    dtt$`DoR (%)`<-dtt$`DoA (G/ha)`/sum(dtt$`DoA (G/ha)`)*100
    dtt$`FA (%)`<-dtt$UA/max(x[,1],na.rm=T)*100
    dtt$`FR (%)`<-dtt$`FA (%)`/sum(dtt$`FA (%)`)*100
    dtt$`IVI (%)`<-dtt$`DR (%)`+dtt$`DoR (%)`+dtt$`FR (%)`
    dtt<-dtt[order(dtt$`IVI (%)`, decreasing = T),]
  }else{
    colnames(dtt)[1]<-"Specie"
    colnames(dtt)[2]<-"n"
    colnames(dtt)[3]<-"G (m2)"
    colnames(dtt)[4]<-"SU"

    dtt$`AD (n/ha)`<-dtt$n/A
    dtt$`RD (%)`<-dtt$`AD (n/ha)`/sum(dtt$`AD (n/ha)`)*100
    dtt$`ADo (G/ha)`<-dtt$G/A
    dtt$`RDo (%)`<-dtt$`ADo (G/ha)`/sum(dtt$`ADo (G/ha)`)*100
    dtt$`AF (%)`<-dtt$SU/max(x[,1],na.rm=T)*100
    dtt$`RF (%)`<-dtt$`AF (%)`/sum(dtt$`AF (%)`)*100
    dtt$`IVI (%)`<-dtt$`RD (%)`+dtt$`RDo (%)`+dtt$`RF (%)`
    dtt<-dtt[order(dtt$`IVI (%)`, decreasing = T),]
  }

  dtt3<-data.table(dtt)


  dtt3[,2]<-format(round(dtt3[,2],0),nsmall=0)
  dtt3[,3]<-format(round(dtt3[,3],4),nsmall=4)
  dtt3[,4]<-format(round(dtt3[,4],0),nsmall=0)
  dtt3[,5]<-format(round(dtt3[,5],0),nsmall=0)
  dtt3[,6]<-format(round(dtt3[,6],2),nsmall=2)
  dtt3[,7]<-format(round(dtt3[,7],2),nsmall=2)
  dtt3[,8]<-format(round(dtt3[,8],2),nsmall=2)
  dtt3[,9]<-format(round(dtt3[,9],2),nsmall=2)
  dtt3[,10]<-format(round(dtt3[,10],2),nsmall=2)
  dtt3[,11]<-format(round(dtt3[,11],2),nsmall=2)

  dtt3 <- flextable(dtt3)
  dtt3<-autofit(dtt3)
  dtt3 <- align(dtt3, align = "center", part="all")
  dtt3<-italic(dtt3,j=1)

  
  #Grafico fito



  if(pt==T){

    t<-t(data.frame(dtt$Especie[1:spivi],dtt$`DR (%)`[1:spivi],dtt$`DoR (%)`[1:spivi],dtt$`FR (%)`[1:spivi]))
    t<-data.frame(t)

    rownames(t)[1]<-"Especie"
    rownames(t)[2]<-"Densidade Relativa (%)"
    rownames(t)[3]<-"Dominancia Relativa (%)"
    rownames(t)[4]<-"Frequencia Relativa (%)"
  }else{
    t<-t(data.frame(dtt$Specie[1:spivi],dtt$`RD (%)`[1:spivi],dtt$`RDo (%)`[1:spivi],dtt$`RF (%)`[1:spivi]))
    t<-data.frame(t)

    rownames(t)[1]<-"Specie"
    rownames(t)[2]<-"Relative Density (%)"
    rownames(t)[3]<-"Relative Dominance (%)"
    rownames(t)[4]<-"Relative Frequency (%)"
  }


  specie <- t(data.frame(rep(t[1,], each=3)))
  value<-data.frame(b=unlist(t[2:4,],use.names=F))
  condition <- data.frame(rep(rownames(t[2:4,]),ncol(t)))
  data <- data.frame(specie,condition,value)
  
  
  # se tiver uma unica especie
  if(un==T){
  
    if(pt==T){
      doc<-read_docx() %>%
        
        body_add_par("Tabela 1. Parametros da amostragem casual simples.", style = "centered") %>%
        body_add_flextable(par) %>%
        body_end_section_portrait() %>%
        
        body_add_break() %>%
        body_add_gg(diam,style="centered", height=4,width=6) %>%
        body_add_par("Figura 1. Distribuicao diametrica.", style = "centered") %>%
        body_end_section_portrait() %>%
        
        body_add_break() %>%
        body_add_par("Tabela 2. Volume lenhoso por parcela.", style = "centered") %>%
        body_add_flextable(vopa2) %>%
        body_end_section_landscape() %>%
        
        body_add_break() %>%
        body_add_par("Tabela 3. Volume lenhoso individual.", style = "centered") %>%
        body_add_flextable(anex) %>%
        body_end_section_landscape()
      
      
    }else{
      
      doc<-read_docx() %>%
        
        body_add_par("Table 1. Simple casual sampling parameters.", style = "centered") %>%
        body_add_flextable(par) %>%
        body_end_section_portrait() %>%
        
        body_add_break() %>%
        body_add_gg(diam,style="centered", height=4,width=6) %>%
        body_add_par("Figure 1. Diameter distribution.", style = "centered") %>%
        body_end_section_portrait() %>%
        
        body_add_break() %>%
        body_add_par("Table 2. Woody volume by plot.", style = "centered") %>%
        body_add_flextable(vopa2) %>%
        body_end_section_landscape() %>%
        
        body_add_break() %>%
        body_add_par("Table 3. Individual woody volume.", style = "centered") %>%
        body_add_flextable(anex)%>%
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
      
      return(list(`vol individual`=anex,
                  `distribuicao diam`=diam,
                  `volume por parcela`=vopa2,
                  `parametros vol`=par))
    }else{
      
      return(list(`individual vol`=anex,
                  `diam distribuction`=diam,
                  `volume by plot`=vopa2,
                  `vol parameters`=par))
    }
    
    
    
  }else{
    
    #mais de uma especie:
    
  data$b<-as.character(data$b)
  data$b<-as.numeric(data$b)

  if(pt==T){
    colnames(data)[1]<-"Especies"
    colnames(data)[2]<-"Parametros"
  }else{
    colnames(data)[1]<-"Species"
    colnames(data)[2]<-"Parameters"
  }




  if(pt==T){

    gg2<-ggplot(data, aes(reorder(Especies,b), b, fill = Parametros)) +
      geom_col(alpha = 0.8) +
      scale_fill_brewer(palette = "Dark2") +
      theme_bw(16)  +
      coord_flip() +
      xlab("Especies\n") + ylab("\nIndice de Valor de Importancia (%)") +
      labs(fill = "Parametros") +
      theme(axis.text.y = element_text(face = "italic",size=8), legend.title=element_blank(),legend.justification = "center" ,legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12),
            legend.position="bottom",legend.direction = "horizontal")

    p2 <- gg2 + theme(legend.position = "none")
    le1 <- cowplot::get_legend(gg2)
    gg3<-cowplot::plot_grid(p2, le1,nrow = 2,rel_heights = c(1, 0.2))


      }else{

        gg2<-ggplot(data, aes(reorder(Species,b), b, fill = Parameters)) +
          geom_col(alpha = 0.8) +
          scale_fill_brewer(palette = "Dark2") +
          theme_bw(16)  +
          coord_flip() +
          xlab("Species\n") + ylab("\nImportance Value Index (%)") +
          labs(fill = "Parameters") +
          theme(axis.text.y = element_text(face = "italic",size=8), legend.title=element_blank(),legend.justification = "center" ,legend.text=element_text(size=10),
                axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
                axis.title.y=element_text(size=12),
                legend.position="bottom",legend.direction = "horizontal")

        p2 <- gg2 + theme(legend.position = "none")
        le1 <- cowplot::get_legend(gg2)
        gg3<-cowplot::plot_grid(p2, le1,nrow = 2,rel_heights = c(1, 0.2))

      }


  #CURVA DE ACUMULACAO DE ESPECIES
  
  
  cc<-as.data.frame.matrix(table(x$Plot, x$Specie))

  suppressMessages({sp2 <- accumresult(cc, method = "random")})

  h<-data.frame(r=sp2$richness,p=sp2$sites, sd=sp2$sd)
  
  if(pt==T){
  curve <- ggplot(h, aes(x=p, y=r))+
    geom_line() +
    geom_ribbon(aes(ymin=r-sd*ci, ymax=r+sd*ci), alpha = 0.2)+
    theme_bw(16)+
    theme(axis.text.y = element_text(size=10),legend.text=element_text(size=10),
          axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
          axis.title.y=element_text(size=12))+
    xlab("\nParcelas")+
    ylab("Riqueza\n")
  }else{
    
    curve <- ggplot(h, aes(x=p, y=r))+
      geom_line() +
      geom_ribbon(aes(ymin=r-sd*ci, ymax=r+sd*ci), alpha = 0.2)+
      theme_bw(16)+
      theme(axis.text.y = element_text(size=10),legend.text=element_text(size=10),
            axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12))+
      xlab("\nPlot")+
      ylab("Richness\n")
  }
  
  
 #VOLUME POR SP

  x<-as.data.frame(x)
  x[,ncol(x)]<-as.numeric(x[,ncol(x)])



  for(i in x[,3]){
    vvol<-c(sum(subset(x[,ncol(x)], x[,3]==i)))
  }
  vvol<-as.data.frame(vvol)

  for(i in x[,3]){
    tryCatch({
    vvol[i]<-c(sum(subset(x[,ncol(x)], x[,3]==i)))
    }, error=function(e){})
  }

  vvol<-as.data.frame(vvol)
  vvol[,1]<-NULL
  vvol<-as.data.frame(t(vvol))
  vvol[,2]<-rownames(vvol)


  if(pt==T){
    colnames(x)[1]<-"Parcela"
    colnames(x)[2]<-"Individuo"
    colnames(x)[3]<-"Especie"
    colnames(x)[4]<-"Altura (m)"
    colnames(x)[5]<-"Diametro (cm)"
    colnames(x)[6]<-"Volume (m3)"
  }else{
    colnames(x)[1]<-"Plot"
    colnames(x)[2]<-"Individual"
    colnames(x)[3]<-"Specie"
    colnames(x)[4]<-"Height (m)"
    colnames(x)[5]<-"Diameter (cm)"
    colnames(x)[6]<-"Volume (m3)"
  }

  x2<-as.data.frame(x)

  x2[,1]<-format(round(x2[,1],0),nsmall=0)
  x2[,2]<-format(round(x2[,2],0),nsmall=0)
  x2[,4]<-format(round(x2[,4],2),nsmall=2)
  x2[,5]<-format(round(x2[,5],2),nsmall=2)
  x2[,6]<-format(round(x2[,6],4),nsmall=4)

  anex <- flextable(x2)
  anex <- autofit(anex)
  anex <- align(anex, align = "center", part="all")
  anex<-italic(anex,j=3)


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

    vtt<-as.data.frame(vt)
    vtt <- flextable(vtt)
    vtt <- autofit(vtt)
    vtt <- align(vtt, align = "center", part="all")
    vtt<-italic(vtt,j=1,i=2:nrow(vt)-1)


if(pt==T){
    doc<-read_docx() %>%

      body_add_par("Tabela 1. Parametros da amostragem casual simples.", style = "centered") %>%
      body_add_flextable(par) %>%
      body_end_section_portrait() %>%

      body_add_break() %>%
      body_add_gg(diam,style="centered", height=4,width=6) %>%
      body_add_par("Figura 1. Distribuicao diametrica.", style = "centered") %>%
      body_end_section_portrait() %>%

      body_add_break() %>%
      body_add_par("Tabela 2. Volume lenhoso por parcela.", style = "centered") %>%
      body_add_flextable(vopa2) %>%
      body_end_section_landscape() %>%

      body_add_break() %>%
      body_add_par("Tabela 3. Volume lenhoso por especie.", style = "centered") %>%
      body_add_flextable(vtt) %>%
      body_end_section_landscape() %>%

      body_add_break() %>%
      body_add_par("Tabela 4. Parametros fitossociologicos, em que: n = quantidade de individuos amostrados; G = area basal; UA = quantidade de unidades amostrais; DA (n/ha) = densidade absoluta; DR (%) = densidade relativa; DoA (G/ha) = dominancia absoluta; DoR (%) = dominancia relativa; FA (%) = frequencia absoluta; FR (%) = frequencia relativa; IVI (%) = Indice de Valor de Importancia.", style = "centered") %>%
      body_add_flextable(dtt3) %>%
      body_end_section_landscape() %>%

      body_add_break() %>%
      body_add_gg(gg3,style="centered", height=3.5,width=6)%>%#grafico fito
      body_add_par("Figura 2. Indice de Valor de Importancia por especie (soma de densidade relativa, dominancia relativa e frequencia relativa).", style = "centered") %>%
      body_end_section_landscape() %>%
      
      body_add_break() %>%
      body_add_gg(curve,style="centered", height=4,width=6)%>%#grafico curva
      body_add_par("Figura 3. Curva de acumulacao de especies, com parcelas adicionadas em ordem aleatoria e 100 permutacoes.", style = "centered") %>%
      body_end_section_landscape() %>%
      
      body_add_break() %>%
      body_add_par("Tabela 5. Volume lenhoso individual.", style = "centered") %>%
      body_add_flextable(anex) %>%
      body_end_section_landscape()

}else{
  doc<-read_docx() %>%

    body_add_par("Table 1. Simple casual sampling parameters.", style = "centered") %>%
    body_add_flextable(par) %>%
    body_end_section_portrait() %>%

    body_add_break() %>%
    body_add_gg(diam,style="centered", height=4,width=6) %>%
    body_add_par("Figure 1. Diameter distribution.", style = "centered") %>%
    body_end_section_portrait() %>%

    body_add_break() %>%
    body_add_par("Table 2. Woody volume by plot.", style = "centered") %>%
    body_add_flextable(vopa2) %>%
    body_end_section_landscape() %>%

    body_add_break() %>%
    body_add_par("Table 3. Woody volume by specie.", style = "centered") %>%
    body_add_flextable(vtt) %>%
    body_end_section_landscape() %>%

    body_add_break() %>%
    body_add_par("Table 4. Phytosociological parameters, where: n = number of sampled individuals; G = basal area; SU = number of sample units; AD (n/ha) = absolute density; RD (%) = relative density; ADo (G/ha) = absolute dominance; RDo (%) = relative dominance; AF (%) = absolute frequency; RF (%) = relative frequency; IVI (%) = Importance Value Index.", style = "centered") %>%
    body_add_flextable(dtt3) %>%
    body_end_section_landscape() %>%

    body_add_break() %>%
    body_add_gg(gg3,style="centered", height=3.5,width=6)%>%#grafico fito
    body_add_par("Figure 2. Importance Value Index by specie (sum of relative density, relative dominancy and relative frequency).", style = "centered") %>%
    body_end_section_landscape() %>%
    
    body_add_break() %>%
    body_add_gg(curve,style="centered", height=4,width=6)%>%#grafico curva
    body_add_par("Figure 3. Species accumulation curve, with plots added in random order and 100 permutations.", style = "centered") %>%
    body_end_section_landscape() %>%

    body_add_break() %>%
    body_add_par("Table 5. Individual woody volume.", style = "centered") %>%
    body_add_flextable(anex)%>%
    body_end_section_landscape()

}

  } else {


    for(i in prot){
      pp<-c(sum(subset(x[,ncol(x)],x[,3]==i),na.rm = T))
    }

    pp<-as.data.frame(pp)

    for(i in prot){
      pp[i]<-c(sum(subset(x[,ncol(x)],x[,3]==i),na.rm = T))
    }

    pp<-as.data.frame(pp[,2:ncol(pp)])

    if(pt==T){
      vt<-data.table(Especie=c(vvol[,2],"Media"), `Media amostrada (m3)`=c(vvol[,1]/n,sum((vvol[,1])/n)), `Media/hectare (m3)`=c((vvol[,1]/n)/a, sum((vvol[,1])/n)/a), `Media/area total (m3)`=c((vvol[,1]/n)*A/a, sum(vvol[,1]/n)*A/a))

      vt$`Media amostrada (m3)`<-as.numeric(vt$`Media amostrada (m3)`)
      vt$`Media amostrada (m3)`<-format(round(vt$`Media amostrada (m3)`,4),nsmall=4)
      vt$`Media/hectare (m3)`<-as.numeric(vt$`Media/hectare (m3)`)
      vt$`Media/hectare (m3)`<-format(round(vt$`Media/hectare (m3)`,4),nsmall=4)
      vt$`Media/area total (m3)`<-as.numeric(vt$`Media/area total (m3)`)
      vt$`Media/area total (m3)`<-format(round(vt$`Media/area total (m3)`,4),nsmall=4)



      ph<-data.table(Especie=c(prot,"Media protegida","Media desprotegida"), `Media amostrada (m3)`=c(pp/n,sum(pp)/n,(sum(x[,ncol(x)],na.rm=T)-sum(pp))/n), `Media/hectare (m3)`=c((pp/n)/a, (sum(pp)/n)/a, ((sum(x[,ncol(x)],na.rm=T)-sum(pp))/n)/a), `Media/area total (m3)`=c((pp/n)*A/a, (sum(pp)/n)*A/a, ((sum(x[,ncol(x)],na.rm=T)-sum(pp))/n)*A/a))

      ph$`Media amostrada (m3)`<-as.numeric(ph$`Media amostrada (m3)`)
      ph$`Media amostrada (m3)`<-format(round(ph$`Media amostrada (m3)`,4),nsmall=4)
      ph$`Media/hectare (m3)`<-as.numeric(ph$`Media/hectare (m3)`)
      ph$`Media/hectare (m3)`<-format(round(ph$`Media/hectare (m3)`,4),nsmall=4)
      ph$`Media/area total (m3)`<-as.numeric(ph$`Media/area total (m3)`)
      ph$`Media/area total (m3)`<-format(round(ph$`Media/area total (m3)`,4),nsmall=4)
    }else{
      vt<-data.table(Specie=c(vvol[,2],"Mean"), `Sampled mean (m3)`=c(vvol[,1]/n,sum((vvol[,1])/n)), `Mean/hectare (m3)`=c((vvol[,1]/n)/a, sum((vvol[,1])/n)/a), `Mean/total area (m3)`=c((vvol[,1]/n)*A/a, sum(vvol[,1]/n)*A/a))

      vt$`Sampled mean (m3)`<-as.numeric(vt$`Sampled mean (m3)`)
      vt$`Sampled mean (m3)`<-format(round(vt$`Sampled mean (m3)`,4),nsmall=4)
      vt$`Mean/hectare (m3)`<-as.numeric(vt$`Mean/hectare (m3)`)
      vt$`Mean/hectare (m3)`<-format(round(vt$`Mean/hectare (m3)`,4),nsmall=4)
      vt$`Mean/total area (m3)`<-as.numeric(vt$`Mean/total area (m3)`)
      vt$`Mean/total area (m3)`<-format(round(vt$`Mean/total area (m3)`,4),nsmall=4)

      ph<-data.table(Specie=c(prot,"Protected mean","Unprotected mean"), `Sampled mean (m3)`=c(pp/n,sum(pp)/n,(sum(x[,ncol(x)],na.rm=T)-sum(pp))/n), `Mean/hectare (m3)`=c((pp/n)/a, (sum(pp)/n)/a, ((sum(x[,ncol(x)],na.rm=T)-sum(pp))/n)/a), `Mean/total area (m3)`=c((pp/n)*A/a, (sum(pp)/n)*A/a, ((sum(x[,ncol(x)],na.rm=T)-sum(pp))/n)*A/a))

      ph$`Sampled mean (m3)`<-as.numeric(ph$`Sampled mean (m3)`)
      ph$`Sampled mean (m3)`<-format(round(ph$`Sampled mean (m3)`,4),nsmall=4)
      ph$`Mean/hectare (m3)`<-as.numeric(ph$`Mean/hectare (m3)`)
      ph$`Mean/hectare (m3)`<-format(round(ph$`Mean/hectare (m3)`,4),nsmall=4)
      ph$`Mean/total area (m3)`<-as.numeric(ph$`Mean/total area (m3)`)
      ph$`Mean/total area (m3)`<-format(round(ph$`Mean/total area (m3)`,4),nsmall=4)

    }


    vtt<-as.data.frame(vt)
    vtt <- flextable(vtt)
    vtt <- autofit(vtt)
    vtt <- align(vtt, align = "center", part="all")
    vtt<-italic(vtt,j=1,i=2:nrow(vt)-1)
    suppressWarnings(vtt)


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


if(pt==T){
    doc<-read_docx() %>%

      body_add_par("Tabela 1. Parametros da amostragem casual simples.", style = "centered") %>%
      body_add_flextable(par) %>%
      body_end_section_portrait() %>%

      body_add_break() %>%
      body_add_gg(diam,style="centered", height=4,width=6) %>%
      body_add_par("Figura 1. Distribuicao diametrica.", style = "centered") %>%
      body_end_section_portrait() %>%

      body_add_break() %>%
      body_add_par("Tabela 2. Volume lenhoso por parcela.", style = "centered") %>%
      body_add_flextable(vopa2) %>%
      body_end_section_landscape() %>%

      body_add_break() %>%
      body_add_par("Tabela 3. Volume lenhoso por especie.", style = "centered") %>%
      body_add_flextable(vtt) %>%
      body_end_section_landscape() %>%

      body_add_break() %>%
      body_add_par("Tabela 4. Volume lenhoso por especie protegida.", style = "centered") %>%
      body_add_flextable(phi) %>%
      body_end_section_landscape() %>%

      body_add_break() %>%
      body_add_par("Tabela 5. Parametros fitossociologicos, em que: n = quantidade de individuos amostrados; G = area basal; UA = quantidade de unidades amostrais; DA (n/ha) = densidade absoluta; DR (%) = densidade relativa; DoA (G/ha) = dominancia absoluta; DoR (%) = dominancia relativa; FA (%) = frequencia absoluta; FR (%) = frequencia relativa; IVI (%) = Indice de Valor de Importancia.", style = "centered") %>%
      body_add_flextable(dtt3) %>%
      body_end_section_landscape() %>%

      body_add_break() %>%
      body_add_gg(gg3,style="centered", height=3.5,width=6)%>%#grafico fito
      body_add_par("Figura 2. Indice de Valor de Importancia por especie (soma de densidade relativa, dominancia relativa e frequencia relativa).", style = "centered") %>%
      body_end_section_landscape() %>%
      
      body_add_break() %>%
      body_add_gg(curve,style="centered", height=4,width=6)%>%#grafico curva
      body_add_par("Figura 3. Curva de acumulacao de especies, com parcelas adicionadas em ordem aleatoria e 100 permutacoes.", style = "centered") %>%
      body_end_section_landscape() %>%

      body_add_break() %>%
      body_add_par("Tabela 6. Volume lenhoso individual.", style = "centered") %>%
      body_add_flextable(anex) %>%
      body_end_section_landscape()


}else{

  doc<-read_docx() %>%

    body_add_par("Table 1. Simple casual sampling parameters.", style = "centered") %>%
    body_add_flextable(par) %>%
    body_end_section_portrait() %>%

    body_add_break() %>%
    body_add_gg(diam,style="centered", height=4,width=6) %>%
    body_add_par("Figure 1. Diameter distribution.", style = "centered") %>%
    body_end_section_portrait() %>%

    body_add_break() %>%
    body_add_par("Table 2. Woody volume by plot.", style = "centered") %>%
    body_add_flextable(vopa2) %>%
    body_end_section_landscape() %>%

    body_add_break() %>%
    body_add_par("Table 3. Woody volume by specie.", style = "centered") %>%
    body_add_flextable(vtt) %>%
    body_end_section_landscape() %>%

    body_add_break() %>%
    body_add_par("Table 4. Woody volume by protected specie.", style = "centered") %>%
    body_add_flextable(phi) %>%
    body_end_section_landscape() %>%

    body_add_break() %>%
    body_add_par("Table 5. Phytosociological parameters, where: n = number of sampled individuals; G = basal area; SU = number of sample units; AD (n/ha) = absolute density; RD (%) = relative density; ADo (G/ha) = absolute dominance; RDo (%) = relative dominance; AF (%) = absolute frequency; RF (%) = relative frequency; IVI (%) = Importance Value Index.", style = "centered") %>%
    body_add_flextable(dtt3) %>%
    body_end_section_landscape() %>%

    body_add_break() %>%
    body_add_gg(gg3,style="centered", height=3.5,width=6)%>%#grafico fito
    body_add_par("Figure 2. Importance Value Index by specie (sum of relative density, relative dominancy and relative frequency).", style = "centered") %>%
    body_end_section_landscape() %>%
    
    body_add_break() %>%
    body_add_gg(curve,style="centered", height=4,width=6)%>%#grafico curva
    body_add_par("Figure 3. Species accumulation curve, with plots added in random order and 100 permutations.", style = "centered") %>%
    body_end_section_landscape() %>%

    body_add_break() %>%
    body_add_par("Table 6. Individual woody volume.", style = "centered") %>%
    body_add_flextable(anex)%>%
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
      
    return(list(`vol individual`=anex,
                `curva especies`=curve,
                `grafico ivi`=gg3,
                `parametros fito`=dtt3,
                `volume por sp`=vtt,
                `distribuicao diam`=diam,
                `volume por parcela`=vopa2,
                `parametros vol`=par))
    }else{
      
      return(list(`individual vol`=anex,
      `species curve`=curve,
      `ivi plot`=gg3,
      `phyto parameters`=dtt3,
      `volume by sp`=vtt,
      `diam distribuction`=diam,
      `volume by plot`=vopa2,
      `vol parameters`=par))
    }
      
      
  }else{
    
    if(pt==T){
      
      return(list(`vol individual`=anex,
                  `curva especies`=curve,
                  `grafico ivi`=gg3,
                  `parametros fito`=dtt3,
                  `spp prot`=phi,
                  `volume por sp`=vtt,
                  `distribuicao diam`=diam,
                  `volume por parcela`=vopa2,
                  `parametros vol`=par))
    }else{
      
      return(list(`individual vol`=anex,
      `species curve`=curve,
      `ivi plot`=gg3,
      `phyto parameters`=dtt3,
      `prot spp`=phi,
      `volume by sp`=vtt,
      `diam distribuction`=diam,
      `volume by plot`=vopa2,
      `vol parameters`=par))
    }
  }

  }
  }



