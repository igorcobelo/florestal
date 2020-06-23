
bit<-function(x,A,k,E=0.1,p=0.05,ampl=5,rn=F,pt=T,...){

  nm <-deparse(substitute(x))

  max<-max(x[,5],na.rm=T)+2


  ##Criar coluna com distancia critica
  x$`Distancia critica`<-50*(x[,5]/100)/sqrt(k)

  #Eliminar linhas com distancia radial>=dist critica

  x$teste <- ifelse(x[,6]>=x[,8] & !(is.na(x[,6]==T)), "APAGAR", "DEIXAR")
  x <- subset(x, !(teste == "APAGAR"))
  x[,ncol(x)]<-NULL
  x[,ncol(x)]<-NULL
  x[,6]<-NULL

  if(pt==T){
    colnames(x)[1]<-"Ponto amostral"
    colnames(x)[2]<-"Individuo"
    colnames(x)[3]<-"Especie"
    colnames(x)[4]<-"Altura (m)"
    colnames(x)[5]<-"Diametro (cm)"
    colnames(x)[6]<-"Volume (m3)"
  }else{
    colnames(x)[1]<-"Sample point"
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


  #encontrar individuos por ponto amostral


  for(i in 1:max(x[, 1])) {
    ss <- c(length(unique(subset(x[, 2], x[, 1] == i))))
  }
  ss <- as.data.frame(ss
  )

  for(i in 1:max(x[,1])){
    ss[i]<-c(length(unique(subset(x[,2], x[,1]==i))))
  }
  ss<-as.data.frame(ss)

  #area basal/ha para cada ponto amostral
  G<-k*ss

  gi<-pi*x[,5]^2/40000
  umsgi<-1/gi

  x[,ncol(x)+1]<-cbind(umsgi)

  for(i in 1:max(x[,1])){
    gg<-c(sum(subset(x[,ncol(x)], x[,1]==i)))
  }
  gg<-as.data.frame(gg)

  for(i in 1:max(x[,1])){
    gg[i]<-c(sum(subset(x[,ncol(x)], x[,1]==i)))
  }
  gg<-as.data.frame(gg)

  #N/ha para cada ponto amostral
  Nha<-k*gg


  #V/ha para cada ponto amostral
  x[,ncol(x)]<-NULL

  Vigi<-x[,ncol(x)]/gi

  x[,ncol(x)+1]<-cbind(Vigi)

  for(i in 1:max(x[,1])){
    vg<-c(sum(subset(x[,ncol(x)], x[,1]==i)))
  }
  vg<-as.data.frame(vg)

  for(i in 1:max(x[,1])){
    vg[i]<-c(sum(subset(x[,ncol(x)], x[,1]==i)))
  }
  vg<-as.data.frame(vg)

  vha<-k*vg

  gg<-as.numeric(gg)
  Nha<-as.numeric(Nha)
  vha<-as.numeric(vha)
n<-ncol(ss)

  if(pt==F){

    vah<-data.table(`Sample point`=1:n, `Basal area/ha (m2)`=gg,
                    `Individuals/ha`=Nha, `Volume/ha (m3)`=vha)
  }else{

    vah<-data.table(`Ponto amostral`=1:n, `Area basal/ha (m2)`=gg,
                    `Individuos/ha`=Nha, `Volume/ha (m3)`=vha)
  }


vahh<-as.data.frame(vah)

  vahh[,2]<-format(round(vahh[,2],4),nsmall=4)
  vahh[,3]<-format(round(vahh[,3],4),nsmall=4)
  vahh[,4]<-format(round(vahh[,4],4),nsmall=4)


  vahh <- flextable(vahh)
  vahh <- align(vahh, align = "center")
  vahh <- align_text_col(vahh, align = "center")
  vahh<-autofit(vahh)

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


  #parametros
  vha<-as.numeric(vha)
  y<-mean(vha)
  var<-var(vha)
  s<-sqrt(var)
  CV<-s/y*100

  invt<-qt(1-p/2, df=n-1)

  s2y<- var/n

  nn<-(invt^2*CV^2)/(E*100)^2

  if(rn==T){
    invt<-qt(1-p/2, df=nn-1)
    nn<-(invt^2*CV^2)/(E*100)^2
  }


  #Continua parametros
  sy<-sqrt(s2y)
  eabs <- invt*sy
  erel <- (eabs/y)*100

  #Estimativa do volume total da populacao
  Y<-mean(vha*A)

  #Intervalo de Confian?a

  ICtotmax<-(y*n)+(eabs*invt*n)
  ICtotmin<-(y*n)-(eabs*invt*n)


  if(pt==F){

    df <- data.table(Parameters=c("Mean", "Mean variance",
                                  "Mean standard error", "Total population volume",
                                  "Tabulated t value",
                                  "Absolute sampling error",
                                  "Relative sampling error",
                                  "Required error", "Significance level",
                                  "Coefficient of variation",
                                  "Sampled points", "Sampling intensity",
                                  "Lower CI for total area",
                                  "Upper CI for total area"),
                     Estimates=c(y, s2y,sy, Y, invt, eabs, erel, E*100,p*100,CV,n,nn,
                                 ICtotmin,ICtotmax),
                     Unit=c("m3/sample point", "m3/sample point","m3/sample point","m3/total area",
                            "","m3/sample point","%", "%", "%","%","Sample points","Sample points","m3/total area","m3/total area"))

  }else{
    df <- data.table(Parametros=c("Media", "Variancia da media",
                                  "Erro padrao da media", "Volume total da populacao",
                                  "Valor de t tabelado",
                                  "Erro de amostragem absoluto",
                                  "Erro de amostragem relativo",
                                  "Erro requerido", "Nivel de significancia",
                                  "Coeficiente de variacao",
                                  "Pontos amostrados", "Intensidade amostral",
                                  "IC inferior para area total",
                                  "IC superior para area total"),
                     Estimativas=c(y, s2y,sy, Y, invt, eabs, erel, E*100,p*100,CV,n,nn,
                                   ICtotmin,ICtotmax),
                     Unidade=c("m3/ponto amostral", "m3/ponto amostral","m3/ponto amostral","m3/area total",
                               "","m3/ponto amostral","%", "%", "%","%","Pontos amostrais","Pontos amostrais","m3/area total","m3/area total"))
  }
  df[,2]<-format(round(df[,2],4),nsmall=4)

  par <- flextable(df)
  par <- align(par, align = "center")
  par <- align_text_col(par, align = "center")
  par<-autofit(par)

  if(n>=nn){

    if(pt==F){

      cat("\n------------------------------------------------------------------------------------\n")
      cat("The sampling intensity satisfies the required error of", E*100,"%, to a significance level of",p*100,"%.")
      cat("\nTherefore, it is not necessary to sample more sample points.\n")
      cat("------------------------------------------------------------------------------------")


    }else{
      cat("\n------------------------------------------------------------------------------------\n")
      cat("A intensidade amostral satisfaz o erro requerido de", E*100,"%, para um nivel de significancia de",p*100,"%.")
      cat("\nPortanto, nao e necessario amostrar mais pontos amostrais.\n")
      cat("------------------------------------------------------------------------------------")
    }
  }

  if(n<nn){

    if(pt==F){

      cat("\n------------------------------------------------------------------------------------\n")
      cat("The sample intensity does not satisfy the required error of", E*100,"%, to a significance level of",p*100,"%.")
      cat("\nTherefore, it is necessary to sample",ceiling(nn-n),"more sample points.\n")
      cat("------------------------------------------------------------------------------------")

    }else{

      cat("\n------------------------------------------------------------------------------------\n")
      cat("A intensidade amostral nao satisfaz o erro requerido de", E*100,"%, para um nivel de significancia de",p*100,"%.")
      cat("\nPortanto, e necessario amostrar mais",ceiling(nn-n),"pontos amostrais.\n")
      cat("------------------------------------------------------------------------------------")
    }
  }


  
    if(pt==T){
      doc <- read_docx() %>%
        body_add_par("Tabela 1. Parametros da amostragem pelo metodo de Bitterlich.", style = "centered") %>%
        body_add_flextable(par) %>% #tabela de parametros volume
        body_end_section_portrait() %>%

        body_add_break(pos="on") %>%
        body_add_par("Figura 1. Distribuicao diametrica.", style = "centered") %>%
        body_add_gg(diam,style="centered") %>% #distribuicao diametrica
        body_end_section_portrait() %>%

        body_add_break(pos="on") %>%
        body_add_par("Tabela 2. Volume, individuos e area basal por hectare, por ponto amostral.", style = "centered") %>%
        body_add_flextable(vahh) %>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Tabela 3. Volume lenhoso individual.", style = "centered") %>%
        body_add_flextable(anex) %>%
        body_end_section_landscape()

    }else{

      doc<-read_docx() %>%

        body_add_par("Table 1. Sampling parameters by the Bitterlich method.", style = "centered") %>%
        body_add_flextable(par) %>% #tabela de parametros volume
        body_end_section_portrait() %>%

        body_add_break(pos="on") %>%
        body_add_par("Figure 1. Diameter distribution", style = "centered") %>%
        body_add_gg(diam,style="centered") %>% #distribuicao diametrica
        body_end_section_portrait() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 2. Volume, individuals and basal area per hectare, by sample point.", style = "centered") %>%
        body_add_flextable(vahh) %>%
        body_end_section_landscape() %>%

        body_add_break(pos="on") %>%
        body_add_par("Table 3. Individual woody volume.", style = "centered")%>%
        body_add_flextable(anex) %>%
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
              `G, N e V/ha`=vahh,
              `distribuicao diam`=diam,
              `parametros vol`=par))
  }else{
    
    return(list(`individual vol`=anex,
                `G, N and V/ha`=vahh,
                `diam distribuction`=diam,
                `vol parameters`=par))
    
  }

}


