
fito <- function(sp, plot, d, stratum=NULL, spivi=15, pt=T){

  
if(is.null(stratum)){   #sem estrato
  
Especie<-sp
parcela<-plot
d<-d

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
  dtt$`FA (%)`<-dtt$UA/max(plot,na.rm=T)*100
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
  dtt$`AF (%)`<-dtt$SU/max(plot,na.rm=T)*100
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

fitot <- flextable(dtt3)
fitot<-autofit(fitot)
fitot <- align(fitot, align = "center", part="all")
fitot<-italic(fitot,j=1)


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
    xlab("\nEspecies") + ylab("Indice de Valor de Importancia (%)\n") +
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
    xlab("\nSpecies") + ylab("Importance Value Index (%)\n") +
    labs(fill = "Parameters") +
    theme(axis.text.y = element_text(face = "italic",size=8), legend.title=element_blank(),legend.justification = "center" ,legend.text=element_text(size=10),
          axis.text.x= element_text(size=10), axis.title.x=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.position="bottom",legend.direction = "horizontal")
  
  p2 <- gg2 + theme(legend.position = "none")
  le1 <- cowplot::get_legend(gg2)
  gg3<-cowplot::plot_grid(p2, le1,nrow = 2,rel_heights = c(1, 0.2))
  
}

}else{
  
#ESTRATIFICADA
  

  Estrato<-stratum
  Especie<-sp
  parcela<-plot
  d<-d
  
  
  
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
  
  
  #Grafico fito
  
  
  if(pt==T){
    
    data <- dtt_g[c(1, 2, 7, 9, 11)] %>%
      gather(Parametros, b, -Estrato, -Especie) %>%
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
    data <- dtt2[c(1, 2, 7, 9, 11)] %>%
      gather(Parameters, b, -Stratum, -Specie) %>%
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
      xlab("\nSpecies") + ylab("Importance Value Index (%)\n") +
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
}

#SALVAR

if(pt==T){
doc<-read_docx() %>%
  
  body_add_par("Tabela 1. Parametros fitossociologicos, em que: n = quantidade de individuos amostrados; G = area basal; UA = quantidade de unidades amostrais; DA (n/ha) = densidade absoluta; DR (%) = densidade relativa; DoA (G/ha) = dominancia absoluta; DoR (%) = dominancia relativa; FA (%) = frequencia absoluta; FR (%) = frequencia relativa; IVI (%) = Indice de Valor de Importancia.", style = "centered") %>%
  body_add_flextable(fitot) %>%
  body_end_section_landscape() %>%
  
  body_add_gg(gg3,style="centered", height=3.5,width=6) %>%
  body_add_par("Figura 1. Indice de Valor de Importancia por especie (soma de densidade relativa, dominancia relativa e frequencia relativa).", style = "centered") %>%
  body_end_section_portrait()

 fileout <- tempfile(fileext = ".docx")
  fileout <- paste(getwd(),"/Fitossociologia - ",nm,".docx",sep="")
  print(doc, target = fileout)

}else{

  doc<-read_docx() %>%
    
    body_add_par("Tabela 1. Phytosociological parameters, where: n = number of sampled individuals; G = basal area; SU = number of sample units; AD (n/ha) = absolute density; RD (%) = relative density; ADo (G/ha) = absolute dominance; RDo (%) = relative dominance; AF (%) = absolute frequency; RF (%) = relative frequency; IVI (%) = Importance Value Index.", style = "centered") %>%
    body_add_flextable(fitot) %>%
    body_end_section_landscape() %>%
    
    body_add_gg(gg3,style="centered", height=6,width=6) %>%
    body_add_par("Figura 1. Importance Value Index by specie (sum of relative density, relative dominancy and relative frequency).", style = "centered") %>%
    body_end_section_portrait()
  
  fileout <- tempfile(fileext = ".docx")
  fileout <- paste(getwd(),"/Phytosociology - ",nm,".docx",sep="")
  print(doc, target = fileout)
  
  
}
  
  if(pt==T){
    return(list(`grafico ivi`=gg3,
                `parametros fito`=fitot))
                
  }else{
    
    return(list(`ivi plot`=gg3,
                `phyto parameters`=fitot))     
  }
  
  }