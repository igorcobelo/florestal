indvol <- function(x, mens="plot", vol=FALSE, myeq=NULL, veg=NULL, f=NULL, circ=FALSE) {
  
  
  if(mens=="plot"){
    colnames(x)[5]<-"d"
    colnames(x)[4]<-"h"
    
    d<-x[,5]
    h<-x[,4]
    
    ##tranformar circunferencia em diametro:
    if(circ==TRUE){
      d<-d/pi
    }
    
    #DIAMETRO MEDIO QUADRATICO
    result<-as.numeric()
    for(i in (x[,2])){
      result[i]<-sqrt(sum(subset(x[,5]^2, x[,2]==i))/length(subset(x[,5]^2, x[,2]==i)))
    }
    result<-as.data.frame(result)
    
    d<- result[!(is.na(result))]
    
    #MAIOR ALTURA
    result2<-as.numeric()
    for(i in 1:max(x[,2])){
      result2[i]<-max(subset(x[,4], x[,2]==i))
    }
    result2<-as.data.frame(result2)
    
    h<- as.numeric(result2[!(result2==-Inf)])
    row<- as.numeric(rownames(result2)[!(result2==-Inf)])
    
    #PARCELAS
    result3<-as.numeric()
    for(i in 1:max(x[,2])){
      result3[i]<-max(subset(x[,1], x[,2]==i))
    }
    result3<-as.data.frame(result3)
    
    p<- as.numeric(result3[!(result3==-Inf)])
    
    #ESPECIES
    x[,3]<-as.character(x[,3])
    
    result4<-as.character()
    suppressWarnings({for(i in x[,2]){
      result4[i]<-subset(x[,3], x[,2]==i)
    }})
    result4<-as.data.frame(result4)
    
    s<- result4[!(is.na(result4))]
    
    #VOLUME
    if(vol==TRUE){
      
      resultvol<-as.numeric()
      for(i in x[,2]){
        resultvol[i]<-sum(subset(x[,6], x[,2]==i))
      }
      resultvol<-as.data.frame(resultvol)
      
      volume<- resultvol[!(is.na(resultvol))]
      
      plan<-data.frame(p,row,s,h,d,volume, stringsAsFactors = F)
      
    }else{
      plan<-data.frame(p,row,s,h,d, stringsAsFactors = F)
    }
    
  }
  
  if(mens=="strata"){
    #para estratificada
    
    colnames(x)[6]<-"d"
    colnames(x)[5]<-"h"
    
    d<-x[,6]
    h<-x[,5]
    
    ##tranformar circunferencia em diametro:
    if(circ==TRUE){
      d<-d/pi
    }
    
    #DIAMETRO MEDIO QUADRATICO
    result<-as.numeric()
    for(i in (x[,3])){
      result[i]<-sqrt(sum(subset(x[,6]^2, x[,3]==i))/length(subset(x[,6]^2, x[,3]==i)))
    }
    result<-as.data.frame(result)
    
    d<- result[!(is.na(result))]
    
    #MAIOR ALTURA
    result2<-as.numeric()
    suppressWarnings({for(i in 1:max(x[,3])){
      result2[i]<-max(subset(x[,5], x[,3]==i))
    }})
    result2<-as.data.frame(result2)
    
    h<- as.numeric(result2[!(result2==-Inf)])
    row<- as.numeric(rownames(result2)[!(result2==-Inf)])
    
    #PARCELAS
    result3<-as.numeric()
    suppressWarnings({for(i in 1:max(x[,3])){
      result3[i]<-max(subset(x[,2], x[,3]==i))
    }})
    result3<-as.data.frame(result3)
    
    p<- as.numeric(result3[!(result3==-Inf)])
    
    #ESPECIES
    x[,4]<-as.character(x[,4])
    
    result4<-as.character()
    suppressWarnings({for(i in x[,3]){
      result4[i]<-subset(x[,4], x[,3]==i)
    }})
    result4<-as.data.frame(result4)
    
    s<- result4[!(is.na(result4))]
    
    #ESTRATO
    result5<-as.numeric()
    suppressWarnings({for(i in 1:max(x[,3])){
      result5[i]<-max(subset(x[,1], x[,3]==i))
    }})
    result5<-as.data.frame(result5)
    
    e<- as.numeric(result5[!(result5==-Inf)])
    
    #VOLUME
    if(vol==TRUE){
      
      resultvol<-as.numeric()
      for(i in x[,3]){
        resultvol[i]<-sum(subset(x[,7], x[,3]==i))
      }
      resultvol<-as.data.frame(resultvol)
      
      volume<- resultvol[!(is.na(resultvol))]
      
      plan<-data.frame(e,p,row,s,h,d,volume, stringsAsFactors = F)
      
    }else{
      plan<-data.frame(e,p,row,s,h,d, stringsAsFactors = F)
    }
  }
  
  
  if(mens=="census"){
    #para censo
    
    colnames(x)[4]<-"d"
    colnames(x)[3]<-"h"
    
    d<-x[,4]
    h<-x[,3]
    
    ##tranformar circunferencia em diametro:
    if(circ==TRUE){
      d<-d/pi
    }
    
    #DIAMETRO MEDIO QUADRATICO
    result<-as.numeric()
    for(i in (x[,1])){
      result[i]<-sqrt(sum(subset(x[,4]^2, x[,1]==i))/length(subset(x[,4]^2, x[,1]==i)))
    }
    result<-as.data.frame(result)
    
    d<- result[!(is.na(result))]
    
    #MAIOR ALTURA
    result2<-as.numeric()
    for(i in 1:max(x[,1])){
      result2[i]<-max(subset(x[,3], x[,1]==i))
    }
    result2<-as.data.frame(result2)
    
    h<- as.numeric(result2[!(result2==-Inf)])
    row<- as.numeric(rownames(result2)[!(result2==-Inf)])
    
    
    #ESPECIES
    x[,2]<-as.character(x[,2])
    
    result4<-as.character()
    suppressWarnings({for(i in x[,1]){
      result4[i]<-subset(x[,2], x[,1]==i)
    }})
    result4<-as.data.frame(result4)
    
    s<- result4[!(is.na(result4))]
    
    #VOLUME
    if(vol==TRUE){
      
      resultvol<-as.numeric()
      for(i in x[,1]){
        resultvol[i]<-sum(subset(x[,5], x[,1]==i))
      }
      resultvol<-as.data.frame(resultvol)
      
      volume<- resultvol[!(is.na(resultvol))]
      
      plan<-data.frame(row,s,h,d,volume, stringsAsFactors = F)
      
    }else{
      plan<-data.frame(row,s,h,d, stringsAsFactors = F)
    }
  }
  
  if(mens=="bit"){
    colnames(x)[5]<-"d"
    colnames(x)[4]<-"h"
    
    d<-x$d
    h<-x$h
    
    ##tranformar circunferencia em diametro:
    if(circ==TRUE){
      d<-d/pi
    }
    
    plan<-x
  }
  
  
  if(vol==FALSE){
    #Para myeq:
    
    if(!(is.null(myeq))){
      
      d<-plan$d
      h<-plan$h
      
      plan$`Volume (m3)` <- eval(parse(text = as.character(myeq)))
      
    }
    
    #Para fator de forma (hf):
    
    if(!(is.null(f))){
      plan$`Volume (m3)`<-(pi*d^2/40000)*h*f
    }
    
    #Para veg:
    if(!is.null(veg)){
      
      #DISTRITO FEDERAL
      if(veg=="matas5-10_df"){
        plan$`Volume (m3)`<-exp(-9.7751+2.2403*log(d)+0.6308*log(h))
      }
      
      if(veg=="matas>10_df"){
        plan$`Volume (m3)`<-exp(-9.3436+2.0437*log(d)+0.7509*log(h))
      }
      
      if(veg=="cerradoss_df"){
        plan$`Volume (m3)`<- 0.000109*d^2+0.0000451*d^2*h
      }
      
      #CEARA
      if(veg=="ceara"){
        plan$`Volume (m3)`<-exp(-9.59340+2.04417*log(d)+0.94531*log(h))
      }
      
      #PARANA
      if(veg=="ombmista5-10_pr"){
        plan$`Volume (m3)`<-exp(-8.875910+1.892219*log(d)+0.739038*log(h))
      }
      
      if(veg=="ombmista>10_pr"){
        plan$`Volume (m3)`<-exp((-17.96+0.96*log(d^2)+0.76*log(h))/1000)
      }
      
      if(veg=="ombdensa>5_pr"){
        plan$`Volume (m3)`<-exp(-10.045586+2.349493*log(d)+0.640598*log(h))
      }
      
      if(veg=="araucaria>5_pr"){
        plan$`Volume (m3)`<- 0.000077*d^1.85794*h^0.93919
      }
      
      #RIO DE JANEIRO
      if(veg=="ombdensa_rj"){
        plan$`Volume (m3)`<-exp(-9.9752493252+2.1719145688*log(d)+0.8083667085*log(h))
      }
      
      if(veg=="estacionalsemi_rj"){
        plan$`Volume (m3)`<-exp(-9.7394993677+2.3219001043*log(d)+0.5645027997*log(h))
      }
      
      if(veg=="estacionaldeci_rj"){
        plan$`Volume (m3)`<-exp(-9.7677720672+2.4886704462*log(d)+0.4406921533*log(h))
      }
      
      if(veg=="restinga_rj"){
        plan$`Volume (m3)`<-exp(-9.42719+1.96900*log(d)+0.831852*log(h))
      }
      
      #RIO GRANDE DO NORTE
      if(veg=="rn"){
        plan$`Volume (m3)`<-exp(-9.59340+2.04417*log(d)+0.94531*log(h))
      }
      
      #RIO GRANDE DO SUL
      if(veg=="rs5-10"){
        plan$`Volume (m3)`<-exp(-8.875910+1.892219*log(d)+0.739038*log(h))
      }
      
      if(veg=="rs>10"){
        plan$`Volume (m3)`<-exp((-17.96+0.96*log(d^2)+0.76*log(h))/1000)
      }
      
      #SANTA CATARINA
      if(veg=="estacionaldeci>10_sc"){
        plan$`Volume (m3)`<-exp((-17.68+0.95*log(d^2)+0.67*log(h))/1000)
      }
      
      if(veg=="ombdensa>10_sc"){
        plan$`Volume (m3)`<-exp((-17.75+0.98*log(d^2)+0.57*log(h))/1000)
      }
      
      if(veg=="ombmista>10_sc"){
        plan$`Volume (m3)`<-exp((-17.96+0.96*log(d^3)+0.76*log(h))/1000)
      }
      
      #SERGIPE
      if(veg=="caatinga_se"){
        plan$`Volume (m3)`<-(-9.59340+2.04417*log(d)+0.94531*log(h))
      }
      
      if(veg=="atlantica_se"){
        plan$`Volume (m3)`<- 0.000074230*d^1.707348*h^1.16873
      }
      
      #CETEC
      if(veg=="cer_regen"){
        plan$`Volume (m3)`<-0.000058468*d^2.160042*h^0.791208
      }
      
      if(veg=="campo_cer"){
        plan$`Volume (m3)`<-0.000024059*d^(2.506122)* h^(0.929214)
      }
      
      if(veg=="cerradao"){
        plan$`V (m3)`<-0.000094001*d^(1.830398)* h^(0.960913)
      }
      
      if(veg=="mata_pri"){
        plan$`Volume (m3)`<-0.00024502*d^(2.265786)* h^(0.150001)
      }
      
      if(veg=="mata_sec"){
        plan$`Volume (m3)`<-0.000074230*d^(1.707348)* h^(1.16873)
      }
      
      if(veg=="mata_ciliar"){
        plan$`Volume (m3)`<-0.000065607*d^(2.084676)* h^(0.752177)
      }
      
      if(veg=="mata_seca"){
        plan$`Volume (m3)`<-0.000074924*d^(1.818557)* h^(1.061157)
      }
      
      if(veg=="trans_cipo"){
        plan$`Volume (m3)`<-0.0000065231*d^(1.64498)* h^(2.234673)
      }
      
      if(veg=="trans_jaiba"){
        plan$`Volume (m3)`<-0.000057947*d^(1.911894)* h^(1.0751)
      }
      
      if(veg=="caat_arborea"){
        plan$`Volume (m3)`<-0.0000408657*d^(2.235528)* h^(0.823993)
      }
      
      if(veg=="caat_arbustiva"){
        plan$`Volume (m3)`<-0.000075999*d^(2.016671)* h^(0.761177)
      }
      
    }
    
  }
  
  
  colnames(plan)[1]<-colnames(x)[1]
  colnames(plan)[2]<-colnames(x)[2]
  colnames(plan)[3]<-colnames(x)[3]
  colnames(plan)[4]<-colnames(x)[4]
  
  if(mens=="plot"){
    colnames(plan)[5]<-colnames(x)[5]
  }
  
  if(mens=="strata"){
    colnames(plan)[6]<-colnames(x)[6]
  }
  
  return(plan)
  
}
