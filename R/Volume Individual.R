indvol <- function(x, d=NULL, h=NULL, myeq=NULL, veg=NULL, f=NULL, circ=F, ...) {
  
  ##tranformar circunferencia em diametro:
  if(circ==T){
    d<-d/pi
  }
  
  #Para myeq:
  x$`Volume (m3)` <- eval(substitute(myeq), envir=x); x
  
  
  #Para fator de forma (hf):
  
  if(!is.null(f)){
    x$`Volume (m3)`<-(pi*d^2/40000)*h*f
  }
  
  #Para veg:
  if(!is.null(veg)){
    
    if(veg=="cerrado"){
      x$`Volume (m3)`<-0.000065661*d^(2.475293)* h^(0.300022)
    }
    if(veg=="cer_regen"){
      x$`Volume (m3)`<-0.000058468*d^(2.160042)* h^(0.791208)
    }
    
    if(veg=="campo_cer"){
      x$`Volume (m3)`<-0.000024059*d^(2.506122)* h^(0.929214)
    }
    
    if(veg=="cerradao"){
      x$`V (m3)`<-0.000094001*d^(1.830398)* h^(0.960913)
    }
    
    if(veg=="mata_pri"){
      x$`Volume (m3)`<-0.00024502*d^(2.265786)* h^(0.150001)
    }
    
    if(veg=="mata_sec"){
      x$`Volume (m3)`<-0.000074230*d^(1.707348)* h^(1.16873)
    }
    
    if(veg=="mata_ciliar"){
      x$`Volume (m3)`<-0.000065607*d^(2.084676)* h^(0.752177)
    }
    
    if(veg=="mata_seca"){
      x$`Volume (m3)`<-0.000074924*d^(1.818557)* h^(1.061157)
    }
    
    if(veg=="trans_cipo"){
      x$`Volume (m3)`<-0.0000065231*d^(1.64498)* h^(2.234673)
    }
    
    if(veg=="trans_jaiba"){
      x$`Volume (m3)`<-0.000057947*d^(1.911894)* h^(1.0751)
    }
    
    if(veg=="caat_arborea"){
      x$`Volume (m3)`<-0.0000408657*d^(2.235528)* h^(0.823993)
    }
    
    if(veg=="caat_arbustiva"){
      x$`Volume (m3)`<-0.000075999*d^(2.016671)* h^(0.761177)
    }
    
  }
  
  return(x)
}
