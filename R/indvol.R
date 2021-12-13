indvol <- function(x,mens="plot",myeq=NULL,veg=NULL,f=NULL,circ=F){

  df <- x

  #PLOT
  if (mens == "plot") {
    colnames(df)[1] <- "parcela"
    colnames(df)[2] <- "ind"
    colnames(df)[3] <- "spp"
    colnames(df)[4] <- "h"
    colnames(df)[5] <- "d"
    d <- df[, 5]
    h <- df[, 4]
  }

  #STRATA
  if (mens == "strata") {
    colnames(df)[1] <- "estrato"
    colnames(df)[2] <- "parcela"
    colnames(df)[3] <- "ind"
    colnames(df)[4] <- "spp"
    colnames(df)[5] <- "h"
    colnames(df)[6] <- "d"
    d <- df[, 6]
    h <- df[, 5]
  }

  #BIT
  if (mens == "bit") {
    colnames(df)[1] <- "pontos"
    colnames(df)[2] <- "ind"
    colnames(df)[3] <- "spp"
    colnames(df)[4] <- "h"
    colnames(df)[5] <- "d"
    colnames(df)[6] <- "radial distance"
    d <- df[, 5]
    h <- df[, 4]
  }

  #CENSUS
  if (mens == "census") {
    colnames(df)[1] <- "ind"
    colnames(df)[2] <- "spp"
    colnames(df)[3] <- "h"
    colnames(df)[4] <- "d"
    d <- df[, 4]
    h <- df[, 3]
  }

  #Transform circ to diameter
  if (circ == TRUE) {
    d <- d/pi
  }

  #Diametro equivalente
  IF <- df %>% group_by(ind) %>% mutate(d = sqrt(sum(d^2, na.rm = T))) %>%
    filter (!duplicated(ind))

  d <- IF$d
  h <- IF$h

  #Equacao propria
  if (!(is.null(myeq))) {
    IF$Volume <- eval(parse(text = as.character(myeq)))
  }

  #Fator de forma
  if (!(is.null(f))) {
    IF$Volume <- ((pi * d^2)/40000) * h * f
  }

  #Equacoes IFN
  if (!is.null(veg)) {
    if (veg == "matas5-10_df") {
      IF$Volume <- exp(-9.7751 + 2.2403 *
                         log(d) + 0.6308 * log(h))
    }
    if (veg == "matas>10_df") {
      IF$Volume <- exp(-9.3436 + 2.0437 *
                         log(d) + 0.7509 * log(h))
    }
    if (veg == "cerradoss_df") {
      IF$Volume <- 0.000109 * d^2 + 4.51e-05 *
        d^2 * h
    }
    if (veg == "ceara") {
      IF$Volume <- exp(-9.5934 + 2.04417 *
                         log(d) + 0.94531 * log(h))
    }
    if (veg == "ombmista5-10_pr") {
      IF$Volume <- exp(-8.87591 + 1.892219 *
                         log(d) + 0.739038 * log(h))
    }
    if (veg == "ombmista>10_pr") {
      IF$Volume <- exp((-17.96 + 0.96 * log(d^2) +
                          0.76 * log(h))) * 1000
    }
    if (veg == "ombdensa>5_pr") {
      IF$Volume <- exp(-10.045586 + 2.349493 *
                         log(d) + 0.640598 * log(h))
    }
    if (veg == "araucaria>5_pr") {
      IF$Volume <- 7.7e-05 * d^1.85794 *
        h^0.93919
    }
    if (veg == "ombdensa_rj") {
      IF$Volume <- exp(-9.9752493252 + 2.1719145688 *
                         log(d) + 0.8083667085 * log(h))
    }
    if (veg == "estacionalsemi_rj") {
      IF$Volume <- exp(-9.7394993677 + 2.3219001043 *
                         log(d) + 0.5645027997 * log(h))
    }
    if (veg == "estacionaldeci_rj") {
      IF$Volume <- exp(-9.7677720672 + 2.4886704462 *
                         log(d) + 0.4406921533 * log(h))
    }
    if (veg == "restinga_rj") {
      IF$Volume <- exp(-9.42719 + 1.969 *
                         log(d) + 0.831852 * log(h))
    }
    if (veg == "rn") {
      IF$Volume <- exp(-9.5934 + 2.04417 *
                         log(d) + 0.94531 * log(h))
    }
    if (veg == "rs5-10") {
      IF$Volume <- exp(-8.87591 + 1.892219 *
                         log(d) + 0.739038 * log(h))
    }
    if (veg == "rs>10") {
      IF$Volume <- exp((-17.96 + 0.96 * log(d^2) +
                          0.76 * log(h))) * 1000
    }
    if (veg == "estacionaldeci>10_sc") {
      IF$Volume <- exp((-17.68 + 0.95 * log(d^2) +
                          0.67 * log(h))) * 1000
    }
    if (veg == "ombdensa>10_sc") {
      IF$Volume <- exp((-17.75 + 0.98 * log(d^2) +
                          0.57 * log(h))) * 1000
    }
    if (veg == "ombmista>10_sc") {
      IF$Volume <- exp((-17.96 + 0.96 * log(d^3) +
                          0.76 * log(h))) * 1000
    }
    if (veg == "caatinga_se") {
      IF$Volume <- (-9.5934 + 2.04417 * log(d) +
                      0.94531 * log(h))
    }
    if (veg == "atlantica_se") {
      IF$Volume <- 7.423e-05 * d^1.707348 *
        h^1.16873
    }
    if (veg == "cer_regen") {
      IF$Volume <- 5.8468e-05 * d^2.160042 *
        h^0.791208
    }
    if (veg == "campo_cer") {
      IF$Volume <- 2.4059e-05 * d^(2.506122) *
        h^(0.929214)
    }
    if (veg == "cerradao") {
      IF$Volume <- 9.4001e-05 * d^(1.830398) *
        h^(0.960913)
    }
    if (veg == "mata_pri") {
      IF$Volume <- 0.00024502 * d^(2.265786) *
        h^(0.150001)
    }
    if (veg == "mata_sec") {
      IF$Volume <- 7.423e-05 * d^(1.707348) *
        h^(1.16873)
    }
    if (veg == "mata_ciliar") {
      IF$Volume <- 6.5607e-05 * d^(2.084676) *
        h^(0.752177)
    }
    if (veg == "mata_seca") {
      IF$Volume <- 7.4924e-05 * d^(1.818557) *
        h^(1.061157)
    }
    if (veg == "trans_cipo") {
      IF$Volume <- 6.5231e-06 * d^(1.64498) *
        h^(2.234673)
    }
    if (veg == "trans_jaiba") {
      IF$Volume <- 5.7947e-05 * d^(1.911894) *
        h^(1.0751)
    }
    if (veg == "caat_arborea") {
      IF$Volume <- 4.08657e-05 * d^(2.235528) *
        h^(0.823993)
    }
    if (veg == "caat_arbustiva") {
      IF$Volume <- 7.5999e-05 * d^(2.016671) *
        h^(0.761177)
    }}

  return(IF)
}
