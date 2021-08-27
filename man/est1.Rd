\name{est1}
\alias{est1}
\docType{data}
\title{
Collected Data by the Stratified Casual Sampling without Volume
}
\description{
Table format for input in the 'indvol' function, with mens="strata".
}
\usage{data("est1")}
\format{
  A data frame with 120 observations on the following 6 variables.
  \describe{
    \item{\code{Stratum}}{a numeric vector}
    \item{\code{Plot}}{a numeric vector}
    \item{\code{Individual}}{a numeric vector}
    \item{\code{Specie}}{a character vector}
    \item{\samp{Height (m)}}{a numeric vector}
    \item{\samp{Diameter (cm)}}{a numeric vector}
  }
}

\examples{
library(florestal)

#loads the data

data(est1)
head(est1)

#calculates the individual woody volume
#create an object for each stratum and join with 'rbind'

IF_e1<-indvol(est1[est1$Stratum==1,],mens="strata",veg="cerradoss_df")
IF_e2<-indvol(est1[est1$Stratum==2,],mens="strata",veg="matas>10_df")

est2<-rbind(IF_e1,IF_e2)
}
\keyword{datasets}
