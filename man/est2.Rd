\name{est2}
\alias{est2}
\docType{data}
\title{
Collected Data by the Stratified Casual Sampling with Volume
}
\description{
Table format for input in the 'ace' function.
}
\usage{data("est2")}
\format{
  A data frame with 116 observations on the following 7 variables.
  \describe{
    \item{\code{Stratum}}{a numeric vector}
    \item{\code{Plot}}{a numeric vector}
    \item{\code{Individual}}{a numeric vector}
    \item{\code{Specie}}{a character vector}
    \item{\samp{h (m)}}{a numeric vector}
    \item{\samp{d (cm)}}{a numeric vector}
    \item{\samp{Volume (m3)}}{a numeric vector}
  }
}

\examples{
library(florestal)

#loads the data

data(est2)
head(est2)

#calculates the sampling parameters

IF_ace <- ace(est2,a=0.1,aj=c(12.6,10.2))
}
\keyword{datasets}
