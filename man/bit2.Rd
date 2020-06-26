\name{bit2}
\alias{bit2}
\docType{data}
\title{
Collected Data by the Bitterlich Method with Volume
}
\description{
Table format for input in the 'bit' function.
}
\usage{data("bit2")}
\format{
  A data frame with 121 observations on the following 7 variables.
  \describe{
    \item{\samp{Sampled Point}}{a numeric vector}
    \item{\code{Individual}}{a numeric vector}
    \item{\code{Specie}}{a character vector}
    \item{\samp{h (m)}}{a numeric vector}
    \item{\samp{d (cm)}}{a numeric vector}
    \item{\samp{Radial Distance (m)}}{a numeric vector}
    \item{\samp{Volume (m3)}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
library(forestry)

#loads the data

data(bit2)
head(bit2)

#calculates the sampling parameters

bit(bit2,A=12,k=2)
}
\keyword{datasets}
