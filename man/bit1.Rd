\name{bit1}
\alias{bit1}
\docType{data}
\title{
Collected Data by the Bitterlich Method without Volume
}
\description{
Table format for input in the 'indvol' function, with mens="bit".
}
\usage{data("bit1")}
\format{
  A data frame with 121 observations on the following 6 variables.
  \describe{
    \item{\samp{Sampled Point}}{a numeric vector}
    \item{\code{Individual}}{a numeric vector}
    \item{\code{Specie}}{a character vector}
    \item{\samp{Height (m)}}{a numeric vector}
    \item{\samp{Diameter (cm)}}{a numeric vector}
    \item{\samp{Radial Distance (m)}}{a numeric vector}
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
library(florestal)

#loads the data

data(bit1)
head(bit1)

#calculates the individual woody volume

bit2 <- indvol(bit1, mens="bit", f=0.7)
}
\keyword{datasets}
