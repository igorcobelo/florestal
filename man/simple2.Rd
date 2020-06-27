\name{simple2}
\alias{simple2}
\docType{data}
\title{
Collected Data by a Simple Casual Sampling with Volume
}
\description{
Table format for input in the 'acs' function.
}
\usage{data("simple2")}
\format{
  A data frame with 116 observations on the following 6 variables.
  \describe{
    \item{\code{Plot}}{a numeric vector}
    \item{\code{Individual}}{a numeric vector}
    \item{\code{Specie}}{a character vector}
    \item{\samp{h (m)}}{a numeric vector}
    \item{\samp{d (cm)}}{a numeric vector}
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
library(florestal)

#loads the data

data(simple2)
head(simple2)

#calculates the parameters sampling

IF_acs <- acs(simple2, a=0.1, A=27)
}
\keyword{datasets}
