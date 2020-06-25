\name{census1}
\alias{census1}
\docType{data}
\title{
Collected Data by a Census without Volume
}
\description{
Table format for input in the 'indvol' function, with mens="census".
}
\usage{data("census1")}
\format{
  A data frame with 291 observations on the following 4 variables.
  \describe{
    \item{\code{Individual}}{a numeric vector}
    \item{\code{Specie}}{a character vector}
    \item{\samp{Height (m)}}{a numeric vector}
    \item{\samp{Diameter (cm)}}{a numeric vector}
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
#loads the data

data(census1)
head(census1)

#calculates the individual woody volume

indvol(census1, mens="census", veg="cerrado")
}
\keyword{datasets}
