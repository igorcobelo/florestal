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
  A data frame with 291 observations on the following 6 variables.
  \describe{
    \item{\code{Stratum}}{a numeric vector}
    \item{\code{Plot}}{a numeric vector}
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
library(forestry)

#loads the data

data(est1)
head(est1)

#calculates the individual woody volume
#create an object for each stratum and join with 'rbind'

IF_strat1 <- indvol(est1[est1$Stratum==1,], mens="strata", veg = "cerrado")
IF_strat2 <- indvol(est1[est1$Stratum==2,], mens="strata", veg = "mata_sec")
IF_strat3 <- indvol(est1[est1$Stratum==3,], mens="strata", veg = "mata_pri")

IF_strata <- rbind(IF_strat1, IF_strat2, IF_strat3)}
\keyword{datasets}
