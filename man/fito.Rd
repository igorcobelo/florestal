\name{fito}
\alias{fito}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Phytosociological Parameters
}
\description{
Calculates the phytosociological sampling parameters.
}
\usage{
fito(sp,plot,d,A,stratum=NULL,spivi=15,pt=TRUE,save=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{A vector or data frame with the species.}
  \item{plot}{A vector or data frame with the plots.}
  \item{d}{A vector or data frame with the diameter.}
  \item{A}{Total area, in hectares.}
  \item{stratum}{Optional. A vector or data frame with the strata.}
  \item{spivi}{number of species in the IVI plot. Default is 15.}       \item{pt}{Logical argument. If TRUE (default), the language of the outputs is Portuguese. If FALSE, the language is English.}
  \item{save}{Logical argument. If TRUE (default), a docx file will be saved in the temporary files directory (run 'tempdir()') (if save=TRUE) with the generated tables and graphics.
}

}

\value{
A list with an Importance Valor Index (IVI) plot and a phytosociological parameters table, and a docx file saved in the temporary files directory (run 'tempdir()') (if save=TRUE) (if save=TRUE).

}

\author{
Igor Cobelo Ferreira
}

\examples{
library(florestal)

#loads the data

data("simple1")
head(simple1)

#Calculate the phytosociological parameters

IF_fito <- fito(sp=simple1$Specie, plot=simple1$Plot, d= simple1$`Diameter (cm)`, A=27)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
\keyword{utilities}% __ONLY ONE__ keyword per line

