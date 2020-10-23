\name{acs}
\alias{acs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple Casual Sampling and Phytosociological Parameters
}
\description{
Calculates the simple casual and phytosociological sampling parameters.
}
\usage{
acs(x,A,a,E=0.1,p=0.05,prot=NULL,ampl=2,rn=FALSE,spivi=15,un=FALSE,pt=TRUE,save=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame with six columns: plots in the first (in numerals), numbering of individuals in the second (in numerals), species in the third, height in the fourth (in meters), diameter in the fifth (in centimeters) and volume in the sixth (in cubic meters).}
    \item{A}{Total area, in hectares.}
    \item{a}{Plot area, in hectares.}
    \item{E}{Required error, in decimal value. Default is 0.1.}
    \item{p}{Significance level, in decimal value. Default is 0.05.}
    \item{prot}{Optional. Protected species.}
    \item{ampl}{Amplitude of diameter class range. Default is 2.}
    \item{rn}{Logical argument. If TRUE, calculates the sample intensity once again, with the degrees of freedom from the previous result.}
    \item{spivi}{number of species in the IVI plot. Default is 15.} 
    \item{un}{Logical argument. If TRUE, the species column have an unique specie, as a planted forest. If FALSE (default), have multiple species (more than one).}
    \item{pt}{Logical argument. If TRUE (default), the language of the outputs is Portuguese. If FALSE, the language is English.}
  \item{save}{Logical argument. If TRUE (default), a docx file will be saved in the temporary files directory (run 'tempdir()') (if save=TRUE) with the generated tables and graphics.}

}

\value{
A list of tables and plots, and a docx file saved in the temporary files directory (run 'tempdir()') (if save=TRUE). The console displays the sampling intensity.
}
\references{
Pellico  Netto,  S.;  Brena,  D. (1997).  Inventario  florestal.  Curitiba:  Universidade  Federal  do  Parana, 316 p.
}
\author{
Igor Cobelo Ferreira
}

\examples{
library(florestal)

#loads the data

data("simple2")
head(simple2)

#Calculate the sampling parameters

IF_acs <- acs(simple2,A=27,a=0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
\keyword{utilities}% __ONLY ONE__ keyword per line
