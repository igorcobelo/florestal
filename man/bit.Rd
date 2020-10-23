\name{bit}
\alias{bit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bitterlich Method Sampling
}
\description{
Calculates the Bitterlich method (or point sampling).
}
\usage{
bit(x,A,k,E=0.1,p=0.05,ampl=2,rn=FALSE,pt=TRUE,save=TRUE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame with seven columns: sample points in the first (in numerals), numbering of individuals in the second (in numerals), species in the third, height in the fourth (in meters), diameter in the fifth (in centimeters), radial distance in the sixth (in meters) and volume in the seventh (in cubic meters).}
  \item{A}{Total area, in hectares.}
  \item{k}{Basal area factor.}
  \item{E}{Required error, in decimal value. Default is 0.1.}
  \item{p}{Significance level, in decimal value. Default is 0.05.}
  \item{ampl}{Amplitude of diameter class range. Default is 2.}
  \item{rn}{Logical argument. If TRUE, calculates the sample intensity once again, with the degrees of freedom from the previous result.}
 \item{pt}{Logical argument. If TRUE (default), the language of the outputs is Portuguese. If FALSE, the language is English.}
 \item{save}{Logical argument. If TRUE (default), a docx file will be saved in the temporary files directory (run 'tempdir()') (if save=TRUE) with the generated tables and graphics.}

}

\value{
A list of tables and graphs, and a docx file saved in the temporary files directory (run 'tempdir()') (if save=TRUE). The console displays the sampling intensity.
}
\references{
Bitterlich, W. (1948). Die Winkelzahlprobe. Allg. Forst-u. Holzwirtsch. Ztg., 59 (1/2): 4-5.

Pellico  Netto,  S.;  Brena,  D. (1997).  Inventario  florestal.  Curitiba:  Universidade  Federal  do  Parana, 316 p.
}
\author{
Igor Cobelo Ferreira
}
\note{
The function calculates the critical distance to check for inclusion or exclusion of doubtful trees, being included in the sample only those with radial distance less than or equal to the critical distance. Even if there are no doubtful trees, the radial distance column (fifth column) is required, in blank.
}

\examples{
library(florestal)

#loads the data

data("bit2")
head(bit2)

#Calculate the sampling parameters

bit(bit2,A=12,k=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
\keyword{utilities}% __ONLY ONE__ keyword per line
