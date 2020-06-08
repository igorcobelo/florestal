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
fito(sp,plot,d,stratum=NULL,spivi=15,pt=T,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{A vector or data frame with the species.}
  \item{plot}{A vector or data frame with the plots.}
  \item{d}{A vector or data frame with the diameter.}
  \item{stratum}{Optional. A vector or data frame with the strata.}
  \item{spivi}{number of species in the IVI plot. Default is 15.}       \item{pt}{Logical argument. If TRUE (default), the language of the outputs is Portuguese. If FALSE, the language is English.}

}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list with a Importance Valor Index (IVI) plot and a phytosociological parameters table, and a docx file saved in the current directory.

}
\references{
}
\author{
Igor Cobelo Ferreira <cobelo.igor@gmail.com>
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

#Set current directory or just check.

getwd()

##Calculate the phytosociological parameters and the IVI plot:

fito(sp=IF1$Specie, plot=IF1$Plot, d=IF1$Diameter)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
