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
acs(x,A,a,E=0.1,p=0.05,rn=T,pt=T,prot=NULL,ampl=5,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame with six columns: plots in the first (in numerals), numbering of individuals in the second (in numerals), species in the third, height in the fourth (in meters), diameter in the fifth (in centimeters) and volume in the sixth (in cubic meters).}
   \item{A}{Total area, in hectares.}
    \item{a}{Plot area, in hectares.}
    \item{E}{Required error, in decimal value. Default is 0.1.}
   \item{p}{Significance level, in decimal value. Default is 0.05.}
  \item{rn}{Logical argument. If TRUE (default) calculates the sample intensity once again, with the degrees of freedom from the previous result.}
\item{pt}{Logical argument. If TRUE (default), the language of the outputs is Portuguese. If FALSE, the language is English.}
  \item{prot}{Optional. Protected species.}
  \item{ampl}{Amplitude of diameter class range. Default is 5.}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of tables and graphs, and a docx file saved in the current directory. The console displays the sampling intensity.
}
\references{
Pellico  Netto,  S.;  Brena,  D. (1997).  Inventario  florestal.  Curitiba:  Universidade  Federal  do  Parana, 316 p.
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

setwd("C:/Users/Igor/Documents")
getwd()

#Calculate the sampling parameters for a required error of 10 percent and a significance level of 5 percent.

acs(x,A=27,a=0.1,E=0.1,p=0.05)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
