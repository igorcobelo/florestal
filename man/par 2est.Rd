\name{dest}
\alias{dest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Two-Stage Sampling and Phytosociological Parameters
}
\description{
Calculates the two-stage and phytosociological sampling parameters.
}
\usage{
dest(x,A,an,am,E=0.1,p=0.05,c=1,rn=F,pt=T,prot=NULL,ampl=5,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame with seven columns: primary units in the first (in numerals), secondary units in the second (in numerals), numbering of individuals in the third (in numerals), species in the fourth, height in the fifth (in meters), diameter in the sixth (in centimeters) and volume in the seventh (in cubic meters).}
  \item{A}{Total area, in hectares.}
  \item{an}{Area of primary units, in hectares.}
  \item{am}{Area of secondary units, in hectares.}
  \item{E}{Required error, in decimal value. Default is 0.1.}
  \item{p}{Significance level, in decimal value. Default is 0.05.}
  \item{c}{Ratio between average displacement cost and average measurement cost (to be used in calculating the sample intensity of secondary units), in decimal value. Default is 1.}
  \item{rn}{Logical argument. If TRUE (default) calculates the sample intensity once again, with the degrees of freedom from the previous result.}
\item{pt}{Logical argument. If TRUE (default), the language of the outputs is Portuguese. If FALSE, the language is English.}
\item{prot}{Optional. Protected species.}
\item{ampl}{Amplitude of diameter class range. Default is 5.}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of tables and graphs, and a docx file saved in the current directory. The console displays the sampling intensity and whether there is significant difference between the primary units from the F test of the single-factor ANOVA with 95 percent probability.
}
\references{
Pellico  Netto,  S.;  Brena,  D. (1997).  Inventario  florestal.  Curitiba:  Universidade  Federal  do  Parana, 316 p.
}
\author{
Igor Cobelo Ferreira <cobelo.igor@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

#Set current directory or just check.

setwd("C:/Users/Igor/Documents")
getwd()

#Calculate the sampling parameters for a total area of 1,000,000 ha, with
10,000 ha primary and 0.25 ha secondary units, for a required error of 10
percent and a significance level of 5 percent.

dest(x,A=1000000,an=10000,am=0.25,E=0.1,p=0.05)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
