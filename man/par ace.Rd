\name{ace}
\alias{ace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stratified Casual Sampling and Phytosociological Parameters
}
\description{
Calculates the stratified casual and phytosociological sampling parameters.
}
\usage{
ace(x,a,aj,E=0.1,p=0.05,rn=F,ampl=5,prop=F,rn=F,spivi=15,un=F,pt=T,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame with seven columns: strata in the first (in numerals), plots in the second (in numerals), numbering of individuals in the third (in numerals), species in the fourth, height in the fifth (in meters), diameter in the sixth (in centimeters) and volume in the seventh (in cubic meters).}
  \item{a}{Plot area, in hectares.}
  \item{aj}{Area of each stratum, in hectares.}
  \item{E}{Required error, in decimal value. Default is 0.1.}
  \item{p}{Significance level, in decimal value. Default is 0.05.}
  \item{ampl}{Amplitude of diameter class range. Default is 5.}
  \item{prop}{Logical argument. If FALSE (default), the allocation of plots by stratum will follow Neyman's optimal allocation method. If TRUE, will follow the proportional allocation by area.}
     \item{rn}{Logical argument. If TRUE (default) calculates the sample intensity once again, with the degrees of freedom from the previous result.}
    \item{spivi}{number of species in the IVI plot. Default is 15.} 
    \item{un}{Logical argument. If TRUE, the species column have an unique specie, as a planted forest. If FALSE (default), have multiple species (more than one).}
    \item{pt}{Logical argument. If TRUE (default), the language of the outputs is Portuguese. If FALSE, the language is English.}

}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of tables and graphs, and a docx file saved in the current directory. The console displays the sampling intensity and whether there is significant difference between the strata from the F test of the single-factor ANOVA and a Tukey's test for significant differences between strata, with 95 per cent confidence.

}
\references{
Pellico  Netto,  S.;  Brena,  D. (1997).  Inventario  florestal.  Curitiba:  Universidade  Federal  do  Parana, 316 p.
}
\author{
Igor Cobelo Ferreira
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

#loads the data
data("est5")
head(est5)

#Calculate the sampling parameters for two strata with areas of 12.6 and 10.2 hectares respectively, for a required error of 10 percent and a significance level of 5 percent. The allocation of plots by stratum follows the optimal allocation method. Each plot have 0.1 hectare.

IF_ace <- ace(est5,a=0.1,aj=c(12.6,10.2))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
