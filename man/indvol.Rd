\name{indvol}
\alias{indvol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Individual Woody Volume and Joins Multiple Shafts

}
\description{
Calculates the individual woody volume from a manually entered equation, an equation from a listed vegetable formation or a form factor. It joins multiple shafts of the same individual through the mean square diameter and greater height.

}
\usage{
indvol(x, mens="plot", vol=FALSE, myeq=NULL, veg=NULL, f=NULL, circ=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame.
  
  - If have strata (mens="strata"), there must be six columns: strata in the first (in numerals), plots in the second (in numerals), numbering of individuals in the third (in numerals), species in the fourth (in character), height in the fifth (in meters) and diameter in the sixth (in centimeters).
  
  - If have only plots (mens="plot"), follows the same order but without the strata column (plot, individuals, species, height and diameter).
  
  - If have not plots (mens="census"), follows the same order but without the strata and plot columns (individuals, species, height and diameter).
  
  - If its sampling by the Bitterlich method (mens="bit"), the order of the columns is as in plots, but it is suggested that the sixth column is the radial distance (point, individuals, specie, height, diameter and radial distance).
  
  - If the goal is only to join multiple shafts that already contain volume, the volume should be in the last column (vol=TRUE).

}
  \item{mens}{Indicates the mensuration process. If have strata (mens="strata"), plots (mens="plot"), whithout strata and plots (mens="census") or by the Bitterlich method (mens="bit").
}
 \item{vol}{Logical. If already have a volume column (last), vol=TRUE.
}
  \item{myeq}{Optional. User can calculate a diferent equation using height (h) and diameter (d), in quotes, e.g.:
  "0.000065661*d^(2.475293)*h^(0.300022)"
}
  \item{veg}{Optional. A vegetable formation. See all: \href{https://github.com/igorcobelo/florestal/blob/master/inst/Equations.md}{Equations}

  }
  \item{f}{Optional. Form factor.}
  \item{circ}{Logical. If TRUE, the values of the argument "d" represent the circumference, and will be automatically transformed into a diameter. If FALSE (default), the values represent the diameter.}


}
\details{
The values in the column of individuals must be sequential (1,2,3,4 ...), repeating them if they represent the same individual.

For the Bitterlich method (mens="bit"), the volume is calculated considering each line an individual, not admitting multiple shafts.

}
\value{
The function returns a new column with the individual volume (if vol = F) and the diameters of multiple shafts are joined by means of the square mean diameter, keeping the value of the highest height.

}
\author{
Igor Cobelo Ferreira
}

\examples{
library(florestal)

#loads the data by an inventory with plots

data("simple1")
head(simple1)

#Using an equation entered manually

IF_simple <- indvol(x = simple1, mens="plot", myeq = "0.000065661*d^2.475293*h^0.300022")

#Using a form factor

IF_f <- indvol(x = simple1, mens="plot", f = 0.7)

#loads the data by an inventory with strata

data("est1")
head(est1)

#create an object for each stratum and join with 'rbind'

IF_e1<-indvol(est1[est1$Stratum==1,],mens="strata",veg="cerradoss_df")
IF_e2<-indvol(est1[est1$Stratum==2,],mens="strata",veg="matas>10_df")

est2<-rbind(IF_e1,IF_e2)

#loads the data by an inventory by the Bitterlich Method

data("bit1")
head(bit1)

IF_bit <- indvol(bit1, mens="bit", f=0.7)

#loads the data by an inventory from a census

data("census1")
head(census1)

IF_census <- indvol(census1, mens="census", veg="cerradoss_df")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
\keyword{utilities}% __ONLY ONE__ keyword per line

