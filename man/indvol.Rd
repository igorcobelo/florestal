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
indvol(x, mens="plot", vol=F, myeq=NULL, veg=NULL, f=NULL, circ=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame.
  
  - If have strata (mens="strata"), there must be six columns: strata in the first (in numerals), plots in the second (in numerals), numbering of individuals in the third (in numerals), species in the fourth (in character), height in the fifth (in meters) and diameter in the sixth (in centimeters).
  
  - If have only plots (mens="plot"), follows the same order but without the strata column (plot, individuals, species, height and diameter).
  
  - If have not plots (mens="census"), follows the same order but without the strata and plot columns (individuals, species, height and diameter).
  
  - If its sampling by the Bitterlich method (mens="bit"), the order of the columns is as in plots, but it is suggested that the sixth column is the radial distance (point, individuals, specie, height, diameter and radial distance).
  
  - If the goal is only to join multiple shafts that already contain volume, the volume should be in the last column (vol=T).

}
  \item{mens}{Indicates the mensuration process. If have strata (mens="strata"), plots (mens="plot"), whithout strata and plots (mens="census") or by the Bitterlich method (mens="bit").
}
 \item{vol}{Logical. If already have a volume column (last), vol=T.
}
  \item{myeq}{Optional. User can calculate a diferent equation using height (h) and diameter (d), in quotes, e.g.:
  "0.000065661*d^(2.475293)*h^(0.300022)"
}
  \item{veg}{Optional. A vegetable formation (equations for volume with bark):
  
  - "cerrado" (cerrado/brazilian savannah): 0.000065661*d^(2.475293)* h^(0.300022)
  
  - "cer_regen" (cerrado em regeneracao): 0.000058468*d^(2.160042)* h^(0.791208)
  
  - "campo_cer" (campo cerrado): 0.000024059*d^(2.506122)* h^(0.929214)
  
  - "cerradao" (cerradao): 0.000094001*d^(1.830398)* h^(0.960913)
  
  - "mata_pri" (mata primaria): 0.00024502*d^(2.265786)* h^(0.150001)
  
  - "mata_sec" (mata secundaria): 0.000074230*d^(1.707348)* h^(1.16873)
  
  - "mata_ciliar" (mata ciliar): 0.000065607*d^(2.084676)* h^(0.752177)
  
  - "mata_seca" (mata seca): 0.000074924*d^(1.818557)* h^(1.061157)
  
  - "trans_cipo" (mata de transicao cipo): 0.0000065231*d^(1.64498)* h^(2.234673)
  
  - "trans_jaiba" (mata de transicao jaiba): 0.000057947*d^(1.911894)* h^(1.0751)
  
  - "caat_arborea" (caatinga arborea): 0.0000408657*d^(2.235528)* h^(0.823993)
  
  - "caat_arbustiva" (caatinga arbustiva): 0.000075999*d^(2.016671)* h^(0.761177)
  
  }
  \item{f}{Optional. Form factor.}
  \item{circ}{Logical. If TRUE, the values of the argument "d" represent the circumference, and will be automatically transformed into a diameter. If FALSE (default), the values represent the diameter.}


}
\details{
For the Bitterlich method (mens="bit"), the volume is calculated considering each line an individual, not admitting multiple shafts.

}
\value{
The function returns a new column with the individual volume (if vol = F) and the diameters of multiple shafts are joined by means of the square mean diameter, keeping the value of the highest height.

}
\references{
CENTRO TECNOLOGICO DE MINAS GERAIS (CETEC). Determinacao de equacoes volumetricas aplicaveis ao manejo sustentado de florestas nativas no estado de Minas Gerais e outras regioes do pais: relatorio final. Belo Horizonte, 1995.}
\author{
Igor Cobelo Ferreira
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
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

IF_strat1 <- indvol(est1[est1$Stratum==1,], mens="strata", veg = "cerrado")
IF_strat2 <- indvol(est1[est1$Stratum==2,], mens="strata", veg = "mata_sec")
IF_strat3 <- indvol(est1[est1$Stratum==3,], mens="strata", veg = "mata_pri")

IF_strata <- rbind(IF_strat1, IF_strat2, IF_strat3)

#loads the data by an inventory by the Bitterlich Method

data("bit1")
head(bit1)

IF_bit <- indvol(bit1, mens="bit", f=0.7)

#loads the data by an inventory from a census

data("census1")
head(census1)

IF_census <- indvol(census1, mens="census", veg="cerrado")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
\keyword{utilities}% __ONLY ONE__ keyword per line

