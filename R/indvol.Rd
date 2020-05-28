\name{indvol}
\alias{indvol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Individual Woody Volume

}
\description{
Calculates the individual woody volume from a manually entered equation, an equation from a listed vegetable formation or a form factor.

}
\usage{
indvol(x, d=NULL, h=NULL, myeq=NULL, veg=NULL, f=NULL, circ=F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame.}
  \item{d}{Optional. Column with diameter in the data frame, in centimeters.}
  \item{h}{Optional. Column with height in the data frame, in meters.}
  \item{myeq}{User can calculate a diferent equation using height and diameter, e.g.:
  0.000065661*d^(2.475293)* h^(0.300022)}
  \item{veg}{Optional. An equation for one of the following vegetable formation: "cerrado" (cerrado/brazilian savannah); "cer_regen" (cerrado in regeneration); "campo_cer" (cerrado field); "cerradao" (cerradao); "mata_pri" (primary forest); "mata_sec" (secondary forest); "mata_ciliar" (riparian forest); "mata_seca" (dry forest); "trans_cipo" (transition forest liana); "trans_jaiba" (jaiba transition forest); "caat_arborea" (arboreal caatinga); "caat_arbustiva" (shrub caatinga). See details.}
  \item{f}{Optional. Form factor.}
  \item{circ}{Logical. If TRUE, the values of the argument "d" represent the circumference, and will be automatically transformed into a diameter. If FALSE (default), the values represent the diameter.}


}
\details{
The equations for vegetable formations represent the woody volume with bark:

- cerrado/brazilian savannah: 0.000065661*d^(2.475293)* h^(0.300022)

- cerrado in regeneration: 0.000058468*d^(2.160042)* h^(0.791208)

- cerrado field (campo cerrado): 0.000024059*d^(2.506122)* h^(0.929214)

- cerradao: 0.000094001*d^(1.830398)* h^(0.960913)

- primary forest (mata primaria): 0.00024502*d^(2.265786)* h^(0.150001)

- secondary forest (mata secundaria): 0.000074230*d^(1.707348)* h^(1.16873)

- riparian forest (mata ciliar): 0.000065607*d^(2.084676)* h^(0.752177)

- dry forest (mata seca): 0.000074924*d^(1.818557)* h^(1.061157)

- transition forest liana (mata de transicao cipo): 0.0000065231*d^(1.64498)* h^(2.234673)

- jaiba transition forest (mata de trasicao jaiba): 0.000057947*d^(1.911894)* h^(1.0751)

- arboreal caatinga (caatinga arborea): 0.0000408657*d^(2.235528)* h^(0.823993)

- shrub caatinga  (caatinga arbustiva): 0.000075999*d^(2.016671)* h^(0.761177)

}
\value{
The function returns a new column in the data frame, with the individual woody volume, according to the chosen method (form factor, equation according to vegetable formation or equation entered manually).
}
\references{
CENTRO TECNOLOGICO DE MINAS GERAIS (CETEC). Determinacao de equacoes volumetricas aplicaveis ao manejo sustentado de florestas nativas no estado de Minas Gerais e outras regioes do pais: relatorio final. Belo Horizonte, 1995.}
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

#Using an equation entered manually:

indvol(x = IF1, d = IF1$Diameter, h = IF1$Height, myeq = 0.000065661*d^(2.475293)* h^(0.300022))

#Using a vegetable formation equation:

indvol(x = IF1, d = IF1$Diameter, h = IF1$Height, veg = "cerrado")

#Using a form factor:

indvol(x = IF1, d = IF1$Diameter, h = IF1$Height, f = 0.7)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
