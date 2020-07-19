[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](https://cran.r-project.org/package=florestal) [![](https://cranlogs.r-pkg.org/badges/grand-total/florestal)](https://cran.r-project.org/package=florestal) [![](https://cranlogs.r-pkg.org/badges/florestal)](https://cran.r-project.org/package=florestal)

# florestal


## Índice
* [Informações Gerais](#informações-gerais)
* [Instalação](#instalação)
* [Volume Individual](#volume-individual)
* [Amostragem Casual Simples](#amostragem-casual-simples)
* [Amostragem Casual Estratificada](#amostragem-casual-estratificada)
* [Método de Bitterlich](#método-de-bitterlich)
* [Parâmetros Fitossociológicos](#parâmetros-fitossociológicos)
* [Referências](#referências)
* [Contato](#contato)

## Informações Gerais

As funções retornam parâmetros de amostragem para inventários florestais com tabelas e gráficos. Os métodos utilizados no pacote referem-se a Pellico & Brena (1997) <<https://bit.ly/2BDbHJI>>.

Funções:

- indvol(): calcula o volume individual a partir de uma equação inserida pelo usuário, por um fator de forma ou por uma das equações utilizadas no Inventário Florestal Nacional, listadas por estado e por fitofisionomia.
- acs(): retorna parâmetros amostrais para Amostragem Casual Simples.
- ace(): retorna parâmetros amostrais para Amostragem Casual Estratificada.
- bit(): retorna parâmetros amostrais para amostragem pelo Método de Bitterlich.
- fito(): retorna parâmetros fitossociológicos.

## Instalação

`install.packages("florestal")`

`library(florestal)`

## Volume Individual

#Para calcular o volume individual, usamos a função 'indvol()'.

#Caso o mesmo indivíduo tenha mais de um fuste mensurado, a numeração do indivíduo deve se repetir na linha de baixo. Nesse caso, será considerada a maior altura e calculado o diâmetro médio quadrático.

#Para Amostragem Casual Simples, a planilha precisa necessariamente conter as colunas nesta ordem:

#Parcelas na primeira (em numeral); indivíduos na segunda (em numeral); espécies na terceira; altura na quarta (em metros); e diâmetro (ou circunferência) na quinta (em centímetros).

#A quinta coluna deve conter o diâmetro, a não ser que a equação que for utilizar considere a circunferência. Se estiver em circunferência e quiser transformar em diâmetro, use o argumento 'circ=T'.

#Carrega nossa planilha de exemplo:

`data("simple1")`

`head(simple1)`

![Example screenshot](./img/df1.png)

#Usando uma equação inserida pelo usuário:

`IF_simple <- indvol(x = simple1, mens="plot", myeq = "0.000065661*d^2.475293*h^0.300022")`

`head(IF_simple)`

#Usando um fator de forma:

`IF_f <- indvol(x = simple1, mens="plot", f = 0.7)`

`head(IF_f)`

#Para Amostragem Casual Estratificada, a planilha precisa necessariamente conter as colunas nesta ordem:

#Estratos na primeira (em numeral); parcelas na segunda (em numeral); indivíduos na terceira (em numeral); espécies na quarta; altura na quinta (em metros); e diâmetro (ou circunferência) na sexta (em centímetros).

#Carrega nossa planilha de exemplo:

`data("est1")`

`head(est1)`

![Example screenshot](./img/df1.png)

#Criamos um objeto para cada estrato e depois os unimos com 'rbind':

`IF_e1 <- indvol(est1[est1$Stratum==1,],mens="strata",veg="cerradoss_df")`

`IF_e2 <- indvol(est1[est1$Stratum==2,],mens="strata",veg="matas>10_df")`

`est2 <- rbind(IF_e1,IF_e2)`

`head(est2)`

#Para o Método de Bitterlich, a planilha precisa necessariamente conter as colunas nesta ordem:

#Pontos amostrais na primeira (em numeral); indivíduos na segunda (em numeral); espécies na terceira; altura na quarta (em metros); diâmetro (ou circunferência) na quinta (em centímetros); e distância radial na sexta (em metros).

#Carrega nossa planilha de exemplo:

`data("bit1")`

`head(bit1)`

![Example screenshot](./img/df1.png)

`IF_bit <- indvol(bit1, mens="bit", f=0.7)`

#Para um censo, a planilha precisa necessariamente conter as colunas nesta ordem:

#Indivíduos na primeira (em numeral); espécies na segunda; altura na terceira (em metros); diâmetro (ou circunferência) na quarta (em centímetros); e distância radial na quinta (em metros).

#Carrega nossa planilha de exemplo:

`data("census1")`

`head(census1)`

![Example screenshot](./img/df1.png)

`IF_census <- indvol(census1, mens="census", veg="cerradoss_df")`

## Amostragem Casual Simples

#Calculamos a Amostragem Casual Simples com a função 'acs()'.

## Amostragem Casual Estratificada

#Calculamos a Amostragem Casual Estratificada com a função 'ace()'.

## Método de Bitterlich

#Calculamos a amostragem pelo Método de Bitterlich com a função 'bit()'.

## Parâmetros Fitossociológicos

#Calculamos os parâmetros fitossociológicos com a função 'fito()'.

## Referências

Pellico Netto, S.; Brena, D. (1997). Inventário Florestal. Curitiba: Universidade Federal do Paraná, 316 p.

## Contato

Igor Cobelo Ferreira <<cobelo.igor@gmail.com>>
Instagram: [@florestalpackage](https://www.instagram.com/florestalpackage/)

Para citar 'florestal' em publicações, por favor use:

Ferreira, Igor C. (2020). florestal: Results for Forest Inventories. R package version 0.1.1. Brasilia, Brazil. <<http://github.com/igorcobelo/florestal>>
