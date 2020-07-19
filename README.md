[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](https://cran.r-project.org/package=florestal) [![](https://cranlogs.r-pkg.org/badges/grand-total/florestal)](https://cran.r-project.org/package=florestal) [![](https://cranlogs.r-pkg.org/badges/florestal)](https://cran.r-project.org/package=florestal)

# florestal
> Resultados para Inventários Florestais.



## Índice
* [1. Informações Gerais](#1-informações-gerais)
* [2. Instalação](#instalação)
* [3. Volume Individual](#volume-individual)
* [4. Amostragem Casual Simples](#amostragem-casual-simples)
* [5. Amostragem Casual Estratificada](#amostragem-casual-estratificada)
* [6. Método de Bitterlich](#método-de-bitterlich)
* [7. Parâmetros Fitossociológicos](#parâmetros-fitossociológicos)
* [8. Referências](#referências)
* [9. Contato](#contato)

## 1. Informações Gerais

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

#Carregamos nossa planilha de exemplo:

`data("simple2")`

`head(simple2)`

#Consideramos uma área total de 27ha, área da parcela de 0.1ha, erro requerido de 10% (padrão) e nível de significância de 5% (padrão):

`IF_acs <- acs(simple2,A=27,a=0.1)`

#A função retorna uma lista dos resultados. Podemos visualizar cada elemento da lista:

`IF_acs$`grafico ivi``

`IF_acs$`distribuicao diam``

`IF_acs$`parametros vol``

#Para alterar a quantidade de espécies no gráfico de IVI, utilizamos o argumento 'spivi' (padrão=15).

#Para alterar a amplitude dos centros de classe da distribuição diamétrica, utilizamos o argumento 'diam' (padrão=5).

#Outros argumentos podem ser visualizados na documentação da função:

`?acs`

#Um ARQUIVO DOCX é salvo na pasta de arquivos temporários do seu computador. Para encontrar a pasta, execute:

`tempfile()`

#O arquivo é salvo com o nome "InventarioFlorestal" seguido de um código, como "InventarioFlorestal1fbc2fa1c72".

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
