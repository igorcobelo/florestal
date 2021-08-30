(https://cran.r-project.org/package=florestal) [![](https://cranlogs.r-pkg.org/badges/grand-total/florestal)](https://cran.r-project.org/package=florestal) [![](https://cranlogs.r-pkg.org/badges/florestal)](https://cran.r-project.org/package=florestal)

# florestal
> Resultados para Inventários Florestais.


<p align="center">
  <img src="https://github.com/igorcobelo/florestal/blob/master/inst/img/logo.jpg" width="40%" height="40%">

<div id="menu" />

---------------------------------------------

## Índice
* [1. Informações Gerais](#1-informações-gerais)
* [2. Instalação](#2-instalação)
* [3. Volume Individual](#3-volume-individual)
* [4. Amostragem Casual Simples](#4-amostragem-casual-simples)
* [5. Amostragem Casual Estratificada](#5-amostragem-casual-estratificada)
* [6. Método de Bitterlich](#6-método-de-bitterlich)
* [7. Parâmetros Fitossociológicos](#7-parâmetros-fitossociológicos)
* [8. Referências](#8-referências)
* [9. Contato](#9-contato)

## 1. Informações Gerais

As funções retornam parâmetros de amostragem para inventários florestais com tabelas e gráficos. Os métodos utilizados no pacote referem-se a Pellico & Brena (1997) <<https://bit.ly/2BDbHJI>>.

Funções:

- indvol(): calcula o volume individual a partir de uma equação inserida pelo usuário, por um fator de forma ou por uma das equações utilizadas no Inventário Florestal Nacional, listadas por estado e por fitofisionomia. Ver as equações: [Equações.](https://github.com/igorcobelo/florestal/blob/master/Equations.md)
- acs(): retorna parâmetros amostrais para Amostragem Casual Simples.
- ace(): retorna parâmetros amostrais para Amostragem Casual Estratificada.
- bit(): retorna parâmetros amostrais para amostragem pelo Método de Bitterlich.
- fito(): retorna parâmetros fitossociológicos.

Obs. 1: As funções 'acs', 'ace', 'bit' e 'fito' retornam, além de uma lista dos resultados, um arquivo docx salvo na pasta de arquivos temporários do seu computador.
Caso tenha problemas devido a questões de administração de usuários, pode-se utilizar o argumento 'save=F', para que o arquivo não seja salvo.

Obs. 2: O idioma das saídas é Português, por padrão. Pode-se alterar para Inglês com o argumento 'pt=F'.

## 2. Instalação

`remotes::install_github("igorcobelo/florestal")`

`library(florestal)`

## 3. Volume Individual

#Para calcular o volume individual, usamos a função 'indvol()'.

#Caso o mesmo indivíduo tenha mais de um fuste mensurado, a numeração do indivíduo deve se repetir na linha de baixo. Nesse caso, será considerada a maior altura e calculado o diâmetro médio quadrático.

#Para Amostragem Casual Simples, a planilha precisa necessariamente conter as colunas nesta ordem:

#Parcelas na primeira (em numeral); indivíduos na segunda (em numeral); espécies na terceira; altura na quarta (em metros); e diâmetro (ou circunferência) na quinta (em centímetros).

#A quinta coluna deve conter o diâmetro, a não ser que a equação que for utilizar considere a circunferência. Se estiver em circunferência e quiser transformar em diâmetro, use o argumento 'circ=T'.

#Carrega nossa planilha de exemplo:

`data("simple1")`

`head(simple1)`

![Example screenshot](https://github.com/igorcobelo/florestal/blob/master/inst/img/head_simple1.png)

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

![Example screenshot](https://github.com/igorcobelo/florestal/blob/master/inst/img/head_est1.png)

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

![Example screenshot](.inst/img/head_bit1.png)

`IF_bit <- indvol(bit1, mens="bit", f=0.7)`

`head(IF_bit)`

#Para um censo, a planilha precisa necessariamente conter as colunas nesta ordem:

#Indivíduos na primeira (em numeral); espécies na segunda; altura na terceira (em metros); diâmetro (ou circunferência) na quarta (em centímetros); e distância radial na quinta (em metros).

#Carrega nossa planilha de exemplo:

`data("census1")`

`head(census1)`

![Example screenshot](https://github.com/igorcobelo/florestal/blob/master/inst/img/head_census1.png)

`IF_census <- indvol(census1, mens="census", veg="cerradoss_df")`

`head(IF_census)`

## 4. Amostragem Casual Simples

#Calculamos a Amostragem Casual Simples com a função 'acs()'.

#O formato da planilha deve ser o mesmo descrito na seção [3. Volume Individual](#3-volume-individual), porém com a última coluna representando o volume individual (a função 'indvol' já retorna nesse formato).

#Carregamos nossa planilha de exemplo:

`data("simple2")`

`head(simple2)`

#Consideramos uma área total de 27ha, área da parcela de 0.1ha, erro requerido de 10% (padrão) e nível de significância de 5% (padrão):

`IF_acs <- acs(simple2,A=27,a=0.1)`

#A função retorna uma lista dos resultados. Podemos visualizar cada elemento da lista utilizando `IF_acs$`.

#Para alterar a quantidade de espécies no gráfico de IVI, utilizamos o argumento 'spivi' (padrão=15).

#Para alterar a amplitude dos centros de classe da distribuição diamétrica, utilizamos o argumento 'ampl' (padrão=5).

#Para alterar o erro requerido, utilizamos o argumento 'E'.

#Para alterar o nível de significância, utilizamos o argumento 'p'.

#A saída padrão é em Português. Caso queria em Inglês, utilizamos o argumento 'pt=F'.

#Outros argumentos podem ser visualizados na documentação da função:

`?acs`

#Um ARQUIVO DOCX é salvo na pasta de arquivos temporários do seu computador. Para encontrar a pasta, execute:

`tempdir()`

#O arquivo é salvo com o nome "InventarioFlorestal" seguido de um código, como "InventarioFlorestal1fbc2fa1c72".

## 5. Amostragem Casual Estratificada

#Calculamos a Amostragem Casual Estratificada com a função 'ace()'.

#O formato da planilha deve ser o mesmo descrito na seção [3. Volume Individual](#3-volume-individual), porém com a última coluna representando o volume individual (a função 'indvol' já retorna nesse formato).

#Carregamos nossa planilha de exemplo:

data("est2")

head(est2)

#Consideramos dois estratos, o primeiro com 12.6 ha e o segundo com 10.2 ha, área da parcela de 0.1 ha, erro requerido de 10% (padrão) e nível de significância de 5% (padrão):

IF_ace <- ace(est2,a=0.1,aj=c(12.6,10.2))

#A função retorna uma lista dos resultados. Podemos visualizar cada elemento da lista utilizando `IF_ace$`.

#Para alterar a quantidade de espécies no gráfico de IVI, utilizamos o argumento 'spivi' (padrão=15).

#Para alterar a amplitude dos centros de classe da distribuição diamétrica, utilizamos o argumento 'ampl' (padrão=5).

#Para alterar o erro requerido, utilizamos o argumento 'E'.

#Para alterar o nível de significância, utilizamos o argumento 'p'.

#A saída padrão é em Português. Caso queria em Inglês, utilizamos o argumento 'pt=F'.

#A alocação de parcelas por estrato segue, por padrão, o método da alocação ótima de Neyman. Caso considere a alocação proporcional por área, utilize o argumento 'prop=T'.

#Outros argumentos podem ser visualizados na documentação da função:

?ace

#Um ARQUIVO DOCX é salvo na pasta de arquivos temporários do seu computador. Para encontrar a pasta, execute:

tempdir()

#O arquivo é salvo com o nome "InventarioFlorestal" seguido de um código, como "InventarioFlorestal1fbc2fa1c72".

## 6. Método de Bitterlich

#Calculamos a amostragem pelo Método de Bitterlich com a função 'bit()'.

#O formato da planilha deve ser o mesmo descrito na seção [3. Volume Individual](#3-volume-individual), porém com a última coluna representando o volume individual (a função 'indvol' já retorna nesse formato).

#Carregamos nossa planilha de exemplo:

data("bit2")

head(bit2)

#Consideramos uma área total de 12 ha, fator de área basal igual a 2, erro requerido de 10% (padrão) e nível de significância de 5% (padrão):

IF_bit <- bit(bit2,A=12,k=2)

#A função retorna uma lista dos resultados. Podemos visualizar cada elemento da lista utilizando `IF_bit$`.

#Para alterar a amplitude dos centros de classe da distribuição diamétrica, utilizamos o argumento 'ampl' (padrão=5).

#Para alterar o erro requerido, utilizamos o argumento 'E'.

#Para alterar o nível de significância, utilizamos o argumento 'p'.

#A saída padrão é em Português. Caso queria em Inglês, utilizamos o argumento 'pt=F'.

#Outros argumentos podem ser visualizados na documentação da função:

?bit

#Um ARQUIVO DOCX é salvo na pasta de arquivos temporários do seu computador. Para encontrar a pasta, execute:

tempdir()

#O arquivo é salvo com o nome "InventarioFlorestal" seguido de um código, como "InventarioFlorestal1fbc2fa1c72".

## 7. Parâmetros Fitossociológicos

#Calculamos os parâmetros fitossociológicos com a função 'fito()'.

#Precisamos de uma planilha contendo: Espécies (sp); parcelas (plot); e diâmetro (d).

#Se tiver estratos, devem estar na primeira coluna (em numeral) e acrescentar na função o argumento 'stratum=T'.

#Carregamos nossa planilha de exemplo:

`data(simple1)`

`head(simple1)`

#Considerando uma área total de 27 ha:

IF_fito <- fito(sp=simple1$Specie, plot=simple1$Plot, d= simple1$Diameter, A=27)

#A função retorna uma lista dos resultados. Podemos visualizar cada elemento da lista utilizando `IF_fito$`.

#Outros argumentos podem ser visualizados na documentação da função:

?fito

#Um ARQUIVO DOCX é salvo na pasta de arquivos temporários do seu computador. Para encontrar a pasta, execute:

tempdir()

#O arquivo é salvo com o nome "Fitossociologia" seguido de um código, como "Fitossociologia1fbc2fa1c72".

## 8. Referências

Pellico Netto, S.; Brena, D. (1997). Inventário Florestal. Curitiba: Universidade Federal do Paraná, 316 p.

## 9. Contato

Igor Cobelo Ferreira <<cobelo.igor@gmail.com>>

Instagram: [@florestalpackage](https://www.instagram.com/florestalpackage/)

Para citar 'florestal' em publicações, por favor use:

Ferreira, Igor C. (2020). florestal: Results for Forest Inventories. R package version 0.1.3. Brasilia, Brazil. <<https://cran.r-project.org/package=florestal>>
>>>>>>> 17c2e886e722bdf46f0a3d819de5129ae4060e9a
