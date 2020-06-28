# Equações para Estimativa do Volume

> Equações por estado do Brasil de acordo com o Inventário Florestal Nacional, conduzido pelo Serviço Florestal Brasileiro (SFB).

## DISTRITO FEDERAL:

Equação utilizada para estimativa do volume (m³) nas Matas de Galeria e Matas Secas para 5<DAP<10 cm:

Ln (Vol) = -9,7751 + 2,2403 * Ln (DAP) + 0,6308 * Ln (HT)

Fonte: SCOLFORO, J. R. et al. (2008)

Nome para o argumento 'veg' da função 'indvol': `veg="matas5-10_df"` <br /><br />


Equação utilizada para estimativa do volume (m³) nas Matas de Galeria e Matas Secas para DAP ≥10 cm:

Ln (Vol) = -9,3436 + 2,0437 * Ln (DAP) + 0,7509 * Ln (HC)

Fonte: SFB não publicado (2016)

Nome para o argumento 'veg' da função 'indvol': `veg="matas>10_df"` <br /><br />


Equação utilizada para estimativa do volume (m³) em Cerrado sensu stricto:

Vol = 0,000109 * Db^2 + 0,0000451 * Db^2 * HT

Fonte: Rezende, A.V. (2002)

Nome para o argumento 'veg' da função 'indvol': `veg="cerradoss_df"` <br /><br />


## CEARÁ:

Equação utilizada para estimativa do volume de madeira (m³):

Ln (Vol) = -9,59340 + 2,04417 Ln (DAP) + 0,94531 * Ln (HT)

Fonte: FIGUEIREDO FILHO, A. et al. (2014)

Nome para o argumento 'veg' da função 'indvol': `veg="ceara"` <br /><br />


## PARANÁ:

Equação utilizada para estimativa do volume de madeira (m³) na Floresta Ombrófila Mista (DAP 5-10cm):

Ln (Vol) = −8,875910 + 1,892219 * Ln (DAP) + 0,739038 * Ln (h)

Fonte: Santos, K. et al. (2006)

Nome para o argumento 'veg' da função 'indvol': `veg="ombmista5-10_pr"` <br /><br />


Equação utilizada para estimativa do volume de madeira (m³) na Floresta Ombrófila Mista (DAP>10cm):

Ln (Vol/1000) = −17,96 + 0,96 * Ln (CAP^2) + 0,76 * Ln (h)

Fonte: Vibrans A. C. et al. (2015)

Nome para o argumento 'veg' da função 'indvol': `veg="ombmista>10_pr"` <br /><br />


Equação utilizada para estimativa do volume de madeira (m³) na Floresta Ombrófila Densa (DAP≥5cm):

Ln (Vol) = −10,045586 + 2,349493 * Ln (DAP) + 0,640598 * Ln (h)

Fonte: Correia, J. et al. (2017)

Nome para o argumento 'veg' da função 'indvol': `veg="ombdensa>5"` <br /><br />


Equação utilizada para estimativa do volume de madeira (m³) da Araucária (DAP≥ 5cm):

Vol = 0,000077𝑑 * DAP^1,85794 * h^0,93919

Fonte: Figuiredo Filho, A. et al. (2014) 

Nome para o argumento 'veg' da função 'indvol': `veg="araucaria>5_pr"` <br /><br />


## RIO DE JANEIRO:

Equação utilizada para estimativa do volume total de madeira na Floresta Ombrófila Densa:

Ln (Vol) = −9,9752493252 + 2,1719145688 Ln (DAP) + 0,8083667085 * Ln (h)

Fonte: Scolforo, J. R. et al. (2008).

Nome para o argumento 'veg' da função 'indvol': `veg="ombdensa_rj"` <br /><br />


Equação utilizada para estimativa do volume total de madeira na Floresta Estacional Semidecidual:

Ln (Vol) = −9,7394993677 + 2,3219001043 * Ln (DAP) + 0,5645027997 * Ln (h)

Fonte: Scolforo, J. R. et al. (2008).

Nome para o argumento 'veg' da função 'indvol': `veg="estacionalsemi_rj"` <br /><br />


Equação utilizada para estimativa do volume total de madeira na Floresta Estacional Decidual:

Ln (Vol) = −9,7677720672 + 2,4886704462 * Ln (DAP) + 0,4406921533 * Ln (h)

Fonte: Scolforo, J. R. et al. (2008).

Nome para o argumento 'veg' da função 'indvol': `veg="estacionaldeci_rj"` <br /><br />


Equação utilizada para estimativa do volume total de madeira na Restinga:

Ln (Vol) = −9,42719 + 1,96900 * Ln (DAP) + 0,831852 * Ln (h)

Fonte: CTEC (1995).

Nome para o argumento 'veg' da função 'indvol': `veg="restinga_rj"` <br /><br />


## RIO GRANDE DO NORTE:

Equação utilizada para estimativa do volume de madeira (m³):

Ln (Vol) = -9,59340 + 2,04417 * Ln (DAP) + 0,94531 * Ln (HT)

Fonte: SOUZA, P.F. (2012) in FIGUEIREDO FILHO, A. et al. (2014).

Nome para o argumento 'veg' da função 'indvol': `veg="rn"` <br /><br />


## RIO GRANDE DO SUL:

Equação utilizada para a estimativa do volume de madeira (m³) (DAP 5-10 cm):

Ln (Vol) = −8,875910 + 1,892219 * Ln(DAP) + 0,739038 * Ln (h)

Fonte: Santos, K. et al. (2006).

Nome para o argumento 'veg' da função 'indvol': `veg="rs5-10"` <br /><br />


Equação utilizada para a estimativa do volume de madeira (m³) (DAP>10 cm):

Ln (Vol/1000) = −17,96 + 0,96 * Ln (CAP^2) + 0,76 * Ln (h)

Fonte: Vibrans A. C. et al. (2015).

Nome para o argumento 'veg' da função 'indvol': `veg="rs>10"` <br /><br />



## SANTA CATARINA:

Modelo utilizado para estimativa do volume de fuste (m³) na Floresta Estacional Decidual para árvores com DAP≥10 cm:

Ln (Vf/1000) = −17,68 + 0,95 Ln (CAP^2) + 0,67 Ln (hf)

Fonte: Vibrans et al. (2015)

Nome para o argumento 'veg' da função 'indvol': `veg="estacionaldeci>10_sc"` <br /><br />


Modelo utilizado para estimativa do volume de fuste (m³) na Floresta Ombrófila Densa para árvores com DAP≥10 cm:

Ln (Vf/1000) = −17,75 + 0,98 * Ln (CAP^2) + 0,57 * Ln (hf)

Nome para o argumento 'veg' da função 'indvol': `veg="ombdensa>10_sc"` <br /><br />


Modelo utilizado para estimativa do volume de fuste (m³) na Floresta Ombrófila Mista para árvores com DAP≥10 cm:

Ln (Vf/1000) = −17,96 + 0,96 * Ln (CAP^2) + 0,76 * Ln (hf)

Fonte: Vibrans et al. (2015)

Nome para o argumento 'veg' da função 'indvol': `veg="ombmista>10_sc"` <br /><br />


## SERGIPE:

Equação utilizada para estimativa do volume de madeira (m³) na região de Caatinga:

Ln (Vol) = -9,59340 + 2,04417 * Ln (DAP) + 0,94531 * Ln (HT)

Fonte: FIGUEIREDO FILHO, A. et al. (2014)

Nome para o argumento 'veg' da função 'indvol': `veg="caatinga_se"` <br /><br />


Equação utilizada para estimativa do volume de madeira (m³) na região de Mata Atlântica:

Vol = 0,000074230 * DAP^1,707348 * HT^1,16873

Fonte: CETEC (1995).

Nome para o argumento 'veg' da função 'indvol': `veg="atlantica_se"` <br /><br />


## REFERÊNCIAS

CETEC (Fundação Centro Tecnológico de Mina Gerais). Determinação de equações volumétricas aplicáveis ao manejo sustentado de florestas nativas do estado de Minas Gerais e outras regiões do país: relatório final. Belo Horizonte: FAPEMIG/CETEC, 295 p. 1995.

CORREIA, J.; FANTINI, A.; PIAZZA, G. Equações volumétricas e fator de forma e de casca para florestas secundárias do litoral de Santa Catarina. Floresta e Ambiente 2017; 24: e20150237.

FIGUEIREDO FILHO, A.; MACHADO, S. A.; MIRANDA, R.O.V.; RETSLAFF, F. Compêndio de equações de volume e de afilamento de espécies florestais plantadas e nativas para as regiões geográficas do Brasil. 2014.

REZENDE, A.V. Diversidade, estrutura, dinâmica e prognose do crescimento de um Cerrado sensu stricto submetido a diferentes distúrbios por desmatamento. Tese doutorado, UnB. 2002.

SANTOS, K.; SANQUETA, C. R.; EISFIELD, R. L.; WATZLAWICK, L.F.; ZILIOTTO, M. A.B. Equações volumétricas por classe diamétrica para algumas espécies folhosas da Floresta Ombrófila Mista no Paraná, Brasil. Revista de Ciências Exatas e Naturais, v.8, n.1, p.99-112,2006.

SCOLFORO, J. R. et al. Equações para estimar o volume de madeira das fisionomias, em Minas Gerais. In: SCOLFORO, J. R.; OLIVEIRA, A. D.; ACERBI JÚNIOR, F. W. (Ed.). Inventário Florestal de Minas Gerais – Equações de Volume, Peso de Matéria Seca e Carbono para Diferentes Fisionomias da Flora Nativa. Lavras: UFLA, 2008. cap. 2, p.67-101.

SOUZA, P.F. Estudos Fitossociológicos e dendrométricos em um fragmentode caatinga, São José de Espinhares-PB. UFCG, Patos, 2012. 97p. Dissertação (Mestrado em Ciências Florestais) - Universidade Federal de Campina Grande, 2012.

VIBRANS, A. C. et al. Generic and specif stem volume models for three subtropical forest types in southern Brazil. Annals of Forest Science, v. 72, n.6, p.865-874, 2015.

