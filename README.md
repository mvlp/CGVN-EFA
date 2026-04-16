# CGVN-EFA

[![R](https://img.shields.io/badge/R-%3E%3D4.2-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![FAPEMIG](https://img.shields.io/badge/Financiamento-FAPEMIG%2001%2F2022-green)](https://fapemig.br/)

> Pereira, M. V. L.; Martucheli, C. T.; Fonseca, S. E.; Lopes, Y. F. (2024).  
> *Sete Dimensões da Governança Corporativa no Brasil: Índices e Disponibilização de Dados.*  
> II Congresso Interinstitucional de Contabilidade e Controladoria (CINCO 2024).

---

Este repositório contém o código fonte utilizado para replicar os
resultados do artigo científico **[“Sete Dimensões da Governança
Corporativa no Brasil: Índices e Disponibilização de
Dados”](https://www.researchgate.net/publication/386986331_Sete_Dimensoes_da_Governanca_Corporativa_no_Brasil_Indices_e_Disponibilizacao_de_Dados)**.
O código foi desenvolvido em R e inclui scripts para análise de dados,
estatísticas e visualizações.

## Dados

Os dados relacionados às características de GC das empresas brasileiras
de capital aberto utilizados na estimação dos índices foram coletados do
Informe do Código de Governança (ICBGC). Um documento eletrônico de
respostas das companhias a um questionário padrão e de encaminhamento
periódico previsto no artigo 32 da Resolução CVM nº 80, em conformidade
com o estabelecido pela instituição. Essas respostas foram coletadas por
meio do [portal de dados
abertos](https://dados.cvm.gov.br/dataset/cia_aberta-doc-cgvn) da CVM.

## Site

Os dados brutos são periodicamente consolidados e fornecidos em um
[*website*](https://mvlp.github.io/celta/#/governance) além da
disponibilização de Índices de Governança Corporativa (IGCs),
estatísticas, gráficos e planilhas.
![image](https://github.com/user-attachments/assets/ddcbb61d-a05a-4fb9-8451-df3bc64d8049)

## Código Fonte (EM CONSTRUÇÃO)

``` r
# Gerando uma matriz de dados qualquer
dados <- matrix(rnorm(100), ncol = 5)

# Mostrando a matriz de correlação
cor(dados)
```

    ##             [,1]       [,2]        [,3]       [,4]       [,5]
    ## [1,]  1.00000000  0.1553372 -0.07888413 -0.3479916  0.2771827
    ## [2,]  0.15533717  1.0000000 -0.29996214  0.5360200  0.4979490
    ## [3,] -0.07888413 -0.2999621  1.00000000 -0.1272502 -0.3697020
    ## [4,] -0.34799157  0.5360200 -0.12725020  1.0000000  0.1387655
    ## [5,]  0.27718273  0.4979490 -0.36970202  0.1387655  1.0000000

## Autores

- **[Marcos Pereira](http://lattes.cnpq.br/1294789533388093)**
  - Afiliado ao
    [DTECH/CAP/UFSJ](https://ufsj.edu.br/dtech/corpo_docente.php)  
  - E-mail: <marcos.vinicius@ufsj.edu.br>
  - Doutor em Administração (Finanças)
- **[Camila Martucheli](http://lattes.cnpq.br/9986835732512415)**
  - Afiliada ao Centro Universitário Universo BH
  - E-mail: <camila.martucheli@gmail.com>
  - Doutora em Administração (Finanças)
- **[Simone Fonseca](http://lattes.cnpq.br/5220117639109190)**
  - Afiliada ao DECAD/ICSA/UFOP
  - E-mail: <simone.fonseca@ufop.edu.br>
  - Doutora em Administração (Finanças)
- **[Yuri Lopes](http://lattes.cnpq.br/1281498889191276)**
  - Aluno de graduação em Engenharia Mecatrônica (UFSJ)  
  - E-mail: <yuri.fnsc.lopes@gmail.com>
  - Graduando em Engenharia Mecatrônica (UFSJ)

## Apoio

Os autores agradecem pelo financiamento de pesquisa feito pela Fundação
de Amparo à Pesquisa do Estado de Minas Gerais (FAPEMIG, CHAMADA FAPEMIG
01/2022, DEMANDA UNIVERSAL, APQ-02135-22).

## Contribuições

Contribuições são bem-vindas! Se você tiver sugestões de melhorias ou
identificar problemas, abra uma *issue* ou envie um *pull request*.
