# CGVN-EFA

[![R](https://img.shields.io/badge/R-%3E%3D4.2-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![FAPEMIG](https://img.shields.io/badge/Financiamento-FAPEMIG%2001%2F2022-green)](https://fapemig.br/)

> Pereira, M. V. L.; Martucheli, C. T.; Fonseca, S. E.; Lopes, Y. F. (2024).  
> *Sete Dimensões da Governança Corporativa no Brasil: Índices e Disponibilização de Dados.*  
> II Congresso Interinstitucional de Contabilidade e Controladoria (CINCO 2024).

---

## Sobre o projeto

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
meio do [Portal de Dados
Abertos](https://dados.cvm.gov.br/dataset/cia_aberta-doc-cgvn) da CVM.

## Site

Os dados brutos são periodicamente consolidados e fornecidos em um
[*website*](https://mvlp.github.io/celta/#/governance) além da
disponibilização de Índices de Governança Corporativa (IGCs),
estatísticas, gráficos e planilhas.
![image](https://github.com/user-attachments/assets/ddcbb61d-a05a-4fb9-8451-df3bc64d8049)

## Código Fonte (EM CONSTRUÇÃO)
### O que este repositório oferece

- **Dados consolidados** do ICBGC de 495 empresas (out/2018–nov/2024) em formato `.xlsx`, prontos para uso.
- **Sete Índices de Governança Corporativa (IGCs)** construídos a partir de Análise Fatorial Exploratória (AFE) e Algoritmos Genéticos:

| Sigla | Dimensão                        | Fator |
|-------|---------------------------------|-------|
| CC    | Controles e Compliance          | 1     |
| REG   | Regulações Estatutárias         | 2     |
| ADM   | Conselho de Administração       | 3     |
| DIR   | Diretoria                       | 4     |
| SEM   | Sociedades de Economia Mista    | 5     |
| DEF   | Medidas de Defesa               | 6     |
| FIS   | Conselho Fiscal                 | 7     |

- **Scripts reprodutíveis** para replicar todos os resultados do artigo.

---

## Estrutura do repositório

```
CGVN-EFA/
├── data/
│   ├── dataset_CGVN.RData          # Dataset processado (gerado por script_get_data.R)
│   ├── input_GA_scale.RData        # Entrada para o AG (gerado pelo script EFA)
│   ├── scale_CINCO2024.RData       # Solução publicada no artigo (IGCs finais)
│   └── loading_cut_off.csv         # Tabela de limiares de carga (Hair et al., 2014)
├── figs/                           # Gráficos gerados (PDF/PNG)
├── script_CGVN_EFA_github.R        # Script 1: estatísticas descritivas + AFE
├── script_GA_scale.R               # Script 2: otimização dos IGCs via AG
└── README.md
```

---

## Pré-requisitos

### R (versão ≥ 4.2)

Instale todos os pacotes necessários de uma vez:

```r
install.packages(c(
  "tibble", "dplyr", "tidyr",      # manipulação de dados
  "ggplot2", "zoo", "lubridate",   # visualização e séries temporais
  "extrafont",                     # fontes para exportação em LaTeX
  "fBasics",                       # estatísticas descritivas
  "psych",                         # AFE, escores fatoriais, Alpha de Cronbach
  "EFAtools",                      # KMO, Bartlett, N_FACTORS
  "ppcor",                         # correlações parciais
  "GA"                             # algoritmos genéticos
))
```

> **Nota sobre fontes LaTeX:** Para usar a fonte CM Roman nos gráficos (padrão do artigo), execute `font_import()` do pacote `extrafont` uma vez após a instalação. Caso prefira não usar essa fonte, altere `flag_font_latex = FALSE` nas chamadas das funções de gráfico.

---

## Como usar

### Passo 1 — Clone o repositório

```bash
git clone https://github.com/mvlp/CGVN-EFA.git
cd CGVN-EFA
```

### Passo 2 — Execute o Script 1 (AFE)

Abra o RStudio e execute `script_CGVN_EFA_github.R` com **Ctrl+Shift+S** (Source).

Este script realiza, em ordem:

1. **Carrega os dados** do ICBGC (`data/dataset_CGVN.RData`)
2. **Tabela 1** — Frequência das respostas por prática e capítulo
3. **Figuras 1–2** — Evolução temporal das respostas (salvas em `figs/`)
4. **Tabela 2** — Estatísticas descritivas + KMO individual + Bartlett por capítulo
5. **KMO global** (0,928) e **Bartlett global** (χ²=60.094,91, p<0,001)
6. **AFE com rotação varimax** — 7 fatores (**Tabela 3**)
7. Exporta `data/input_GA_scale.RData` para o Script 2

> ⚠️ O script pausa em um `browser()` após imprimir a Tabela 3. Pressione **`c`** no console do RStudio para continuar.

### Passo 3 — Execute o Script 2 (Algoritmo Genético)

Abra e execute `script_GA_scale.R` com **Ctrl+Shift+S**.

Este script realiza:

1. **Carrega** os resultados da AFE (`data/input_GA_scale.RData`)
2. **Diagnóstico da solução inicial** (warm-start baseado na maior carga por fator)
3. **Otimização via Algoritmo Genético** → produz os 7 IGCs finais
4. **Tabela 4** — Pesos dos IGCs, correlações entre IGCs e correlações IGC × fatores
5. **Alpha de Cronbach** — confiabilidade de cada IGC
6. **Gráficos temporais** de cada IGC (salvo em `figs/`)

> 💡 **Reproduzindo os resultados do artigo:** Por padrão, `FLAG_LOAD_SCALE_CINCO2024 = TRUE` carrega a solução exata publicada no artigo, reproduzindo a Tabela 4 instantaneamente.  
> Para rodar o AG do zero, altere para `FLAG_LOAD_SCALE_CINCO2024 = FALSE`. Atenção: a execução pode levar horas dependendo do hardware.

---

## Reprodução dos resultados principais

A tabela abaixo resume os resultados esperados e onde encontrá-los no artigo:

| Resultado                        | Script   | Seção do artigo | Saída esperada                                      |
|----------------------------------|----------|-----------------|-----------------------------------------------------|
| Frequência das respostas         | Script 1 | Seção 4.1 / Tab. 1 | Tabela impressa no console por capítulo           |
| Gráficos de evolução             | Script 1 | Figura 1–2      | PDFs em `figs/p1_*.pdf` e `figs/p2_*.pdf`         |
| Estatísticas descritivas         | Script 1 | Seção 4.2 / Tab. 2 | Tabela com média, desvio, KMO, etc. por variável  |
| KMO global                       | Script 1 | Seção 4.3.1     | `0.928` (overall)                                  |
| Bartlett                         | Script 1 | Seção 4.3.1     | χ²(1431) = 60.094,91, p < 0,001                    |
| Matriz de cargas varimax         | Script 1 | Seção 4.3.3 / Tab. 3 | 7 fatores, 43,69% de variância explicada      |
| Pesos e correlações dos IGCs     | Script 2 | Seção 4.3.4 / Tab. 4 | Correlação máxima entre IGCs: 0,048            |
| Correlação IGC × fator           | Script 2 | Seção 4.3.5 / Tab. 4 | Correlações > 0,68 com o fator correspondente  |
| Alpha de Cronbach                | Script 2 | —               | Impresso no console por IGC                        |
| Gráficos dos IGCs                | Script 2 | —               | PDFs em `figs/scale_*.pdf`                         |

---

## Metodologia resumida

```
Dados ICBGC (CVM)
       │
       ▼
Conversão qualitativo → quantitativo
  "Não se Aplica"=0 │ "Não"=1 │ "Parcialmente"=2 │ "Sim"=3
       │
       ▼
Testes de adequação (KMO e Bartlett)
  KMO global = 0,928 ✓  │  Bartlett: p < 0,001 ✓
       │
       ▼
Análise Fatorial Exploratória (AFE)
  Método:   fatores comuns (psych::fa)
  Rotação:  varimax
  Fatores:  7 (selecionados entre soluções de 4, 6, 7 e 9)
       │
       ▼
Otimização por Algoritmo Genético
  Cromossomo: pesos ∈ {-1, 0, +1} por variável × fator
  Objetivo:   min [ ||corr(S)-I||² + ||corr(S,F)-corr(F)||² ]
  Restrição:  cada variável entra em no máximo 1 IGC
       │
       ▼
7 IGCs validados (CC, REG, ADM, DIR, SEM, DEF, FIS)
  Correlação máxima entre IGCs: 0,048
  Correlação mínima IGC × fator: 0,684
```

---

## Dados disponibilizados

Os dados consolidados do ICBGC (formato `.xlsx`) estão disponíveis para download direto no site do projeto. O arquivo contém:

- **2.294 documentos** de **495 empresas** distintas
- Período: **outubro de 2018 a novembro de 2024**
- **54 práticas recomendadas** avaliadas em cada informe
- Respostas originais e valores numéricos convertidos

A atualização ocorre automaticamente à medida que novos informes são publicados pela CVM.

---

## Como citar

```bibtex
@inproceedings{pereira2024cgvn,
  author    = {Pereira, Marcos Vinicius Lopes and Martucheli, Camila Teresa 
               and Fonseca, Simone Evangelista and Lopes, Yuri Fonseca},
  title     = {Sete Dimensões da Governança Corporativa no Brasil: 
               Índices e Disponibilização de Dados},
  booktitle = {II Congresso Interinstitucional de Contabilidade e Controladoria (CINCO 2024)},
  year      = {2024},
  month     = {novembro},
  note      = {Financiamento: FAPEMIG, Chamada 01/2022, Demanda Universal}
}
```

---

## Referências principais

- Hair, J. F., Black, W. C., Babin, B. J., & Anderson, R. E. (2014). *Multivariate Data Analysis* (7ª ed.). Pearson.
- Scrucca, L. (2013). GA: A Package for Genetic Algorithms in R. *Journal of Statistical Software*, 53(4), 1–37.
- Scrucca, L. (2017). On some extensions to GA package. *The R Journal*, 9(1), 187–206.
- Steiner, M. D., & Grieder, S. (2020). EFAtools. *Journal of Open Source Software*, 5(53), 2521.
- CVM (2024). *Portal Dados Abertos CVM*. https://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/CGVN/DADOS/
- IBGC (2023). *Código de Melhores Práticas de Governança Corporativa* (6ª ed.).

---

## Financiamento

Os autores agradecem pelo financiamento de pesquisa feito pela **Fundação de Amparo à Pesquisa do Estado de Minas Gerais** 
(FAPEMIG, CHAMADA FAPEMIG 01/2022, DEMANDA UNIVERSAL, APQ-02135-22).

---

## Contato

| Autor | Instituição | E-mail | Titulação |
|-------|-------------|--------|-----------|
| **[Marcos Pereira](http://lattes.cnpq.br/1294789533388093)** | [DTECH/CAP/UFSJ](https://ufsj.edu.br/dtech/corpo_docente.php) | <marcos.vinicius@ufsj.edu.br> | Doutor em Administração (Finanças) |
| **[Camila Martucheli](http://lattes.cnpq.br/9986835732512415)** | [Universo BH](https://universobh.com.br/gestores-de-curso/)  | <camila.martucheli@gmail.com> | Doutora em Administração (Finanças) |
| **[Simone Fonseca](http://lattes.cnpq.br/5220117639109190)** | [DECAD/ICSA/UFOP](https://icsa.ufop.br/corpo-docente-3) | <simone.fonseca@ufop.edu.br> | Doutora em Administração (Finanças) |
| **[Yuri Lopes](http://lattes.cnpq.br/1281498889191276)** | UFSJ | <yuri.fnsc.lopes@gmail.com> | Graduação em Engenharia Mecatrônica |

---

## Contribuições

Contribuições são bem-vindas! Se você tiver sugestões de melhorias ou
identificar problemas, abra uma *issue* ou envie um *pull request*.
