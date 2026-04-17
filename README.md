# CGVN-EFA — Governança Corporativa no Brasil: Índices e Dados

[![R](https://img.shields.io/badge/R-%3E%3D4.2-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![FAPEMIG](https://img.shields.io/badge/Financiamento-FAPEMIG%2001%2F2022-green)](https://fapemig.br/)
[![ResearchGate](https://img.shields.io/badge/Artigo-ResearchGate-00CCBB)](https://www.researchgate.net/publication/386986331_Sete_Dimensoes_da_Governanca_Corporativa_no_Brasil_Indices_e_Disponibilizacao_de_Dados)

> **Pereira, M. V. L.; Martucheli, C. T.; Fonseca, S. E.; Lopes, Y. F. (2024).**  
> *Sete Dimensões da Governança Corporativa no Brasil: Índices e Disponibilização de Dados.*  
> II Congresso Interinstitucional de Contabilidade e Controladoria — CINCO 2024.  
> 📄 [Acesse o artigo completo no ResearchGate](https://www.researchgate.net/publication/386986331_Sete_Dimensoes_da_Governanca_Corporativa_no_Brasil_Indices_e_Disponibilizacao_de_Dados)

---

## Sumário

- [Sobre o projeto](#sobre-o-projeto)
- [Website e dados consolidados](#website-e-dados-consolidados)
- [Fonte dos dados](#fonte-dos-dados)
- [Os sete IGCs](#os-sete-igcs)
- [Estrutura do repositório](#estrutura-do-repositório)
- [Instalação](#instalação)
- [Como usar](#como-usar)
- [Reprodução dos resultados](#reprodução-dos-resultados)
- [Metodologia resumida](#metodologia-resumida)
- [Como citar](#como-citar)
- [Referências](#referências)
- [Financiamento](#financiamento)
- [Contato](#contato)
- [Contribuindo](#contribuindo)

---

## Sobre o projeto

Este repositório disponibiliza o **código-fonte em R** para replicação completa dos resultados do artigo científico, incluindo:

- **Dados consolidados** do Informe sobre o Código Brasileiro de Governança Corporativa (ICBGC) de 495 empresas brasileiras de capital aberto (out/2018–nov/2024), prontos para uso em pesquisa.
- **Sete Índices de Governança Corporativa (IGCs)** construídos via Análise Fatorial Exploratória (AFE) e Algoritmos Genéticos — metodologicamente robustos, não correlacionados entre si e de fácil replicação.
- **Scripts reprodutíveis** que geram todas as tabelas e figuras do artigo a partir dos dados brutos da CVM.

A motivação central do projeto é suprir uma lacuna identificada na literatura brasileira: a ausência de índices de GC **gratuitos, contínuos e acessíveis** para pesquisadores, investidores e gestores.

---

## Website e dados consolidados

Os dados brutos são periodicamente consolidados e disponibilizados em um **website interativo**, junto com os IGCs calculados, estatísticas descritivas, gráficos de evolução temporal e planilhas para download.

🌐 **Acesse em: [mvlp.github.io/celta/#/governance](https://mvlp.github.io/celta/#/governance)**

![Screenshot do website CGVN](https://github.com/user-attachments/assets/ddcbb61d-a05a-4fb9-8451-df3bc64d8049)

---

## Fonte dos dados

Os dados utilizados na construção dos IGCs são extraídos do **Informe sobre o Código Brasileiro de Governança Corporativa (ICBGC)** — documento eletrônico de encaminhamento periódico anual previsto no artigo 32 da **Resolução CVM nº 80**. Por meio dele, as companhias abertas listadas na B³ reportam sua adesão a 54 práticas recomendadas pelo IBGC, respondendo "Sim", "Não", "Parcialmente" ou "Não se Aplica".

📂 **Fonte:** [Portal de Dados Abertos da CVM](https://dados.cvm.gov.br/dataset/cia_aberta-doc-cgvn)

| Característica       | Valor                                   |
|----------------------|-----------------------------------------|
| Empresas             | 495 (listadas e não listadas na B³)     |
| Documentos           | 2.294 informes                          |
| Período              | Outubro de 2018 a Novembro de 2024      |
| Práticas avaliadas   | 54 (variáveis)                          |
| Razão obs./variáveis | 42:1                                    |

---

## Os sete IGCs

A AFE identificou sete dimensões latentes da GC nas empresas brasileiras. Os IGCs são índices compostos com pesos `{-1, 0, +1}`, normalizados na escala **[0, 10]**, e validados como não correlacionados entre si (correlação máxima entre pares: **0,048**).

| # | Sigla | Dimensão                        | Correlação com o fator |
|---|-------|---------------------------------|------------------------|
| 1 | **CC**  | Controles e Compliance          | 0,812                  |
| 2 | **REG** | Regulações Estatutárias         | 0,812                  |
| 3 | **ADM** | Conselho de Administração       | 0,704                  |
| 4 | **DIR** | Diretoria                       | 0,736                  |
| 5 | **SEM** | Sociedades de Economia Mista    | 0,684                  |
| 6 | **DEF** | Medidas de Defesa               | 0,849                  |
| 7 | **FIS** | Conselho Fiscal                 | 0,879                  |

---

## Estrutura do repositório

```
CGVN-EFA/
│
├── scripts/
│   ├── script_CGVN_EFA.R           # Script 1 — Estatísticas descritivas + AFE
│   └── script_GA_scale.R           # Script 2 — Construção dos IGCs via AG
│
├── data/
│   ├── dataset_CGVN.RData          # Dataset processado (gerado por script_get_data.R)
│   ├── input_GA_scale.RData        # Saída do Script 1, entrada do Script 2
│   ├── scale_CINCO2024.RData       # Solução publicada no artigo (IGCs finais)
│   └── loading_cut_off.csv         # Limiares de carga fatorial (Hair et al., 2014)
│
├── figs/                           # Gráficos gerados (PDF/PNG)
|
├── output/
│   ├── IGC_empresas.xlsx          # Série temporal dos IGCs 
│   ├── loadings.xlsx              # Cargas fatoriais (AFE)
│   └── pesos_escalas.xlsx         # Matriz de pesos que determinam cada escala
│
└── README.md
```

---

## Instalação

### 1. Baixe o repositório

**Se você usa Git:**

```bash
git clone https://github.com/mvlp/CGVN-EFA.git
cd CGVN-EFA
```

**Se você não usa Git (mais simples):** clique no botão verde **`<> Code`** no topo desta página e depois em **`Download ZIP`**. Extraia a pasta onde preferir e abra os scripts diretamente no RStudio — não é necessário nenhum comando.

### 2. Instale os pacotes R necessários

Abra o R ou RStudio e execute:

```r
install.packages(c(
  # Manipulação de dados
  "tibble", "dplyr", "tidyr",
  # Visualização e séries temporais
  "ggplot2", "zoo", "lubridate",
  # Exportação com fontes LaTeX
  "extrafont",
  # Estatísticas descritivas
  "fBasics",
  # Análise fatorial e confiabilidade
  "psych",
  # KMO, Bartlett e critérios de retenção de fatores
  "EFAtools",
  # Correlações parciais
  "ppcor",
  # Algoritmos genéticos
  "GA"
))
```

> **Sobre fontes LaTeX:** Os gráficos usam a fonte "CM Roman" por padrão para compatibilidade com LaTeX. Para instalá-la, execute `extrafont::font_import()` uma vez após a instalação dos pacotes. Caso prefira não usá-la, passe `flag_font_latex = FALSE` nas chamadas às funções de gráfico dentro dos scripts.

---

## Como usar

Os dois scripts devem ser executados **em ordem**. O Script 1 gera um arquivo intermediário (`.RData`) que alimenta o Script 2.

### Script 1 — `scripts/script_CGVN_EFA.R`

**O que faz:** carrega e processa os dados do ICBGC, gera estatísticas descritivas, gráficos de evolução temporal e realiza a Análise Fatorial Exploratória (AFE) com rotação varimax.

**Como executar:** abra o arquivo no RStudio e pressione **Ctrl+Shift+S** (Source).

| Etapa | O que acontece | Saída |
|-------|---------------|-------|
| Carregamento | Lê `data/dataset_CGVN.RData` | — |
| Análise por capítulo | Calcula frequência de respostas por prática | **Tabela 1** no console |
| Gráficos temporais | Evolução trimestral das respostas por capítulo | PDFs em `figs/p1_*.pdf` e `figs/p2_*.pdf` |
| Estatísticas descritivas | Média, desvio, KMO individual, curtose etc. | **Tabela 2** no console |
| Adequação da amostra | KMO global e teste de Bartlett | Impressos no console |
| AFE varimax (7 fatores) | Cargas fatoriais e comunalidades | **Tabela 3** no console |
| Exportação | Prepara entrada para o Script 2 | `data/input_GA_scale.RData` |

---

### Script 2 — `scripts/script_GA_scale.R`

**O que faz:** constrói os 7 IGCs finais resolvendo um problema de otimização combinatória via Algoritmo Genético — encontra os pesos `{-1, 0, +1}` que tornam os índices ao mesmo tempo fiéis aos fatores da AFE e não correlacionados entre si.

**Como executar:** abra o arquivo no RStudio e pressione **Ctrl+Shift+S** (Source).

| Etapa | O que acontece | Saída |
|-------|---------------|-------|
| Carregamento | Lê `data/input_GA_scale.RData` | — |
| Diagnóstico inicial | Avalia o warm-start (solução pré-AG) | Correlações no console |
| Otimização (AG) | Busca os pesos ótimos `{-1, 0, +1}` | *(ver nota abaixo)* |
| Resultados | Pesos, correlações entre IGCs e entre IGCs × fatores | **Tabela 4** no console |
| Confiabilidade | Alpha de Cronbach por IGC | Impresso no console |
| Gráficos dos IGCs | Evolução trimestral de cada índice | PDFs em `figs/scale_*.pdf` |

> 💡 **Reproduzindo o artigo vs. rodando do zero:**
>
> O parâmetro `FLAG_LOAD_SCALE_CINCO2024` no início do script controla o modo de operação:
>
> ```r
> FLAG_LOAD_SCALE_CINCO2024 = TRUE   # (padrão) carrega a solução exata do artigo
> FLAG_LOAD_SCALE_CINCO2024 = FALSE  # roda o AG do zero (pode levar horas)
> ```
>
> Mantenha `TRUE` para reproduzir instantaneamente a Tabela 4. Altere para `FALSE` apenas se quiser explorar soluções alternativas — o AG é estocástico e pode gerar resultados ligeiramente diferentes mesmo com semente fixa, dependendo do hardware e das versões dos pacotes.

---

## Reprodução dos resultados

A tabela abaixo lista os resultados numéricos reportados no artigo e como verificá-los:

| Resultado | Script | Referência no artigo | Valor esperado |
|-----------|--------|----------------------|----------------|
| Razão obs./variáveis | 1 | Seção 3.1 | 42:1 (N=2.294, M=54) |
| KMO global | 1 | Seção 4.3.1 | **0,928** |
| Teste de Bartlett | 1 | Seção 4.3.1 | χ²(1431) = 60.094,91; p < 0,001 |
| Limiar de carga significativa | 1 | Seção 3.3.4, Eq. (1) | **≈ 0,123** (para N=2.294) |
| Número de fatores retidos | 1 | Seção 4.3.2 | **7** (solução mais parcimoniosa) |
| Variância explicada total | 1 | Seção 4.3.3 / Tab. 3 | **43,69%** |
| Autovalores (F1–F7) | 1 | Tab. 3 | 7,54; 3,82; 3,71; 2,34; 2,30; 2,18; 1,71 |
| Correlação máxima entre IGCs | 2 | Seção 4.3.5 / Tab. 4 | **0,048** (DIR–CC) |
| Correlação mínima IGC × fator | 2 | Seção 4.3.5 / Tab. 4 | **0,684** (SEM–Fator5) |

---

## Metodologia resumida

```
┌─────────────────────────────────────────────────────────────────┐
│  DADOS  →  Portal de Dados Abertos CVM (ICBGC)                  │
│             495 empresas │ 2.294 informes │ 54 práticas         │
└───────────────────────────────┬─────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│  CONVERSÃO  →  Qualitativo para numérico                        │
│   "Não se Aplica"=0 │ "Não"=1 │ "Parcialmente"=2 │ "Sim"=3      │
└───────────────────────────────┬─────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│  ADEQUAÇÃO  →  KMO (0,928) e Bartlett (p < 0,001)  ✓            │
└───────────────────────────────┬─────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│  AFE  →  psych::fa() │ rotação varimax │ 7 fatores              │
│          variância explicada: 43,69%                            │
│                                                                 │
│   Fator 1: Controles e Compliance (CC)                          │
│   Fator 2: Regulações Estatutárias (REG)                        │
│   Fator 3: Conselho de Administração (ADM)                      │
│   Fator 4: Diretoria (DIR)                                      │
│   Fator 5: Sociedades de Economia Mista (SEM)                   │
│   Fator 6: Medidas de Defesa (DEF)                              │
│   Fator 7: Conselho Fiscal (FIS)                                │
└───────────────────────────────┬─────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│  OTIMIZAÇÃO  →  Algoritmo Genético (GA)                         │
│                                                                 │
│  Cromossomo: pesos ∈ {-1, 0, +1} por variável × fator           │
│                                                                 │
│  Objetivo (Eq. 2):                                              │
│    min [ ‖corr(S) − I‖² + ‖corr(S,F) − corr(F)‖² ]              │
│                                                                 │
│  Restrição (Eq. 3):                                             │
│    Σ|X_ij| ≤ 1 ∀i  →  cada variável entra em 1 IGC no máximo    │
└───────────────────────────────┬─────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│  7 IGCs VALIDADOS  →  escala [0, 10]                            │
│   Correlação máxima entre IGCs:    0,048  ✓                     │
│   Correlação mínima IGC × fator:   0,684  ✓                     │
└─────────────────────────────────────────────────────────────────┘
```

---

## Como citar

```bibtex
@inproceedings{cgvn.cinco.2024,
	title        = {Sete Dimensões da Governança Corporativa no Brasil:	Índices e Disponibilização de Dados},
	author       = {Marcos Vinicius Lopes Pereira and Camila Teresa Martucheli and Simone Evangelista Fonseca and Yuri Fonseca Lopes},
	year         = {2024},
	booktitle    = {Anais do Congresso Interinstitucional de Contabilidade e Controladoria (CINCO)},
	note 		 = {Evento Online},
	url          = {https://www.even3.com.br/anais/ii-congresso-interinstitucional-de-contabilidade-e-controladoria-502331/1042664-sete-dimensoes-da-governanca-corporativa-no-brasil--indices-e-disponibilizacao-de-dados},
	address      = {Santa Maria, RS},
	ISBN         = {978-65-272-1134-1}
}
```

---

## Referências

- CVM (2022). *Resolução CVM nº 80*. Diário Oficial da União. Brasil.
- CVM (2024). *Portal Dados Abertos CVM*. https://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/CGVN/DADOS/
- Hair, J. F., Black, W. C., Babin, B. J., & Anderson, R. E. (2014). *Multivariate Data Analysis* (7ª ed.). Pearson.
- IBGC (2023). *Código de Melhores Práticas de Governança Corporativa* (6ª ed.).
- R Core Team (2023). *R: A Language and Environment for Statistical Computing*. R Foundation for Statistical Computing.
- Scrucca, L. (2013). GA: A Package for Genetic Algorithms in R. *Journal of Statistical Software*, 53(4), 1–37.
- Scrucca, L. (2017). On some extensions to GA package. *The R Journal*, 9(1), 187–206.
- Steiner, M. D., & Grieder, S. (2020). EFAtools. *Journal of Open Source Software*, 5(53), 2521.

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

## Contribuindo

Contribuições são bem-vindas, especialmente em:

- melhoria da documentação;
- modularização dos scripts;
- criação de funções auxiliares;
- exportação de resultados em formatos mais amigáveis;
- exemplos reproduzíveis para terceiros.

### Fluxo sugerido

1. faça um fork do repositório;
2. crie uma branch para sua contribuição;
3. implemente a alteração;
4. documente o que foi modificado;
5. abra um pull request com descrição objetiva.

### Recomendações para contribuições

- preserve compatibilidade com execução por caminhos relativos;
- evite dependências desnecessárias;
- prefira alterações incrementais e bem documentadas;
- inclua comentários claros quando a lógica estatística não for imediata.


