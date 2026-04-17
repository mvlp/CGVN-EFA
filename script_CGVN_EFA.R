# =============================================================================
#  SCRIPT 1: script_CGVN_EFA_github.R
#  Sete Dimensões da Governança Corporativa no Brasil:
#  Índices e Disponibilização de Dados
#
#  Pereira, M. V. L.; Martucheli, C. T.; Fonseca, S. E.; Lopes, Y. F. (2024)
#  Financiamento: FAPEMIG, Chamada 01/2022, Demanda Universal
#
#  OBJETIVO:
#    Este script implementa as etapas de coleta, pré-processamento, análise
#    descritiva e Análise Fatorial Exploratória (AFE) dos dados do Informe
#    sobre o Código Brasileiro de Governança Corporativa (ICBGC), publicado
#    pelas empresas de capital aberto e disponibilizado pela CVM por meio do
#    Portal Dados Abertos (https://dados.cvm.gov.br).
#
#  FLUXO DO SCRIPT:
#    1. Carregamento e configuração
#    2. Definição de funções auxiliares (gráficos, estatísticas, AFE)
#    3. Análise por capítulo do ICBGC → Tabela 1 e Figuras 1–2 do artigo
#    4. Análise descritiva agregada   → Tabela 2
#    5. AFE com rotação varimax       → Tabela 3
#    6. Exportação de dados para o script de otimização (script_GA_scale.R)
#
#  COMO EXECUTAR:
#    Use Ctrl+Shift+S ("Source") para rodar o script completo sem eco no
#    console; ou Ctrl+Shift+Enter para rodar linha a linha no RStudio.
#
# =============================================================================

rm(list = ls())   # Remove todos os objetos do ambiente para garantir execução limpa
cat("\014")       # Limpa o console (equivalente a Ctrl+L)
setwd(dirname(sys.frame(1)$ofile))  # Define o diretório de trabalho como o do próprio script


# =============================================================================
# SEÇÃO 1 – PACOTES
# =============================================================================
# Manipulação e transformação de dados
library(tibble)   # Visualização aprimorada de data.frames
library(dplyr)    # Verbos de transformação (filter, mutate, group_by, etc.)
library(tidyr)    # Reorganização de dados (pivot_longer/wider)


# =============================================================================
# SEÇÃO 2 – FUNÇÕES AUXILIARES
# =============================================================================

# -----------------------------------------------------------------------------
# 2.1  Helpers internos de visualização
# -----------------------------------------------------------------------------

# Carrega pacotes de gráfico sob demanda (evita carregar na inicialização global)
.load_chart_libs <- function() {
    library(ggplot2)   # Gramática de gráficos
    library(zoo)       # Manipulação de séries temporais (yearqtr, yearmon)
    library(extrafont) # Suporte a fontes adicionais (e.g., CM Roman para LaTeX)
    loadfonts(device = "pdf", quiet = TRUE)
}

# Define o tema visual padrão dos gráficos do artigo.
# flag_font_latex = TRUE → usa fonte "CM Roman" para compatibilidade com LaTeX
.chart_theme <- function(flag_font_latex) {
    base_size <- 12
    
    base <- theme(
        strip.text.y    = element_text(size = base_size,     family = if (flag_font_latex) "CM Roman" else NULL),
        axis.text       = element_text(size = base_size - 2, family = if (flag_font_latex) "CM Roman" else NULL),
        axis.title      = element_text(size = base_size + 2, family = if (flag_font_latex) "CM Roman" else NULL),
        plot.title      = element_text(size = base_size, hjust = 0.5, family = if (flag_font_latex) "CM Roman" else NULL),
        legend.position = "bottom",
        legend.title    = element_blank()
    )
    
    if (flag_font_latex)
        list(
            base,
            theme(
                legend.text     = element_text(size = base_size + 2, family = "CM Roman"),
                legend.key.size = unit(2, "lines")
            ),
            guides(color = guide_legend(override.aes = list(size = 2.5)))
        )
    else
        list(base)
}


# -----------------------------------------------------------------------------
# 2.2  gen_chart() — Gera e salva Figuras 1 e 2 do artigo
# -----------------------------------------------------------------------------
#
# REFERÊNCIA NO ARTIGO: Seção 4.1.1 e Figuras 1–2
#
# Dois tipos de gráfico são suportados:
#   "freq_response"   → evolução trimestral das respostas (Figura 1)
#                        Eixo Y: percentual médio de cada resposta (%)
#                        Faixa sombreada: intervalo entre percentis 5% e 95%
#   "delivery_report" → volume trimestral de informes entregues (Figura 1)
#                        Eixo Y: quantidade absoluta de respostas
#
# Parâmetros:
#   data_block      – data.frame filtrado por capítulo do ICBGC
#   short_chapter   – sigla do capítulo (e.g., "AC", "CA") usada no nome do arquivo
#   type            – tipo de gráfico (ver acima)
#   flag_font_latex – usa fonte CM Roman para exportação em LaTeX (padrão: TRUE)
#   format          – formato de exportação ("pdf", "png", etc.)
#
# Saída: arquivo salvo em ../figs/<prefixo>_<short_chapter>.<format>

gen_chart <- function(data_block,
                      short_chapter,
                      type            = c("freq_response", "delivery_report"),
                      flag_font_latex = TRUE,
                      format          = "pdf") {
    
    type <- match.arg(type)
    .load_chart_libs()
    
    # Converte a coluna de data conforme granularidade do gráfico:
    #   freq_response   → trimestral (yearqtr)
    #   delivery_report → mensal    (yearmon)
    if (type == "freq_response") {
        data_block$Data_Entrega <- zoo::as.yearqtr(data_block$Data_Entrega)
    } else {
        data_block$Data_Entrega <- zoo::as.yearmon(data_block$Data_Entrega)
    }
    
    # Calcula a frequência relativa de cada tipo de resposta por empresa/entrega/documento,
    # depois agrega a média e os percentis 5%–95% ao longo do tempo.
    # Isso gera a série temporal que aparece nos gráficos do artigo (Figura 1).
    tabela_freq <- data_block |>
        group_by(CNPJ_Companhia, Data_Entrega, ID_Documento, Pratica_Adotada) |>
        summarise(cnt = n(), .groups = "drop") |>
        group_by(CNPJ_Companhia, Data_Entrega, ID_Documento) |>
        mutate(freq = cnt / sum(cnt)) |>
        ungroup()
    
    summary_freq <- tabela_freq |>
        group_by(Data_Entrega, Pratica_Adotada) |>
        summarise(
            ybar  = 100 * mean(freq),              # média percentual das respostas
            ylow  = 100 * quantile(freq, probs = 0.05),  # percentil 5%
            yhigh = 100 * quantile(freq, probs = 0.95),  # percentil 95%
            n     = sum(cnt),                        # contagem total
            .groups = "drop"
        )
    
    # Monta o gráfico base com facet por tipo de resposta
    base_plot <- ggplot(summary_freq, aes(x = Data_Entrega)) +
        facet_grid(rows = vars(Pratica_Adotada)) +
        xlab("Data de Entrega") +
        ggtitle(data_block$Capitulo[1]) +
        theme_bw() +
        .chart_theme(flag_font_latex)
    
    if (type == "freq_response") {
        # Seleciona apenas datas do 4º trimestre para os rótulos do eixo X
        chart_breaks <- unique(summary_freq$Data_Entrega)
        chart_breaks <- chart_breaks[lubridate::quarter(chart_breaks) == 4]
        
        # Gráfico de frequência de respostas com faixa de incerteza (ribbon)
        p <- base_plot +
            aes(y = ybar) +
            geom_ribbon(aes(ymin = ylow, ymax = yhigh), alpha = alpha_level_ribbon) +
            geom_line(color  = "blue") +
            geom_point(color = "blue") +
            coord_cartesian(ylim = c(0, 100)) +
            ylab("Percentual de respostas (%)") +
            scale_x_yearqtr(
                breaks = chart_breaks,
                format = "%qT-%Y"   # formato "4T-2023"
            )
        prefix <- "p1"
    } else {
        # Gráfico de volume de entregas
        p <- base_plot +
            aes(y = n) +
            geom_line(color  = "black") +
            geom_point(color = "black") +
            ylab("Quantidade de respostas")
        prefix <- "p2"
    }
    
    # Salva o gráfico em alta resolução (600 dpi) no formato especificado
    image_file <- sprintf("../figs/%s_%s.%s", prefix, short_chapter, format)
    
    ggsave(
        filename = image_file,
        plot     = p,
        dpi      = 600,
        width    = 183,   # largura em mm (padrão coluna dupla de periódico)
        height   = 183,
        units    = "mm",
        device   = format
    )
    
    # Embute as fontes no PDF para garantir portabilidade (necessário para LaTeX)
    if (flag_font_latex && format == "pdf")
        embed_fonts(image_file)
    
    invisible(p)
}


# -----------------------------------------------------------------------------
# 2.3  descriptive_dataset_long() — Prepara dados e chama gen_stats_table()
# -----------------------------------------------------------------------------
# Recebe o dataset no formato longo (uma linha por prática × empresa × data),
# converte para o formato largo (uma linha por empresa × data, colunas = práticas)
# e calcula as estatísticas descritivas do artigo (Tabela 2).
#
# REFERÊNCIA NO ARTIGO: Seção 3.2 e Tabela 2

descriptive_dataset_long = function(local_dataset, cor.method)
{
    # Pivota para formato largo: linhas = observações, colunas = práticas (ID_Item)
    local_dataset_wide = reshape(local_dataset[,c('CNPJ_Companhia','Data_Entrega','ID_Documento','ID_Item','gc_value')],
                                 timevar = 'ID_Item', idvar = c('CNPJ_Companhia','Data_Entrega','ID_Documento'), 
                                 direction = 'wide')
    colnames(local_dataset_wide) = gsub("gc_value.", "", colnames(local_dataset_wide))
    
    # Remove colunas de identificação, mantendo apenas as variáveis de GC
    local_dataset_wide_no_ID = local_dataset_wide[,-which(colnames(local_dataset_wide) %in% c('CNPJ_Companhia','Data_Entrega','ID_Documento'))]
    
    # Descarta observações com NA em qualquer coluna (análise de casos completos).
    # O artigo reporta N = 2.294 observações após este filtro (Seção 3.1).
    linhas_validas = complete.cases(local_dataset_wide_no_ID)
    cat(sprintf('\n\tWARNING:: descartando %d relatórios contendo NA em qualquer uma das colunas\n', sum(!linhas_validas))) 
    
    local_dataset_wide_no_ID = local_dataset_wide_no_ID[linhas_validas,]
    local_descriptive_stats = gen_stats_table(local_dataset_wide_no_ID,
                                              alias = sort(unique(local_dataset$ID_Item)),
                                              cor.method = cor.method)
    
    return(local_descriptive_stats)
}


# -----------------------------------------------------------------------------
# 2.4  gen_stats_table() — Gera Tabela 2 do artigo
# -----------------------------------------------------------------------------
# Calcula estatísticas descritivas, correlações (simples e parciais),
# e testes de adequação da amostra (KMO e Bartlett) para um conjunto de variáveis.
#
# REFERÊNCIA NO ARTIGO: Seção 3.2, Seção 3.3.1 e Tabela 2
#
# Métricas calculadas:
#   Descritivas: Mínimo, Quartis, Média, Mediana, Máximo, Desvio Padrão,
#                Assimetria, Curtose, Coeficiente de Variação
#   Adequação:   KMO individual por variável (Kaiser, 1970)
#   Correlações: Pearson ou Spearman (definido em correlation.method)
#   Correlações parciais: controlando todas as demais variáveis (ppcor)
#   Bartlett:    Testa se a matriz de correlação difere da identidade (Bartlett, 1951)

gen_stats_table = function(dataset, 
                           alias, 
                           cor.method = "pearson",
                           language = 'PT-BR')
{
    force(dataset)
    table_stats = fBasics::basicStats(dataset)
    
    # Seleciona e renomeia as linhas conforme o idioma desejado
    if (language == 'PT-BR')
    {
        table_stats = table_stats[c("Minimum","1. Quartile", "Mean", "Median","3. Quartile","Maximum","Stdev","Skewness","Kurtosis"),]
        rownames(table_stats) = c("Mínimo","1º quartil", "Média", "Mediana", "3º quartil", "Máximo","Desvio P.","Assimetria","Curtose")
    }
    else
    {
        table_stats = table_stats[c("Mean","Stdev","Minimum","Median","Maximum","Skewness","Kurtosis"),]
        rownames(table_stats) = c("Avg", "Std. dev.", "Minimum", "Median", "Maximum", "Skewness", "Kurtosis")
    }
    colnames(table_stats) = alias
    
    # Coeficiente de variação: σ/μ (medida de dispersão relativa)
    if (language == 'PT-BR')
    {
        table_stats["Coef. Vari.",] = table_stats["Desvio P.",]/table_stats["Média",]
    }
    else
    {
        table_stats["CV",] = table_stats["Std. dev.",]/table_stats["Avg",]
    }

    # Matriz de correlações com p-valores e estatísticas de teste
    # (Pearson ou Spearman conforme o método especificado)
    table_correlation = vector("list",3)
    table_correlation[[1]] = NA*cor(dataset)   # [[1]] = correlações
    rownames(table_correlation[[1]]) = alias
    table_correlation[[2]] = NA*cor(dataset)   # [[2]] = p-valores
    table_correlation[[3]] = NA*cor(dataset)   # [[3]] = estatísticas de teste
    for (i in 1:ncol(dataset)) 
    {
        for (j in 1:ncol(dataset)) 
        {
            if (i != j)
            {
                test_cor = cor.test(dataset[,i],
                                    dataset[,j],
                                    method = cor.method)
                table_correlation[[1]][i,j] = test_cor$estimate
                table_correlation[[2]][i,j] = test_cor$p.value
                if (is.na(test_cor$estimate))
                {
                    # browser()
                }
                # Para Spearman, calcula Z-score manualmente (aproximação para N grande)
                if (cor.method == 'pearson')
                {
                    table_correlation[[3]][i,j] = test_cor$statistic
                }
                else
                {
                    n = nrow(dataset)
                    Z_stats = (sqrt(n - 2) * test_cor$estimate) / sqrt(1 - test_cor$estimate^2)
                    table_correlation[[3]][i,j] = Z_stats
                }
            }
            else
            {
                table_correlation[[1]][i,j] = 1
                table_correlation[[2]][i,j] = 0
            }
        }
    }
    
    # Correlações parciais: relação entre par de variáveis removendo
    # o efeito linear das demais variáveis do conjunto
    table_partial_correlation = ppcor::pcor(dataset, method = cor.method)
    
    # ─────────────────────────────────────────────────────────────────────────
    # Testes de adequação para a AFE (Seção 3.3.1 do artigo)
    # ─────────────────────────────────────────────────────────────────────────
    
    # KMO (Kaiser-Meyer-Olkin): valores > 0,5 indicam adequação.
    # O artigo reporta KMO global = 0,928 e todos os KMOs individuais > 0,5.
    KMO_corr = EFAtools::KMO(dataset, cor_method = cor.method)
    table_stats["KMO",] = KMO_corr$KMO_i   # adiciona linha de KMO à tabela descritiva
    
    # Bartlett: rejeitar H0 (matriz = identidade) confirma que há correlações
    # entre as variáveis, pré-requisito para a AFE.
    # O artigo reporta χ²(1431) = 60.094,91, p < 0,1%.
    BTS_test = EFAtools::BARTLETT(dataset, cor_method = cor.method)
    
    return(list(descriptive = table_stats,
                correlation = table_correlation,
                partial.correlation = table_partial_correlation,
                KMO = KMO_corr,
                BTS = BTS_test,
                method = cor.method))
}


# -----------------------------------------------------------------------------
# 2.5  component_dataset_long() — Determina o número de fatores a reter (AFE)
# -----------------------------------------------------------------------------
# Aplica múltiplos critérios de retenção de fatores e testes sequenciais,
# seguindo as recomendações de Henson & Roberts (2006) e Steiner & Grieder (2020).
#
# REFERÊNCIA NO ARTIGO: Seção 3.3.3
#
# Critérios implementados via EFAtools::N_FACTORS():
#   - Comparison data (cd)
#   - Critério empírico de Kaiser
#   - Critério de Kaiser-Guttman (autovalor > 1)
#   - Análise paralela (Horn, 1965)
#   - Testes sequenciais do Qui-Quadrado (via factanal)
#   - Limite inferior do IC 90% do RMSEA
#   - Critério de Informação de Akaike (AIC)
#
# O artigo reporta resultados inconclusivos (entre 4 e 38 fatores)
# e optou por 7 fatores pela parcimônia e interpretabilidade.

component_dataset_long = function(local_dataset)
{
    local_dataset_wide = reshape(local_dataset[,c('CNPJ_Companhia','Data_Entrega','ID_Documento','ID_Item','gc_value')],
                                 timevar = 'ID_Item', idvar = c('CNPJ_Companhia','Data_Entrega','ID_Documento'), 
                                 direction = 'wide')
    colnames(local_dataset_wide) = gsub("gc_value.", "", colnames(local_dataset_wide))
    
    local_dataset_wide_no_ID = local_dataset_wide[,-which(colnames(local_dataset_wide) %in% c('CNPJ_Companhia','Data_Entrega','ID_Documento'))]
    
    linhas_validas = complete.cases(local_dataset_wide_no_ID)
    cat(sprintf('\n\tWARNING:: descartando %d relatórios contendo NA em qualquer uma das colunas\n', sum(!linhas_validas))) 
    local_dataset_wide_no_ID = local_dataset_wide_no_ID[linhas_validas,]
    
    # Aplica todos os critérios de retenção de fatores disponíveis no EFAtools
    n.factors = EFAtools::N_FACTORS(local_dataset_wide_no_ID)

    # Monta tabela de autovalores com percentual e percentual acumulado da variância
    table_6 = data.frame(Component = 1:length(n.factors$outputs$cd_out$eigenvalues),
                         Eigenvalues = n.factors$outputs$cd_out$eigenvalues,
                         perc_var = NA,
                         cum_var = NA)
    sum_eigen = sum(table_6[,'Eigenvalues'])
    table_6[,'perc_var']  = 100*table_6[,'Eigenvalues']/sum_eigen      # % de variância explicada
    table_6[,'cum_var']   = cumsum(table_6[,'perc_var'])                # % acumulado
    table_6[,'dy_dx']     = c(NA, diff(100*table_6[,'Eigenvalues']))    # 1ª derivada dos autovalores
    table_6[,'d2y_dx2']   = c(NA, diff(table_6[,'dy_dx']))              # 2ª derivada (cotovelo do Scree)
    table_6[,'dvar']      = c(NA, diff(table_6[,'cum_var']))
    table_6[,'d2var']     = c(NA, diff(table_6[,'dvar']))
    
    # Teste sequencial do Qui-Quadrado via factanal:
    # incrementa o número de fatores até que o p-valor supere o nível de significância,
    # indicando que o modelo com aquele número de fatores não pode ser rejeitado.
    factors_suggested = 1
    level_confidence = 5/100
    for (i in 1:ncol(local_dataset_wide_no_ID)) 
    {
        flag_error = FALSE
        tryCatch(
            {
                factors_N = factanal(local_dataset_wide_no_ID, 
                                     factors = factors_suggested, 
                                     rotation = "none")
            },
            error = function(cond) {
                flag_error = TRUE
            })
        
        if (!flag_error)
        {
            if(factors_N$PVAL > level_confidence) { break }
        }
        else { break }
        
        if (i < ncol(local_dataset_wide_no_ID))
        {
            factors_suggested = factors_suggested + 1    
        }
    }
    
    return(list(table_EFA = table_6,
                sufficient_factors = list(factanal = factors_suggested,
                                          criteria = n.factors),
                sufficient_EFA = n.factors$outputs$parallel_out$n_fac_EFA,
                sufficient_PCA = n.factors$outputs$parallel_out$n_fac_PCA))
}


# -----------------------------------------------------------------------------
# 2.6  sort_loadings() — Ordena variáveis por maior carga fatorial
# -----------------------------------------------------------------------------
# Utilidade interna: reordena as variáveis de acordo com o fator em que
# apresentam a maior carga (em valor absoluto), facilitando a leitura
# da matriz de cargas (Tabela 3 do artigo).

sort_loadings = function(x, sort=TRUE)
{
    load <- x$loadings
    if (is.null(cut)) 
        cut <- 0
    nitems <- dim(load)[1]
    nfactors <- dim(load)[2]
    if (sum(x$uniqueness) + sum(x$communality) > nitems) 
    {
        covar <- TRUE
    }
    else 
    {
        covar <- FALSE
    }
    loads <- data.frame(item = seq(1:nitems), cluster = rep(0, nitems), unclass(load))
    u2.order <- 1:nitems
    if (sort) 
    {
        loads$cluster <- apply(abs(load), 1, which.max)
        ord <- sort(loads$cluster, index.return = TRUE)
        loads[1:nitems, ] <- loads[ord$ix, ]
        rownames(loads)[1:nitems] <- rownames(loads)[ord$ix]
        items <- table(loads$cluster)
        first <- 1
        item <- loads$item
        for (i in 1:length(items)) 
        {
            if (items[i] > 0) 
            {
                last <- first + items[i] - 1
                ord <- sort(abs(loads[first:last, i + 2]), 
                            decreasing = TRUE, 
                            index.return = TRUE)
                u2.order[first:last] <- item[ord$ix + first - 1]
                loads[first:last, 3:(nfactors + 2)] <- load[item[ord$ix + first - 1], ]
                loads[first:last, 1] <- item[ord$ix + first - 1]
                rownames(loads)[first:last] <- rownames(loads)[ord$ix + first - 1]
                first <- first + items[i]
            }
        }
    }
    ncol <- dim(loads)[2] - 2
    
    return(list(item = loads$item,
                cluster = loads$cluster))
}


# -----------------------------------------------------------------------------
# 2.7  estimate_loading_cut_off() — Limiar de carga fatorial significativa
# -----------------------------------------------------------------------------
# Estima o valor mínimo de carga fatorial considerado significativo, com base
# no tamanho amostral N, conforme recomendações de Hair et al. (2014, p.115).
#
# REFERÊNCIA NO ARTIGO: Seção 3.3.4, Equação (1)
# Modelo paramétrico: limiar = C · N^a  (ajustado por regressão log-log)
#   C ≈ 4,912309   a ≈ −0,476427
#
# Para N = 2.294 (tamanho amostral do artigo), o limiar estimado é ≈ 0,123.

estimate_loading_cut_off = function(N)
{
    # Lê tabela de referência com valores de corte para diferentes N
    loading_cut_off = read.csv(file = '../data/loading_cut_off.csv', header = TRUE)
    colnames(loading_cut_off) = c('cut_off', 'N')
    # Ajusta modelo log-log por regressão linear
    modelo = lm(log(cut_off) ~ log(N), data = loading_cut_off)
    # Transforma de volta para escala original
    loading_cut_off = as.numeric(exp(coef(modelo) %*% c(1, log(N))))  
    return(loading_cut_off)
}


# -----------------------------------------------------------------------------
# 2.8  factors_dataset_long() — Executa AFE ou ACP e retorna cargas e escores
# -----------------------------------------------------------------------------
# Realiza a Análise Fatorial Exploratória (AFE) ou a Análise de Componentes
# Principais (ACP) sobre o conjunto de dados, com rotação especificada.
#
# REFERÊNCIA NO ARTIGO: Seções 3.3.2–3.3.4 e Tabela 3
#
# Parâmetros:
#   local_dataset    – dataset em formato longo
#   rotation         – método de rotação ("varimax", "quartimax", "equamax")
#   factors_selected – número de fatores a reter
#   mode             – "EFA" (padrão) ou "PCA"
#   exclude_var      – índices de colunas a excluir da análise
#
# O artigo compara varimax, quartimax e equamax, selecionando varimax pela
# melhor combinação entre minimização de cargas cruzadas e distribuição
# equilibrada das variáveis entre fatores.

factors_dataset_long = function(local_dataset,
                                rotation,
                                factors_selected, 
                                mode = 'EFA',
                                exclude_var = NULL)
{
    local_dataset_wide = reshape(local_dataset[,c('CNPJ_Companhia','Data_Entrega','ID_Documento','ID_Item','gc_value')],
                                 timevar = 'ID_Item', idvar = c('CNPJ_Companhia','Data_Entrega','ID_Documento'), 
                                 direction = 'wide')
    colnames(local_dataset_wide) = gsub("gc_value.", "", colnames(local_dataset_wide))
    
    local_dataset_wide_no_ID = local_dataset_wide[,-which(colnames(local_dataset_wide) %in% c('CNPJ_Companhia','Data_Entrega','ID_Documento'))]

    linhas_validas = complete.cases(local_dataset_wide_no_ID)
    cat(sprintf('\n\tWARNING:: descartando %d relatórios contendo NA em qualquer uma das colunas\n', sum(!linhas_validas))) 
    local_dataset_wide_no_ID = local_dataset_wide_no_ID[linhas_validas,]
    
    if (!is.null(exclude_var))
    {
        local_dataset_wide_no_ID = local_dataset_wide_no_ID[,-exclude_var]
    }
    
    # Executa o método selecionado (PCA ou EFA)
    if (mode == 'PCA')
    {
        factors_N = psych::principal(local_dataset_wide_no_ID, 
                                     nfactors = factors_selected, 
                                     rotate = rotation)
    }
    else
    {
        if (mode == 'EFA')
        {
            # psych::fa() implementa AFE com extração de fatores comuns,
            # eliminando variância específica e de erro (diferentemente do PCA)
            factors_N = psych::fa(local_dataset_wide_no_ID, 
                                  nfactors = factors_selected, 
                                  rotate = rotation)
        }
        else 
        { 
            browser() 
        }
    }
    
    # Monta tabela de cargas fatoriais + comunalidades (Tabela 3 do artigo)
    table_7_A = data.frame(Variables = colnames(local_dataset_wide_no_ID),
                           factors = factors_N$loadings[1:ncol(local_dataset_wide_no_ID),1:factors_selected],
                           Communality = factors_N$communality)
    
    # Soma dos quadrados das cargas (autovalores após rotação) e % da variância explicada
    sum_eigen = ncol(local_dataset_wide_no_ID)
    SS = rbind(colSums(table_7_A[,2:(factors_selected+1)]^2),
               100*colSums(table_7_A[,2:(factors_selected+1)]^2)/sum_eigen)
    table_7_B = data.frame(Variables = c('Soma dos Quadrados (autovalor)','Percentual do Traço'),
                           SS,
                           Total = rowSums(SS))
    
    table_7_A_sorted = table_7_A
    sorted_loadings = sort_loadings(factors_N)
    
    # Calcula escores fatoriais pelo método de Thurstone (regressão)
    escores_fatoriais = psych::factor.scores(local_dataset_wide_no_ID, 
                                      factors_N$loadings, 
                                      method = "Thurstone")
    
    # Padroniza os escores para média 0 e desvio padrão 1
    escores_padronizados = scale(escores_fatoriais$scores)
    
    # Índice composto ponderado pela variância explicada de cada fator
    variancia_explicada = factors_N$Vaccounted[2, ]
    pesos = variancia_explicada / sum(variancia_explicada)
    indice_composto = escores_padronizados %*% pesos
    colnames(indice_composto) = sprintf("%s.%s",'F',dataset$Capitulo[1])
    
    # Escala somada simples (soma de todas as variáveis, sem ponderação)
    escala_somada = as.matrix(rowSums(local_dataset_wide_no_ID))
    colnames(escala_somada) = sprintf("%s.%s",'S',dataset$Capitulo[1])
    
    return(list(factors = table_7_A, 
                stats = table_7_B,
                EFA = factors_N,
                loading_cut_off = estimate_loading_cut_off(nrow(local_dataset_wide_no_ID)),
                sorted = sorted_loadings,
                idx.composite = indice_composto,
                idx.summed = escala_somada))
}


# =============================================================================
# SEÇÃO 3 – CONFIGURAÇÕES
# =============================================================================

dataset_path = '../data/'

# Método de correlação para todas as análises.
# O artigo usa Spearman por ser mais robusto a outliers e a distribuições
# assimétricas (como as observadas nas variáveis ordinais do ICBGC).
correlation.method = 'spearman'

# Opacidade da faixa de incerteza (ribbon) nos gráficos (0 = transparente, 1 = opaco)
alpha_level_ribbon = 0.25


# =============================================================================
# SEÇÃO 4 – CARREGAMENTO DOS DADOS
# =============================================================================
# O arquivo 'dataset_CGVN.RData' é disponibilizado pelo site
# https://mvlp.github.io/celta/#/governance
# que baixa e processa os CSVs anuais do Portal Dados Abertos da CVM
# (https://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/CGVN/DADOS/).
#
# Estrutura do dataset (formato longo):
#   CNPJ_Companhia  – identificador único da empresa
#   Data_Entrega    – data de envio do informe
#   ID_Documento    – identificador único do documento
#   ID_Item         – código da prática (e.g., "1.1.1", "2.3.1")
#   Capitulo        – capítulo do ICBGC (1=Acionistas, ..., 5=Ética e Conflito de Interess)
#   Principio       – nome do princípio
#   Pratica_Recomendada – descritivo de como a prática deve ser adotada
#   Pratica_Adotada – resposta qualitativa ("Sim", "Não", "Parcialmente", "Não se Aplica")
#   gc_value        – resposta convertida para escala numérica (0, 1, 2 ou 3)
#                     Escala de conversão (Seção 3.1.1 do artigo):
#                       "Não se Aplica" → 0
#                       "Não"           → 1
#                       "Parcialmente"  → 2
#                       "Sim"           → 3

load(paste0(dataset_path, "dataset_CGVN.RData"))

# Nomes curtos dos 5 capítulos do ICBGC:
#   AC  = Acionistas
#   CA  = Conselho de Administração
#   DR  = Diretoria
#   OFC = Órgãos de Fiscalização e Controle
#   ECI = Ética e Conflitos de Interesses
short_chapter_name = c('AC','CA','DR','OFC','ECI')


# =============================================================================
# SEÇÃO 5 – ANÁLISE POR CAPÍTULO
# Gera: Tabela 1, Figuras 1–2 e Tabela 2 (por capítulo)
# =============================================================================

for (i in 1:length(short_chapter_name)) 
{
    # Filtra as práticas do capítulo i (identificadas pelo prefixo numérico do ID_Item)
    table_i = dataset[grep(sprintf("^%d\\.",i), dataset$ID_Item),]
    table_i = table_i[!is.na(table_i$gc_value),]

    # ─── Tabela 1: Frequência das respostas por prática ─────────────────────
    # Calcula a proporção de cada tipo de resposta para cada ID_Item × Capítulo
    # REFERÊNCIA NO ARTIGO: Seção 4.1 e Tabela 1
    tabela_freq = table(table_i$ID_Item, table_i$Capitulo, table_i$Pratica_Adotada)
    proporcoes = margin.table(prop.table(tabela_freq, margin = c(1, 2)), c(1, 3))
    proporcoes = 100*proporcoes
    proporcoes = reshape(as.data.frame(proporcoes), 
                         timevar= 'Var2', 
                         idvar  = 'Var1',
                         direction='wide')
    colnames(proporcoes)[1] = 'ID_Item'
    colnames(proporcoes)[-1] = substr(colnames(proporcoes)[-1],nchar('Freq.')+1, nchar(colnames(proporcoes)[-1]))
    
    survey = table_i[,c('ID_Item',"Capitulo","Principio","Pratica_Recomendada")]
    survey = survey[!duplicated(survey),]
    
    table_freq_responses = merge.data.frame(survey, proporcoes, by = 'ID_Item')
    
    # Imprime Tabela 1 para o capítulo atual
    print(as_tibble(table_freq_responses))

    
    # ─── Figuras 1–2: Evolução temporal das respostas ───────────────────────
    # "freq_response"   → Figura 1: série temporal do % de cada resposta
    # "delivery_report" → Figura 1: volume de informes entregues por período
    gen_chart(table_i, 
              short_chapter_name[i], 
              type = "freq_response", 
              format = "pdf")
    gen_chart(table_i, 
              short_chapter_name[i], 
              type = "delivery_report", 
              format = "pdf")

    
    # ─── Tabela 2: Estatísticas descritivas e adequação da amostra ──────────
    # Gera as estatísticas descritivas do capítulo i, incluindo KMO e Bartlett.
    # REFERÊNCIA NO ARTIGO: Seção 4.2 e Tabela 2
    descriptive_i = descriptive_dataset_long(table_i, cor.method = correlation.method)
    
    cat(sprintf("\n## Estatísticas descritivas para o capítulo %s\n", short_chapter_name[i]))
    print(as_tibble(descriptive_i$descriptive, rownames = "variavel"), width = Inf)
    
    cat(sprintf("\n## Correlações entre as variáveis do capítulo %s\n", short_chapter_name[i]))
    print(as_tibble(descriptive_i$correlation[[1]]))

    cat(sprintf("\n## p-valores das correlações entre as variáveis do capítulo %s\n", short_chapter_name[i]))
    print(as_tibble(round(descriptive_i$correlation[[2]], 3)))
    
    cat(sprintf("\n## Correlações parciais entre as variáveis do capítulo %s\n", short_chapter_name[i]))
    print(as_tibble(descriptive_i$partial.correlation$estimate))
    
    cat(sprintf("\n## p-valores das correlações parciais entre as variáveis do capítulo %s\n", short_chapter_name[i]))
    print(as_tibble(round(descriptive_i$partial.correlation$p.value, 3)))

    # KMO por capítulo (reportado na nota de rodapé da Tabela 2)
    cat(sprintf("\n## KMO - Kaiser-Meyer-Olkin Measure of Sampling Adequacy para o capítulo %s\n", short_chapter_name[i]))
    print(descriptive_i$KMO)
    
    # Bartlett por capítulo
    cat(sprintf("\n## Teste de esfericidade de Bartlett para o capítulo %s\n", short_chapter_name[i]))
    print(descriptive_i$BTS)
    
}


# =============================================================================
# SEÇÃO 6 – ANÁLISE DESCRITIVA AGREGADA (todos os capítulos)
# Gera: Tabela 2 completa e KMO/Bartlett global (não reportado no artigo)
# REFERÊNCIA NO ARTIGO: Seção 4.3.1
# =============================================================================


descriptive_full = descriptive_dataset_long(dataset, cor.method = correlation.method)

cat("\n## Correlações entre todas as variáveis do IGC\n")
cat('\tAVISO::visualização omitida\n')
correlation_full = descriptive_full$correlation[[1]]
#View(correlation_full)

cat("\n## p-valores das correlações entre a todas as variáveis do IGC\n")
cat('\tAVISO::visualização omitida\n')
correlation_full_pvalues = round(descriptive_full$correlation[[2]],3)
#View(correlation_full_pvalues)

cat("\n## Correlações parciais entre a todas as variáveis do IGC\n")
cat('\tAVISO::visualização omitida\n')
partial_correlation_full = descriptive_full$partial.correlation$estimate
#View(partial_correlation_full)

cat("\n## p-valores das correlações parciais entre a todas as variáveis do IGC\n")
cat('\tAVISO::visualização omitida\n')
partial_correlation_full_pvalues = round(descriptive_full$partial.correlation$p.value,3)
#View(partial_correlation_full_pvalues)

# KMO global reportado no artigo: 0,928 (Seção 4.3.1)
# Bartlett global: χ²(1431) = 60.094,91, p < 0,1%
cat("\n## KMO - Kaiser-Meyer-Olkin Measure of Sampling Adequacy para o conjunto completo de dados do IGC\n")
KMO_full = descriptive_full$KMO
print(KMO_full)

# Bartlett global (Seção 4.3.1)
cat("\n## Teste de esfericidade de Bartlett para o conjunto completo de dados do IGC\n")
BTS_full = descriptive_full$BTS
print(BTS_full)


# =============================================================================
# SEÇÃO 7 – ANÁLISE FATORIAL EXPLORATÓRIA (AFE) GLOBAL
# Gera: Tabela 3 (matriz de cargas após rotação varimax)
# REFERÊNCIA NO ARTIGO: Seção 4.3 e Tabela 3
# =============================================================================

# Pivota para formato largo com todas as 54 práticas como colunas
dataset_wide = reshape(dataset[,c('CNPJ_Companhia','Data_Entrega','ID_Documento','ID_Item','gc_value')],
                       timevar = 'ID_Item', idvar = c('CNPJ_Companhia','Data_Entrega','ID_Documento'), 
                       direction = 'wide')
colnames(dataset_wide) = gsub("gc_value.", "", colnames(dataset_wide))

dataset_wide_no_ID = dataset_wide[,-which(colnames(dataset_wide) %in% c('CNPJ_Companhia','Data_Entrega','ID_Documento'))]

linhas_validas = complete.cases(dataset_wide_no_ID)
cat(sprintf('\n\tWARNING:: descartando %d relatórios contendo NA em qualquer uma das colunas\n', sum(!linhas_validas))) 
dataset_wide_no_ID = dataset_wide_no_ID[linhas_validas,]


# ─── 7.1 ACP (Análise de Componentes Principais) – comparação ─────────────
# Estimada para fins comparativos. O artigo optou pela AFE por não haver
# conhecimento prévio sobre a variância de erro dos dados (Seção 3.3.2).
# Referência: Hair et al. (2014), Tabela 6, p. 132

# Número de fatores retidos: 7 (selecionado após comparação de soluções com
# 4, 6, 7 e 9 fatores – ver Seção 4.3.2 do artigo)
n_factors_full = 7

factors_PCA = psych::principal(dataset_wide_no_ID,
                             nfactors = n_factors_full,
                             rotate = 'varimax')
print(factors_PCA, sort=TRUE)


# ─── 7.2 AFE com rotação varimax – solução final ──────────────────────────
# psych::fa() usa a metodologia de fatores comuns (common factor analysis),
# que, ao contrário da ACP, separa a variância compartilhada da variância
# específica e de erro de cada variável.
#
# Rotação varimax: maximiza a variância das cargas ao quadrado dentro de cada
# fator, buscando soluções "simples" onde cada variável carrega fortemente em
# poucos fatores. Selecionado pelo melhor equilíbrio entre minimização de
# cargas cruzadas e distribuição das variáveis (Seção 4.3.3 do artigo).
#
# Resultado gerado → Tabela 3:
#   - 7 fatores com autovalores: 7,54; 3,82; 3,71; 2,34; 2,30; 2,18; 1,71
#   - Variância explicada total: 43,69%
#   - Limiar de carga significativa: ≈ 0,123 (para N = 2.294)
#   - Comunalidades mínimas aceitáveis: 0,30

factors_EFA = psych::fa(dataset_wide_no_ID,
                      nfactors = n_factors_full,
                      rotate = 'varimax')

# Tabela 3 – Matriz da análise de fatores comuns rotacionadas por VARIMAX
print(factors_EFA, sort=TRUE)

# Correlação entre os escores fatoriais da AFE.
# Com rotação varimax (ortogonal), espera-se correlação próxima de zero entre fatores.
cor_factors = cor(factors_EFA$scores)
cat("\n## Correlação entre os fatores estimados\n")
print(round(cor_factors, 3))


# =============================================================================
# SEÇÃO 8 – ÍNDICES COMPOSTOS SIMPLES (por capítulo)
# Gerados como referência de comparação antes da otimização por AG
# =============================================================================

# Índices por capítulo: média simples das variáveis de cada capítulo
table_summed_chapter = matrix(NA, 
                              nrow = nrow(dataset_wide_no_ID),
                              ncol = length(short_chapter_name))
colnames(table_summed_chapter) = short_chapter_name
col_chapter = as.numeric(substr(colnames(dataset_wide_no_ID),1,1))

for (j in 1:length(short_chapter_name)) 
{
    cols_selected = which(col_chapter == j)
    table_summed_chapter[,j] = rowMeans(dataset_wide_no_ID[,cols_selected])
}

# Correlação entre os índices por capítulo.
# Problema: esses índices são fortemente correlacionados entre si,
# o que compromete a independência informacional entre as dimensões.
cat("\n## Correlação entre as escalas somadas (índices) agregados por capítulo\n")
print(round(cor(table_summed_chapter), 3))

# Correlação entre índices por capítulo e escores fatoriais da AFE.
# Um índice por capítulo ideal deveria se correlacionar fortemente
# com apenas um fator – o que raramente ocorre.
cor_chapter_factors = cor(table_summed_chapter, factors_EFA$scores)
cat("\n## Correlação entre os índices agregados por capítulo e os fatores estimados\n")
print(round(cor_chapter_factors, 3))


# =============================================================================
# SEÇÃO 9 – ÍNDICES COMPOSTOS VIA MAIOR CARGA (pré-otimização)
# Atribui cada variável ao fator em que tem a maior carga fatorial absoluta,
# sem resolver o problema de cargas cruzadas (cross-loadings).
# Este é o ponto de partida (warm-start) para o Algoritmo Genético.
# REFERÊNCIA NO ARTIGO: Seção 3.3.5 e início da Seção 4.3.4
# =============================================================================

variable_grt_loading = t(apply(factors_EFA$loadings, 1, function(x) 
{
    bin <- rep(0, length(x))
    bin[which.max(abs(x))] <- 1   # peso 1 no fator de maior carga, 0 nos demais
    bin
}))
    
table_summed_scale = as.matrix(dataset_wide_no_ID) %*% (variable_grt_loading)
colnames(table_summed_scale) = sprintf("F_%d",1:n_factors_full)

# Comparação: correlação entre índices por fator (maior carga) vs por capítulo.
# Esperado: menor correlação entre os índices por fator, confirmando maior independência.
cat("\n## Correlação entre as escalas somadas (índices) agregados por fator (maior carga)\n")
print(round(cor(table_summed_scale), 3))

cor_scale_factors = cor(table_summed_scale, factors_EFA$scores)
cat("\n## Correlação entre os índices por fator (maior carga) e os fatores estimados\n")
print(round(cor_scale_factors, 3))


# =============================================================================
# SEÇÃO 10 – EXPORTAÇÃO PARA O ALGORITMO GENÉTICO
# Os dados exportados alimentam o script_GA_scale.R, que resolve o problema
# de otimização combinatória para construção dos 7 IGCs finais.
# REFERÊNCIA NO ARTIGO: Seção 3.3.5, Equações (2) e (3)
# =============================================================================

data = dataset_wide_no_ID
desired_factors = 7
custom_mat = matrix(0,
                    nrow = ncol(data), 
                    ncol = desired_factors*2)
row.names(custom_mat)= colnames(data)
colnames(custom_mat) = c(paste0('F',1:desired_factors),
                         paste0('-F',1:desired_factors))

initial_solution = variable_grt_loading   # solução inicial (warm-start) para o GA
scale_levels = unique(dataset$gc_value)   # valores únicos da escala (0, 1, 2, 3)

# Salva: dataset em formato largo, solução inicial, objeto da AFE e escala de valores.
# Esses objetos são carregados pelo script_GA_scale.R.
save(dataset_wide, initial_solution, factors_EFA, scale_levels, file = '../data/input_GA_scale.RData')
