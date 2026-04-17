rm(list = ls()) # apaga todas as variáveis 
cat("\014") # limpa a tela (CTRL+L)     
setwd(dirname(sys.frame(1)$ofile)) # diretório corrente é o do script

# execute o código com ctsl+shifht+S
# - Equivalente ao botão "Source" (source the contents of the active document) ao lado do botão "Run"
# - tal comando executa o código do script inteiro, sem ecoar os comandos no console, 
# e é útil para carregar funções e variáveis de configuração, por exemplo, 
# sem poluir o console 

# bibliotecas
library(tibble)
library(dplyr)
library(tidyr)

## functions
# ── helpers ───────────────────────────────────────────────────────────────────

.load_chart_libs <- function() {
    library(ggplot2)
    library(zoo)
    library(extrafont)
    loadfonts(device = "pdf", quiet = TRUE)
}

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
            guides(color = guide_legend(override.aes = list(size = 2.5)))  # ← agora dentro da lista
        )
    else
        list(base)
}

# ── Gera gráficos CGVN ────────────────────────────────────────────────────────

#' Gera gráficos de frequência de resposta ou volume de entrega
#'
#' @param data_block   data.frame com as colunas esperadas
#' @param short_chapter sufixo usado no nome do arquivo de saída
#' @param type         "freq_response" (padrão) ou "delivery_report"
#' @param flag_font_latex usa fonte CM Roman para LaTeX (padrão TRUE)
#' @param format       formato de saída ('pdf', 'png', …)
gen_chart <- function(data_block,
                      short_chapter,
                      type            = c("freq_response", "delivery_report"),
                      flag_font_latex = TRUE,
                      format          = "pdf") {
    
    type <- match.arg(type)
    .load_chart_libs()
    
    # ── conversão de data e limite temporal ────────────────────────────────────
    if (type == "freq_response") {
        data_block$Data_Entrega <- zoo::as.yearqtr(data_block$Data_Entrega)
    } else {
        data_block$Data_Entrega <- zoo::as.yearmon(data_block$Data_Entrega)
    }
    
    # ── agregações ──────────────────────────────────────────────
    tabela_freq <- data_block |>
        group_by(CNPJ_Companhia, Data_Entrega, ID_Documento, Pratica_Adotada) |>
        summarise(cnt = n(), .groups = "drop") |>
        group_by(CNPJ_Companhia, Data_Entrega, ID_Documento) |>
        mutate(freq = cnt / sum(cnt)) |>
        ungroup()
    
    summary_freq <- tabela_freq |>
        group_by(Data_Entrega, Pratica_Adotada) |>
        summarise(
            ybar  = 100 * mean(freq),
            ylow  = 100 * quantile(freq, probs = 0.05),
            yhigh = 100 * quantile(freq, probs = 0.95),
            n     = sum(cnt),
            .groups = "drop"
        )
    
    # ── construção do gráfico ──────────────────────────────────────────────────
    base_plot <- ggplot(summary_freq, aes(x = Data_Entrega)) +
        facet_grid(rows = vars(Pratica_Adotada)) +
        xlab("Data de Entrega") +
        ggtitle(data_block$Capitulo[1]) +
        theme_bw() +
        .chart_theme(flag_font_latex)
    
    if (type == "freq_response") {
        chart_breaks <- unique(summary_freq$Data_Entrega)
        chart_breaks <- chart_breaks[lubridate::quarter(chart_breaks) == 4]
        
        p <- base_plot +
            aes(y = ybar) +
            geom_ribbon(aes(ymin = ylow, ymax = yhigh), alpha = alpha_level_ribbon) +
            geom_line(color  = "blue") +
            geom_point(color = "blue") +
            coord_cartesian(ylim = c(0, 100)) +
            ylab("Percentual de respostas (%)") +
            scale_x_yearqtr(
                breaks = chart_breaks,
                format = "%qT-%Y"
            )
        prefix <- "p1"
    } else {
        p <- base_plot +
            aes(y = n) +
            geom_line(color  = "black") +
            geom_point(color = "black") +
            ylab("Quantidade de respostas")
        prefix <- "p2"
    }
    
    # ── exportação ─────────────────────────────────────────────────────────────
    image_file <- sprintf("./figs/%s_%s.%s", prefix, short_chapter, format)
    
    ggsave(
        filename = image_file,
        plot     = p,
        dpi      = 600,
        width    = 183,
        height   = 183,
        units    = "mm",
        device   = format
    )
    
    if (flag_font_latex && format == "pdf")
        embed_fonts(image_file)
    
    invisible(p)
}

descriptive_dataset_long = function(local_dataset,
                                    cor.method)
{
    local_dataset_wide = reshape(local_dataset[,c('CNPJ_Companhia','Data_Entrega','ID_Documento','ID_Item','gc_value')],
                                 timevar = 'ID_Item', idvar = c('CNPJ_Companhia','Data_Entrega','ID_Documento'), 
                                 direction = 'wide')
    colnames(local_dataset_wide) = gsub("gc_value.", "", colnames(local_dataset_wide))
    
    local_dataset_wide_no_ID = local_dataset_wide[,-which(colnames(local_dataset_wide) %in% c('CNPJ_Companhia','Data_Entrega','ID_Documento'))]
    
    linhas_validas = complete.cases(local_dataset_wide_no_ID)
    
    cat(sprintf('\n\tWARNING:: descartando %d relatórios contendo NA em qualquer uma das colunas\n', sum(!linhas_validas))) 
    
    local_dataset_wide_no_ID = local_dataset_wide_no_ID[linhas_validas,]
    local_descriptive_stats = gen_stats_table(local_dataset_wide_no_ID,
                                              alias = sort(unique(local_dataset$ID_Item)),
                                              cor.method = cor.method)
    
    return(local_descriptive_stats)
}

gen_stats_table = function(dataset, 
                           alias, 
                           cor.method = "pearson",
                           language = 'PT-BR')
{
    force(dataset)
    table_stats = fBasics::basicStats(dataset)
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
    if (language == 'PT-BR')
    {
        table_stats["Coef. Vari.",] = table_stats["Desvio P.",]/table_stats["Média",]
    }
    else
    {
        table_stats["CV",] = table_stats["Std. dev.",]/table_stats["Avg",]
    }

        
    table_correlation = vector("list",3)
    table_correlation[[1]] = NA*cor(dataset)
    rownames(table_correlation[[1]]) = alias
    table_correlation[[2]] = NA*cor(dataset)
    table_correlation[[3]] = NA*cor(dataset)
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
    
    table_partial_correlation = ppcor::pcor(dataset,
                                     method = cor.method)
    
    # Testes de normalidade (necessários?)
    # normality_tests = lapply(dataset,
    #                          function(x) shapiro.test(x))
    
    #########################################################
    ## Testes de adequação da amostra para análise fatorial
    #########################################################
    KMO_corr = EFAtools::KMO(dataset,
                             cor_method = cor.method)

    table_stats["KMO",] = KMO_corr$KMO_i
    
    BTS_test = EFAtools::BARTLETT(dataset, 
                                  cor_method = cor.method)
    
    return(list(descriptive = table_stats,
                correlation = table_correlation,
                partial.correlation = table_partial_correlation,
                KMO = KMO_corr,
                BTS = BTS_test,
                method = cor.method))
}

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
    
    ####################################
    ## Extraction of Component Factors 
    n.factors = EFAtools::N_FACTORS(local_dataset_wide_no_ID)

    table_6 = data.frame(Component = 1:length(n.factors$outputs$cd_out$eigenvalues),
                         Eigenvalues = n.factors$outputs$cd_out$eigenvalues,
                         perc_var = NA,
                         cum_var = NA)
    sum_eigen = sum(table_6[,'Eigenvalues'])
    table_6[,'perc_var'] = 100*table_6[,'Eigenvalues']/sum_eigen
    table_6[,'cum_var'] = cumsum(table_6[,'perc_var'])
    table_6[,'dy_dx'] = c(NA,diff(100*table_6[,'Eigenvalues']))
    table_6[,'d2y_dx2'] = c(NA,diff(table_6[,'dy_dx']))
    table_6[,'dvar'] = c(NA,diff(table_6[,'cum_var']))
    table_6[,'d2var'] = c(NA,diff(table_6[,'dvar']))
    
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
            if(factors_N$PVAL > level_confidence)
            {
                break
            }
        }
        else
        {
            break
        }
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
    
    ################################################
    ## Unrotated/Rotated Component Analysis Factor Matrix
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
            factors_N = psych::fa(local_dataset_wide_no_ID, 
                                  nfactors = factors_selected, 
                                  rotate = rotation)
        }
        else
        {
            browser()
        }
    }
    table_7_A = data.frame(Variables = colnames(local_dataset_wide_no_ID),
                           factors = factors_N$loadings[1:ncol(local_dataset_wide_no_ID),1:factors_selected],
                           Communality = factors_N$communality)
    sum_eigen = ncol(local_dataset_wide_no_ID)
    SS = rbind(colSums(table_7_A[,2:(factors_selected+1)]^2),
               100*colSums(table_7_A[,2:(factors_selected+1)]^2)/sum_eigen)
    table_7_B = data.frame(Variables = c('Soma dos Quadrados (autovalor)','Percentual do Traço'), #c('Sum of Squares (eigenvalue)','Percentage of trace'), 
                           SS,
                           Total = rowSums(SS))
    
    table_7_A_sorted = table_7_A
    sorted_loadings = sort_loadings(factors_N)
    #table_7_A = table_7_A_sorted[sorted_loadings$item,]
    
    
    escores_fatoriais = psych::factor.scores(local_dataset_wide_no_ID, 
                                      factors_N$loadings, 
                                      method = "Thurstone")
    
    escores_padronizados = scale(escores_fatoriais$scores)
    
    # Atribuir pesos aos fatores
    variancia_explicada = factors_N$Vaccounted[2, ]
    pesos = variancia_explicada / sum(variancia_explicada)
    
    # Calculando índices
    indice_composto = escores_padronizados %*% pesos
    colnames(indice_composto) = sprintf("%s.%s",'F',dataset$Capitulo[1])
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

###################################
## CONFIGURAÇÕES
dataset_path = './data/'
dataset_path = '/home/mvlp/Dropbox/Articles (code)/cgvn2024/results/'
#doc_path = '../doc/tabs/'
#results_path = './/'

correlation.method = 'spearman'
alpha_level_ribbon = 0.25
###################################


# arquivo gerado pelo script_get_data.R
load(paste0(dataset_path, "dataset_CGVN.RData"))

short_chapter_name = c('AC','CA','DR','OFC','ECI')

# Análises dos dados por capítulo
for (i in 1:length(short_chapter_name)) 
{
    table_i = dataset[grep(sprintf("^%d\\.",i), dataset$ID_Item),]
    table_i = table_i[!is.na(table_i$gc_value),]

    ## tabelas de percentual de respostas por pergunta
    tabela_freq = table(table_i$ID_Item, table_i$Capitulo, table_i$Pratica_Adotada)
    # Calcular as proporções em relação às combinações únicas de ID_Item e Capitulo
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
    
    # Tabela 1 – Frequência das respostas para o Informe sobre o Código Brasileiro de Governança Corporativa (ICBGC)
    print(as_tibble(table_freq_responses))

    
    # Figura 1 – Evolução das respostas por capítulo.
    gen_chart(table_i, 
              short_chapter_name[i], 
              type = "freq_response", 
              format = "pdf")
    gen_chart(table_i, 
              short_chapter_name[i], 
              type = "delivery_report", 
              format = "pdf")

    
    # Tabela 2 – Práticas adotadas convertidas em valores numéricos: Estatísticas descritivas.
    descriptive_i = descriptive_dataset_long(table_i,
                                             cor.method = correlation.method)
    # Estatística descritiva das variáveis do capítulo
    cat(sprintf("\n## Estatísticas descritivas para o capítulo %s\n", short_chapter_name[i]))
    print(as_tibble(descriptive_i$descriptive, rownames = "variavel"), width = Inf)
    
    # Correlações entre as variáveis do capítulo
    cat(sprintf("\n## Correlações entre as variáveis do capítulo %s\n", short_chapter_name[i]))
    print(as_tibble(descriptive_i$correlation[[1]]))

    # p-valores das correlações entre as variáveis do capítulo
    cat(sprintf("\n## p-valores das correlações entre as variáveis do capítulo %s\n", short_chapter_name[i]))
    print(as_tibble(round(descriptive_i$correlation[[2]], 3)))
    
    # Correlações parciais entre as variáveis do capítulo
    cat(sprintf("\n## Correlações parciais entre as variáveis do capítulo %s\n", short_chapter_name[i]))
    print(as_tibble(descriptive_i$partial.correlation$estimate))
    
    # p-valores das correlações parciais entre as variáveis do capítulo
    cat(sprintf("\n## p-valores das correlações parciais entre as variáveis do capítulo %s\n", short_chapter_name[i]))
    print(as_tibble(round(descriptive_i$partial.correlation$p.value, 3)))

    # KMO - Kaiser-Meyer-Olkin Measure of Sampling Adequacy
    cat(sprintf("\n## KMO - Kaiser-Meyer-Olkin Measure of Sampling Adequacy para o capítulo %s\n", short_chapter_name[i]))
    print(descriptive_i$KMO)
    
    # Teste de esfericidade de Bartlett
    cat(sprintf("\n## Teste de esfericidade de Bartlett para o capítulo %s\n", short_chapter_name[i]))
    print(descriptive_i$BTS)
    
}

# Análise dos dados de forma geral (agregada)
# Tabela 2b – Práticas adotadas convertidas em valores numéricos: Estatísticas descritivas.
# Resultados omitidos do artigo em razão de falta de espaço
descriptive_full = descriptive_dataset_long(dataset,
                                            cor.method = correlation.method)

# Correlações entre todas as variáveis presentes nos Informe do Código de Governança
cat("\n## Correlações entre todas as variáveis do IGC\n")
cat('\tAVISO::visualização omitida\n')
correlation_full = descriptive_full$correlation[[1]]
#View(correlation_full)

# p-valores das correlações entre todas as variáveis presentes nos Informe do Código de Governança
cat("\n## p-valores das correlações entre a todas as variáveis do IGC\n")
cat('\tAVISO::visualização omitida\n')
correlation_full_pvalues = round(descriptive_full$correlation[[2]],3)
#View(correlation_full_pvalues)

# Correlações parciais entre entre todas as variáveis presentes nos Informe do Código de Governança
cat("\n## Correlações parciais entre a todas as variáveis do IGC\n")
cat('\tAVISO::visualização omitida\n')
partial_correlation_full = descriptive_full$partial.correlation$estimate
#View(partial_correlation_full)

# p-valores das correlações parciais entre todas as variáveis presentes nos Informe do Código de Governança
cat("\n## p-valores das correlações parciais entre a todas as variáveis do IGC\n")
cat('\tAVISO::visualização omitida\n')
partial_correlation_full_pvalues = round(descriptive_full$partial.correlation$p.value,3)
#View(partial_correlation_full_pvalues)

# KMO - Kaiser-Meyer-Olkin Measure of Sampling Adequacy
cat("\n## KMO - Kaiser-Meyer-Olkin Measure of Sampling Adequacy para o conjunto completo de dados do IGC\n")
KMO_full = descriptive_full$KMO
print(KMO_full)

# Teste de esfericidade de Bartlett
cat("\n## Teste de esfericidade de Bartlett para o conjunto completo de dados do IGC\n")
BTS_full = descriptive_full$BTS
print(BTS_full)

# 
dataset_wide = reshape(dataset[,c('CNPJ_Companhia','Data_Entrega','ID_Documento','ID_Item','gc_value')],
                       timevar = 'ID_Item', idvar = c('CNPJ_Companhia','Data_Entrega','ID_Documento'), 
                       direction = 'wide')
colnames(dataset_wide) = gsub("gc_value.", "", colnames(dataset_wide))

dataset_wide_no_ID = dataset_wide[,-which(colnames(dataset_wide) %in% c('CNPJ_Companhia','Data_Entrega','ID_Documento'))]

linhas_validas = complete.cases(dataset_wide_no_ID)
cat(sprintf('\n\tWARNING:: descartando %d relatórios contendo NA em qualquer uma das colunas\n', sum(!linhas_validas))) 
dataset_wide_no_ID = dataset_wide_no_ID[linhas_validas,]

# Multivariate Data Analysis
# Joseph F. Hair Jr. William C. Black
# Barry J. Babin Rolph E. Anderson
# Seventh Edition
# 3. Exploratory Factor Analysis
# TABLE 6 Results for the Extraction of Component Factors, pag 132
#componentes = component_dataset_long(dataset)
#cat("\n## Análise fatorial exploratória para o conjunto completo de dados do IGC\n")
#print(componentes$sufficient_factors$criteria)
#print(componentes$table_EFA)


# A partir dos critérios de análise fatorial exploratória, selecionamos 7 fatores para a análise fatorial com rotação varimax
n_factors_full = 7 # resultados apontaram uma quantidade superior mas inconclusiva (entre 10 e 28), mas optamos por 7 fatores para facilitar a interpretação dos resultados


factors_PCA = psych::principal(dataset_wide_no_ID,
                             nfactors = n_factors_full,
                             rotate = 'varimax')
print(factors_PCA,sort=TRUE)


factors_EFA = psych::fa(dataset_wide_no_ID,
                      nfactors = n_factors_full,
                      rotate = 'varimax')
# Tabela 3 – Matriz da análise de fatores comuns rotacionadas por VARIMAX.
print(factors_EFA,sort=TRUE)

# correlação entre as fatores estimados 
# supostamente independentes (correlação entre fatores igual a zero)
cor_factors = cor(factors_EFA$scores)
cat("\n## Correlação entre os fatores estimados\n")
print(round(cor_factors, 3))

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
# correlação entre as escalas somadas (índices) agregados por capítulo
# (1) Acionistas (AC)
# (2) Conselho de Administração (CA)
# (3) Diretoria (DR) 
# (4) Órgãos de Fiscalização e Controle (OFC)
# (5) Ética e Conflitos de Interesses (ECI)
# Idealmente, as escalas somadas (índices) agregados por capítulo 
# não deveriam se correlacionar fortemente entre si, ou seja,
# deveriam ser independentes
cat("\n## Correlação entre as escalas somadas (índices) agregados por capítulo\n")
print(round(cor(table_summed_chapter), 3))

# correlação entre os índices agregados por capítulo e os fatores estimados
# idealmente, os índices agregados por capítulo deveriam se correlacionar 
# fortemente com um fator específico, e pouco ou nada com os demais fatores.
# Entretanto, a escolha de agregação por capítulo gerou índices que refletem
# informações presentes em diversos fatores
cor_chapter_factors = cor(table_summed_chapter, factors_EFA$scores)
cat("\n## Correlação entre os índices agregados por capítulo e os fatores estimados\n")
print(round(cor_chapter_factors, 3))

# sugestão de escala somada a partir da EFA
variable_grt_loading = t(apply(factors_EFA$loadings, 1, function(x) 
{
    bin <- rep(0, length(x))
    bin[which.max(abs(x))] <- 1
    bin
}))
    
table_summed_scale = as.matrix(dataset_wide_no_ID) %*% (variable_grt_loading)
colnames(table_summed_scale) = sprintf("F_%d",1:n_factors_full)

# diminuição da correlação entre as escalas somadas (índices) agregados por fator, em comparação com a correlação entre as escalas somadas (índices) agregados por capítulo
cat("\n## Correlação entre as escalas somadas (índices) agregados por capítulo\n")
print(round(cor(table_summed_scale), 3))

cor_scale_factors = cor(table_summed_scale, factors_EFA$scores)
cat("\n## Correlação entre os índices agregados por capítulo e os fatores estimados\n")
print(round(cor_scale_factors, 3))



# vamos gerar outro agrupamento de variáveis que gere índices (escalas somadas)
# correlacionados com os fatores e não correlacionados entre si (independentes)

data = dataset_wide_no_ID
desired_factors = 7
custom_mat = matrix(0,
                    nrow = ncol(data), 
                    ncol = desired_factors*2)
row.names(custom_mat)= colnames(data)
colnames(custom_mat) = c(paste0('F',1:desired_factors),
                         paste0('-F',1:desired_factors))

initial_solution = variable_grt_loading
scale_levels = unique(dataset$gc_value)

# salva dados para uso no script de otimização de agrupamento de variáveis 
# (script_GA_scale.R)
save(dataset_wide, initial_solution, factors_EFA, scale_levels, file = './data/input_GA_scale.RData')

