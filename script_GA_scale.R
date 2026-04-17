# ============================================================
#  GA-BASED SCALE CONSTRUCTION FROM EFA
#  Busca de pesos unitários {-1, 0, +1} via Algoritmo Genético
#  para construção de escalas próximas aos fatores da AFE,
#  com ortogonalidade entre escalas.
# ============================================================

rm(list = ls())
cat("\014")
setwd(dirname(sys.frame(1)$ofile)) # diretório corrente é o do script

library(GA)
library(psych)

estimate_loading_cut_off = function(N)
{
    loading_cut_off = read.csv(file = './data/loading_cut_off.csv', 
                               header = TRUE)
    colnames(loading_cut_off) = c('cut_off', 'N')
    modelo = lm(log(cut_off) ~ log(N), 
                data = loading_cut_off)
    loading_cut_off = as.numeric(exp(coef(modelo)%*%c(1,log(N))))  
    return(loading_cut_off)
}

# ============================================================
# [1] PARÂMETROS GLOBAIS — ajuste aqui
# ============================================================

FLAG_LOAD_SCALE_CINCO2024 = TRUE # carrega os dados do artigo

N_FACTORS    = 7      # número de fatores da AFE
SCALE_MIN    = 1      # valor mínimo da escala Likert
SCALE_MAX    = 5      # valor máximo da escala Likert

# Pesos da função objetivo
W_ORTHO      = 1.0    # ortogonalidade entre escalas (diagonal = identidade)
W_FIDELITY   = 1.0    # fidelidade aos scores fatoriais da AFE
W_SPARSITY   = 0.05   # penalidade por item usado em mais de um fator
W_CONTRADICTION = 10.0   # penalidade por item com peso +1 e -1 no mesmo fator

# Parâmetros do GA
GA_POP_SIZE  = 500            # tamanho da população
GA_MAXITER   = 5000           # máximo de gerações
GA_RUN       = 0.1*GA_MAXITER # early stopping: gerações sem melhoria (ajustar)
GA_PMUT      = 0.04           # taxa de mutação
GA_PCROSS    = 0.85           # taxa de crossover
GA_SEED      = 17             # semente para reprodutibilidade
GA_PARALLEL  = TRUE           # paralelismo (requer parallel package)

# ============================================================
# [2] DADOS E REFERÊNCIAS EFA
# ============================================================

# carrega data, initial_solution, factors_EFA e scale_range
load('./data/input_GA_scale.RData') 
data = dataset_wide[, -c(1,2,3)] # remove colunas de "CNPJ_Companhia"/"Data_Entrega"/"ID_Documento" 

DATA_MAT     = as.matrix(data)
N_ITEMS      = ncol(data)
ITEM_NAMES   = colnames(data)

# Extração dos objetos EFA (ajuste conforme seu pipeline)
EFA          = factors_EFA
CUT_OFF      = estimate_loading_cut_off(nrow(data))

# Pré-computado UMA vez fora da fitness (ganho de performance)
FA_SCORES    = EFA$scores[, 1:N_FACTORS]         # scores fatoriais da AFE
FA_COR       = cor(FA_SCORES)                    # correlação alvo F x F

# ============================================================
# [3] UPPER BOUND — restrição de busca baseada nas cargas
#
#  Coluna +Fj = 1  →  variável PODE ter peso +1 no fator j
#               0  →  variável proibida de ter peso +1 no fator j
#  Coluna -Fj = 1  →  variável PODE ter peso -1 no fator j
#               0  →  variável proibida de ter peso -1 no fator j
# ============================================================

build_upper_bound = function(loadings, cutoff, n_factors) {
    above = (loadings[, 1:n_factors] >= cutoff) * 1L
    below = (loadings[, 1:n_factors] <  cutoff) * 1L
    as.integer(c(above, below))   # vetor linearizado: [+F1..+Fn | -F1..-Fn] x items
}

UPPER_BOUND = build_upper_bound(EFA$loadings, CUT_OFF, N_FACTORS)

# ============================================================
# [4] SOLUÇÃO INICIAL (warm-start via initial_solution)
#
#  Cada item recebe peso +1 no fator em que tem maior carga.
#  Isso garante que o GA parta de uma solução plausível.
# ============================================================

# adiciona colunas de peso negativo (inicialmente 0)
initial_solution = cbind(initial_solution, 
                         matrix(0, nrow(initial_solution), ncol(initial_solution))) 
INITIAL_SEED  = matrix(as.integer(initial_solution), nrow = 1L)

# ============================================================
# [5] NORMALIZAÇÃO DAS ESCALAS
#
#  Mapeia cada escala para o intervalo [SCALE_MIN, SCALE_MAX]
#  com base nos limites teóricos da combinação linear.
# ============================================================
scale.normalize = function(scores, loadings_bin, scale_vals) 
{
    pos_max = colSums(loadings_bin[,  1:N_FACTORS])    *  max(scale_vals)
    neg_min = colSums(loadings_bin[, -c(1:N_FACTORS)]) * -max(scale_vals)
    range   = pos_max - neg_min

    scores = sweep(scores, 2, neg_min,   "-")
    scores = sweep(scores, 2, range, "/")
    
    return(10 * scores)
}


# ============================================================
# [6] FUNÇÃO OBJETIVO (componentes separados para transparência)
# ============================================================

calc_objective = function(latent_vars) {
    
    # Ortogonalidade: correlação entre escalas deve ser identidade
    corr_scales  = cor(latent_vars)
    ortho_loss   = sum(abs(corr_scales - diag(N_FACTORS)))
    
    # Fidelidade: correlação cruzada (escalas x fatores) deve replicar FA_COR
    cross_cor    = cor(latent_vars, FA_SCORES)
    fidelity_loss = sum(abs(cross_cor - FA_COR))
    
    list(ortho = ortho_loss, fidelity = fidelity_loss,
         total = W_ORTHO * ortho_loss + W_FIDELITY * fidelity_loss)
}

# ============================================================
# [7] FUNÇÃO FITNESS
#
#  Codificação do cromossomo (vetor binário de tamanho N_ITEMS × 2 × N_FACTORS):
#    Posições  1 .. N_ITEMS*N_FACTORS         → pesos POSITIVOS (+1)
#    Posições  N_ITEMS*N_FACTORS+1 .. 2*end   → pesos NEGATIVOS (-1)
#
#  O GA MAXIMIZA → retornamos o negativo da perda.
# ============================================================

fitness_func = function(solution) {
    
    # — Aplicar upper_bound diretamente (elimina bits inválidos sem penalidade)
    solution = solution * UPPER_BOUND
    
    # — Decodificar em matriz (N_ITEMS × 2*N_FACTORS)
    weight_mat  = matrix(solution, nrow = N_ITEMS, ncol = 2L * N_FACTORS)
    pos_w       = weight_mat[, 1:N_FACTORS,              drop = FALSE]
    neg_w       = weight_mat[, (N_FACTORS + 1):(2 * N_FACTORS), drop = FALSE]
    
    # — Pesos líquidos {-1, 0, +1}
    net_weights = pos_w - neg_w
    
    # — Penalidade fatal: algum fator ficou vazio
    if (any(colSums(abs(net_weights)) == 0L)) return(-1e6)
    
    # — Penalidade de contradição: mesmo item com pos=1 e neg=1 no mesmo fator
    contradiction = sum(pos_w * neg_w)
    
    # — Penalidade de esparsidade: item usado em mais de um fator
    items_per_factor  = rowSums(abs(net_weights))
    sparsity_penalty  = sum(pmax(items_per_factor - 1L, 0L))
    
    # — Calcular variáveis latentes normalizadas
    latent_vars = DATA_MAT %*% net_weights
    latent_vars = scale.normalize(latent_vars, weight_mat, scale_levels)
    
    # — Função objetivo
    obj = calc_objective(latent_vars)
    
    total_loss = obj$total +
        W_SPARSITY * sparsity_penalty +
        W_CONTRADICTION * contradiction
    
    return(-total_loss)   # negativo: GA maximiza, nós minimizamos a perda
}

# ============================================================
# [8] DIAGNÓSTICO INICIAL
# ============================================================

cat("==== SOLUÇÃO INICIAL (custom_mat) ====\n")
net_init      = initial_solution[, 1:N_FACTORS] - initial_solution[, -c(1:N_FACTORS)]
latent_init   = scale.normalize(DATA_MAT %*% net_init, initial_solution, scale_levels)

cat("\nCorrelação entre escalas iniciais:\n")
print(round(cor(latent_init), 3))
cat("\nCorrelação escalas iniciais vs fatores EFA:\n")
print(round(cor(latent_init, FA_SCORES), 3))
cat(sprintf("\nFitness inicial: %.4f\n\n", fitness_func(INITIAL_SEED)))

cat("\n==== CONFIABILIDADE (Alpha de Cronbach) ====\n")
# Efeito colaterar
scores  = scoreItems(net_init, data, totals = FALSE)
print(scores$alpha)

# ============================================================
# [9] EXECUÇÃO DO GA
# ============================================================
if(FLAG_LOAD_SCALE_CINCO2024) {
    load('./data/scale_CINCO2024.RData')
    matrix_scale = scale_CINCO2024
} else {
    # buscar nova solução via algoritmo genético
    cat("==== INICIANDO ALGORITMO GENÉTICO ====\n\n")
    set.seed(GA_SEED)
    
    ga_result = ga(
        type        = "binary",
        fitness     = fitness_func,
        nBits       = N_ITEMS * 2L * N_FACTORS,
        suggestions = INITIAL_SEED,
        popSize     = GA_POP_SIZE,
        maxiter     = GA_MAXITER,
        run         = GA_RUN,
        pmutation   = GA_PMUT,
        pcrossover  = GA_PCROSS,
        parallel    = GA_PARALLEL,
        seed        = GA_SEED,
        optim       = TRUE, # utiliza busca local para refinar a melhor solução
        monitor     = function(object) {
            fitness <- na.exclude(object@fitness)
            sumryStat <- c(mean(fitness), max(fitness))
            cat(sprintf("GA | iter = %5d | Mean = %8.3f | Best = %8.3f\n", 
                        object@iter, sumryStat[1], sumryStat[2]))
            flush.console()
        }
    )
    # Aplicar upper_bound na melhor solução (consistência)
    best_raw      = ga_result@solution[1L, ] * UPPER_BOUND
    best_mat      = matrix(best_raw, nrow = N_ITEMS, ncol = 2L * N_FACTORS)
    matrix_scale  = best_mat[, 1:N_FACTORS] - best_mat[, -c(1:N_FACTORS)]
}


# ============================================================
# [10] EXTRAÇÃO E EXIBIÇÃO DOS RESULTADOS
# ============================================================

rownames(matrix_scale) = ITEM_NAMES
colnames(matrix_scale) = paste0("F", 1:N_FACTORS)

latent_final  = scale.normalize(DATA_MAT %*% matrix_scale, 
                                cbind((matrix_scale == 1) * 1L, (matrix_scale == -1) * 1L), 
                                scale_levels)
colnames(latent_final) = paste0("Scale", 1:N_FACTORS)

## Tabela 4 – Avaliação dos diferentes IGCs (índices compostos) em relação aos 
## fatores originalmente estimados.

# — Sumário por fator
cat("\n\n==== RESULTADO FINAL ====\n")
cat("\nItens selecionados por fator:\n")
for (f in seq_len(N_FACTORS)) {
    pos_items = ITEM_NAMES[matrix_scale[, f] ==  1]
    neg_items = ITEM_NAMES[matrix_scale[, f] == -1]
    n_total   = length(pos_items) + length(neg_items)
    cat(sprintf(
        "\n  F%d (%d itens)\n    [+] %s\n    [-] %s\n",
        f, n_total,
        ifelse(length(pos_items) > 0, paste(pos_items, collapse = ", "), "nenhum"),
        ifelse(length(neg_items) > 0, paste(neg_items, collapse = ", "), "nenhum")
    ))
}

cat("\n\nCorrelação entre escalas finais:\n")
print(round(cor(latent_final), 3))

cat("\nCorrelação escalas finais vs fatores EFA:\n")
print(round(cor(latent_final, FA_SCORES), 3))

obj_final = calc_objective(latent_final)
cat(sprintf(
    "\nPerda ortogonalidade : %.4f\nPerda fidelidade     : %.4f\nPerda total          : %.4f\n",
    obj_final$ortho, obj_final$fidelity, obj_final$total
))

# ============================================================
# [11] CONFIABILIDADE DAS ESCALAS FINAIS (Alpha)
# ============================================================

cat("\n==== CONFIABILIDADE (Alpha de Cronbach) ====\n")
# Efeito colaterar
scores  = scoreItems(matrix_scale, data, totals = FALSE)
print(scores$alpha)

# ============================================================
# [12] 
# ============================================================

## gráficos dos índices
gen_chart_factors = function(data_block,
                             id, alias.short,
                             flag_font_latex = TRUE,
                             format = 'pdf',
                             alpha_level_ribbon = 0.25)
{
    library(ggplot2)
    library(zoo)
    library(dplyr)
    library(tidyr)
    
    library(extrafont)
    #font_install('fontcm') # deve ser instalada com sudo no prompt
    loadfonts(device = "pdf", quiet = TRUE)
    
    data_block$Data_Entrega = zoo::as.yearqtr(data_block$Data_Entrega)

    summary_tabela = data_block %>%
        group_by(Data_Entrega) %>%
        summarise(across(Scale1:Scale7, 
                         list(ymed = ~ median(.x), 
                              ylow = ~quantile(.x, probs=5/100),
                              yhigh = ~quantile(.x, probs=95/100)), 
                         .names = "{col}_{fn}")) %>%
        pivot_longer(cols = -Data_Entrega, 
                     names_to = c("variable", ".value"), 
                     names_sep = "_")
    variable = unique(summary_tabela$variable)
    summary_tabela = summary_tabela[summary_tabela$variable == variable[id],]
    
    chart_breaks = unique(summary_tabela$Data_Entrega)
    chart_breaks = chart_breaks[lubridate::quarter(unique(summary_tabela$Data_Entrega)) == 4]

    p1 = ggplot(summary_tabela, aes(x=Data_Entrega, y=ymed)) + 
        geom_ribbon(aes(ymin=ylow, 
                        ymax=yhigh),
                    alpha=alpha_level_ribbon) +
        geom_line(color = "blue") +
        geom_point(color = "blue") +
        coord_cartesian(ylim=c(0,10))+
        xlab("Data")+
        ylab(alias.short[id])+
        scale_x_yearqtr(breaks = chart_breaks,
                        format = '%qT-%Y')+
        theme_bw()
    
    if(flag_font_latex)
    {
        base_size = 12
        p1 = p1+
            theme(#text = element_text(size = base_size, family = "CM Roman"),
                strip.text.y = element_text(size = base_size, family = "CM Roman"),
                axis.text=element_text(size = base_size - 2, family = "CM Roman"),
                axis.title = element_text(size = base_size + 2, family = "CM Roman"),
                plot.title = element_text(size = base_size, hjust = 0.5, family = "CM Roman"),
                legend.position = 'bottom',
                legend.text = element_text(size = base_size + 2, family = "CM Roman"),
                legend.title = element_blank(),
                legend.key.size = unit(2, "lines"))+
            guides(color = guide_legend(override.aes = list(size = 2.5)))
    }
    else
    {
        p1 = p1+
            theme(strip.text.y = element_text(size = 12),
                  axis.text=element_text(size=12),
                  axis.title = element_text(size=14),
                  plot.title = element_text(size=12, hjust = 0.5),
                  legend.position = 'bottom',
                  legend.title=element_blank())
    }    
    
    image_file_name = sprintf("./figs/scale_%d_%s.%s", id, alias.short[id], format)
    
    ggsave(filename = image_file_name,
           plot=p1,
           dpi=600,
           width = 183,
           height = 183,
           units = 'mm',
           device = format)
    
    if (flag_font_latex)
    {
        embed_fonts(image_file_name)
    }
}

# Mnemônicos para os fatores construídos no artigo:
# Sete Dimensões da Governança Corporativa no Brasil:
#     Índices e Disponibilização de Dados
alias.short = c('CC','REG','ADM','DIR','SEM','DEF','FIS')
for (i in 1:N_FACTORS) 
{
    gen_chart_factors(cbind(dataset_wide[,c(1,2,3)], latent_final),
                      id = i, 
                      alias.short)
}

