# =============================================================================
#  SCRIPT 2: script_GA_scale.R
#  Sete Dimensões da Governança Corporativa no Brasil:
#  Índices e Disponibilização de Dados
#
#  Pereira, M. V. L.; Martucheli, C. T.; Fonseca, S. E.; Lopes, Y. F. (2024)
#  Financiamento: FAPEMIG, Chamada 01/2022, Demanda Universal
#
#  OBJETIVO:
#    Resolve o problema de otimização combinatória para construção dos 7
#    Índices de Governança Corporativa (IGCs) compostos (Tabela 4 do artigo).
#
#    O problema consiste em encontrar uma matriz de pesos X ∈ {-1, 0, +1}
#    (N_variáveis × N_fatores) que produza índices compostos S = D · X que:
#      (i)  sejam não correlacionados entre si (ortogonalidade);
#      (ii) se correlacionem fortemente com o respectivo escore fatorial da AFE
#           (fidelidade).
#
#    REFERÊNCIA NO ARTIGO: Seção 3.3.5 (Criação de Índices Compostos),
#    Equações (2) e (3), e Tabela 4
#
#  PRÉ-REQUISITO:
#    Executar script_CGVN_EFA_github.R primeiro, pois ele gera o arquivo
#    '../data/input_GA_scale.RData' com os escores fatoriais da AFE.
#
#  FLUXO DO SCRIPT:
#    1. Parâmetros globais e pacotes
#    2. Carregamento dos dados e da AFE
#    3. Construção do upper bound (restrições de busca)
#    4. Solução inicial (warm-start)
#    5. Normalização das escalas
#    6. Função objetivo
#    7. Função fitness para o GA
#    8. Diagnóstico da solução inicial
#    9. Execução do Algoritmo Genético (ou carregamento da solução do artigo)
#   10. Extração e exibição dos resultados (Tabela 4)
#   11. Confiabilidade (Alpha de Cronbach)
#   12. Geração de gráficos dos índices ao longo do tempo
# =============================================================================

rm(list = ls())
cat("\014")
setwd(dirname(sys.frame(1)$ofile))

# ─── Pacotes ─────────────────────────────────────────────────────────────────
library(GA)    # Algoritmos genéticos (Scrucca, 2013, 2017)
library(psych) # Factor scores e confiabilidade (Alpha de Cronbach)


# =============================================================================
# SEÇÃO 1 – FUNÇÃO AUXILIAR: estimate_loading_cut_off()
# =============================================================================
# Estima o limiar mínimo de carga fatorial significativa em função do tamanho
# amostral N, extrapolando a tabela de Hair et al. (2014, p.115) via regressão
# log-log: limiar = C · N^a  (C ≈ 4,912 ; a ≈ −0,476)
#
# REFERÊNCIA NO ARTIGO: Seção 3.3.4, Equação (1)

estimate_loading_cut_off = function(N)
{
    loading_cut_off = read.csv(file = '../data/loading_cut_off.csv', header = TRUE)
    colnames(loading_cut_off) = c('cut_off', 'N')
    modelo = lm(log(cut_off) ~ log(N), data = loading_cut_off)
    loading_cut_off = as.numeric(exp(coef(modelo) %*% c(1, log(N))))  
    return(loading_cut_off)
}


# =============================================================================
# SEÇÃO 2 – PARÂMETROS GLOBAIS
# =============================================================================
# FLAG_LOAD_SCALE_CINCO2024: se TRUE, carrega a solução exata publicada no
# artigo (recomendado para reprodução dos resultados da Tabela 4).
# Se FALSE, roda o AG do zero (pode demorar horas e gerar solução diferente
# por causa da natureza estocástica do algoritmo, mesmo com semente fixa).

FLAG_LOAD_SCALE_CINCO2024 = TRUE   # reproduz os resultados do artigo

N_FACTORS    = 7      # número de fatores da AFE (Seção 4.3.2 do artigo)
SCALE_MIN    = 1      # valor mínimo da escala likert original
SCALE_MAX    = 5      # valor máximo (após normalização os IGCs variam de 0 a 10)

# ─── Pesos da função objetivo ─────────────────────────────────────────────
# Controlam o trade-off entre ortogonalidade e fidelidade na minimização.
# W_ORTHO e W_FIDELITY iguais a 1.0 dão peso igual aos dois critérios
# principais da Equação (2) do artigo.
W_ORTHO         = 1.0    # peso da perda de ortogonalidade (||corr(S) − I||²)
W_FIDELITY      = 1.0    # peso da perda de fidelidade    (||corr(S,F) − corr(F)||²)
W_SPARSITY      = 0.05   # penalidade por variável usada em mais de um fator
W_CONTRADICTION = 10.0   # penalidade por peso +1 e −1 simultâneos no mesmo fator

# ─── Parâmetros do Algoritmo Genético (Scrucca, 2013) ────────────────────
# O GA é usado devido à natureza discreta das variáveis ({-1, 0, +1}) e à
# não-linearidade da função objetivo (Seção 3.3.5 do artigo).
GA_POP_SIZE  = 500             # tamanho da população (indivíduos por geração)
GA_MAXITER   = 5000            # número máximo de gerações
GA_RUN       = 0.1*GA_MAXITER  # early stopping: para após 500 gerações sem melhoria
GA_PMUT      = 0.04            # probabilidade de mutação por bit
GA_PCROSS    = 0.85            # probabilidade de crossover
GA_SEED      = 17              # semente para reprodutibilidade (Scrucca, 2013)
GA_PARALLEL  = TRUE            # usa múltiplos núcleos (requer pacote 'parallel')


# =============================================================================
# SEÇÃO 3 – CARREGAMENTO DOS DADOS E DA AFE
# =============================================================================
# Carrega os objetos gerados pelo script_CGVN_EFA_github.R:
#   dataset_wide     – dados em formato largo (N × 54 práticas + 3 IDs)
#   initial_solution – matriz binária (54 × 7): solução warm-start para o GA
#   factors_EFA      – objeto psych::fa com os resultados da AFE (7 fatores, varimax)
#   scale_levels     – valores únicos da escala (0, 1, 2, 3)

load('../data/input_GA_scale.RData') 
data = dataset_wide[, -c(1,2,3)]   # remove CNPJ_Companhia, Data_Entrega, ID_Documento

DATA_MAT   = as.matrix(data)
N_ITEMS    = ncol(data)        # 54 variáveis (práticas do ICBGC)
ITEM_NAMES = colnames(data)

EFA     = factors_EFA
CUT_OFF = estimate_loading_cut_off(nrow(data))   # ≈ 0,123 para N = 2.294

# Pré-computa escores e correlações alvo UMA vez fora da função fitness
# (ganho significativo de performance durante a otimização iterativa)
FA_SCORES = EFA$scores[, 1:N_FACTORS]   # matriz N × 7 de escores fatoriais
FA_COR    = cor(FA_SCORES)              # correlação alvo entre fatores (≈ identidade para varimax)


# =============================================================================
# SEÇÃO 4 – UPPER BOUND (restrição de busca)
# =============================================================================
# A busca do AG é restrita: uma variável SOMENTE pode receber peso +1 (ou −1)
# em um fator se sua carga fatorial naquele fator for significativa (≥ CUT_OFF).
# Isso reduz drasticamente o espaço de busca e acelera a convergência.
#
# Codificação do upper bound como vetor binário (linearizado):
#   Posições 1 .. N_ITEMS*N_FACTORS            → bits de peso POSITIVO (+1)
#   Posições N_ITEMS*N_FACTORS+1 .. 2*N_ITEMS*N_FACTORS → bits de peso NEGATIVO (−1)

build_upper_bound = function(loadings, cutoff, n_factors) {
    # above[i,j] = 1 se a carga da variável i no fator j ≥ cutoff (pode ser +1)
    above = (loadings[, 1:n_factors] >= cutoff) * 1L
    # below[i,j] = 1 se a carga da variável i no fator j < cutoff (pode ser −1)
    below = (loadings[, 1:n_factors] <  cutoff) * 1L
    as.integer(c(above, below))   # vetor linearizado: [+F1..+F7 | -F1..-F7] × itens
}

UPPER_BOUND = build_upper_bound(EFA$loadings, CUT_OFF, N_FACTORS)


# =============================================================================
# SEÇÃO 5 – SOLUÇÃO INICIAL (warm-start)
# =============================================================================
# Ponto de partida: cada variável recebe peso +1 no fator com a maior carga
# absoluta (gerado na Seção 9 do script_CGVN_EFA_github.R).
# Isso injeta conhecimento prévio no GA, acelerando a convergência.
#
# A matriz initial_solution (54 × 7) é expandida para incluir as colunas de
# pesos negativos (54 × 14), inicialmente todas zero.

initial_solution = cbind(initial_solution, 
                         matrix(0, nrow(initial_solution), ncol(initial_solution)))
INITIAL_SEED  = matrix(as.integer(initial_solution), nrow = 1L)   # cromossomo inicial


# =============================================================================
# SEÇÃO 6 – NORMALIZAÇÃO DAS ESCALAS
# =============================================================================
# Após calcular S = D · X (índices compostos brutos), normaliza cada escala
# para o intervalo [0, 10] com base nos limites teóricos da combinação linear.
# Isso torna os IGCs comparáveis entre si e interpretáveis.

scale.normalize = function(scores, loadings_bin, scale_vals) 
{
    # Limite superior teórico: soma dos pesos positivos × valor máximo da escala
    pos_max = colSums(loadings_bin[,  1:N_FACTORS])    *  max(scale_vals)
    # Limite inferior teórico: soma dos pesos negativos × (−valor máximo)
    neg_min = colSums(loadings_bin[, -c(1:N_FACTORS)]) * -max(scale_vals)
    range   = pos_max - neg_min

    scores = sweep(scores, 2, neg_min, "-")   # desloca para 0
    scores = sweep(scores, 2, range,   "/")   # normaliza para [0, 1]
    
    return(10 * scores)   # escala final: [0, 10]
}


# =============================================================================
# SEÇÃO 7 – FUNÇÃO OBJETIVO
# =============================================================================
# Implementa a Equação (2) do artigo:
#   min_X [ ||corr(S) − I||² + ||corr(S, F) − corr(F)||² ]
#
# Componentes:
#   ortho_loss   → desvio da correlação entre índices em relação à identidade
#                  (mede falta de ortogonalidade entre os 7 IGCs)
#   fidelity_loss → desvio da correlação cruzada (IGCs × fatores) em relação
#                  à correlação dos escores fatoriais da AFE
#                  (mede infidelidade aos fatores latentes estimados)

calc_objective = function(latent_vars) {
    
    # Ortogonalidade: corr(S) deve ser próxima à matriz identidade
    corr_scales = cor(latent_vars)
    ortho_loss  = sum(abs(corr_scales - diag(N_FACTORS)))
    
    # Fidelidade: corr(S, F) deve replicar a correlação entre os próprios fatores
    cross_cor     = cor(latent_vars, FA_SCORES)
    fidelity_loss = sum(abs(cross_cor - FA_COR))
    
    list(ortho    = ortho_loss,
         fidelity = fidelity_loss,
         total    = W_ORTHO * ortho_loss + W_FIDELITY * fidelity_loss)
}


# =============================================================================
# SEÇÃO 8 – FUNÇÃO FITNESS DO ALGORITMO GENÉTICO
# =============================================================================
# O GA do pacote {GA} MAXIMIZA a função fitness.
# Como queremos MINIMIZAR a perda total, retornamos o negativo da perda.
#
# Codificação do cromossomo (vetor binário de tamanho N_ITEMS × 2 × N_FACTORS):
#   Bits 1 .. N_ITEMS*N_FACTORS                  → pesos positivos (+1) por fator
#   Bits N_ITEMS*N_FACTORS+1 .. 2*N_ITEMS*N_FACTORS → pesos negativos (−1) por fator
#
# Restrições implementadas como penalidades:
#   Fatal        – fator vazio: retorna −1e6 (solução inviável)
#   Contradição  – mesmo item com +1 e −1 no mesmo fator: penalidade W_CONTRADICTION
#   Esparsidade  – item em mais de um fator: penalidade W_SPARSITY
#                  (implementa a Equação (3) do artigo: Σ|X_ij| ≤ 1, ∀i)

fitness_func = function(solution) {
    
    # Aplica upper_bound: zera bits onde a variável não tem carga significativa
    solution = solution * UPPER_BOUND
    
    # Decodifica o vetor em matriz (N_ITEMS × 2*N_FACTORS)
    weight_mat = matrix(solution, nrow = N_ITEMS, ncol = 2L * N_FACTORS)
    pos_w      = weight_mat[, 1:N_FACTORS,                    drop = FALSE]  # pesos +1
    neg_w      = weight_mat[, (N_FACTORS + 1):(2 * N_FACTORS), drop = FALSE] # pesos −1
    
    # Pesos líquidos: +1 (só positivo), −1 (só negativo), 0 (nenhum ou ambos)
    net_weights = pos_w - neg_w
    
    # Penalidade fatal: algum fator ficou sem nenhuma variável
    if (any(colSums(abs(net_weights)) == 0L)) return(-1e6)
    
    # Penalidade de contradição: mesmo item com pos=1 e neg=1 no mesmo fator
    # (produto pos_w * neg_w = 1 somente quando ambos são 1)
    contradiction = sum(pos_w * neg_w)
    
    # Penalidade de esparsidade: itens com participação em mais de 1 fator
    # (implementação da restrição da Equação (3): cada item só pode entrar em 1 IGC)
    items_per_factor = rowSums(abs(net_weights))
    sparsity_penalty = sum(pmax(items_per_factor - 1L, 0L))
    
    # Calcula os índices compostos brutos: S = D · X  (N × K)
    latent_vars = DATA_MAT %*% net_weights
    # Normaliza para [0, 10]
    latent_vars = scale.normalize(latent_vars, weight_mat, scale_levels)
    
    # Avalia a função objetivo (Equação 2)
    obj = calc_objective(latent_vars)
    
    total_loss = obj$total +
        W_SPARSITY * sparsity_penalty +
        W_CONTRADICTION * contradiction
    
    return(-total_loss)   # negativo: GA maximiza → equivale a minimizar total_loss
}


# =============================================================================
# SEÇÃO 9 – DIAGNÓSTICO DA SOLUÇÃO INICIAL
# =============================================================================
# Avalia a qualidade da solução warm-start antes de rodar o GA.
# Permite comparar o ponto de partida com a solução final.

cat("==== SOLUÇÃO INICIAL (maior carga por fator) ====\n")
net_init    = initial_solution[, 1:N_FACTORS] - initial_solution[, -c(1:N_FACTORS)]
latent_init = scale.normalize(DATA_MAT %*% net_init, initial_solution, scale_levels)

cat("\nCorrelação entre escalas iniciais:\n")
print(round(cor(latent_init), 3))
cat("\nCorrelação escalas iniciais vs fatores EFA:\n")
print(round(cor(latent_init, FA_SCORES), 3))
cat(sprintf("\nFitness inicial: %.4f\n\n", fitness_func(INITIAL_SEED)))

# Alpha de Cronbach da solução inicial (confiabilidade das escalas antes do AG)
cat("\n==== CONFIABILIDADE INICIAL (Alpha de Cronbach) ====\n")
scores = scoreItems(net_init, data, totals = FALSE)
print(scores$alpha)


# =============================================================================
# SEÇÃO 10 – EXECUÇÃO DO ALGORITMO GENÉTICO
# =============================================================================
# Dois modos de operação:
#
#   FLAG_LOAD_SCALE_CINCO2024 = TRUE  → carrega a solução publicada no artigo
#     (arquivo '../data/scale_CINCO2024.RData', objeto 'scale_CINCO2024')
#     → reproduce exatamente a Tabela 4 do artigo
#
#   FLAG_LOAD_SCALE_CINCO2024 = FALSE → executa nova otimização via GA
#     → pode gerar solução ligeiramente diferente (algoritmo estocástico)
#     → tempo estimado: varia conforme hardware (horas para convergência total)
#
# O GA usa busca local adicional (optim = TRUE) para refinar a melhor solução
# de cada geração, combinando exploração global (AG) com refinamento local.
# Referência: Scrucca (2017).

if(FLAG_LOAD_SCALE_CINCO2024) {
    # Carrega a solução exata do artigo (recomendado para reprodução)
    load('../data/scale_CINCO2024.RData')
    matrix_scale = scale_CINCO2024
} else {
    cat("\n\n==== INICIANDO ALGORITMO GENÉTICO ====\n\n")
    set.seed(GA_SEED)
    
    ga_result = ga(
        type        = "binary",       # cromossomo binário
        fitness     = fitness_func,   # função a maximizar (negativo da perda)
        nBits       = N_ITEMS * 2L * N_FACTORS,  # 54 × 2 × 7 = 756 bits
        suggestions = INITIAL_SEED,   # injeta o warm-start como indivíduo inicial
        popSize     = GA_POP_SIZE,    # 500 indivíduos por geração
        maxiter     = GA_MAXITER,     # até 5.000 gerações
        run         = GA_RUN,         # para se não houver melhoria por 500 gerações
        pmutation   = GA_PMUT,        # taxa de mutação: 4%
        pcrossover  = GA_PCROSS,      # taxa de crossover: 85%
        parallel    = GA_PARALLEL,    # processamento paralelo
        seed        = GA_SEED,        # semente para reprodutibilidade
        optim       = TRUE,           # busca local pós-AG para refinamento
        monitor     = function(object) {
            # Imprime estatísticas de fitness a cada geração
            fitness <- na.exclude(object@fitness)
            sumryStat <- c(mean(fitness), max(fitness))
            cat(sprintf("GA | iter = %5d | Mean = %8.3f | Best = %8.3f\n", 
                        object@iter, sumryStat[1], sumryStat[2]))
            flush.console()
        }
    )
    
    # Extrai a melhor solução e aplica o upper_bound para garantir consistência
    best_raw     = ga_result@solution[1L, ] * UPPER_BOUND
    best_mat     = matrix(best_raw, nrow = N_ITEMS, ncol = 2L * N_FACTORS)
    # Pesos líquidos: coluna j de pos_w menos coluna j de neg_w → {-1, 0, +1}
    matrix_scale = best_mat[, 1:N_FACTORS] - best_mat[, -c(1:N_FACTORS)]
}


# =============================================================================
# SEÇÃO 11 – EXTRAÇÃO E EXIBIÇÃO DOS RESULTADOS (Tabela 4 do artigo)
# =============================================================================
# Tabela 4 reporta:
#   (a) Itens e pesos de cada IGC
#   (b) Correlações entre os 7 IGCs (devem ser próximas de zero)
#   (c) Correlações entre os IGCs e os escores fatoriais da AFE
#       (IGC j deve se correlacionar fortemente com Fator j e pouco com os demais)
#
# REFERÊNCIA NO ARTIGO: Seção 4.3.4, Seção 4.3.5 e Tabela 4

rownames(matrix_scale) = ITEM_NAMES
colnames(matrix_scale) = paste0("F", 1:N_FACTORS)

# Calcula os IGCs finais normalizados [0, 10]
latent_final = scale.normalize(DATA_MAT %*% matrix_scale, 
                               cbind((matrix_scale == 1) * 1L, (matrix_scale == -1) * 1L), 
                               scale_levels)
# Nomeia conforme as sete dimensões do artigo (Seção 4.4):
#   CC  = Controles e Compliance         (Fator 1)
#   REG = Regulações Estatutárias        (Fator 2)
#   ADM = Conselho de Administração      (Fator 3)
#   DIR = Diretoria                      (Fator 4)
#   SEM = Sociedades de Economia Mista   (Fator 5)
#   DEF = Medidas de Defesa              (Fator 6)
#   FIS = Conselho Fiscal                (Fator 7)
colnames(latent_final) = paste0("Scale", 1:N_FACTORS)

cat("\n\n==== RESULTADO FINAL (Tabela 4 do artigo) ====\n")

# (a) Itens e pesos por fator
cat("\nItens selecionados por fator (pesos +1 e −1):\n")
for (f in seq_len(N_FACTORS)) {
    pos_items = ITEM_NAMES[matrix_scale[, f] ==  1]   # itens com peso +1
    neg_items = ITEM_NAMES[matrix_scale[, f] == -1]   # itens com peso −1
    n_total   = length(pos_items) + length(neg_items)
    cat(sprintf(
        "\n  F%d (%d itens)\n    [+] %s\n    [-] %s\n",
        f, n_total,
        ifelse(length(pos_items) > 0, paste(pos_items, collapse = ", "), "nenhum"),
        ifelse(length(neg_items) > 0, paste(neg_items, collapse = ", "), "nenhum")
    ))
}

# (b) Correlações entre os 7 IGCs.
# O maior valor absoluto reportado no artigo é 0,048 (DIR–CC),
# estatisticamente significativo, mas de magnitude desprezível (Seção 4.3.5).
cat("\n\nCorrelação entre os IGCs finais (Tabela 4 – parte inferior):\n")
print(round(cor(latent_final), 3))

# (c) Correlações entre IGCs e escores fatoriais.
# Correlações superiores a 0,68 com o fator correspondente (destacadas em azul
# na Tabela 4). Correlações com fatores não associados permanecem < 0,150.
cat("\nCorrelação entre IGCs finais e escores fatoriais da AFE (Tabela 4):\n")
print(round(cor(latent_final, FA_SCORES), 3))

# Decomposição das perdas finais da função objetivo
obj_final = calc_objective(latent_final)
cat(sprintf(
    "\nPerda ortogonalidade : %.4f\nPerda fidelidade     : %.4f\nPerda total          : %.4f\n",
    obj_final$ortho, obj_final$fidelity, obj_final$total
))


# =============================================================================
# SEÇÃO 12 – CONFIABILIDADE (Alpha de Cronbach)
# =============================================================================
# Avalia a consistência interna de cada IGC.
# Valores de Alpha > 0,70 são geralmente considerados aceitáveis em ciências sociais.

cat("\n==== CONFIABILIDADE FINAL (Alpha de Cronbach) ====\n")
scores = scoreItems(matrix_scale, data, totals = FALSE)
print(scores$alpha)


# =============================================================================
# SEÇÃO 13 – GRÁFICOS TEMPORAIS DOS 7 IGCs
# =============================================================================
# Gera gráficos de evolução trimestral de cada IGC (mediana e faixa 5%–95%)
# para o período out/2018 a nov/2024.
# Arquivos salvos em: ../figs/scale_<id>_<sigla>.<formato>
#
# Mnemônicos dos 7 IGCs (Seção 4.4 do artigo):
#   Scale1 = CC   (Controles e Compliance)
#   Scale2 = REG  (Regulações Estatutárias)
#   Scale3 = ADM  (Conselho de Administração)
#   Scale4 = DIR  (Diretoria)
#   Scale5 = SEM  (Sociedades de Economia Mista)
#   Scale6 = DEF  (Medidas de Defesa)
#   Scale7 = FIS  (Conselho Fiscal)

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
    loadfonts(device = "pdf", quiet = TRUE)
    
    # Converte Data_Entrega para formato trimestral (yearqtr)
    data_block$Data_Entrega = zoo::as.yearqtr(data_block$Data_Entrega)

    # Agrega por trimestre: mediana e percentis 5% e 95% de cada escala
    # (mesma lógica da Figura 1 do artigo, mas para os IGCs)
    summary_tabela = data_block %>%
        group_by(Data_Entrega) %>%
        summarise(across(Scale1:Scale7, 
                         list(ymed  = ~ median(.x),              # mediana
                              ylow  = ~quantile(.x, probs=5/100),  # percentil 5%
                              yhigh = ~quantile(.x, probs=95/100)), # percentil 95%
                         .names = "{col}_{fn}")) %>%
        pivot_longer(cols = -Data_Entrega, 
                     names_to = c("variable", ".value"), 
                     names_sep = "_")
    
    # Filtra para o IGC solicitado
    variable = unique(summary_tabela$variable)
    summary_tabela = summary_tabela[summary_tabela$variable == variable[id],]
    
    # Usa apenas o 4º trimestre de cada ano como rótulo do eixo X
    chart_breaks = unique(summary_tabela$Data_Entrega)
    chart_breaks = chart_breaks[lubridate::quarter(unique(summary_tabela$Data_Entrega)) == 4]

    p1 = ggplot(summary_tabela, aes(x=Data_Entrega, y=ymed)) + 
        geom_ribbon(aes(ymin=ylow, ymax=yhigh), alpha=alpha_level_ribbon) +
        geom_line(color = "blue") +
        geom_point(color = "blue") +
        coord_cartesian(ylim=c(0,10)) +  # eixo Y: [0, 10] (escala dos IGCs)
        xlab("Data") +
        ylab(alias.short[id]) +
        scale_x_yearqtr(breaks = chart_breaks, format = '%qT-%Y') +
        theme_bw()
    
    # Ajuste de tema: fonte CM Roman para exportação LaTeX
    if(flag_font_latex)
    {
        base_size = 12
        p1 = p1 +
            theme(strip.text.y = element_text(size = base_size,     family = "CM Roman"),
                  axis.text    = element_text(size = base_size - 2, family = "CM Roman"),
                  axis.title   = element_text(size = base_size + 2, family = "CM Roman"),
                  plot.title   = element_text(size = base_size, hjust = 0.5, family = "CM Roman"),
                  legend.position = 'bottom',
                  legend.text  = element_text(size = base_size + 2, family = "CM Roman"),
                  legend.title = element_blank(),
                  legend.key.size = unit(2, "lines")) +
            guides(color = guide_legend(override.aes = list(size = 2.5)))
    }
    else
    {
        p1 = p1 +
            theme(strip.text.y  = element_text(size = 12),
                  axis.text     = element_text(size = 12),
                  axis.title    = element_text(size = 14),
                  plot.title    = element_text(size = 12, hjust = 0.5),
                  legend.position = 'bottom',
                  legend.title  = element_blank())
    }    
    
    # Exporta o gráfico em alta resolução (600 dpi)
    image_file_name = sprintf("../figs/scale_%d_%s.%s", id, alias.short[id], format)
    ggsave(filename = image_file_name,
           plot = p1,
           dpi = 600,
           width = 183,
           height = 183,
           units = 'mm',
           device = format)
    
    if (flag_font_latex) { embed_fonts(image_file_name) }
}

# Gera um gráfico para cada um dos 7 IGCs
alias.short = c('CC','REG','ADM','DIR','SEM','DEF','FIS')

for (i in 1:N_FACTORS) 
{
    gen_chart_factors(cbind(dataset_wide[,c(1,2,3)], latent_final),
                      id = i, 
                      alias.short)
}

# =============================================================================
# SEÇÃO 14 – EXPORTAÇÃO DOS RESULTADOS EM XLSX
#
# Gera três planilhas Excel com os principais produtos da análise,
# facilitando o uso dos resultados em outros softwares (Excel, Stata, Python):
#
#   1. loadings.xlsx      — Cargas fatoriais da AFE (Tabela 3 do artigo)
#   2. pesos_escalas.xlsx — Matriz de pesos dos IGCs (Tabela 4 do artigo)
#   3. IGC_empresas.xlsx  — IGCs calculados por empresa e data de entrega
#
# Requer o pacote openxlsx. Instale com: install.packages("openxlsx")
# Os arquivos são salvos no diretório 'output' do repositório.
# =============================================================================

cat('\n\n==== EXPORTANDO RESULTADOS EM XLSX ====\n\n')

library(openxlsx)

# -----------------------------------------------------------------------------
# 14.1  Cargas fatoriais da AFE → loadings.xlsx
# -----------------------------------------------------------------------------
# Exporta a matriz de cargas fatoriais com rotação varimax (objeto EFA$loadings),
# que corresponde à Tabela 3 do artigo. Cada linha é uma variável (prática do
# ICBGC) e cada coluna é um dos 7 fatores estimados.
#
# unclass() é necessário porque EFA$loadings pertence à classe especial
# "loadings" do pacote psych, que não é um data.frame comum. A conversão
# via as.data.frame(unclass(...)) extrai a matriz numérica subjacente.

df_cargas_fatoriais = as.data.frame(unclass(EFA$loadings))

# Adiciona o código do item (e.g., "1.1.1", "4.5.2") como coluna explícita,
# pois write.xlsx não preserva rownames por padrão quando rowNames = FALSE.
df_cargas_fatoriais = cbind(item = rownames(df_cargas_fatoriais),
                            df_cargas_fatoriais)
rownames(df_cargas_fatoriais) = NULL

openxlsx::write.xlsx(df_cargas_fatoriais,
                     file     = "../output/loadings.xlsx",
                     rowNames = FALSE)   # item já está como coluna, rownames redundantes


# -----------------------------------------------------------------------------
# 14.2  Matriz de pesos dos IGCs → pesos_escalas.xlsx
# -----------------------------------------------------------------------------
# Exporta a matriz de pesos X ∈ {-1, 0, +1} que define a composição de cada
# IGC (Tabela 4 do artigo). Linhas = variáveis (práticas), colunas = IGCs.
#
# As colunas são renomeadas das siglas genéricas "F1"..."F7" para os nomes
# das sete dimensões do artigo (CC, REG, ADM, DIR, SEM, DEF, FIS) antes
# da exportação, para que a planilha seja autoexplicativa.
#
# rowNames = TRUE mantém os códigos dos itens (e.g., "1.1.1") na primeira
# coluna da planilha, identificando a qual prática cada peso pertence.

colnames(matrix_scale) = alias.short   # CC, REG, ADM, DIR, SEM, DEF, FIS

openxlsx::write.xlsx(matrix_scale,
                     file     = "../output/pesos_escalas.xlsx",
                     rowNames = TRUE)   # preserva os códigos dos itens nas linhas


# -----------------------------------------------------------------------------
# 14.3  IGCs por empresa e data → IGC_empresas.xlsx
# -----------------------------------------------------------------------------
# Exporta o painel de dados final com os 7 IGCs calculados para cada empresa
# em cada data de entrega do informe. É o principal produto do artigo para
# uso em pesquisas derivadas (e.g., regressões, análises de painel).
#
# Estrutura da planilha (colunas):
#   CNPJ_Companhia  — identificador único da empresa
#   Data_Entrega    — data de envio do informe à CVM
#   ID_Documento    — identificador único do documento
#   CC              — Índice de Controles e Compliance          [0, 10]
#   REG             — Índice de Regulações Estatutárias         [0, 10]
#   ADM             — Índice do Conselho de Administração       [0, 10]
#   DIR             — Índice da Diretoria                       [0, 10]
#   SEM             — Índice de Sociedades de Economia Mista    [0, 10]
#   DEF             — Índice de Medidas de Defesa               [0, 10]
#   FIS             — Índice do Conselho Fiscal                 [0, 10]

colnames(latent_final) = alias.short   # renomeia Scale1...Scale7 → CC...FIS

IGC_empresas = cbind(dataset_wide[, c(1, 2, 3)],   # CNPJ, Data_Entrega, ID_Documento
                     latent_final)                   # os 7 IGCs normalizados [0,10]

openxlsx::write.xlsx(IGC_empresas, file = "../output/IGC_empresas.xlsx")
