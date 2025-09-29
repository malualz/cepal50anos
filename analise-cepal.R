## =========================================================
## CEPAL Review — Importação, padronização e métricas (sem summary)
## =========================================================

# 0) Pacotes
packs <- c("bibliometrix","dplyr","stringr","readr","tibble")
new <- setdiff(packs, rownames(installed.packages()))
if (length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(packs, library, character.only = TRUE))

set.seed(123)

# 1) INPUTS ------------------------------------------------
wos_files    <- c("raw/wos_cepal1.txt", "raw/wos_cepal2.txt")  # WoS: Plain Text (Full Record + Cited Refs)
scopus_files <- c("raw/scopus_cepal.csv")                      # Scopus: CSV (completo)

# 2) IMPORTAR E CONVERTER ---------------------------------
df_wos <- convert2df(file = wos_files,    dbsource = "wos",    format = "plaintext")
df_sco <- convert2df(file = scopus_files, dbsource = "scopus", format = "csv")

# --- Substituir AU por AF limpo (Scopus) ------------------
# Coloque imediatamente após:
# df_sco <- convert2df(file = scopus_files, dbsource = "scopus", format = "csv")

library(stringr)

# 1) Limpa AF: remove sufixos " (ID)", padroniza espaços e separadores
clean_AF_to_AU <- function(AF_col) {
  x <- ifelse(is.na(AF_col), "", AF_col)
  # remove qualquer " (algo)" ao final de cada autor (ex.: "(ID)", "(Scopus Author ID)")
  x <- gsub("\\s*\\([^)]+\\)", "", x)      # remove parênteses e conteúdo
  x <- gsub("\\s+", " ", x)                # normaliza espaços
  x <- trimws(x)
  
  # alguns exports usam ";" para separar autores; outros usam " ; "
  # garantimos separação por ";"
  parts <- strsplit(x, ";", fixed = TRUE)
  parts <- lapply(parts, function(v) {
    v <- trimws(v)
    v <- v[v != ""]
    paste(v, collapse = ";")
  })
  unlist(parts, use.names = FALSE)
}

# 2) Substitui AU pela versão limpa de AF
if ("AF" %in% names(df_sco)) {
  df_sco$AU <- clean_AF_to_AU(df_sco$AF)
} else {
  stop("A coluna AF não está presente no df_sco; verifique a exportação do Scopus.")
}

# 3) Conferência (opcional)
# head(df_sco$AF, 3)
# head(df_sco$AU, 3)
# table(sapply(strsplit(df_sco$AU, ";", fixed = TRUE), length))

# 3) FUNÇÕES DE PADRONIZAÇÃO ------------------------------

# 3.1 Países básicos
fix_countries_basic <- function(D){
  if (!"AU_CO" %in% names(D)) D$AU_CO <- NA_character_
  
  # tenta extrair país de C1 apenas se AU_CO estiver todo vazio
  if (all(is.na(D$AU_CO) | trimws(D$AU_CO) == "") && "C1" %in% names(D)) {
    D$AU_CO <- stringr::str_extract(D$C1, "[A-Z][A-Za-zÀ-ÖØ-öø-ÿ\\- ]+$")
  }
  
  D$AU_CO[is.na(D$AU_CO) | trimws(D$AU_CO) == ""] <- "Unknown"
  D$AU_CO <- stringr::str_to_upper(D$AU_CO)
  D$AU_CO <- dplyr::recode(D$AU_CO,
                           "KINGDOM" = "UNITED KINGDOM",
                           "U.S.A."  = "UNITED STATES",
                           "USA"     = "UNITED STATES")
  D
}

# 3.2 Normalizar separadores em AU e AU_CO (remove entradas vazias)
normalize_semicolons <- function(x){
  x <- ifelse(is.na(x), "", x)
  parts <- strsplit(x, ";", fixed = TRUE)
  parts <- lapply(parts, function(v){
    v <- trimws(v); v <- v[v != ""]
    paste(v, collapse = ";")
  })
  unlist(parts, use.names = FALSE)
}

# 3.3 Alinhar AU_CO ao nº de autores (um país por autor)
align_countries_to_authors <- function(D, sep = ";"){
  if (!"AU" %in% names(D)) D$AU <- ""
  if (!"AU_CO" %in% names(D)) D$AU_CO <- "Unknown"
  
  D$AU    <- normalize_semicolons(D$AU)
  D$AU_CO <- normalize_semicolons(D$AU_CO)
  
  AU_list <- strsplit(D$AU, sep, fixed = TRUE)
  CO_list <- strsplit(D$AU_CO, sep, fixed = TRUE)
  
  D$AU_CO <- mapply(function(au, co){
    au <- trimws(au); au <- au[au != ""]
    n  <- length(au)
    if (n == 0) return("Unknown")
    
    co <- trimws(co); co <- co[co != ""]
    if (length(co) == 0)          return(paste(rep("Unknown", n), collapse = sep))
    if (length(co) == 1 && n > 1) return(paste(rep(co, n), collapse = sep))
    if (length(co) >= n)          return(paste(co[seq_len(n)], collapse = sep))
    paste(c(co, rep(tail(co, 1), n - length(co))), collapse = sep)
  }, AU_list, CO_list, USE.NAMES = FALSE)
  
  D
}

# 3.4 Normalização de DOI e Título para deduplicação
add_norm_fields <- function(D){
  D |>
    mutate(
      DI = if_else(is.na(DI), NA_character_, str_to_lower(str_trim(DI))),
      TI = if_else(is.na(TI), "", TI),
      TI_norm = TI |>
        str_to_lower() |>
        str_replace_all("[[:punct:]]", " ") |>
        str_squish()
    )
}

# 3.5 Métricas principais (substitui “Main statistics”)
calc_main_metrics <- function(D){
  split_auth <- function(x) if (is.na(x) || x=="") character(0) else unlist(strsplit(x, ";", fixed = TRUE))
  n_authors_doc <- sapply(D$AU, split_auth) |> sapply(length)
  
  Years <- if ("PY" %in% names(D)) range(D$PY, na.rm = TRUE) else c(NA_integer_, NA_integer_)
  GTP   <- nrow(D)
  GTA   <- length(unique(unlist(sapply(D$AU, split_auth))))
  GTC   <- if ("TC" %in% names(D)) sum(D$TC, na.rm = TRUE) else NA
  TP1   <- sum(n_authors_doc == 1, na.rm = TRUE)
  TPm   <- sum(n_authors_doc >= 2, na.rm = TRUE)
  TA1   <- length(unique(unlist(sapply(D$AU[n_authors_doc == 1], split_auth))))
  TAm   <- length(unique(unlist(sapply(D$AU[n_authors_doc >= 2], split_auth))))
  TAapp <- sum(n_authors_doc, na.rm = TRUE)
  ADy   <- if (!any(is.na(D$PY))) GTP / length(unique(D$PY)) else NA
  ADa   <- if (GTA > 0) GTP / GTA else NA
  AAd   <- if (GTP > 0) GTA / GTP else NA
  ACAd  <- if (GTP > 0) TAapp / GTP else NA
  CI    <- if (TPm > 0) sum(n_authors_doc[n_authors_doc >= 2]) / TPm else NA
  ACd   <- if (!is.na(GTC) && GTP > 0) GTC / GTP else NA
  
  H <- tryCatch(
    Hindex(D, field = "AU",
           elements = unique(unlist(sapply(D$AU, split_auth))),
           sep = ";"),
    error = function(e) NULL
  )
  h <- if (!is.null(H) && !is.null(H$H)) suppressWarnings(max(H$H$H, na.rm = TRUE)) else NA
  
  get_terms <- function(vec){
    v <- unique(unlist(strsplit(paste(na.omit(vec), collapse = ";"), ";", fixed = TRUE)))
    v <- trimws(v); v <- v[v != ""]
    tolower(v)
  }
  GTKde <- if ("DE" %in% names(D)) length(unique(get_terms(D$DE))) else NA
  GTKid <- if ("ID" %in% names(D)) length(unique(get_terms(D$ID))) else NA
  
  tibble::tibble(
    Years_start = Years[1], Years_end = Years[2],
    GTP = GTP, GTA = GTA, GTC = GTC,
    TP1 = TP1, TPm = TPm, TA1 = TA1, TAm = TAm, TAapp = TAapp,
    ADy = ADy, ADa = ADa, AAd = AAd, ACAd = ACAd, CI = CI,
    ACd = ACd, h = h, GTKde = GTKde, GTKid = GTKid
  )
}

# 4) PADRONIZAR PAÍSES E CAMPOS ---------------------------------------
df_wos <- df_wos |> fix_countries_basic() |> align_countries_to_authors()
df_sco <- df_sco |> fix_countries_basic() |> align_countries_to_authors()

# Harmonizar colunas antes do merge
cols_missing_in_sco <- setdiff(names(df_wos), names(df_sco))
for (cc in cols_missing_in_sco) df_sco[[cc]] <- NA
cols_missing_in_wos <- setdiff(names(df_sco), names(df_wos))
for (cc in cols_missing_in_wos) df_wos[[cc]] <- NA

# Normalizar para deduplicar
df_wos <- add_norm_fields(df_wos)
df_sco <- add_norm_fields(df_sco)

# 5) MERGE E DEDUPLICAÇÃO ---------------------------------------------
df_mrg <- mergeDbSources(list(df_wos, df_sco), remove.duplicated = TRUE)
df_mrg <- df_mrg |> arrange(PY)

df_mrg_has_doi <- df_mrg |> dplyr::filter(!is.na(DI) & DI != "") |> distinct(DI, .keep_all = TRUE)
df_mrg_no_doi  <- df_mrg |> dplyr::filter(is.na(DI) | DI == "") |> distinct(TI_norm, PY, SO, .keep_all = TRUE)
df_mrg <- dplyr::bind_rows(df_mrg_has_doi, df_mrg_no_doi) |> dplyr::select(-TI_norm)

# Reforço final de países no merged
df_mrg <- df_mrg |> fix_countries_basic() |> align_countries_to_authors()

# 6) FUNÇÃO PRINCIPAL (sem summary; biblioAnalysis opcional) -----------
run_all <- function(D, tag){
  D <- D |>
    mutate(
      AU = as.character(AU),
      PY = suppressWarnings(as.integer(PY)),
      DI = ifelse(is.na(DI), NA_character_, tolower(trimws(DI)))
    ) |>
    fix_countries_basic() |>
    align_countries_to_authors()
  
  # biblioAnalysis pode falhar em algumas versões; tratamos como opcional
  BA <- tryCatch(biblioAnalysis(D), error = function(e) NULL)
  
  yearly <- D |> count(PY, name = "Docs") |> arrange(PY)
  main_stats <- calc_main_metrics(D)
  
  list(tag = tag, BA = BA, yearly = yearly, main_stats = main_stats, data = D)
}

# 7) EXECUÇÃO ----------------------------------------------------------
r_wos <- run_all(df_wos, "WoS")
r_sco <- run_all(df_sco, "Scopus")
r_mrg <- run_all(df_mrg, "Merged")

# 8) EXPORTAÇÃO (opcional) --------------------------------------------
dir.create("outputs_cepal", showWarnings = FALSE)
readr::write_csv(r_wos$yearly,      "outputs_cepal/yearly_wos.csv")
readr::write_csv(r_sco$yearly,      "outputs_cepal/yearly_scopus.csv")
readr::write_csv(r_mrg$yearly,      "outputs_cepal/yearly_merged.csv")
readr::write_csv(r_wos$main_stats,  "outputs_cepal/main_stats_wos.csv")
readr::write_csv(r_sco$main_stats,  "outputs_cepal/main_stats_scopus.csv")
readr::write_csv(r_mrg$main_stats,  "outputs_cepal/main_stats_merged.csv")

library(dplyr)

# Lista de variáveis e siglas
variaveis <- tibble::tibble(
  Variavel = c(
    "Período (ano inicial)", "Período (ano final)", "Total de Documentos",
    "Total de Autores", "Total de Citações", "Documentos Single-authored",
    "Documentos Multi-authored", "Autores de documentos single-authored",
    "Autores de documentos multi-authored", "Aparições de autor",
    "Média de documentos por ano", "Média de documentos por autor",
    "Autores por documento", "Co-autores por documento",
    "Índice de colaboração", "Citações por documento",
    "h-index", "Palavras-chave de autor", "Keywords Plus"
  ),
  Sigla = c(
    "Years_start", "Years_end", "GTP", "GTA", "GTC",
    "TP1", "TPm", "TA1", "TAm", "TAapp",
    "ADy", "ADa", "AAd", "ACAd", "CI", "ACd",
    "h", "GTKde", "GTKid"
  )
)

# Função auxiliar para extrair valores
extrair <- function(main_stats, sigla){
  if (sigla %in% names(main_stats)) main_stats[[sigla]][1] else NA
}

# Construção da tabela consolidada
tabela_main <- variaveis %>%
  rowwise() %>%
  mutate(
    WoS    = extrair(r_wos$main_stats, Sigla),
    Scopus = extrair(r_sco$main_stats, Sigla),
    Merged = extrair(r_mrg$main_stats, Sigla)
  )

# Visualizar resultado
print(tabela_main, n = nrow(tabela_main))

# Exportar se desejar
readr::write_csv(tabela_main, "outputs_cepal/tabela_main_statistics.csv")

# h-index de uma coleção (com base nas citações dos documentos)
h_index_collection <- function(tc_vec) {
  if (is.null(tc_vec) || length(tc_vec) == 0) return(NA_integer_)
  tc <- suppressWarnings(as.numeric(tc_vec))
  tc <- tc[!is.na(tc)]
  if (length(tc) == 0) return(NA_integer_)
  tc <- sort(tc, decreasing = TRUE)
  sum(tc >= seq_along(tc))
}

# Atualize sua função de métricas principais para incluir h_collection
calc_main_metrics <- function(D){
  split_auth <- function(x) if (is.na(x) || x=="") character(0) else unlist(strsplit(x, ";", fixed = TRUE))
  n_authors_doc <- sapply(D$AU, split_auth) |> sapply(length)
  
  Years <- if ("PY" %in% names(D)) range(D$PY, na.rm = TRUE) else c(NA_integer_, NA_integer_)
  GTP   <- nrow(D)
  GTA   <- length(unique(unlist(sapply(D$AU, split_auth))))
  GTC   <- if ("TC" %in% names(D)) sum(D$TC, na.rm = TRUE) else NA
  TP1   <- sum(n_authors_doc == 1, na.rm = TRUE)
  TPm   <- sum(n_authors_doc >= 2, na.rm = TRUE)
  TA1   <- length(unique(unlist(sapply(D$AU[n_authors_doc == 1], split_auth))))
  TAm   <- length(unique(unlist(sapply(D$AU[n_authors_doc >= 2], split_auth))))
  TAapp <- sum(n_authors_doc, na.rm = TRUE)
  ADy   <- if (!any(is.na(D$PY))) GTP / length(unique(D$PY)) else NA
  ADa   <- if (GTA > 0) GTP / GTA else NA
  AAd   <- if (GTP > 0) GTA / GTP else NA
  ACAd  <- if (GTP > 0) TAapp / GTP else NA
  CI    <- if (TPm > 0) sum(n_authors_doc[n_authors_doc >= 2]) / TPm else NA
  ACd   <- if (!is.na(GTC) && GTP > 0) GTC / GTP else NA
  
  # h por autor (máximo entre autores na coleção, opcional)
  H <- tryCatch(
    Hindex(D, field = "AU",
           elements = unique(unlist(sapply(D$AU, split_auth))),
           sep = ";"),
    error = function(e) NULL
  )
  h_authors_max <- if (!is.null(H) && !is.null(H$H)) suppressWarnings(max(H$H$H, na.rm = TRUE)) else NA
  
  # h da coleção (com base em TC)
  h_collection <- if ("TC" %in% names(D)) h_index_collection(D$TC) else NA
  
  # Keywords
  get_terms <- function(vec){
    v <- unique(unlist(strsplit(paste(na.omit(vec), collapse = ";"), ";", fixed = TRUE)))
    v <- trimws(v); v <- v[v != ""]
    tolower(v)
  }
  GTKde <- if ("DE" %in% names(D)) length(unique(get_terms(D$DE))) else NA
  GTKid <- if ("ID" %in% names(D)) length(unique(get_terms(D$ID))) else NA
  
  tibble::tibble(
    Years_start = Years[1], Years_end = Years[2],
    GTP = GTP, GTA = GTA, GTC = GTC,
    TP1 = TP1, TPm = TPm, TA1 = TA1, TAm = TAm, TAapp = TAapp,
    ADy = ADy, ADa = ADa, AAd = AAd, ACAd = ACAd, CI = CI,
    ACd = ACd,
    h = h_authors_max,            # h (máximo por autor na coleção)
    h_collection = h_collection,  # h-index da coleção
    GTKde = GTKde, GTKid = GTKid
  )
}

r_wos <- run_all(df_wos, "WoS")
r_sco <- run_all(df_sco, "Scopus")
r_mrg <- run_all(df_mrg, "Merged")

library(dplyr)
variaveis <- tibble::tibble(
  Variavel = c(
    "Período (ano inicial)", "Período (ano final)", "Total de Documentos",
    "Total de Autores", "Total de Citações", "Documentos Single-authored",
    "Documentos Multi-authored", "Autores de documentos single-authored",
    "Autores de documentos multi-authored", "Aparições de autor",
    "Média de documentos por ano", "Média de documentos por autor",
    "Autores por documento", "Co-autores por documento",
    "Índice de colaboração", "Citações por documento",
    "h-index da coleção",
    "Palavras-chave de autor", "Keywords Plus"
  ),
  Sigla = c(
    "Years_start", "Years_end", "GTP", "GTA", "GTC",
    "TP1", "TPm", "TA1", "TAm", "TAapp",
    "ADy", "ADa", "AAd", "ACAd", "CI", "ACd",
    "h_collection",
    "GTKde", "GTKid"
  )
)

extrair <- function(main_stats, sigla){
  if (sigla %in% names(main_stats)) main_stats[[sigla]][1] else NA
}

tabela_main <- variaveis %>%
  rowwise() %>%
  mutate(
    WoS    = extrair(r_wos$main_stats, Sigla),
    Scopus = extrair(r_sco$main_stats, Sigla),
    Merged = extrair(r_mrg$main_stats, Sigla)
  ) %>%
  ungroup()

# Visualização
print(tabela_main, n = nrow(tabela_main))

# Exportação opcional
readr::write_csv(tabela_main, "outputs_cepal/tabela_main_statistics.csv")

library(dplyr)
library(stringr)

# --- Normalização de título para comparação ---
norm_title <- function(x) {
  x <- ifelse(is.na(x), "", x)
  # remove acentos, se stringi estiver disponível
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  }
  x <- str_to_upper(x)
  x <- gsub("[[:punct:]]+", " ", x)  # remove pontuação
  x <- str_squish(x)
  x
}

# --- Mapeamento TITULO -> AUTOR institucional (como informado) ---
map_titulo_autor <- c(
  "THE AGRICULTURE OF LATIN AMERICA: CHANGES, TRENDS AND OUTLINES OF STRATEGY" = "United Nations",
  "PRODUCTIVE ABSORPTION OF THE LABOUR FORCE: AN ONGOING CONTROVERSY"          = "UN ECLAC Economic Projection Centre",
  "LATIN AMERICAN DEVELOPMENT PROBLEMS AND THE WORLD ECONOMIC CRISIS"          = "UN ECLAC Economic Projection Centre"
)

# normaliza as chaves do mapa
map_keys_norm <- names(map_titulo_autor) |> norm_title()
names(map_titulo_autor) <- map_keys_norm

# --- Localiza e corrige na base Scopus ---
ti_norm <- norm_title(df_sco$TI)

for (k in names(map_titulo_autor)) {
  idx <- which(ti_norm == k)
  if (length(idx) > 0) {
    df_sco$AU[idx] <- map_titulo_autor[[k]]
    # opcional: manter AF coerente
    if ("AF" %in% names(df_sco)) df_sco$AF[idx] <- map_titulo_autor[[k]]
  }
}

# --- Limpeza final de AU (separador e remoção de vazios) ---
df_sco$AU <- sapply(strsplit(ifelse(is.na(df_sco$AU), "", df_sco$AU), ";", fixed = TRUE),
                    function(v) { v <- trimws(v); v <- v[v != ""]; paste(v, collapse = ";") },
                    USE.NAMES = FALSE)

# --- Recontagem de autores por documento (para conferência) ---
count_authors <- function(x) {
  parts <- strsplit(ifelse(is.na(x), "", x), ";", fixed = TRUE)
  sapply(parts, function(v) { v <- trimws(v); v <- v[v != ""]; length(v) })
}
n_auth_scopus <- count_authors(df_sco$AU)
print(table(n_auth_scopus))   # esperado: nenhum 0

# --- Recalcular métricas de autoria de Scopus e (opcional) atualizar r_sco$main_stats ---
TP1_sco   <- sum(n_auth_scopus == 1)
TPm_sco   <- sum(n_auth_scopus >= 2)
GTA_sco   <- length(unique(unlist(strsplit(paste(df_sco$AU, collapse=";"), ";", fixed=TRUE))))
TAapp_sco <- sum(n_auth_scopus)
AAd_sco   <- if (nrow(df_sco) > 0) GTA_sco / nrow(df_sco) else NA_real_
ACAd_sco  <- if (nrow(df_sco) > 0) TAapp_sco / nrow(df_sco) else NA_real_
CI_sco    <- if (TPm_sco > 0) sum(n_auth_scopus[n_auth_scopus >= 2]) / TPm_sco else NA_real_

# Identidade: TP1 + TPm deve igualar nrow(df_sco)
stopifnot(TP1_sco + TPm_sco == nrow(df_sco))

# Se já calculou r_sco antes, recomende recalcular; ou atualize os campos:
# r_sco <- run_all(df_sco, "Scopus")
if (exists("r_sco") && is.list(r_sco) && "main_stats" %in% names(r_sco) && nrow(r_sco$main_stats) > 0) {
  ms <- r_sco$main_stats
  if ("TP1"   %in% names(ms)) ms$TP1   <- TP1_sco
  if ("TPm"   %in% names(ms)) ms$TPm   <- TPm_sco
  if ("GTA"   %in% names(ms)) ms$GTA   <- GTA_sco
  if ("TAapp" %in% names(ms)) ms$TAapp <- TAapp_sco
  if ("AAd"   %in% names(ms)) ms$AAd   <- AAd_sco
  if ("ACAd"  %in% names(ms)) ms$ACAd  <- ACAd_sco
  if ("CI"    %in% names(ms)) ms$CI    <- CI_sco
  r_sco$main_stats <- ms
}

# Instalar se necessário
if (!require(openxlsx)) install.packages("openxlsx")

library(openxlsx)

# Exportar para Excel
write.xlsx(
  x = tabela_main,
  file = "outputs_cepal/tabela_main_statistics.xlsx",
  asTable = TRUE
)

message("Tabela exportada para outputs_cepal/tabela_main_statistics.xlsx")

# EM INGLES

library(dplyr)

# Cria uma tabela de tradução das variáveis para inglês
traducao_en <- tibble::tibble(
  Variavel = c(
    "Período (ano inicial)", "Período (ano final)", "Total de Documentos",
    "Total de Autores", "Total de Citações", "Documentos Single-authored",
    "Documentos Multi-authored", "Autores de documentos single-authored",
    "Autores de documentos multi-authored", "Aparições de autor",
    "Média de documentos por ano", "Média de documentos por autor",
    "Autores por documento", "Co-autores por documento",
    "Índice de colaboração", "Citações por documento",
    "h-index (máximo por autor)", "h-index da coleção",
    "Palavras-chave de autor", "Keywords Plus"
  ),
  Variavel_EN = c(
    "Period (start year)", "Period (end year)", "Total Documents",
    "Total Authors", "Total Citations", "Single-authored Documents",
    "Multi-authored Documents", "Authors of Single-authored Documents",
    "Authors of Multi-authored Documents", "Author Appearances",
    "Documents per Year", "Documents per Author",
    "Authors per Document", "Co-authors per Document",
    "Collaboration Index", "Citations per Document",
    "h-index (max per author)", "Collection h-index",
    "Authors' Keywords", "Keywords Plus"
  )
)

# Junta a tabela original com a tradução
tabela_main_en <- tabela_main %>%
  left_join(traducao_en, by = "Variavel") %>%
  select(Variable = Variavel_EN, Sigla, WoS, Scopus, Merged)

# Exporta para Excel
if (!require(openxlsx)) install.packages("openxlsx")
library(openxlsx)

write.xlsx(
  x = tabela_main_en,
  file = "outputs_cepal/main_statistics_EN.xlsx",
  asTable = TRUE
)

message("Tabela exportada para outputs_cepal/main_statistics_EN.xlsx")

# Pacotes necessários
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(viridis)) install.packages("viridis")
if (!require(dplyr)) install.packages("dplyr")

library(ggplot2)
library(viridis)
library(dplyr)

# Construção do data frame consolidado
df_yearly_all <- bind_rows(
  r_wos$yearly  |> mutate(Source = "WoS"),
  r_sco$yearly  |> mutate(Source = "Scopus"),
  r_mrg$yearly  |> mutate(Source = "Merged")
)

# Garantir ordem temporal
df_yearly_all <- df_yearly_all |> arrange(PY)

# Gráfico
ggplot(df_yearly_all, aes(x = PY, y = Docs, color = Source)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    title = "Evolution of Publications by Year",
    x = "Publication Year",
    y = "Number of Documents",
    color = "Dataset"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

