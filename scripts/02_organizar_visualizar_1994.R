# Le a RAIS 1994, organiza variaveis de interesse e gera visualizacoes.
# Uso:
#   Rscript scripts/02_organizar_visualizar_1994.R
#   Rscript scripts/02_organizar_visualizar_1994.R raw/AC1994/AC1994.txt

args <- commandArgs(trailingOnly = TRUE)
arquivo <- if (length(args) >= 1) args[[1]] else "raw/AC1994/AC1994.txt"
arquivo_identificacao <- if (length(args) >= 2) {
  args[[2]]
} else {
  "intermediate/variaveis_identificadas_1994.csv"
}

if (!file.exists(arquivo)) {
  stop("Arquivo nao encontrado: ", arquivo, call. = FALSE)
}

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote data.table para rodar este script.", call. = FALSE)
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Instale o pacote ggplot2 para gerar as visualizacoes.", call. = FALSE)
}

dir.create("intermediate", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE, recursive = TRUE)

read_header <- function(path) {
  con <- file(path, open = "r", encoding = "latin1")
  on.exit(close(con), add = TRUE)
  header_line <- readLines(con, n = 1, warn = FALSE)
  trimws(strsplit(header_line, ";", fixed = TRUE)[[1]])
}

normalize_name <- function(x) {
  y <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  y <- tolower(y)
  y <- gsub("[^a-z0-9]+", "_", y)
  y <- gsub("^_+|_+$", "", y)
  y
}

find_first <- function(patterns, normalized_names) {
  for (pattern in patterns) {
    hit <- grep(pattern, normalized_names, perl = TRUE)
    if (length(hit) > 0) return(hit[[1]])
  }
  NA_integer_
}

to_code <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

to_digit_code <- function(x) {
  x <- to_code(x)
  valid <- is.na(x) | grepl("^[0-9]+$", x)
  x[!valid] <- NA_character_
  x
}

to_int <- function(x) {
  x <- to_code(x)
  suppressWarnings(as.integer(gsub("[^0-9-]", "", x)))
}

to_num_br <- function(x) {
  x <- to_code(x)
  x <- gsub(".", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

infer_uf <- function(path) {
  base <- basename(path)
  uf <- toupper(sub("([A-Z]{2}).*", "\\1", base))
  if (grepl("^[A-Z]{2}$", uf)) uf else NA_character_
}

make_row <- function(nome_df, posicao, header, observacao = "") {
  data.frame(
    variavel_df = nome_df,
    status = ifelse(is.na(posicao), "AUSENTE", "OK"),
    posicao = ifelse(is.na(posicao), NA_integer_, posicao),
    coluna_origem = ifelse(is.na(posicao), NA_character_, header[[posicao]]),
    observacao = observacao,
    stringsAsFactors = FALSE
  )
}

header <- read_header(arquivo)
normalized_header <- normalize_name(header)
tipo_estab_pos <- which(normalized_header == "tipo_estab")

pos <- list(
  pis = find_first(c("^pis$", "pis_pasep", "pasep", "identificador.*trabalhador"), normalized_header),
  sexo = find_first(c("^sexo_trabalhador$", "^sexo$"), normalized_header),
  idade = find_first(c("^idade$"), normalized_header),
  natureza_juridica = find_first(c("^natureza_juridica$"), normalized_header),
  remuneracao_media = find_first(c("^vl_remun_media_sm$", "^valor_remun_media_sm$", "^vl_remuneracao_media"), normalized_header),
  remuneracao_dezembro = find_first(c("^vl_remun_dezembro_sm$", "^valor_remun_dezembro_sm$"), normalized_header),
  qtd_horas = find_first(c("^qtd_hora_contr$", "^qtd_horas_contrat"), normalized_header),
  cbo_94 = find_first(c("^cbo_94_ocupacao$"), normalized_header),
  cnae_95 = find_first(c("^cnae_95_classe$"), normalized_header),
  municipio = find_first(c("^municipio$"), normalized_header),
  vinculo_ativo = find_first(c("^vinculo_ativo_31_12$"), normalized_header),
  motivo_desligamento = find_first(c("^motivo_desligamento$"), normalized_header),
  mes_admissao = find_first(c("^mes_admissao$"), normalized_header),
  mes_desligamento = find_first(c("^mes_desligamento$"), normalized_header),
  grau_instrucao = find_first(c("^grau_instrucao"), normalized_header),
  tamanho_estabelecimento = find_first(c("^tamanho_estabelecimento$"), normalized_header),
  tempo_emprego = find_first(c("^tempo_emprego$"), normalized_header),
  tipo_vinculo = find_first(c("^tipo_vinculo$"), normalized_header),
  tipo_estab_codigo = if (length(tipo_estab_pos) >= 1) tipo_estab_pos[[1]] else NA_integer_,
  tipo_estab_id = if (length(tipo_estab_pos) >= 2) tipo_estab_pos[[2]] else NA_integer_
)

dicionario <- do.call(
  rbind,
  list(
    make_row("pis", pos$pis, header, "Ausente no TXT testado; necessario para agregar vinculos por trabalhador."),
    make_row("sexo_codigo", pos$sexo, header, "Codigo 01 tratado como Homem e 02 como Mulher."),
    make_row("idade", pos$idade, header, ""),
    make_row("natureza_juridica_codigo", pos$natureza_juridica, header, "Usar para setor privado apos confirmar layout oficial."),
    make_row("remuneracao_media_sm", pos$remuneracao_media, header, "Valor em salarios minimos no arquivo de 1994."),
    make_row("remuneracao_dezembro_sm", pos$remuneracao_dezembro, header, "Valor em salarios minimos no arquivo de 1994."),
    make_row("qtd_horas_contratadas", pos$qtd_horas, header, ""),
    make_row("cbo_94", pos$cbo_94, header, ""),
    make_row("cnae_95_classe", pos$cnae_95, header, ""),
    make_row("municipio", pos$municipio, header, ""),
    make_row("vinculo_ativo_31_12", pos$vinculo_ativo, header, ""),
    make_row("motivo_desligamento", pos$motivo_desligamento, header, ""),
    make_row("mes_admissao", pos$mes_admissao, header, ""),
    make_row("mes_desligamento", pos$mes_desligamento, header, ""),
    make_row("grau_instrucao", pos$grau_instrucao, header, ""),
    make_row("tamanho_estabelecimento", pos$tamanho_estabelecimento, header, ""),
    make_row("tempo_emprego", pos$tempo_emprego, header, ""),
    make_row("tipo_vinculo", pos$tipo_vinculo, header, ""),
    make_row("tipo_estab_codigo", pos$tipo_estab_codigo, header, "Primeira coluna chamada Tipo Estab."),
    make_row("tipo_estab_id", pos$tipo_estab_id, header, "Segunda coluna chamada Tipo Estab; em AC1994 aparece como CNPJ.")
  )
)

data.table::fwrite(
  dicionario,
  file = "intermediate/dicionario_variaveis_interesse_1994.csv"
)

if (file.exists(arquivo_identificacao)) {
  identificacao <- utils::read.csv(
    arquivo_identificacao,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )
  cat("\nArquivo de identificacao revisado:\n")
  print(identificacao, row.names = FALSE)
}

cat("\nDicionario usado para organizar o dataframe:\n")
print(dicionario, row.names = FALSE)

presentes <- dicionario[!is.na(dicionario$posicao), c("variavel_df", "posicao")]

raw <- data.table::fread(
  arquivo,
  sep = ";",
  header = TRUE,
  encoding = "Latin-1",
  colClasses = "character",
  select = presentes$posicao,
  na.strings = c("", "NA"),
  showProgress = FALSE,
  check.names = FALSE
)
data.table::setnames(raw, presentes$variavel_df)

n <- nrow(raw)
col_or_na <- function(name) {
  if (name %in% names(raw)) raw[[name]] else rep(NA_character_, n)
}

sexo_codigo <- to_digit_code(col_or_na("sexo_codigo"))
sexo <- rep("Ignorado", length(sexo_codigo))
sexo[sexo_codigo %in% c("1", "01")] <- "Homem"
sexo[sexo_codigo %in% c("2", "02")] <- "Mulher"
sexo[is.na(sexo_codigo)] <- NA_character_

idade <- to_int(col_or_na("idade"))
remuneracao_media_sm <- to_num_br(col_or_na("remuneracao_media_sm"))
remuneracao_dezembro_sm <- to_num_br(col_or_na("remuneracao_dezembro_sm"))
qtd_horas_contratadas <- to_int(col_or_na("qtd_horas_contratadas"))
natureza_juridica_codigo <- to_digit_code(col_or_na("natureza_juridica_codigo"))

rais_1994 <- data.frame(
  ano = 1994L,
  uf = infer_uf(arquivo),
  pis = to_code(col_or_na("pis")),
  sexo_codigo = sexo_codigo,
  sexo = sexo,
  idade = idade,
  grupo_idade = cut(
    idade,
    breaks = c(-Inf, 24, 34, 44, 55, Inf),
    labels = c("0-24", "25-34", "35-44", "45-55", "56+"),
    right = TRUE
  ),
  homem_25_55 = sexo_codigo %in% c("1", "01") & idade >= 25 & idade <= 55,
  natureza_juridica_codigo = natureza_juridica_codigo,
  remuneracao_media_sm = remuneracao_media_sm,
  remuneracao_dezembro_sm = remuneracao_dezembro_sm,
  remuneracao_media_positiva = remuneracao_media_sm > 0,
  log_remuneracao_media_sm = ifelse(remuneracao_media_sm > 0, log(remuneracao_media_sm), NA_real_),
  qtd_horas_contratadas = qtd_horas_contratadas,
  cbo_94 = to_code(col_or_na("cbo_94")),
  cnae_95_classe = to_code(col_or_na("cnae_95_classe")),
  municipio = to_code(col_or_na("municipio")),
  vinculo_ativo_31_12 = to_digit_code(col_or_na("vinculo_ativo_31_12")),
  motivo_desligamento = to_digit_code(col_or_na("motivo_desligamento")),
  mes_admissao = to_digit_code(col_or_na("mes_admissao")),
  mes_desligamento = to_digit_code(col_or_na("mes_desligamento")),
  grau_instrucao = to_digit_code(col_or_na("grau_instrucao")),
  tamanho_estabelecimento = to_digit_code(col_or_na("tamanho_estabelecimento")),
  tempo_emprego = to_num_br(col_or_na("tempo_emprego")),
  tipo_vinculo = to_digit_code(col_or_na("tipo_vinculo")),
  tipo_estab_codigo = to_digit_code(col_or_na("tipo_estab_codigo")),
  tipo_estab_id = to_code(col_or_na("tipo_estab_id")),
  stringsAsFactors = FALSE
)

saveRDS(rais_1994, "intermediate/rais_1994_variaveis_interesse.rds")
data.table::fwrite(
  rais_1994,
  file = "intermediate/rais_1994_variaveis_interesse.csv"
)

dt <- data.table::as.data.table(rais_1994)

resumo_geral <- data.frame(
  indicador = c(
    "linhas",
    "pis_preenchido",
    "homens_25_55",
    "homens_25_55_remuneracao_positiva",
    "remuneracao_media_sm_media",
    "remuneracao_media_sm_mediana",
    "remuneracao_media_sm_p10",
    "remuneracao_media_sm_p90"
  ),
  valor = c(
    nrow(rais_1994),
    sum(!is.na(rais_1994$pis)),
    sum(rais_1994$homem_25_55, na.rm = TRUE),
    sum(rais_1994$homem_25_55 & rais_1994$remuneracao_media_positiva, na.rm = TRUE),
    mean(rais_1994$remuneracao_media_sm, na.rm = TRUE),
    stats::median(rais_1994$remuneracao_media_sm, na.rm = TRUE),
    stats::quantile(rais_1994$remuneracao_media_sm, 0.10, na.rm = TRUE, names = FALSE),
    stats::quantile(rais_1994$remuneracao_media_sm, 0.90, na.rm = TRUE, names = FALSE)
  ),
  stringsAsFactors = FALSE
)

resumo_sexo <- dt[
  ,
  .(
    n = .N,
    idade_media = mean(idade, na.rm = TRUE),
    remuneracao_media_sm_media = mean(remuneracao_media_sm, na.rm = TRUE),
    remuneracao_media_sm_mediana = stats::median(remuneracao_media_sm, na.rm = TRUE),
    remuneracao_media_sm_p10 = stats::quantile(remuneracao_media_sm, 0.10, na.rm = TRUE, names = FALSE),
    remuneracao_media_sm_p90 = stats::quantile(remuneracao_media_sm, 0.90, na.rm = TRUE, names = FALSE)
  ),
  by = sexo
]

data.table::fwrite(resumo_geral, "intermediate/resumo_geral_1994.csv")
data.table::fwrite(resumo_sexo, "intermediate/resumo_por_sexo_1994.csv")

theme_rais <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

sexo_cores <- c(Homem = "#2C7FB8", Mulher = "#D95F02", Ignorado = "#7A7A7A")

p_idade <- ggplot2::ggplot(
  dt[!is.na(idade) & !is.na(sexo)],
  ggplot2::aes(x = idade, fill = sexo)
) +
  ggplot2::geom_histogram(binwidth = 1, color = "white", linewidth = 0.1, alpha = 0.85) +
  ggplot2::scale_fill_manual(values = sexo_cores, drop = FALSE) +
  ggplot2::labs(
    title = "RAIS 1994 - Distribuicao de idade",
    x = "Idade",
    y = "Numero de vinculos",
    fill = "Sexo"
  ) +
  theme_rais

ggplot2::ggsave(
  "figures/1994_distribuicao_idade.png",
  p_idade,
  width = 8,
  height = 5,
  dpi = 160
)

p99_rem <- stats::quantile(
  dt[remuneracao_media_sm > 0, remuneracao_media_sm],
  0.99,
  na.rm = TRUE,
  names = FALSE
)

p_rem <- ggplot2::ggplot(
  dt[remuneracao_media_sm > 0 & remuneracao_media_sm <= p99_rem & !is.na(sexo)],
  ggplot2::aes(x = remuneracao_media_sm, fill = sexo)
) +
  ggplot2::geom_histogram(binwidth = 0.25, color = "white", linewidth = 0.1, alpha = 0.82) +
  ggplot2::scale_fill_manual(values = sexo_cores, drop = FALSE) +
  ggplot2::labs(
    title = "RAIS 1994 - Remuneracao media em salarios minimos",
    subtitle = "Amostra com remuneracao positiva, truncada no p99 para visualizacao",
    x = "Remuneracao media (salarios minimos)",
    y = "Numero de vinculos",
    fill = "Sexo"
  ) +
  theme_rais

ggplot2::ggsave(
  "figures/1994_distribuicao_remuneracao_media_sm.png",
  p_rem,
  width = 8,
  height = 5,
  dpi = 160
)

idade_sexo <- dt[
  !is.na(idade) & idade >= 16 & idade <= 70 &
    remuneracao_media_sm > 0 & !is.na(sexo),
  .(
    n = .N,
    remuneracao_mediana_sm = stats::median(remuneracao_media_sm, na.rm = TRUE)
  ),
  by = .(idade, sexo)
]

p_idade_rem <- ggplot2::ggplot(
  idade_sexo[n >= 20],
  ggplot2::aes(x = idade, y = remuneracao_mediana_sm, color = sexo)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.4) +
  ggplot2::scale_color_manual(values = sexo_cores, drop = FALSE) +
  ggplot2::labs(
    title = "RAIS 1994 - Remuneracao mediana por idade",
    subtitle = "Idades com pelo menos 20 observacoes por sexo",
    x = "Idade",
    y = "Remuneracao mediana (salarios minimos)",
    color = "Sexo"
  ) +
  theme_rais

ggplot2::ggsave(
  "figures/1994_remuneracao_mediana_por_idade_sexo.png",
  p_idade_rem,
  width = 8,
  height = 5,
  dpi = 160
)

natureza_top <- dt[
  !is.na(natureza_juridica_codigo),
  .(n = .N),
  by = natureza_juridica_codigo
][order(-n)][1:min(.N, 12)]

p_natureza <- ggplot2::ggplot(
  natureza_top,
  ggplot2::aes(x = stats::reorder(natureza_juridica_codigo, n), y = n)
) +
  ggplot2::geom_col(fill = "#4C78A8", width = 0.75) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "RAIS 1994 - Natureza juridica mais frequente",
    x = "Codigo de natureza juridica",
    y = "Numero de vinculos"
  ) +
  theme_rais

ggplot2::ggsave(
  "figures/1994_natureza_juridica_top.png",
  p_natureza,
  width = 8,
  height = 5,
  dpi = 160
)

p_homens <- ggplot2::ggplot(
  dt[homem_25_55 == TRUE & remuneracao_media_sm > 0 & remuneracao_media_sm <= p99_rem],
  ggplot2::aes(x = remuneracao_media_sm)
) +
  ggplot2::geom_histogram(binwidth = 0.25, fill = "#2C7FB8", color = "white", linewidth = 0.1) +
  ggplot2::labs(
    title = "RAIS 1994 - Homens 25-55",
    subtitle = "Remuneracao positiva, truncada no p99 da amostra completa",
    x = "Remuneracao media (salarios minimos)",
    y = "Numero de vinculos"
  ) +
  theme_rais

ggplot2::ggsave(
  "figures/1994_homens_25_55_remuneracao_media_sm.png",
  p_homens,
  width = 8,
  height = 5,
  dpi = 160
)

cat("\nDataframe salvo em:\n")
cat("  intermediate/rais_1994_variaveis_interesse.rds\n")
cat("  intermediate/rais_1994_variaveis_interesse.csv\n")

cat("\nResumos salvos em:\n")
cat("  intermediate/resumo_geral_1994.csv\n")
cat("  intermediate/resumo_por_sexo_1994.csv\n")
cat("  intermediate/dicionario_variaveis_interesse_1994.csv\n")

cat("\nFiguras salvas em:\n")
cat("  figures/1994_distribuicao_idade.png\n")
cat("  figures/1994_distribuicao_remuneracao_media_sm.png\n")
cat("  figures/1994_remuneracao_mediana_por_idade_sexo.png\n")
cat("  figures/1994_natureza_juridica_top.png\n")
cat("  figures/1994_homens_25_55_remuneracao_media_sm.png\n")

if (all(is.na(rais_1994$pis))) {
  cat("\nATENCAO: PIS esta ausente neste arquivo. A base ainda esta em nivel vinculo-ano,\n")
  cat("nao trabalhador-ano. Para somar vinculos por trabalhador, precisamos de um arquivo\n")
  cat("ou layout de 1994 que contenha identificador do trabalhador.\n")
}
