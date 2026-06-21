# Le o TXT da RAIS 1994 e identifica as variaveis-alvo do projeto.
# Uso:
#   Rscript scripts/01_identificar_variaveis_1994.R
#   Rscript scripts/01_identificar_variaveis_1994.R raw/AC1994/AC1994.txt

args <- commandArgs(trailingOnly = TRUE)
arquivo <- if (length(args) >= 1) args[[1]] else "raw/AC1994/AC1994.txt"

if (!file.exists(arquivo)) {
  stop("Arquivo nao encontrado: ", arquivo, call. = FALSE)
}

dir.create("intermediate", showWarnings = FALSE, recursive = TRUE)

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

read_selected_columns <- function(path, header, selected_pos) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    out <- data.table::fread(
      path,
      sep = ";",
      header = TRUE,
      encoding = "Latin-1",
      colClasses = "character",
      select = selected_pos,
      na.strings = c("", "NA"),
      showProgress = FALSE,
      check.names = FALSE
    )
    return(as.data.frame(out, stringsAsFactors = FALSE))
  }

  col_classes <- rep("NULL", length(header))
  col_classes[selected_pos] <- "character"

  utils::read.delim(
    path,
    sep = ";",
    header = TRUE,
    fileEncoding = "latin1",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = col_classes
  )
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

header <- read_header(arquivo)
normalized_header <- normalize_name(header)

targets <- list(
  pis = c("^pis$", "pis_pasep", "pasep", "identificador.*trabalhador"),
  sexo = c("^sexo_trabalhador$", "^sexo$"),
  idade = c("^idade$"),
  natureza_juridica = c("^natureza_juridica$"),
  remuneracao_media = c("^vl_remun_media_sm$", "^valor_remun_media_sm$", "^vl_remuneracao_media")
)

target_pos <- vapply(targets, find_first, integer(1), normalized_names = normalized_header)
target_col <- ifelse(is.na(target_pos), NA_character_, header[target_pos])

matches <- data.frame(
  variavel_projeto = names(targets),
  status = ifelse(is.na(target_pos), "AUSENTE", "OK"),
  posicao = ifelse(is.na(target_pos), NA_integer_, target_pos),
  coluna_no_arquivo = target_col,
  stringsAsFactors = FALSE
)

cat("\nArquivo lido:\n")
cat("  ", arquivo, "\n", sep = "")
cat("\nColunas encontradas no cabecalho:\n")
print(data.frame(posicao = seq_along(header), coluna = header), row.names = FALSE)

cat("\nVariaveis-alvo identificadas:\n")
print(matches, row.names = FALSE)

utils::write.csv(
  matches,
  file = "intermediate/variaveis_identificadas_1994.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

selected_pos <- sort(unique(stats::na.omit(as.integer(target_pos))))

if (length(selected_pos) == 0) {
  stop("Nenhuma variavel-alvo foi encontrada no arquivo.", call. = FALSE)
}

raw <- read_selected_columns(arquivo, header, selected_pos)

get_target <- function(target_name) {
  pos <- target_pos[[target_name]]
  if (is.na(pos)) return(rep(NA_character_, nrow(raw)))
  raw[[header[[pos]]]]
}

pis_raw <- get_target("pis")
sexo_raw <- get_target("sexo")
idade_raw <- get_target("idade")
natureza_juridica_raw <- get_target("natureza_juridica")
remuneracao_media_raw <- get_target("remuneracao_media")

natureza_juridica_invalida <- !is.na(to_code(natureza_juridica_raw)) &
  !grepl("^[0-9]+$", to_code(natureza_juridica_raw))

rais_1994_vars <- data.frame(
  pis = to_code(pis_raw),
  sexo = to_digit_code(sexo_raw),
  idade = to_int(idade_raw),
  natureza_juridica = to_digit_code(natureza_juridica_raw),
  remuneracao_media = to_num_br(remuneracao_media_raw),
  stringsAsFactors = FALSE
)

cat("\nDiagnosticos rapidos:\n")
cat("  Linhas lidas: ", nrow(rais_1994_vars), "\n", sep = "")

if (all(is.na(rais_1994_vars$pis))) {
  cat("  ATENCAO: PIS nao aparece no cabecalho deste TXT de 1994.\n")
  cat("  Sem PIS, ainda nao da para agregar vinculos por trabalhador.\n")
}

cat("\nDistribuicao de sexo:\n")
print(sort(table(rais_1994_vars$sexo, useNA = "ifany"), decreasing = TRUE))

cat("\nResumo de idade:\n")
print(summary(rais_1994_vars$idade))

cat("\nNatureza juridica - codigos mais frequentes:\n")
print(head(sort(table(rais_1994_vars$natureza_juridica, useNA = "ifany"), decreasing = TRUE), 20))
cat("  Linhas com Natureza Juridica nao numerica/ilegivel: ",
    sum(natureza_juridica_invalida, na.rm = TRUE),
    "\n",
    sep = "")

cat("\nResumo da remuneracao media:\n")
print(summary(rais_1994_vars$remuneracao_media))

homem_25_55 <- rais_1994_vars$sexo %in% c("1", "01") &
  rais_1994_vars$idade >= 25 &
  rais_1994_vars$idade <= 55

cat("\nAmostra inicial, antes do filtro de setor privado:\n")
cat("  Homens 25-55: ", sum(homem_25_55, na.rm = TRUE), "\n", sep = "")
cat("  Homens 25-55 com remuneracao media positiva: ",
    sum(homem_25_55 & rais_1994_vars$remuneracao_media > 0, na.rm = TRUE),
    "\n",
    sep = "")

cat("\nObservacao:\n")
cat("  Confirme no layout oficial de 1994 quais codigos de Natureza Juridica\n")
cat("  correspondem ao setor privado antes de aplicar esse filtro.\n")
cat("  O arquivo de diagnostico foi salvo em intermediate/variaveis_identificadas_1994.csv\n")
