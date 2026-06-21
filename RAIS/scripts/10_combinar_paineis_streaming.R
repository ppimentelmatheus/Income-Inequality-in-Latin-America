# Combina paineis de percentis produzidos pelo pipeline streaming.
#
# Uso:
#   Rscript scripts/10_combinar_paineis_streaming.R
#   Rscript scripts/10_combinar_paineis_streaming.R 1994:2024

if (basename(getwd()) == "scripts" && file.exists("../scripts/10_combinar_paineis_streaming.R")) {
  setwd("..")
}

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote data.table.", call. = FALSE)
}

parse_anos <- function(x, default) {
  if (length(x) == 0 || is.na(x) || trimws(x) == "") return(default)
  if (grepl("^[0-9]{4}:[0-9]{4}$", x)) {
    z <- as.integer(strsplit(x, ":", fixed = TRUE)[[1]])
    return(seq(z[[1]], z[[2]]))
  }
  as.integer(trimws(unlist(strsplit(x, ",", fixed = TRUE))))
}

args <- commandArgs(trailingOnly = TRUE)
anos <- parse_anos(if (length(args) >= 1) args[[1]] else "1994:2024", 1994:2024)
renda_var <- Sys.getenv("RAIS_RENDA_VAR", "remuneracao_media_sm")

dir.create("final", showWarnings = FALSE, recursive = TRUE)

candidatos <- c(
  list.files(
    "final",
    pattern = "^painel_percentis_streaming_.*_homens_25_55_[0-9]{4}_[0-9]{4}\\.csv$",
    full.names = TRUE
  ),
  list.files(
    "intermediate",
    pattern = "^painel_percentis_streaming_.*_homens_25_55_[0-9]{4}_[0-9]{4}\\.csv$",
    full.names = TRUE
  ),
  list.files(
    "scripts/final",
    pattern = "^painel_percentis_streaming_.*_homens_25_55_[0-9]{4}_[0-9]{4}\\.csv$",
    full.names = TRUE
  ),
  list.files(
    "scripts/intermediate",
    pattern = "^painel_percentis_streaming_.*_homens_25_55_[0-9]{4}_[0-9]{4}\\.csv$",
    full.names = TRUE
  )
)

candidatos <- unique(candidatos[file.exists(candidatos)])

if (length(candidatos) == 0) {
  stop("Nenhum painel streaming encontrado.", call. = FALSE)
}

paineis <- lapply(candidatos, function(path) {
  dt <- data.table::fread(path)
  dt[, arquivo_origem := path]
  dt
})

painel <- data.table::rbindlist(paineis, fill = TRUE)
painel <- painel[ano %in% anos]

if (nrow(painel) == 0) {
  stop("Nenhum ano solicitado aparece nos paineis encontrados.", call. = FALSE)
}

painel[, prioridade := ifelse(grepl("^final/", arquivo_origem), 1L,
  ifelse(grepl("^intermediate/", arquivo_origem), 2L,
    ifelse(grepl("^scripts/final/", arquivo_origem), 3L, 4L)
  )
)]
data.table::setorder(painel, ano, prioridade)
painel <- painel[, .SD[1], by = ano]
painel[, c("arquivo_origem", "prioridade") := NULL]
data.table::setorder(painel, ano)

ano_min <- min(painel$ano, na.rm = TRUE)
ano_max <- max(painel$ano, na.rm = TRUE)
arquivo_saida <- file.path(
  "final",
  sprintf("painel_percentis_streaming_%s_homens_25_55_%s_%s.csv", renda_var, ano_min, ano_max)
)

data.table::fwrite(painel, arquivo_saida)

cat("\nPainel combinado salvo em:\n")
cat("  ", arquivo_saida, "\n", sep = "")
cat("\nAnos no painel combinado:\n")
print(painel$ano)

faltantes <- setdiff(anos, painel$ano)
if (length(faltantes) > 0) {
  cat("\nAnos ainda faltantes:\n")
  print(faltantes)
}
