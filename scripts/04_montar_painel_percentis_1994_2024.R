# Junta os anos tratados e calcula percentis anuais de renda.
#
# Entrada:
#   intermediate/anos/rais_<ano>_homens_25_55.rds
#
# Uso:
#   Rscript scripts/04_montar_painel_percentis_1994_2024.R
#   Rscript scripts/04_montar_painel_percentis_1994_2024.R 1994:2024
#
# Por padrao, tambem salva o painel micro tratado em RDS. Se ficar grande:
#   RAIS_SALVAR_PAINEL_MICRO=FALSE Rscript scripts/04_montar_painel_percentis_1994_2024.R

source("scripts/rais_utils.R")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote data.table para montar o painel.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
anos_arg <- if (length(args) >= 1) args[[1]] else Sys.getenv("RAIS_ANOS", "1994:1995")
anos <- as.integer(parse_lista(anos_arg, 1994:1995))

renda_var <- Sys.getenv("RAIS_RENDA_VAR", "remuneracao_media_sm")
salvar_painel_micro <- !(toupper(Sys.getenv("RAIS_SALVAR_PAINEL_MICRO", "TRUE")) %in% c("FALSE", "0", "NAO", "NO"))
salvar_painel_csv <- toupper(Sys.getenv("RAIS_SALVAR_PAINEL_CSV", "FALSE")) %in% c("TRUE", "1", "SIM", "YES")

dir.create("intermediate", showWarnings = FALSE, recursive = TRUE)
dir.create("final", showWarnings = FALSE, recursive = TRUE)

arquivos <- list.files(
  "intermediate/anos",
  pattern = "^rais_[0-9]{4}_homens_25_55\\.rds$",
  full.names = TRUE
)

ano_arquivo <- as.integer(sub("^rais_([0-9]{4})_homens_25_55\\.rds$", "\\1", basename(arquivos)))
arquivos <- arquivos[ano_arquivo %in% anos]
ano_arquivo <- ano_arquivo[ano_arquivo %in% anos]

ord <- order(ano_arquivo)
arquivos <- arquivos[ord]
ano_arquivo <- ano_arquivo[ord]

if (length(arquivos) == 0) {
  stop(
    "Nenhum arquivo anual encontrado em intermediate/anos. Rode antes scripts/03_processar_rais_1994_2024.R.",
    call. = FALSE
  )
}

cat("\nArquivos anuais encontrados:\n")
print(data.frame(ano = ano_arquivo, arquivo = arquivos), row.names = FALSE)

percentis_list <- list()
painel_list <- if (salvar_painel_micro) list() else NULL

for (i in seq_along(arquivos)) {
  arquivo <- arquivos[[i]]
  cat("\nLendo painel anual: ", arquivo, "\n", sep = "")
  dt <- readRDS(arquivo)
  data.table::setDT(dt)

  if (!(renda_var %in% names(dt))) {
    warning("Variavel de renda ausente em ", arquivo, ": ", renda_var, call. = FALSE)
    next
  }

  percentis <- calcular_percentis_renda(dt, renda_var = renda_var)
  percentis$variavel_renda <- renda_var
  percentis$nivel_observacao <- paste(sort(unique(dt$nivel_observacao)), collapse = "|")
  percentis_list[[length(percentis_list) + 1]] <- percentis

  if (salvar_painel_micro) {
    painel_list[[length(painel_list) + 1]] <- dt
  }
}

if (length(percentis_list) == 0) {
  stop("Nao foi possivel calcular percentis para os arquivos encontrados.", call. = FALSE)
}

painel_percentis <- data.table::rbindlist(percentis_list, fill = TRUE)
data.table::setorder(painel_percentis, ano)

ano_min <- min(painel_percentis$ano, na.rm = TRUE)
ano_max <- max(painel_percentis$ano, na.rm = TRUE)
sufixo <- sprintf("%s_%s", ano_min, ano_max)

arquivo_percentis_intermediate <- file.path(
  "intermediate",
  sprintf("painel_percentis_%s_homens_25_55_%s.csv", renda_var, sufixo)
)
arquivo_percentis_final <- file.path(
  "final",
  sprintf("painel_percentis_%s_homens_25_55_%s.csv", renda_var, sufixo)
)

data.table::fwrite(painel_percentis, arquivo_percentis_intermediate)
data.table::fwrite(painel_percentis, arquivo_percentis_final)

cat("\nPainel de percentis salvo em:\n")
cat("  ", arquivo_percentis_intermediate, "\n", sep = "")
cat("  ", arquivo_percentis_final, "\n", sep = "")

if (salvar_painel_micro) {
  painel_micro <- data.table::rbindlist(painel_list, fill = TRUE)
  arquivo_painel_micro <- file.path(
    "intermediate",
    sprintf("painel_rais_homens_25_55_%s.rds", sufixo)
  )
  saveRDS(painel_micro, arquivo_painel_micro)

  cat("\nPainel micro tratado salvo em:\n")
  cat("  ", arquivo_painel_micro, "\n", sep = "")

  if (salvar_painel_csv) {
    arquivo_painel_csv <- file.path(
      "intermediate",
      sprintf("painel_rais_homens_25_55_%s.csv.gz", sufixo)
    )
    data.table::fwrite(painel_micro, arquivo_painel_csv)
    cat("  ", arquivo_painel_csv, "\n", sep = "")
  }
}

cat("\nResumo dos percentis:\n")
print(painel_percentis)
