# Processa microdados da RAIS para homens de 25 a 55 anos.
#
# Uso local, sem download:
#   Rscript scripts/03_processar_rais_1994_2024.R
#   Rscript scripts/03_processar_rais_1994_2024.R 1994 AC
#   Rscript scripts/03_processar_rais_1994_2024.R 1994:2024 AC,SP,RJ
#
# Para baixar arquivos ausentes do FTP antes de extrair/processar:
#   RAIS_BAIXAR=TRUE Rscript scripts/03_processar_rais_1994_2024.R 1994:2024
#
# Saida principal:
#   intermediate/anos/rais_<ano>_homens_25_55.rds

source("scripts/rais_utils.R")

args <- commandArgs(trailingOnly = TRUE)

anos_arg <- if (length(args) >= 1) args[[1]] else Sys.getenv("RAIS_ANOS", "1994:2024")
ufs_arg <- if (length(args) >= 2) args[[2]] else Sys.getenv("RAIS_UFS", "")

anos <- as.integer(parse_lista(anos_arg, 1994:2024))
ufs <- parse_lista(ufs_arg, UFS_RAIS)
ufs <- toupper(ufs)

baixar <- toupper(Sys.getenv("RAIS_BAIXAR", "FALSE")) %in% c("TRUE", "1", "SIM", "YES")
extrair <- !(toupper(Sys.getenv("RAIS_EXTRAIR", "TRUE")) %in% c("FALSE", "0", "NAO", "NO"))
salvar_csv <- toupper(Sys.getenv("RAIS_SALVAR_CSV_ANUAL", "FALSE")) %in% c("TRUE", "1", "SIM", "YES")
agregar_trabalhador <- !(toupper(Sys.getenv("RAIS_AGREGAR_TRABALHADOR", "TRUE")) %in% c("FALSE", "0", "NAO", "NO"))

dir.create("intermediate", showWarnings = FALSE, recursive = TRUE)
dir.create("intermediate/anos", showWarnings = FALSE, recursive = TRUE)

cat("\nProcessamento RAIS\n")
cat("  Anos: ", paste(anos, collapse = ", "), "\n", sep = "")
cat("  UFs: ", paste(ufs, collapse = ", "), "\n", sep = "")
cat("  Baixar: ", baixar, "\n", sep = "")
cat("  Extrair .7z: ", extrair, "\n", sep = "")
cat("  Agregar por trabalhador quando PIS existir: ", agregar_trabalhador, "\n", sep = "")
cat("  Filtro aplicado: homens, 25 a 55 anos\n\n")

resultados <- list()

for (ano in anos) {
  cat("\n==== Ano ", ano, " ====\n", sep = "")
  resumo <- processar_rais_ano(
    ano = ano,
    raw_dir = "raw",
    output_dir = "intermediate/anos",
    ufs = ufs,
    baixar = baixar,
    extrair = extrair,
    agregar_trabalhador = agregar_trabalhador,
    salvar_csv = salvar_csv
  )
  resultados[[length(resultados) + 1]] <- resumo
  print(resumo, row.names = FALSE)
}

resumo_final <- do.call(rbind, resultados)
utils::write.csv(
  resumo_final,
  "intermediate/resumo_processamento_rais_1994_2024.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nResumo salvo em:\n")
cat("  intermediate/resumo_processamento_rais_1994_2024.csv\n")
