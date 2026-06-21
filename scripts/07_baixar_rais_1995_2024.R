# Baixa os microdados da RAIS de 1994 a 2024 por UF.
#
# Uso padrao, todos os anos e UFs:
#   Rscript scripts/07_baixar_rais_1995_2024.R
#
# Subconjuntos:
#   Rscript scripts/07_baixar_rais_1995_2024.R 1995 AC
#   Rscript scripts/07_baixar_rais_1995_2024.R 1995:2024 SP,RJ,MG
#
# Teste sem baixar:
#   RAIS_DRY_RUN=TRUE Rscript scripts/07_baixar_rais_1995_2024.R 1995 AC
#
# Rebaixar arquivos existentes:
#   RAIS_OVERWRITE=TRUE Rscript scripts/07_baixar_rais_1995_2024.R
#
# Baixar tambem os arquivos de estabelecimentos ESTB<ano>.7z:
#   RAIS_INCLUIR_ESTB=TRUE Rscript scripts/07_baixar_rais_1995_2024.R

source("scripts/rais_utils.R")

args <- commandArgs(trailingOnly = TRUE)

anos_arg <- if (length(args) >= 1) args[[1]] else Sys.getenv("RAIS_ANOS", "1994:2024")
ufs_arg <- if (length(args) >= 2) args[[2]] else Sys.getenv("RAIS_UFS", "")

anos <- as.integer(parse_lista(anos_arg, 1995:2024))
ufs <- toupper(parse_lista(ufs_arg, UFS_RAIS))

raw_dir <- Sys.getenv("RAIS_RAW_DIR", "raw")
base_url <- Sys.getenv("RAIS_BASE_URL", "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS")

overwrite <- toupper(Sys.getenv("RAIS_OVERWRITE", "FALSE")) %in% c("TRUE", "1", "SIM", "YES")
dry_run <- toupper(Sys.getenv("RAIS_DRY_RUN", "FALSE")) %in% c("TRUE", "1", "SIM", "YES")
incluir_estb <- toupper(Sys.getenv("RAIS_INCLUIR_ESTB", "FALSE")) %in% c("TRUE", "1", "SIM", "YES")
max_tentativas <- as.integer(Sys.getenv("RAIS_MAX_TENTATIVAS", "3"))
pausa_segundos <- as.numeric(Sys.getenv("RAIS_PAUSA_SEGUNDOS", "0.5"))

if (any(is.na(anos))) {
  stop("Anos invalidos. Use formato como 1994, 1994:2024 ou 1994,1996.", call. = FALSE)
}

ufs_invalidas <- setdiff(ufs, UFS_RAIS)
if (length(ufs_invalidas) > 0) {
  stop("UF(s) invalida(s): ", paste(ufs_invalidas, collapse = ", "), call. = FALSE)
}

if (is.na(max_tentativas) || max_tentativas < 1) max_tentativas <- 1L
if (is.na(pausa_segundos) || pausa_segundos < 0) pausa_segundos <- 0

dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create("intermediate/downloads", showWarnings = FALSE, recursive = TRUE)

montar_url <- function(ano, arquivo) {
  paste0(gsub("/+$", "", base_url), "/", ano, "/", arquivo)
}

baixar_arquivo <- function(url, destino, overwrite = FALSE, dry_run = FALSE, max_tentativas = 3) {
  dir.create(dirname(destino), showWarnings = FALSE, recursive = TRUE)

  if (file.exists(destino) && !overwrite) {
    return(list(
      status = "ja_existe",
      mensagem = "Arquivo ja existe; download pulado.",
      tamanho_bytes = file.info(destino)$size
    ))
  }

  if (dry_run) {
    return(list(
      status = "dry_run",
      mensagem = "Simulacao; nenhum arquivo baixado.",
      tamanho_bytes = if (file.exists(destino)) file.info(destino)$size else NA_real_
    ))
  }

  destino_tmp <- paste0(destino, ".part")
  if (file.exists(destino_tmp)) {
    unlink(destino_tmp)
  }

  ultimo_erro <- NA_character_

  for (tentativa in seq_len(max_tentativas)) {
    ok <- tryCatch(
      {
        utils::download.file(
          url = url,
          destfile = destino_tmp,
          mode = "wb",
          quiet = FALSE
        )
        TRUE
      },
      warning = function(w) {
        ultimo_erro <<- conditionMessage(w)
        FALSE
      },
      error = function(e) {
        ultimo_erro <<- conditionMessage(e)
        FALSE
      }
    )

    tamanho_tmp <- if (file.exists(destino_tmp)) file.info(destino_tmp)$size else 0

    if (ok && is.finite(tamanho_tmp) && tamanho_tmp > 0) {
      if (file.exists(destino) && overwrite) {
        unlink(destino)
      }
      file.rename(destino_tmp, destino)

      return(list(
        status = "baixado",
        mensagem = paste0("Download concluido na tentativa ", tentativa, "."),
        tamanho_bytes = file.info(destino)$size
      ))
    }

    if (file.exists(destino_tmp)) {
      unlink(destino_tmp)
    }

    if (tentativa < max_tentativas) {
      Sys.sleep(1 + tentativa)
    }
  }

  list(
    status = "falhou",
    mensagem = ifelse(is.na(ultimo_erro), "Falha sem mensagem detalhada.", ultimo_erro),
    tamanho_bytes = NA_real_
  )
}

plano <- do.call(
  rbind,
  lapply(anos, function(ano) {
    arquivos_uf <- data.frame(
      ano = ano,
      tipo = "vinculos",
      uf = ufs,
      arquivo = paste0(ufs, ano, ".7z"),
      stringsAsFactors = FALSE
    )

    if (!incluir_estb) return(arquivos_uf)

    rbind(
      arquivos_uf,
      data.frame(
        ano = ano,
        tipo = "estabelecimentos",
        uf = NA_character_,
        arquivo = paste0("ESTB", ano, ".7z"),
        stringsAsFactors = FALSE
      )
    )
  })
)

plano$url <- mapply(montar_url, plano$ano, plano$arquivo, USE.NAMES = FALSE)
plano$destino <- file.path(raw_dir, as.character(plano$ano), plano$arquivo)

cat("\nDownload RAIS\n")
cat("  Anos: ", paste(range(anos), collapse = "-"), " (", length(anos), " anos)\n", sep = "")
cat("  UFs: ", paste(ufs, collapse = ", "), "\n", sep = "")
cat("  Arquivos planejados: ", nrow(plano), "\n", sep = "")
cat("  Diretorio destino: ", raw_dir, "/<ano>/\n", sep = "")
cat("  Sobrescrever existentes: ", overwrite, "\n", sep = "")
cat("  Incluir ESTB: ", incluir_estb, "\n", sep = "")
cat("  Dry run: ", dry_run, "\n\n", sep = "")

log_list <- vector("list", nrow(plano))

for (i in seq_len(nrow(plano))) {
  item <- plano[i, ]
  cat(sprintf("[%04d/%04d] %s\n", i, nrow(plano), item$url))

  inicio <- Sys.time()
  resultado <- baixar_arquivo(
    url = item$url,
    destino = item$destino,
    overwrite = overwrite,
    dry_run = dry_run,
    max_tentativas = max_tentativas
  )
  fim <- Sys.time()

  log_list[[i]] <- data.frame(
    ano = item$ano,
    tipo = item$tipo,
    uf = item$uf,
    arquivo = item$arquivo,
    url = item$url,
    destino = item$destino,
    status = resultado$status,
    tamanho_bytes = resultado$tamanho_bytes,
    mensagem = resultado$mensagem,
    inicio = format(inicio, "%Y-%m-%d %H:%M:%S"),
    fim = format(fim, "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )

  cat("  -> ", resultado$status, " | ", resultado$mensagem, "\n", sep = "")

  if (pausa_segundos > 0 && i < nrow(plano) && !dry_run) {
    Sys.sleep(pausa_segundos)
  }
}

log_download <- do.call(rbind, log_list)
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
arquivo_log <- file.path(
  "intermediate/downloads",
  paste0("log_download_rais_", min(anos), "_", max(anos), "_", timestamp, ".csv")
)

utils::write.csv(
  log_download,
  file = arquivo_log,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nResumo:\n")
print(table(log_download$status, useNA = "ifany"))

cat("\nLog salvo em:\n")
cat("  ", arquivo_log, "\n", sep = "")

if (any(log_download$status == "falhou")) {
  cat("\nArquivos com falha:\n")
  print(
    log_download[log_download$status == "falhou", c("ano", "uf", "arquivo", "mensagem")],
    row.names = FALSE
  )
}
