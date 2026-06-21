# Pipeline leve para calcular percentis da RAIS sem guardar todos os
# microdados no disco.
#
# Ideia:
#   1. Processa uma UF por vez.
#   2. Usa arquivo local se ja existir em raw/.
#   3. Se nao existir, baixa para diretorio temporario.
#   4. Extrai em diretorio temporario.
#   5. Le, filtra homens 25-55 e guarda apenas a renda positiva do ano.
#   6. Calcula os percentis anuais.
#   7. Apaga temporarios.
#
# Uso:
#   Rscript scripts/08_pipeline_leve_percentis_streaming.R 1994:2024
#   Rscript scripts/08_pipeline_leve_percentis_streaming.R 1994:2024 SP,RJ,MG
#
# Depois:
#   Rscript scripts/09_grafico_heathcote_streaming.R final/painel_percentis_streaming_remuneracao_media_sm_homens_25_55_1994_2024.csv

if (basename(getwd()) == "scripts" && file.exists("../scripts/08_pipeline_leve_percentis_streaming.R")) {
  setwd("..")
}

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote data.table.", call. = FALSE)
}

if (!requireNamespace("archive", quietly = TRUE)) {
  stop("Instale o pacote archive.", call. = FALSE)
}

UFS_RAIS <- c(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
  "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
  "RO", "RR", "RS", "SC", "SE", "SP", "TO"
)

UF_MUN_PREFIX <- c(
  RO = "11", AC = "12", AM = "13", RR = "14", PA = "15", AP = "16", TO = "17",
  MA = "21", PI = "22", CE = "23", RN = "24", PB = "25", PE = "26", AL = "27",
  SE = "28", BA = "29", MG = "31", ES = "32", RJ = "33", SP = "35",
  PR = "41", SC = "42", RS = "43", MS = "50", MT = "51", GO = "52", DF = "53"
)

ARQUIVOS_REGIONAIS_POS_2017 <- list(
  RAIS_VINC_PUB_NORTE = c("RO", "AC", "AM", "RR", "PA", "AP", "TO"),
  RAIS_VINC_PUB_NORDESTE = c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"),
  RAIS_VINC_PUB_MG_ES_RJ = c("MG", "ES", "RJ"),
  RAIS_VINC_PUB_SP = c("SP"),
  RAIS_VINC_PUB_SUL = c("PR", "SC", "RS"),
  RAIS_VINC_PUB_CENTRO_OESTE = c("MS", "MT", "GO", "DF"),
  RAIS_VINC_PUB_NI = character()
)

PERCENTIS_RENDA <- c(0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)

parse_lista <- function(x, default) {
  if (length(x) == 0 || is.na(x) || trimws(x) == "") return(default)
  x <- trimws(x)
  if (grepl("^[0-9]{4}:[0-9]{4}$", x)) {
    limits <- as.integer(strsplit(x, ":", fixed = TRUE)[[1]])
    return(seq(limits[[1]], limits[[2]]))
  }
  unique(trimws(unlist(strsplit(x, ",", fixed = TRUE))))
}

normalizar_nome <- function(x) {
  y <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  y <- tolower(y)
  y <- gsub("[^a-z0-9]+", "_", y)
  y <- gsub("^_+|_+$", "", y)
  y
}

read_first_line_utf8 <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "raw", n = 100000)
  lf <- match(as.raw(0x0a), bytes)
  if (!is.na(lf)) bytes <- bytes[seq_len(lf - 1)]
  if (length(bytes) > 0 && bytes[[length(bytes)]] == as.raw(0x0d)) {
    bytes <- bytes[-length(bytes)]
  }
  line <- rawToChar(bytes)
  utf8_line <- iconv(line, from = "UTF-8", to = "UTF-8", sub = NA)
  if (!is.na(utf8_line)) return(utf8_line)
  iconv(line, from = "latin1", to = "UTF-8", sub = "")
}

detectar_encoding <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "raw", n = 100000)
  lf <- match(as.raw(0x0a), bytes)
  if (!is.na(lf)) bytes <- bytes[seq_len(lf - 1)]
  line <- rawToChar(bytes)
  utf8_line <- iconv(line, from = "UTF-8", to = "UTF-8", sub = NA)
  if (!is.na(utf8_line)) "UTF-8" else "Latin-1"
}

ler_cabecalho_rais <- function(path) {
  header_line <- read_first_line_utf8(path)
  trimws(strsplit(header_line, ";", fixed = TRUE)[[1]])
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
  out <- rep(NA_real_, length(x))
  idx <- !is.na(x)
  y <- x[idx]
  has_comma <- grepl(",", y, fixed = TRUE)

  y_comma <- y[has_comma]
  y_comma <- gsub(".", "", y_comma, fixed = TRUE)
  y_comma <- gsub(",", ".", y_comma, fixed = TRUE)
  y_comma <- gsub("[^0-9.-]", "", y_comma)

  y_dot <- y[!has_comma]
  y_dot <- gsub("[^0-9.-]", "", y_dot)

  vals <- rep(NA_real_, length(y))
  suppressWarnings(vals[has_comma] <- as.numeric(y_comma))
  suppressWarnings(vals[!has_comma] <- as.numeric(y_dot))
  out[idx] <- vals
  out
}

normalizar_sexo_codigo <- function(x) {
  raw <- to_code(x)
  y <- normalizar_nome(raw)
  out <- rep(NA_character_, length(raw))
  out[raw %in% c("1", "01")] <- "01"
  out[raw %in% c("2", "02")] <- "02"
  out[y %in% c("m", "masc", "masculino", "homem")] <- "01"
  out[y %in% c("f", "fem", "feminino", "mulher")] <- "02"
  out
}

alvos_rais <- list(
  sexo = c("^sexo_trabalhador$", "^sexo$", "genero"),
  idade = c("^idade$", "idade_trabalhador"),
  municipio = c("^municipio$", "id_municipio"),
  remuneracao_media_sm = c("^vl_remun_media_sm$", "remun.*media.*sm", "remun.*media.*salario"),
  remuneracao_media_nom = c("^vl_remun_media_nom$", "remun.*media.*nom", "remun.*media.*reais")
)

mapear_colunas_rais <- function(path) {
  header <- ler_cabecalho_rais(path)
  normalized <- normalizar_nome(header)
  pos <- vapply(alvos_rais, find_first, integer(1), normalized_names = normalized)
  data.frame(
    variavel_df = names(pos),
    status = ifelse(is.na(pos), "AUSENTE", "OK"),
    posicao = ifelse(is.na(pos), NA_integer_, as.integer(pos)),
    coluna_origem = ifelse(is.na(pos), NA_character_, header[as.integer(pos)]),
    stringsAsFactors = FALSE
  )
}

baixar_rais_uf <- function(ano, uf, raw_dir = "raw", overwrite = FALSE) {
  uf <- toupper(uf)
  dir_ano <- file.path(raw_dir, as.character(ano))
  dir.create(dir_ano, showWarnings = FALSE, recursive = TRUE)

  destino <- file.path(dir_ano, paste0(uf, ano, ".7z"))
  if (file.exists(destino) && !overwrite) return(destino)

  destino_tmp <- paste0(destino, ".part")
  if (file.exists(destino_tmp)) unlink(destino_tmp)

  url <- sprintf(
    "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/%s/%s%s.7z",
    ano,
    uf,
    ano
  )

  utils::download.file(url, destino_tmp, mode = "wb", quiet = FALSE)
  if (!file.exists(destino_tmp) || file.info(destino_tmp)$size == 0) {
    stop("Download vazio ou inexistente: ", url, call. = FALSE)
  }
  file.rename(destino_tmp, destino)
  destino
}

baixar_rais_arquivo <- function(ano, nome_arquivo, raw_dir = "raw", overwrite = FALSE) {
  dir_ano <- file.path(raw_dir, as.character(ano))
  dir.create(dir_ano, showWarnings = FALSE, recursive = TRUE)

  destino <- file.path(dir_ano, nome_arquivo)
  if (file.exists(destino) && !overwrite) return(destino)

  destino_tmp <- paste0(destino, ".part")
  if (file.exists(destino_tmp)) unlink(destino_tmp)

  url <- sprintf(
    "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/%s/%s",
    ano,
    nome_arquivo
  )

  utils::download.file(url, destino_tmp, mode = "wb", quiet = FALSE)
  if (!file.exists(destino_tmp) || file.info(destino_tmp)$size == 0) {
    stop("Download vazio ou inexistente: ", url, call. = FALSE)
  }
  file.rename(destino_tmp, destino)
  destino
}

municipio_para_uf <- function(municipio) {
  codigo <- substr(gsub("[^0-9]", "", to_code(municipio)), 1, 2)
  out <- rep(NA_character_, length(codigo))
  for (uf in names(UF_MUN_PREFIX)) {
    out[codigo == UF_MUN_PREFIX[[uf]]] <- uf
  }
  out
}

ler_tratar_rais_arquivo <- function(path, ano, uf = NA_character_, ufs_filtro = NULL) {
  mapa <- mapear_colunas_rais(path)
  obrigatorias <- c("sexo", "idade", "remuneracao_media_sm")
  faltantes <- obrigatorias[!(obrigatorias %in% mapa$variavel_df[mapa$status == "OK"])]
  if (length(faltantes) > 0) {
    warning(
      "Arquivo ignorado por falta de coluna(s): ",
      paste(faltantes, collapse = ", "),
      " | ",
      path,
      call. = FALSE
    )
    return(data.table::data.table())
  }

  presentes <- mapa[mapa$status == "OK", c("variavel_df", "posicao")]
  raw <- data.table::fread(
    path,
    sep = ";",
    header = TRUE,
    encoding = detectar_encoding(path),
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

  sexo_codigo <- normalizar_sexo_codigo(col_or_na("sexo"))
  idade <- to_int(col_or_na("idade"))

  out <- data.table::data.table(
    ano = as.integer(ano),
    uf = uf,
    uf_municipio = municipio_para_uf(col_or_na("municipio")),
    sexo_codigo = sexo_codigo,
    idade = idade,
    remuneracao_media_sm = to_num_br(col_or_na("remuneracao_media_sm")),
    remuneracao_media_nom = to_num_br(col_or_na("remuneracao_media_nom"))
  )

  if (!is.null(ufs_filtro)) {
    ufs_filtro <- toupper(ufs_filtro)
    if (any(!is.na(out$uf_municipio))) {
      out <- out[uf_municipio %in% ufs_filtro]
    }
  }

  out[
    sexo_codigo == "01" &
      !is.na(idade) &
      idade >= 25 &
      idade <= 55
  ]
}

args <- commandArgs(trailingOnly = TRUE)

anos_arg <- if (length(args) >= 1) args[[1]] else Sys.getenv("RAIS_ANOS", "1994:2024")
ufs_arg <- if (length(args) >= 2) args[[2]] else Sys.getenv("RAIS_UFS", "")

anos <- as.integer(parse_lista(anos_arg, 1994:2024))
ufs <- toupper(parse_lista(ufs_arg, UFS_RAIS))

renda_var <- Sys.getenv("RAIS_RENDA_VAR", "remuneracao_media_sm")
raw_dir <- Sys.getenv("RAIS_RAW_DIR", "raw")
baixar_ausentes <- !(toupper(Sys.getenv("RAIS_BAIXAR_AUSENTES", "TRUE")) %in% c("FALSE", "0", "NAO", "NO"))
usar_cache_raw <- toupper(Sys.getenv("RAIS_USAR_CACHE_RAW", "FALSE")) %in% c("TRUE", "1", "SIM", "YES")
manter_temp <- toupper(Sys.getenv("RAIS_MANTER_TEMP", "FALSE")) %in% c("TRUE", "1", "SIM", "YES")
ano_base <- as.integer(Sys.getenv("RAIS_ANO_BASE", "1994"))

if (any(is.na(anos))) {
  stop("Anos invalidos.", call. = FALSE)
}

ufs_invalidas <- setdiff(ufs, UFS_RAIS)
if (length(ufs_invalidas) > 0) {
  stop("UF(s) invalida(s): ", paste(ufs_invalidas, collapse = ", "), call. = FALSE)
}

dir.create("intermediate", showWarnings = FALSE, recursive = TRUE)
dir.create("final", showWarnings = FALSE, recursive = TRUE)

encontrar_7z_local <- function(ano, uf, raw_dir = "raw") {
  alvo <- paste0(toupper(uf), ano, ".7z")
  candidatos <- list.files(raw_dir, recursive = TRUE, full.names = TRUE)
  candidatos <- candidatos[tolower(basename(candidatos)) == tolower(alvo)]

  if (length(candidatos) == 0) return(NA_character_)

  caminho_ano <- paste0("/", ano, "/")
  preferido <- grepl(caminho_ano, candidatos, fixed = TRUE)
  candidatos <- candidatos[order(!preferido, nchar(candidatos), candidatos)]
  candidatos[[1]]
}

encontrar_7z_local_nome <- function(ano, nome_arquivo, raw_dir = "raw") {
  candidatos <- list.files(raw_dir, recursive = TRUE, full.names = TRUE)
  candidatos <- candidatos[tolower(basename(candidatos)) == tolower(nome_arquivo)]

  if (length(candidatos) == 0) return(NA_character_)

  caminho_ano <- paste0("/", ano, "/")
  preferido <- grepl(caminho_ano, candidatos, fixed = TRUE)
  candidatos <- candidatos[order(!preferido, nchar(candidatos), candidatos)]
  candidatos[[1]]
}

plano_arquivos_ano <- function(ano, ufs) {
  ufs <- toupper(ufs)

  if (ano <= 2017) {
    return(data.frame(
      ano = ano,
      item = ufs,
      tipo = "uf",
      arquivo = paste0(ufs, ano, ".7z"),
      ufs_filtro = I(as.list(ufs)),
      stringsAsFactors = FALSE
    ))
  }

  linhas <- list()
  for (nome_base in names(ARQUIVOS_REGIONAIS_POS_2017)) {
    ufs_bloco <- ARQUIVOS_REGIONAIS_POS_2017[[nome_base]]
    if (length(ufs_bloco) == 0) next
    ufs_interesse <- intersect(ufs, ufs_bloco)
    if (length(ufs_interesse) == 0) next
    linhas[[length(linhas) + 1]] <- data.frame(
      ano = ano,
      item = nome_base,
      tipo = "regional",
      arquivo = paste0(nome_base, ".7z"),
      ufs_filtro = I(list(ufs_interesse)),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, linhas)
}

extrair_7z_temp <- function(arquivo_7z, temp_root, ano, uf) {
  dir_saida <- file.path(temp_root, "extract", as.character(ano), uf)
  if (dir.exists(dir_saida)) unlink(dir_saida, recursive = TRUE)
  dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)
  archive::archive_extract(arquivo_7z, dir = dir_saida)
  dir_saida
}

arquivos_texto_extraidos <- function(dir_saida) {
  arquivos <- list.files(dir_saida, recursive = TRUE, full.names = TRUE)
  arquivos <- arquivos[grepl("\\.(txt|csv|comt)$", arquivos, ignore.case = TRUE)]
  arquivos <- arquivos[!grepl("ESTB|ESTAB", basename(arquivos), ignore.case = TRUE)]
  sort(arquivos)
}

calcular_percentis_vetor <- function(renda, ano, probs = PERCENTIS_RENDA) {
  renda <- renda[!is.na(renda) & renda > 0]
  if (length(renda) == 0) return(data.frame())

  qs <- stats::quantile(renda, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  names(qs) <- sprintf("p%02.0f", probs * 100)

  out <- as.data.frame(as.list(qs), stringsAsFactors = FALSE)
  out$ano <- as.integer(ano)
  out$n <- length(renda)
  out$media <- mean(renda, na.rm = TRUE)
  out$desvio_padrao <- stats::sd(renda, na.rm = TRUE)
  out$p90_p10 <- out$p90 / out$p10
  out$p90_p50 <- out$p90 / out$p50
  out$p50_p10 <- out$p50 / out$p10
  out$variavel_renda <- renda_var
  out$nivel_observacao <- "vinculo_ano"
  out$cobertura_ufs <- paste(ufs, collapse = ",")
  out[, c(
    "ano", "n", "media", "desvio_padrao", names(qs),
    "p90_p10", "p90_p50", "p50_p10",
    "variavel_renda", "nivel_observacao", "cobertura_ufs"
  )]
}

temp_root <- file.path(tempdir(), paste0("rais_streaming_", Sys.getpid()))
dir.create(temp_root, showWarnings = FALSE, recursive = TRUE)

if (!manter_temp) {
  on.exit(unlink(temp_root, recursive = TRUE), add = TRUE)
}

ano_min <- min(anos)
ano_max <- max(anos)
arquivo_checkpoint <- file.path(
  "intermediate",
  sprintf("painel_percentis_streaming_%s_homens_25_55_%s_%s.csv", renda_var, ano_min, ano_max)
)
arquivo_final <- file.path(
  "final",
  sprintf("painel_percentis_streaming_%s_homens_25_55_%s_%s.csv", renda_var, ano_min, ano_max)
)

if (file.exists(arquivo_checkpoint)) unlink(arquivo_checkpoint)
if (file.exists(arquivo_final)) unlink(arquivo_final)

cat("\nPipeline leve RAIS percentis\n")
cat("  Anos: ", paste(anos, collapse = ", "), "\n", sep = "")
cat("  UFs: ", paste(ufs, collapse = ", "), "\n", sep = "")
cat("  Renda: ", renda_var, "\n", sep = "")
cat("  Baixar ausentes: ", baixar_ausentes, "\n", sep = "")
cat("  Usar cache raw para downloads novos: ", usar_cache_raw, "\n", sep = "")
cat("  Temporarios: ", temp_root, "\n\n", sep = "")

percentis_list <- list()
log_list <- list()

for (ano in anos) {
  cat("\n==== Ano ", ano, " ====\n", sep = "")
  renda_ano <- numeric()
  arquivos_lidos_ano <- 0L
  plano_ano <- plano_arquivos_ano(ano, ufs)

  for (i in seq_len(nrow(plano_ano))) {
    item <- plano_ano[i, ]
    ufs_item <- unlist(item$ufs_filtro)
    label_item <- if (item$tipo == "uf") paste0("UF ", item$item) else paste0("Bloco ", item$item)
    cat(label_item, ": ", sep = "")

    arquivo_7z <- if (item$tipo == "uf") {
      encontrar_7z_local(ano, item$item, raw_dir = raw_dir)
    } else {
      encontrar_7z_local_nome(ano, item$arquivo, raw_dir = raw_dir)
    }
    origem <- "raw_local"

    if (is.na(arquivo_7z)) {
      if (!baixar_ausentes) {
        cat("arquivo ausente; pulando.\n")
        log_list[[length(log_list) + 1]] <- data.frame(
          ano = ano,
          item = item$item,
          tipo = item$tipo,
          ufs = paste(ufs_item, collapse = ","),
          status = "ausente",
          origem = NA_character_,
          arquivo_7z = NA_character_,
          arquivos_lidos = 0L,
          observacoes_renda = 0L,
          stringsAsFactors = FALSE
        )
        next
      }

      origem <- if (usar_cache_raw) "download_raw" else "download_temp"
      destino_download <- if (usar_cache_raw) raw_dir else file.path(temp_root, "download")
      arquivo_7z <- tryCatch(
        if (item$tipo == "uf") {
          baixar_rais_uf(ano, item$item, raw_dir = destino_download, overwrite = FALSE)
        } else {
          baixar_rais_arquivo(ano, item$arquivo, raw_dir = destino_download, overwrite = FALSE)
        },
        error = function(e) {
          warning("Falha ao baixar ", item$arquivo, ": ", conditionMessage(e), call. = FALSE)
          NA_character_
        }
      )
    }

    if (is.na(arquivo_7z) || !file.exists(arquivo_7z)) {
      cat("falhou no download/localizacao.\n")
      log_list[[length(log_list) + 1]] <- data.frame(
        ano = ano,
        item = item$item,
        tipo = item$tipo,
        ufs = paste(ufs_item, collapse = ","),
        status = "falhou_arquivo",
        origem = origem,
        arquivo_7z = arquivo_7z,
        arquivos_lidos = 0L,
        observacoes_renda = 0L,
        stringsAsFactors = FALSE
      )
      next
    }

    dir_extraido <- tryCatch(
      extrair_7z_temp(arquivo_7z, temp_root, ano, item$item),
      error = function(e) {
        warning("Falha ao extrair ", arquivo_7z, ": ", conditionMessage(e), call. = FALSE)
        NA_character_
      }
    )

    if (is.na(dir_extraido)) {
      cat("falhou ao extrair.\n")
      next
    }

    arquivos_txt <- arquivos_texto_extraidos(dir_extraido)
    renda_uf <- numeric()

    for (arquivo_txt in arquivos_txt) {
      dt <- tryCatch(
        ler_tratar_rais_arquivo(
          arquivo_txt,
          ano = ano,
          uf = item$item,
          ufs_filtro = ufs_item
        ),
        error = function(e) {
          warning("Falha ao ler ", arquivo_txt, ": ", conditionMessage(e), call. = FALSE)
          data.table::data.table()
        }
      )

      if (nrow(dt) == 0 || !(renda_var %in% names(dt))) next

      r <- dt[[renda_var]]
      r <- r[!is.na(r) & r > 0]
      renda_uf <- c(renda_uf, r)
      arquivos_lidos_ano <- arquivos_lidos_ano + 1L
    }

    renda_ano <- c(renda_ano, renda_uf)

    if (!manter_temp && dir.exists(dir_extraido)) {
      unlink(dir_extraido, recursive = TRUE)
    }

    if (!usar_cache_raw && origem == "download_temp") {
      dir_download_ano <- file.path(temp_root, "download", as.character(ano))
      if (dir.exists(dir_download_ano)) unlink(dir_download_ano, recursive = TRUE)
    }

    cat(length(renda_uf), " rendas positivas.\n", sep = "")

    log_list[[length(log_list) + 1]] <- data.frame(
      ano = ano,
      item = item$item,
      tipo = item$tipo,
      ufs = paste(ufs_item, collapse = ","),
      status = "ok",
      origem = origem,
      arquivo_7z = arquivo_7z,
      arquivos_lidos = length(arquivos_txt),
      observacoes_renda = length(renda_uf),
      stringsAsFactors = FALSE
    )

    rm(renda_uf)
    gc(verbose = FALSE)
  }

  percentis_ano <- calcular_percentis_vetor(renda_ano, ano = ano)

  if (nrow(percentis_ano) > 0) {
    percentis_list[[length(percentis_list) + 1]] <- percentis_ano
    painel_checkpoint <- data.table::rbindlist(percentis_list, fill = TRUE)
    data.table::setorder(painel_checkpoint, ano)
    data.table::fwrite(painel_checkpoint, arquivo_checkpoint)
    data.table::fwrite(painel_checkpoint, arquivo_final)
    cat("Percentis de ", ano, " salvos. N = ", length(renda_ano), "\n", sep = "")
  } else {
    warning("Sem rendas validas para ", ano, ".", call. = FALSE)
  }

  rm(renda_ano)
  gc(verbose = FALSE)
}

log_download_processamento <- data.table::rbindlist(log_list, fill = TRUE)
arquivo_log <- file.path(
  "intermediate",
  sprintf("log_pipeline_leve_percentis_%s_%s.csv", ano_min, ano_max)
)
data.table::fwrite(log_download_processamento, arquivo_log)

arquivo_base <- NA_character_

if (file.exists(arquivo_final)) {
  painel <- data.table::fread(arquivo_final)

  if (ano_base %in% painel$ano) {
    percentil_cols <- grep("^p[0-9]{2}$", names(painel), value = TRUE)
    base <- painel[painel$ano == ano_base, percentil_cols, drop = FALSE]

    painel_base <- data.table::copy(painel)
    for (col in percentil_cols) {
      painel_base[[paste0(col, "_indice_", ano_base, "_100")]] <- 100 * painel_base[[col]] / base[[col]][[1]]
      painel_base[[paste0(col, "_desvio_log_", ano_base)]] <- log(painel_base[[col]]) - log(base[[col]][[1]])
    }

    arquivo_base <- file.path(
      "final",
      sprintf("painel_percentis_streaming_indices_base_%s_homens_25_55_%s_%s.csv", ano_base, ano_min, ano_max)
    )
    data.table::fwrite(painel_base, arquivo_base)
  }
}

cat("\nArquivos gerados:\n")
if (file.exists(arquivo_checkpoint)) cat("  ", arquivo_checkpoint, "\n", sep = "")
if (file.exists(arquivo_final)) cat("  ", arquivo_final, "\n", sep = "")
if (!is.na(arquivo_base) && file.exists(arquivo_base)) cat("  ", arquivo_base, "\n", sep = "")
cat("  ", arquivo_log, "\n", sep = "")

if (file.exists(arquivo_final)) {
  cat("\nProximo passo para o grafico:\n")
  cat("  Rscript scripts/09_grafico_heathcote_streaming.R ", arquivo_final, "\n", sep = "")
} else {
  cat("\nNenhum painel de percentis foi gerado nesta rodada.\n")
  cat("Verifique o log acima e o arquivo de log para saber quais UFs/anos estavam ausentes ou falharam.\n")
}
