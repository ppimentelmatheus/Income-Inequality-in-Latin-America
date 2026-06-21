# Funcoes gerais para baixar, extrair, ler e tratar microdados da RAIS.

UFS_RAIS <- c(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
  "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
  "RO", "RR", "RS", "SC", "SE", "SP", "TO"
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

alvos_rais <- list(
  pis = c("^pis$", "pis_pasep", "pasep", "identificador.*trabalhador"),
  sexo = c("^sexo_trabalhador$", "^sexo$", "genero"),
  idade = c("^idade$", "idade_trabalhador"),
  natureza_juridica = c("^natureza_juridica$", "natureza.*juridica"),
  remuneracao_media_sm = c(
    "^vl_remun_media_sm$",
    "remun.*media.*sm",
    "remun.*media.*salario"
  ),
  remuneracao_media_nom = c(
    "^vl_remun_media_nom$",
    "remun.*media.*nom",
    "remun.*media.*reais",
    "remuneracao.*media.*r"
  ),
  remuneracao_dezembro_sm = c(
    "^vl_remun_dezembro_sm$",
    "remun.*dezembro.*sm",
    "remun.*dez.*salario"
  ),
  remuneracao_dezembro_nom = c(
    "^vl_remun_dezembro_nom$",
    "remun.*dezembro.*nom",
    "remun.*dez.*reais"
  ),
  qtd_horas = c("^qtd_hora_contr$", "qtd.*hora.*contr"),
  cbo_94 = c("^cbo_94_ocupacao$"),
  cbo_2002 = c("^cbo_ocupacao_2002$", "^cbo_2002", "cbo.*2002"),
  cnae_95 = c("^cnae_95_classe$"),
  cnae_20 = c("^cnae_2_0_classe$", "^cnae_20_classe$", "cnae.*2.*0.*classe"),
  municipio = c("^municipio$", "id_municipio"),
  vinculo_ativo = c("^vinculo_ativo_31_12$", "ativo.*31.*12"),
  motivo_desligamento = c("^motivo_desligamento$", "causa.*deslig", "motivo.*deslig"),
  mes_admissao = c("^mes_admissao$"),
  mes_desligamento = c("^mes_desligamento$"),
  grau_instrucao = c("^grau_instrucao", "instrucao"),
  tamanho_estabelecimento = c("^tamanho_estabelecimento$", "tamanho.*estab"),
  tempo_emprego = c("^tempo_emprego$"),
  tipo_vinculo = c("^tipo_vinculo$"),
  tipo_estab = c("^tipo_estab$")
)

mapear_colunas_rais <- function(path) {
  header <- ler_cabecalho_rais(path)
  normalized <- normalizar_nome(header)
  pos <- vapply(alvos_rais, find_first, integer(1), normalized_names = normalized)

  tipo_estab_pos <- which(normalized == "tipo_estab")
  if (length(tipo_estab_pos) >= 1) pos[["tipo_estab_codigo"]] <- tipo_estab_pos[[1]]
  if (length(tipo_estab_pos) >= 2) pos[["tipo_estab_id"]] <- tipo_estab_pos[[2]]
  pos <- pos[names(pos) != "tipo_estab"]

  data.frame(
    variavel_df = names(pos),
    status = ifelse(is.na(pos), "AUSENTE", "OK"),
    posicao = ifelse(is.na(pos), NA_integer_, as.integer(pos)),
    coluna_origem = ifelse(is.na(pos), NA_character_, header[as.integer(pos)]),
    stringsAsFactors = FALSE
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

first_not_na <- function(x) {
  y <- x[!is.na(x) & x != ""]
  if (length(y) == 0) return(NA)
  y[[1]]
}

sum_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

max_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

inferir_ano_rais <- function(path) {
  hits <- regmatches(path, gregexpr("(19|20)[0-9]{2}", path, perl = TRUE))[[1]]
  if (length(hits) == 0) return(NA_integer_)
  as.integer(tail(hits, 1))
}

inferir_uf_rais <- function(path) {
  candidatos <- toupper(c(
    basename(path),
    basename(dirname(path)),
    basename(dirname(dirname(path)))
  ))

  for (uf in UFS_RAIS) {
    inicio <- paste0("^", uf, "((19|20)[0-9]{2})?")
    isolado <- paste0("(^|[^A-Z])", uf, "([^A-Z]|$)")
    if (any(grepl(inicio, candidatos, perl = TRUE)) ||
        any(grepl(isolado, candidatos, perl = TRUE))) {
      return(uf)
    }
  }

  NA_character_
}

descobrir_arquivos_rais <- function(ano, raw_dir = "raw", ufs = UFS_RAIS) {
  arquivos <- list.files(raw_dir, recursive = TRUE, full.names = TRUE)
  arquivos <- arquivos[grepl("\\.(txt|csv|comt)$", arquivos, ignore.case = TRUE)]
  arquivos <- arquivos[grepl(as.character(ano), arquivos, fixed = TRUE)]
  arquivos <- arquivos[!grepl("ESTB|ESTAB", basename(arquivos), ignore.case = TRUE)]

  if (!is.null(ufs)) {
    ufs <- toupper(ufs)
    arquivos <- arquivos[vapply(arquivos, function(x) {
      uf <- inferir_uf_rais(x)
      is.na(uf) || uf %in% ufs
    }, logical(1))]
  }

  arquivos <- sort(unique(arquivos))

  if (length(arquivos) > 1) {
    chave <- tolower(basename(arquivos))
    caminho_ano <- paste0("/", ano, "/")
    preferido <- grepl(caminho_ano, arquivos, fixed = TRUE)
    ord <- order(chave, !preferido, arquivos)
    arquivos <- arquivos[ord]
    arquivos <- arquivos[!duplicated(chave[ord])]
  }

  arquivos
}

baixar_rais_uf <- function(ano, uf, raw_dir = "raw", overwrite = FALSE) {
  uf <- toupper(uf)
  dir_ano <- file.path(raw_dir, as.character(ano))
  dir.create(dir_ano, showWarnings = FALSE, recursive = TRUE)

  destino <- file.path(dir_ano, paste0(uf, ano, ".7z"))
  if (file.exists(destino) && !overwrite) return(destino)

  url <- sprintf(
    "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/%s/%s%s.7z",
    ano,
    uf,
    ano
  )

  utils::download.file(url, destino, mode = "wb", quiet = FALSE)
  destino
}

extrair_arquivos_rais <- function(ano, raw_dir = "raw", ufs = UFS_RAIS, overwrite = FALSE) {
  if (!requireNamespace("archive", quietly = TRUE)) {
    stop("Instale o pacote archive para extrair arquivos .7z.", call. = FALSE)
  }

  arquivos_7z <- list.files(raw_dir, recursive = TRUE, full.names = TRUE)
  arquivos_7z <- arquivos_7z[grepl("\\.7z$", arquivos_7z, ignore.case = TRUE)]
  arquivos_7z <- arquivos_7z[grepl(as.character(ano), arquivos_7z, fixed = TRUE)]
  arquivos_7z <- arquivos_7z[!grepl("ESTB|ESTAB", basename(arquivos_7z), ignore.case = TRUE)]

  if (!is.null(ufs)) {
    ufs <- toupper(ufs)
    arquivos_7z <- arquivos_7z[vapply(arquivos_7z, function(x) {
      uf <- inferir_uf_rais(x)
      is.na(uf) || uf %in% ufs
    }, logical(1))]
  }

  saidas <- character()
  for (arquivo_7z in arquivos_7z) {
    dir_saida <- tools::file_path_sans_ext(arquivo_7z)
    ja_extraido <- dir.exists(dir_saida) && length(list.files(dir_saida)) > 0
    if (ja_extraido && !overwrite) {
      saidas <- c(saidas, dir_saida)
      next
    }
    dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)
    archive::archive_extract(arquivo_7z, dir = dir_saida)
    saidas <- c(saidas, dir_saida)
  }

  unique(saidas)
}

ler_tratar_rais_arquivo <- function(path, ano = NULL, uf = NULL) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Instale o pacote data.table para ler a RAIS.", call. = FALSE)
  }

  if (is.null(ano) || is.na(ano)) ano <- inferir_ano_rais(path)
  if (is.null(uf) || is.na(uf)) uf <- inferir_uf_rais(path)

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
  encoding <- detectar_encoding(path)
  raw <- data.table::fread(
    path,
    sep = ";",
    header = TRUE,
    encoding = encoding,
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
  remuneracao_media_sm <- to_num_br(col_or_na("remuneracao_media_sm"))
  remuneracao_media_nom <- to_num_br(col_or_na("remuneracao_media_nom"))
  remuneracao_dezembro_sm <- to_num_br(col_or_na("remuneracao_dezembro_sm"))
  remuneracao_dezembro_nom <- to_num_br(col_or_na("remuneracao_dezembro_nom"))

  out <- data.table::data.table(
    ano = as.integer(ano),
    uf = uf,
    pis = to_code(col_or_na("pis")),
    sexo_codigo = sexo_codigo,
    sexo = ifelse(sexo_codigo == "01", "Homem", ifelse(sexo_codigo == "02", "Mulher", NA_character_)),
    idade = idade,
    natureza_juridica_codigo = to_digit_code(col_or_na("natureza_juridica")),
    remuneracao_media_sm = remuneracao_media_sm,
    remuneracao_media_nom = remuneracao_media_nom,
    remuneracao_dezembro_sm = remuneracao_dezembro_sm,
    remuneracao_dezembro_nom = remuneracao_dezembro_nom,
    renda_valida_sm = remuneracao_media_sm > 0,
    qtd_horas_contratadas = to_int(col_or_na("qtd_horas")),
    cbo_94 = to_code(col_or_na("cbo_94")),
    cbo_2002 = to_code(col_or_na("cbo_2002")),
    cnae_95_classe = to_code(col_or_na("cnae_95")),
    cnae_20_classe = to_code(col_or_na("cnae_20")),
    municipio = to_code(col_or_na("municipio")),
    vinculo_ativo_31_12 = to_digit_code(col_or_na("vinculo_ativo")),
    motivo_desligamento = to_digit_code(col_or_na("motivo_desligamento")),
    mes_admissao = to_digit_code(col_or_na("mes_admissao")),
    mes_desligamento = to_digit_code(col_or_na("mes_desligamento")),
    grau_instrucao = to_digit_code(col_or_na("grau_instrucao")),
    tamanho_estabelecimento = to_digit_code(col_or_na("tamanho_estabelecimento")),
    tempo_emprego = to_num_br(col_or_na("tempo_emprego")),
    tipo_vinculo = to_digit_code(col_or_na("tipo_vinculo")),
    tipo_estab_codigo = to_digit_code(col_or_na("tipo_estab_codigo")),
    tipo_estab_id = to_code(col_or_na("tipo_estab_id")),
    arquivo_origem = path
  )

  out <- out[
    sexo_codigo == "01" &
      !is.na(idade) &
      idade >= 25 &
      idade <= 55
  ]

  out[, log_remuneracao_media_sm := ifelse(remuneracao_media_sm > 0, log(remuneracao_media_sm), NA_real_)]
  out[]
}

agregar_trabalhador_ano <- function(dt) {
  if (nrow(dt) == 0) return(dt)

  if (!("pis" %in% names(dt)) || !any(!is.na(dt$pis))) {
    dt[, nivel_observacao := "vinculo_ano"]
    dt[, n_vinculos := 1L]
    return(dt[])
  }

  sem_pis <- dt[is.na(pis)]
  com_pis <- dt[!is.na(pis)]

  agg <- com_pis[
    ,
    list(
      uf = paste(sort(unique(uf[!is.na(uf)])), collapse = "|"),
      sexo_codigo = first_not_na(sexo_codigo),
      sexo = first_not_na(sexo),
      idade = first_not_na(idade),
      natureza_juridica_codigo = first_not_na(natureza_juridica_codigo),
      remuneracao_media_sm = sum_or_na(remuneracao_media_sm),
      remuneracao_media_nom = sum_or_na(remuneracao_media_nom),
      remuneracao_dezembro_sm = sum_or_na(remuneracao_dezembro_sm),
      remuneracao_dezembro_nom = sum_or_na(remuneracao_dezembro_nom),
      qtd_horas_contratadas = sum_or_na(qtd_horas_contratadas),
      municipio = first_not_na(municipio),
      vinculo_ativo_31_12 = first_not_na(vinculo_ativo_31_12),
      grau_instrucao = first_not_na(grau_instrucao),
      tempo_emprego = max_or_na(tempo_emprego),
      n_vinculos = .N,
      arquivo_origem = paste(sort(unique(arquivo_origem)), collapse = "|")
    ),
    by = c("ano", "pis")
  ]

  agg[, renda_valida_sm := remuneracao_media_sm > 0]
  agg[, log_remuneracao_media_sm := ifelse(remuneracao_media_sm > 0, log(remuneracao_media_sm), NA_real_)]
  agg[, nivel_observacao := "trabalhador_ano"]

  if (nrow(sem_pis) > 0) {
    sem_pis[, nivel_observacao := "vinculo_ano_sem_pis"]
    sem_pis[, n_vinculos := 1L]
    return(data.table::rbindlist(list(agg, sem_pis), fill = TRUE))
  }

  agg[]
}

processar_rais_ano <- function(
  ano,
  raw_dir = "raw",
  output_dir = "intermediate/anos",
  ufs = UFS_RAIS,
  baixar = FALSE,
  extrair = TRUE,
  agregar_trabalhador = TRUE,
  salvar_csv = FALSE
) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  if (baixar) {
    for (uf in ufs) {
      message("Baixando ", uf, ano, ".7z")
      tryCatch(
        baixar_rais_uf(ano, uf, raw_dir = raw_dir),
        error = function(e) warning("Falha no download de ", uf, ano, ": ", conditionMessage(e), call. = FALSE)
      )
    }
  }

  if (extrair) {
    tryCatch(
      extrair_arquivos_rais(ano, raw_dir = raw_dir, ufs = ufs),
      error = function(e) warning("Falha ao extrair arquivos de ", ano, ": ", conditionMessage(e), call. = FALSE)
    )
  }

  arquivos <- descobrir_arquivos_rais(ano, raw_dir = raw_dir, ufs = ufs)
  if (length(arquivos) == 0) {
    warning("Nenhum arquivo TXT/CSV/COMT encontrado para ", ano, ".", call. = FALSE)
    return(data.frame(
      ano = ano,
      arquivos_lidos = 0L,
      observacoes = 0L,
      renda_positiva = 0L,
      nivel_observacao = NA_character_,
      arquivo_saida = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  lidos <- list()
  for (arquivo in arquivos) {
    message("Lendo ", arquivo)
    dt <- tryCatch(
      ler_tratar_rais_arquivo(arquivo, ano = ano),
      error = function(e) {
        warning("Falha ao ler ", arquivo, ": ", conditionMessage(e), call. = FALSE)
        data.table::data.table()
      }
    )
    if (nrow(dt) > 0) lidos[[length(lidos) + 1]] <- dt
  }

  if (length(lidos) == 0) {
    warning("Nenhum dado valido para homens 25-55 em ", ano, ".", call. = FALSE)
    return(data.frame(
      ano = ano,
      arquivos_lidos = length(arquivos),
      observacoes = 0L,
      renda_positiva = 0L,
      nivel_observacao = NA_character_,
      arquivo_saida = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  dt_ano <- data.table::rbindlist(lidos, fill = TRUE)
  if (agregar_trabalhador) dt_ano <- agregar_trabalhador_ano(dt_ano)

  arquivo_saida <- file.path(output_dir, sprintf("rais_%s_homens_25_55.rds", ano))
  saveRDS(dt_ano, arquivo_saida)

  if (salvar_csv) {
    data.table::fwrite(
      dt_ano,
      file = file.path(output_dir, sprintf("rais_%s_homens_25_55.csv.gz", ano))
    )
  }

  data.frame(
    ano = ano,
    arquivos_lidos = length(arquivos),
    observacoes = nrow(dt_ano),
    renda_positiva = sum(dt_ano$remuneracao_media_sm > 0, na.rm = TRUE),
    nivel_observacao = paste(sort(unique(dt_ano$nivel_observacao)), collapse = "|"),
    arquivo_saida = arquivo_saida,
    stringsAsFactors = FALSE
  )
}

calcular_percentis_renda <- function(dt, renda_var = "remuneracao_media_sm", probs = PERCENTIS_RENDA) {
  if (nrow(dt) == 0 || !(renda_var %in% names(dt))) {
    return(data.frame())
  }

  renda <- dt[[renda_var]]
  renda <- renda[!is.na(renda) & renda > 0]
  if (length(renda) == 0) return(data.frame())

  qs <- stats::quantile(renda, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  names(qs) <- sprintf("p%02.0f", probs * 100)

  out <- as.data.frame(as.list(qs), stringsAsFactors = FALSE)
  out$ano <- unique(dt$ano)[[1]]
  out$n <- length(renda)
  out$media <- mean(renda, na.rm = TRUE)
  out$desvio_padrao <- stats::sd(renda, na.rm = TRUE)
  out$p90_p10 <- out$p90 / out$p10
  out$p90_p50 <- out$p90 / out$p50
  out$p50_p10 <- out$p50 / out$p10
  out <- out[, c("ano", "n", "media", "desvio_padrao", names(qs), "p90_p10", "p90_p50", "p50_p10")]
  out
}
