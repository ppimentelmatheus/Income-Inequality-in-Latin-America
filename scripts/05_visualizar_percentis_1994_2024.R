# Gera visualizacoes da evolucao dos percentis de renda.
#
# Uso:
#   Rscript scripts/05_visualizar_percentis_1994_2024.R
#   Rscript scripts/05_visualizar_percentis_1994_2024.R final/painel_percentis_remuneracao_media_sm_homens_25_55_1994_2024.csv

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote data.table para ler o painel de percentis.", call. = FALSE)
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Instale o pacote ggplot2 para gerar os graficos.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) {
  arquivo_percentis <- args[[1]]
} else {
  candidatos <- c(
    list.files(
      "final",
      pattern = "^painel_percentis_.*_homens_25_55_[0-9]{4}_[0-9]{4}\\.csv$",
      full.names = TRUE
    ),
    list.files(
      "intermediate",
      pattern = "^painel_percentis_.*_homens_25_55_[0-9]{4}_[0-9]{4}\\.csv$",
      full.names = TRUE
    )
  )
  if (length(candidatos) == 0) {
    stop(
      "Nenhum painel de percentis encontrado. Rode antes scripts/04_montar_painel_percentis_1994_2024.R.",
      call. = FALSE
    )
  }
  arquivo_percentis <- candidatos[which.max(file.info(candidatos)$mtime)]
}

if (!file.exists(arquivo_percentis)) {
  stop("Arquivo de percentis nao encontrado: ", arquivo_percentis, call. = FALSE)
}

dir.create("figures", showWarnings = FALSE, recursive = TRUE)

percentis <- data.table::fread(arquivo_percentis)
data.table::setorder(percentis, ano)

percentil_cols <- grep("^p[0-9]{2}$", names(percentis), value = TRUE)
percentil_cols <- percentil_cols[percentil_cols %in% c("p01", "p05", "p10", "p20", "p25", "p50", "p75", "p90", "p95", "p99")]

if (length(percentil_cols) == 0) {
  stop("O arquivo nao contem colunas de percentis no padrao p01, p05, ..., p99.", call. = FALSE)
}

for (col in percentil_cols) {
  percentis[, (col) := as.numeric(get(col))]
}

long <- data.table::melt(
  percentis,
  id.vars = c("ano", "n"),
  measure.vars = percentil_cols,
  variable.name = "percentil",
  value.name = "renda_sm"
)

long[, percentil := factor(percentil, levels = percentil_cols)]

ano_min <- min(percentis$ano, na.rm = TRUE)
ano_max <- max(percentis$ano, na.rm = TRUE)
sufixo <- sprintf("%s_%s", ano_min, ano_max)
n_anos <- data.table::uniqueN(percentis$ano)

base <- long[ano == ano_min, list(percentil, base = renda_sm)]
long_index <- merge(long, base, by = "percentil", all.x = TRUE)
long_index[, indice := 100 * renda_sm / base]

percentil_cores <- c(
  p01 = "#5B5B5B",
  p05 = "#7B3294",
  p10 = "#008837",
  p20 = "#5DA5DA",
  p25 = "#4C78A8",
  p50 = "#F58518",
  p75 = "#54A24B",
  p90 = "#E45756",
  p95 = "#B279A2",
  p99 = "#72B7B2"
)

theme_rais <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

p_nivel <- ggplot2::ggplot(
  long,
  ggplot2::aes(x = ano, y = renda_sm, color = percentil, group = percentil)
) +
  {if (n_anos > 1) ggplot2::geom_line(linewidth = 0.9)} +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_color_manual(values = percentil_cores[percentil_cols], drop = FALSE) +
  ggplot2::scale_x_continuous(breaks = sort(unique(percentis$ano))) +
  ggplot2::labs(
    title = "RAIS - Percentis da renda, homens 25-55",
    subtitle = "Renda medida por remuneracao media em salarios minimos",
    x = "Ano",
    y = "Remuneracao media (salarios minimos)",
    color = "Percentil"
  ) +
  theme_rais

ggplot2::ggsave(
  file.path("figures", sprintf("percentis_renda_sm_homens_25_55_%s.png", sufixo)),
  p_nivel,
  width = 9,
  height = 5.5,
  dpi = 160
)

p_indice <- ggplot2::ggplot(
  long_index,
  ggplot2::aes(x = ano, y = indice, color = percentil, group = percentil)
) +
  ggplot2::geom_hline(yintercept = 100, color = "#666666", linewidth = 0.3) +
  {if (n_anos > 1) ggplot2::geom_line(linewidth = 0.9)} +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_color_manual(values = percentil_cores[percentil_cols], drop = FALSE) +
  ggplot2::scale_x_continuous(breaks = sort(unique(percentis$ano))) +
  ggplot2::labs(
    title = "RAIS - Percentis da renda indexados",
    subtitle = paste0("Base ", ano_min, " = 100; homens 25-55"),
    x = "Ano",
    y = paste0("Indice, ", ano_min, " = 100"),
    color = "Percentil"
  ) +
  theme_rais

ggplot2::ggsave(
  file.path("figures", sprintf("percentis_renda_sm_indexado_homens_25_55_%s.png", sufixo)),
  p_indice,
  width = 9,
  height = 5.5,
  dpi = 160
)

razao_cols <- c("p90_p10", "p90_p50", "p50_p10")
razao_cols <- razao_cols[razao_cols %in% names(percentis)]

if (length(razao_cols) > 0) {
  razoes <- data.table::melt(
    percentis,
    id.vars = "ano",
    measure.vars = razao_cols,
    variable.name = "razao",
    value.name = "valor"
  )

  p_razoes <- ggplot2::ggplot(
    razoes,
    ggplot2::aes(x = ano, y = valor, color = razao, group = razao)
  ) +
    {if (n_anos > 1) ggplot2::geom_line(linewidth = 0.9)} +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_color_manual(
      values = c(p90_p10 = "#E45756", p90_p50 = "#4C78A8", p50_p10 = "#F58518"),
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(breaks = sort(unique(percentis$ano))) +
    ggplot2::labs(
      title = "RAIS - Razoes entre percentis",
      subtitle = "Homens 25-55; remuneracao media em salarios minimos",
      x = "Ano",
      y = "Razao",
      color = "Razao"
    ) +
    theme_rais

  ggplot2::ggsave(
    file.path("figures", sprintf("razoes_percentis_renda_sm_homens_25_55_%s.png", sufixo)),
    p_razoes,
    width = 9,
    height = 5.5,
    dpi = 160
  )
}

cat("\nFiguras salvas em:\n")
cat("  ", file.path("figures", sprintf("percentis_renda_sm_homens_25_55_%s.png", sufixo)), "\n", sep = "")
cat("  ", file.path("figures", sprintf("percentis_renda_sm_indexado_homens_25_55_%s.png", sufixo)), "\n", sep = "")
if (length(razao_cols) > 0) {
  cat("  ", file.path("figures", sprintf("razoes_percentis_renda_sm_homens_25_55_%s.png", sufixo)), "\n", sep = "")
}
