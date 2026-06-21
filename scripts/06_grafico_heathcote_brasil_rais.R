# Reproduz o grafico de evolucao dos percentis da renda no estilo
# Heathcote et al. (2020), usando RAIS Brasil e base 1994.
#
# O grafico original usa desvios log em relacao ao ano-base. Este script
# tambem salva uma versao indexada com 1994 = 100.
#
# Uso:
#   Rscript scripts/06_grafico_heathcote_brasil_rais.R
#   Rscript scripts/06_grafico_heathcote_brasil_rais.R final/painel_percentis_remuneracao_media_sm_homens_25_55_1994_2024.csv

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote data.table para ler o painel de percentis.", call. = FALSE)
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Instale o pacote ggplot2 para gerar os graficos.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
ano_base <- as.integer(Sys.getenv("RAIS_ANO_BASE", "1994"))

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
dir.create("final", showWarnings = FALSE, recursive = TRUE)

percentis <- data.table::fread(arquivo_percentis)
data.table::setorder(percentis, ano)

if (!(ano_base %in% percentis$ano)) {
  stop("O ano-base ", ano_base, " nao aparece no painel de percentis.", call. = FALSE)
}

percentis_plot <- c("p20", "p50", "p90", "p95")
percentis_extra <- c("p10", "p25", "p75", "p99")
percentis_disponiveis <- grep("^p[0-9]{2}$", names(percentis), value = TRUE)
percentis_usar <- c(
  percentis_plot[percentis_plot %in% percentis_disponiveis],
  percentis_extra[percentis_extra %in% percentis_disponiveis]
)

if (length(percentis_usar) == 0) {
  stop("O painel nao contem colunas de percentis no padrao p20, p50, p90, p95 etc.", call. = FALSE)
}

for (col in percentis_usar) {
  percentis[, (col) := as.numeric(get(col))]
}

long <- data.table::melt(
  percentis,
  id.vars = c("ano", "n"),
  measure.vars = percentis_usar,
  variable.name = "percentil",
  value.name = "renda_sm"
)

base <- long[
  ano == ano_base,
  .(percentil, renda_base = renda_sm)
]

long <- merge(long, base, by = "percentil", all.x = TRUE)
long <- long[!is.na(renda_base) & renda_base > 0 & !is.na(renda_sm) & renda_sm > 0]
long[, indice_1994_100 := 100 * renda_sm / renda_base]
long[, desvio_log_1994 := log(renda_sm) - log(renda_base)]
long[, destaque := percentil %in% percentis_plot]

ordem <- c("p10", "p20", "p25", "p50", "p75", "p90", "p95", "p99")
ordem <- ordem[ordem %in% unique(long$percentil)]
long[, percentil := factor(percentil, levels = ordem)]

rotulos <- c(
  p10 = "P10",
  p20 = "P20",
  p25 = "P25",
  p50 = "P50",
  p75 = "P75",
  p90 = "P90",
  p95 = "P95",
  p99 = "P99"
)

cores <- c(
  p10 = "#008837",
  p20 = "#5DA5DA",
  p25 = "#4477AA",
  p50 = "#FFB000",
  p75 = "#59A14F",
  p90 = "#D55E00",
  p95 = "#666666",
  p99 = "#222222"
)

theme_heathcote <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    plot.title.position = "plot",
    legend.position = "none",
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

anos <- sort(unique(long$ano))
ano_min <- min(anos)
ano_max <- max(anos)
sufixo <- sprintf("%s_%s", ano_min, ano_max)
n_anos <- data.table::uniqueN(long$ano)

last_points <- long[
  ,
  .SD[which.max(ano)],
  by = percentil
]
last_points[, label := rotulos[as.character(percentil)]]

p_log <- ggplot2::ggplot(
  long,
  ggplot2::aes(x = ano, y = desvio_log_1994, color = percentil, group = percentil)
) +
  ggplot2::geom_hline(yintercept = 0, color = "#666666", linewidth = 0.3) +
  {if (n_anos > 1) ggplot2::geom_line(ggplot2::aes(linewidth = destaque), alpha = 0.95)} +
  ggplot2::geom_point(ggplot2::aes(size = destaque), alpha = 0.95) +
  ggplot2::geom_text(
    data = last_points[destaque == TRUE],
    ggplot2::aes(label = label),
    hjust = -0.10,
    vjust = 0.4,
    show.legend = FALSE,
    size = 4
  ) +
  ggplot2::scale_color_manual(values = cores[ordem], labels = rotulos[ordem], drop = FALSE) +
  ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.0, `FALSE` = 0.55), guide = "none") +
  ggplot2::scale_size_manual(values = c(`TRUE` = 2.2, `FALSE` = 1.4), guide = "none") +
  ggplot2::scale_x_continuous(breaks = anos, expand = ggplot2::expansion(mult = c(0.01, 0.08))) +
  ggplot2::labs(
    title = "Percentis da distribuicao de renda do trabalho - Brasil, RAIS",
    subtitle = paste0("Homens 25-55; desvios log em relacao a ", ano_base),
    x = NULL,
    y = paste0("Desvio log de ", ano_base)
  ) +
  theme_heathcote

arquivo_log <- file.path(
  "figures",
  sprintf("heathcote_brasil_rais_desvio_log_%s_homens_25_55.png", sufixo)
)

ggplot2::ggsave(
  arquivo_log,
  p_log,
  width = 9,
  height = 6,
  dpi = 180
)

p_indice <- ggplot2::ggplot(
  long,
  ggplot2::aes(x = ano, y = indice_1994_100, color = percentil, group = percentil)
) +
  ggplot2::geom_hline(yintercept = 100, color = "#666666", linewidth = 0.3) +
  {if (n_anos > 1) ggplot2::geom_line(ggplot2::aes(linewidth = destaque), alpha = 0.95)} +
  ggplot2::geom_point(ggplot2::aes(size = destaque), alpha = 0.95) +
  ggplot2::geom_text(
    data = last_points[destaque == TRUE],
    ggplot2::aes(y = indice_1994_100, label = label),
    hjust = -0.10,
    vjust = 0.4,
    show.legend = FALSE,
    size = 4
  ) +
  ggplot2::scale_color_manual(values = cores[ordem], labels = rotulos[ordem], drop = FALSE) +
  ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.0, `FALSE` = 0.55), guide = "none") +
  ggplot2::scale_size_manual(values = c(`TRUE` = 2.2, `FALSE` = 1.4), guide = "none") +
  ggplot2::scale_x_continuous(breaks = anos, expand = ggplot2::expansion(mult = c(0.01, 0.08))) +
  ggplot2::labs(
    title = "Percentis da distribuicao de renda do trabalho - Brasil, RAIS",
    subtitle = paste0("Homens 25-55; indice ", ano_base, " = 100"),
    x = NULL,
    y = paste0("Indice, ", ano_base, " = 100")
  ) +
  theme_heathcote

arquivo_indice <- file.path(
  "figures",
  sprintf("heathcote_brasil_rais_indice_%s_100_homens_25_55.png", sufixo)
)

ggplot2::ggsave(
  arquivo_indice,
  p_indice,
  width = 9,
  height = 6,
  dpi = 180
)

arquivo_base <- file.path(
  "final",
  sprintf("percentis_renda_base_%s_100_homens_25_55_%s.csv", ano_base, sufixo)
)

data.table::fwrite(
  long[
    ,
    .(
      ano,
      percentil = as.character(percentil),
      renda_sm,
      renda_base,
      indice_1994_100,
      desvio_log_1994,
      n
    )
  ],
  arquivo_base
)

cat("\nArquivos gerados:\n")
cat("  ", arquivo_log, "\n", sep = "")
cat("  ", arquivo_indice, "\n", sep = "")
cat("  ", arquivo_base, "\n", sep = "")

if (ano_min == ano_max) {
  cat("\nObservacao: o painel atual tem apenas ", ano_min, ".\n", sep = "")
  cat("Quando os demais anos forem processados, este mesmo script desenhara as series temporais.\n")
}
