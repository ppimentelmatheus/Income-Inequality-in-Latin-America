# Gera o grafico de percentis no estilo Heathcote a partir do painel
# produzido pelo pipeline leve:
#   scripts/08_pipeline_leve_percentis_streaming.R
#
# Uso:
#   Rscript scripts/09_grafico_heathcote_streaming.R
#   Rscript scripts/09_grafico_heathcote_streaming.R final/painel_percentis_streaming_remuneracao_media_sm_homens_25_55_1994_2024.csv

if (basename(getwd()) == "scripts" && file.exists("../scripts/09_grafico_heathcote_streaming.R")) {
  setwd("..")
}

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote data.table.", call. = FALSE)
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Instale o pacote ggplot2.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
ano_base <- as.integer(Sys.getenv("RAIS_ANO_BASE", "1994"))

if (length(args) >= 1) {
  arquivo_percentis <- args[[1]]
} else {
  candidatos <- list.files(
    "final",
    pattern = "^painel_percentis_streaming_.*_homens_25_55_[0-9]{4}_[0-9]{4}\\.csv$",
    full.names = TRUE
  )
  if (length(candidatos) == 0) {
    stop("Nenhum painel streaming encontrado em final/.", call. = FALSE)
  }
  arquivo_percentis <- candidatos[which.max(file.info(candidatos)$mtime)]
}

if (!file.exists(arquivo_percentis)) {
  stop("Arquivo nao encontrado: ", arquivo_percentis, call. = FALSE)
}

dir.create("figures", showWarnings = FALSE, recursive = TRUE)

dt <- data.table::fread(arquivo_percentis)
data.table::setorder(dt, ano)

if (!(ano_base %in% dt$ano)) {
  stop("Ano-base ", ano_base, " nao aparece no painel.", call. = FALSE)
}

percentis_foco <- c("p20", "p50", "p90", "p95")
percentis_extra <- c("p10", "p25", "p75", "p99")
percentis_disponiveis <- grep("^p[0-9]{2}$", names(dt), value = TRUE)
percentis_usar <- c(
  percentis_foco[percentis_foco %in% percentis_disponiveis],
  percentis_extra[percentis_extra %in% percentis_disponiveis]
)

if (length(percentis_usar) == 0) {
  stop("Nenhuma coluna de percentil encontrada.", call. = FALSE)
}

for (col in percentis_usar) {
  dt[, (col) := as.numeric(get(col))]
}

long <- data.table::melt(
  dt,
  id.vars = c("ano", "n"),
  measure.vars = percentis_usar,
  variable.name = "percentil",
  value.name = "renda_sm"
)

base <- long[ano == ano_base, .(percentil, renda_base = renda_sm)]
long <- merge(long, base, by = "percentil", all.x = TRUE)
long <- long[!is.na(renda_sm) & renda_sm > 0 & !is.na(renda_base) & renda_base > 0]
long[, indice_base_100 := 100 * renda_sm / renda_base]
long[, desvio_log_base := log(renda_sm) - log(renda_base)]
long[, destaque := percentil %in% percentis_foco]

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
periodo <- sprintf("%s-%s", ano_min, ano_max)
ano_cobertura_parcial <- as.integer(Sys.getenv("RAIS_PARTIAL_COVERAGE_FROM", "2018"))
tem_cobertura_parcial <- any(anos >= ano_cobertura_parcial)
x_cobertura_parcial <- max(ano_cobertura_parcial, ano_min)

caption_log <- paste0(
  "Average remuneration in multiples of the minimum wage; log deviations relative to ",
  ano_base,
  ". Declines indicate a reduction in the number of minimum wages, not necessarily a fall in real earnings.",
  if (tem_cobertura_parcial) " Shaded area denotes partial RAIS coverage from 2018 onward." else ""
)

caption_indice <- paste0(
  "Average remuneration in multiples of the minimum wage; index ",
  ano_base,
  " = 100. Declines indicate a reduction in the number of minimum wages, not necessarily a fall in real earnings.",
  if (tem_cobertura_parcial) " Shaded area denotes partial RAIS coverage from 2018 onward." else ""
)

last_points <- long[, .SD[which.max(ano)], by = percentil]
last_points[, label := rotulos[as.character(percentil)]]

p_log <- ggplot2::ggplot(
  long,
  ggplot2::aes(x = ano, y = desvio_log_base, color = percentil, group = percentil)
) +
  ggplot2::geom_hline(yintercept = 0, color = "#666666", linewidth = 0.3) +
  {if (tem_cobertura_parcial) ggplot2::annotate(
    "rect",
    xmin = ano_cobertura_parcial - 0.5,
    xmax = ano_max + 0.5,
    ymin = -Inf,
    ymax = Inf,
    fill = "#D9D9D9",
    alpha = 0.35
  )} +
  {if (n_anos > 1) ggplot2::geom_line(ggplot2::aes(linewidth = destaque), alpha = 0.95)} +
  ggplot2::geom_point(ggplot2::aes(size = destaque), alpha = 0.95) +
  {if (tem_cobertura_parcial) ggplot2::annotate(
    "text",
    x = x_cobertura_parcial,
    y = max(long$desvio_log_base, na.rm = TRUE),
    label = "Partial data from 2018",
    hjust = 0,
    vjust = 1.2,
    color = "#666666",
    size = 3.2
  )} +
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
    title = paste0("Percentiles of Brazilian formal labor earnings, ", periodo),
    subtitle = paste0("Men aged 25-55; log deviations relative to ", ano_base),
    x = NULL,
    y = paste0("Log deviation from ", ano_base),
    caption = caption_log
  ) +
  theme_heathcote

p_indice <- ggplot2::ggplot(
  long,
  ggplot2::aes(x = ano, y = indice_base_100, color = percentil, group = percentil)
) +
  ggplot2::geom_hline(yintercept = 100, color = "#666666", linewidth = 0.3) +
  {if (tem_cobertura_parcial) ggplot2::annotate(
    "rect",
    xmin = ano_cobertura_parcial - 0.5,
    xmax = ano_max + 0.5,
    ymin = -Inf,
    ymax = Inf,
    fill = "#D9D9D9",
    alpha = 0.35
  )} +
  {if (n_anos > 1) ggplot2::geom_line(ggplot2::aes(linewidth = destaque), alpha = 0.95)} +
  ggplot2::geom_point(ggplot2::aes(size = destaque), alpha = 0.95) +
  {if (tem_cobertura_parcial) ggplot2::annotate(
    "text",
    x = x_cobertura_parcial,
    y = max(long$indice_base_100, na.rm = TRUE),
    label = "Partial data from 2018",
    hjust = 0,
    vjust = 1.2,
    color = "#666666",
    size = 3.2
  )} +
  ggplot2::geom_text(
    data = last_points[destaque == TRUE],
    ggplot2::aes(y = indice_base_100, label = label),
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
    title = paste0("Percentiles of Brazilian formal labor earnings, ", periodo),
    subtitle = paste0("Men aged 25-55; index ", ano_base, " = 100"),
    x = NULL,
    y = paste0("Index, ", ano_base, " = 100"),
    caption = caption_indice
  ) +
  theme_heathcote

arquivo_log <- file.path(
  "figures",
  sprintf("heathcote_streaming_desvio_log_%s_homens_25_55.png", sufixo)
)
arquivo_indice <- file.path(
  "figures",
  sprintf("heathcote_streaming_indice_%s_100_homens_25_55.png", sufixo)
)

ggplot2::ggsave(arquivo_log, p_log, width = 9, height = 6, dpi = 180)
ggplot2::ggsave(arquivo_indice, p_indice, width = 9, height = 6, dpi = 180)

cat("\nFiguras geradas:\n")
cat("  ", arquivo_log, "\n", sep = "")
cat("  ", arquivo_indice, "\n", sep = "")
