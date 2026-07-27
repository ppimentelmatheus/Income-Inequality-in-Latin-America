library(data.table)
library(ggplot2)

if (basename(getwd()) == "scripts" && file.exists("../scripts/11_graphs.R")) {
  setwd("..")
}

arquivo_painel <- "final/painel_percentis_streaming_remuneracao_media_sm_homens_25_55_1994_2024.csv"

if (!file.exists(arquivo_painel)) {
  stop("Percentile panel not found: ", arquivo_painel, call. = FALSE)
}

dir.create("figures", showWarnings = FALSE, recursive = TRUE)

painel <- fread(arquivo_painel)
setorder(painel, ano)

required_cols <- c("ano", "p20", "p50", "p90")
missing_cols <- setdiff(required_cols, names(painel))

if (length(missing_cols) > 0) {
  stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

for (col in intersect(c("p20", "p50", "p90", "p95"), names(painel))) {
  painel[, (col) := as.numeric(get(col))]
}

painel[
  ,
  `:=`(
    P50_P20 = p50 / p20,
    P90_P50 = p90 / p50
  )
]

if ("p95" %in% names(painel)) {
  painel[, P95_P50 := p95 / p50]
}

ano_min <- min(painel$ano, na.rm = TRUE)
ano_max <- max(painel$ano, na.rm = TRUE)
periodo <- sprintf("%s-%s", ano_min, ano_max)
sufixo <- sprintf("%s_%s", ano_min, ano_max)
ano_cobertura_parcial <- as.integer(Sys.getenv("RAIS_PARTIAL_COVERAGE_FROM", "2018"))
tem_cobertura_parcial <- any(painel$ano >= ano_cobertura_parcial)
x_cobertura_parcial <- max(ano_cobertura_parcial, ano_min)

caption_ratio <- paste0(
  "Men aged 25-55; average remuneration in multiples of the minimum wage.",
  if (tem_cobertura_parcial) " Shaded area denotes partial RAIS coverage from 2018 onward." else ""
)

theme_ratio <- theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "bottom"
  )

plot_ratio <- function(y_var, y_label, color, title, subtitle) {
  dt_plot <- painel[, .(ano, valor = get(y_var))]
  y_max <- max(dt_plot$valor, na.rm = TRUE)

  ggplot(dt_plot, aes(x = ano, y = valor)) +
    {if (tem_cobertura_parcial) annotate(
      "rect",
      xmin = ano_cobertura_parcial - 0.5,
      xmax = ano_max + 0.5,
      ymin = -Inf,
      ymax = Inf,
      fill = "#D9D9D9",
      alpha = 0.35
    )} +
    geom_line(linewidth = 1.15, color = color) +
    geom_point(size = 2.1, color = color) +
    {if (tem_cobertura_parcial) annotate(
      "text",
      x = x_cobertura_parcial,
      y = y_max,
      label = "Partial data from 2018",
      hjust = 0,
      vjust = 1.2,
      color = "#666666",
      size = 3.3
    )} +
    annotate(
      "text",
      x = ano_max,
      y = dt_plot[ano == ano_max, valor][1],
      label = y_label,
      hjust = -0.08,
      vjust = 0.4,
      color = color,
      fontface = "bold",
      size = 4.5
    ) +
    scale_x_continuous(
      breaks = seq(ano_min, ano_max, by = 1),
      expand = expansion(mult = c(0.01, 0.08))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = y_label,
      caption = caption_ratio
    ) +
    theme_ratio
}

g_p50_p20 <- plot_ratio(
  y_var = "P50_P20",
  y_label = "P50/P20",
  color = "#E69F00",
  title = paste0("Lower-tail earnings inequality, ", periodo),
  subtitle = "P50/P20 percentile ratio - Brazil, RAIS"
)

g_p90_p50 <- plot_ratio(
  y_var = "P90_P50",
  y_label = "P90/P50",
  color = "#0072B2",
  title = paste0("Upper-tail earnings inequality, ", periodo),
  subtitle = "P90/P50 percentile ratio - Brazil, RAIS"
)

grafico <- melt(
  painel,
  id.vars = "ano",
  measure.vars = c("P50_P20", "P90_P50"),
  variable.name = "series",
  value.name = "ratio"
)

grafico[
  ,
  series_label := fifelse(
    series == "P50_P20",
    "P50/P20 (lower tail)",
    "P90/P50 (upper tail)"
  )
]

g_combined <- ggplot(grafico, aes(x = ano, y = ratio, color = series_label, group = series_label)) +
  {if (tem_cobertura_parcial) annotate(
    "rect",
    xmin = ano_cobertura_parcial - 0.5,
    xmax = ano_max + 0.5,
    ymin = -Inf,
    ymax = Inf,
    fill = "#D9D9D9",
    alpha = 0.35,
    color = NA
  )} +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.1) +
  scale_color_manual(
    values = c(
      "P50/P20 (lower tail)" = "#E69F00",
      "P90/P50 (upper tail)" = "#0072B2"
    )
  ) +
  scale_x_continuous(
    breaks = seq(ano_min, ano_max, by = 1),
    expand = expansion(mult = c(0.01, 0.04))
  ) +
  labs(
    title = paste0("Percentile ratios of Brazilian formal labor earnings, ", periodo),
    subtitle = "Brazil, RAIS; men aged 25-55",
    x = NULL,
    y = "Percentile ratio",
    color = NULL,
    caption = caption_ratio
  ) +
  theme_ratio

ggsave(
  "figures/p50_20.png",
  plot = g_p50_p20,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  "figures/p90_50.png",
  plot = g_p90_p50,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  file.path("figures", sprintf("percentile_ratios_%s_homens_25_55.png", sufixo)),
  plot = g_combined,
  width = 12,
  height = 7,
  dpi = 300
)
