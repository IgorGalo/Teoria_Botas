library(shiny)
library(ggplot2)
library(scales)

quintil_labels <- c(
  "F1\nmenor renda",
  "F2",
  "F3",
  "F4",
  "F5\nmaior renda"
)

quintil_plot_labels <- c(
  "Q1\nmenor renda",
  "Q2",
  "Q3",
  "Q4",
  "Q5\nmaior renda"
)

money_columns <- c(
  "Renda mensal",
  "Renda per capita",
  "Alimentacao",
  "Moradia",
  "Transporte",
  "Saude",
  "Educacao",
  "Gasto essencial",
  "Folga financeira"
)

required_columns <- c(
  "Total Household Income",
  "Region",
  "Total Food Expenditure",
  "Main Source of Income",
  "Housing and water Expenditure",
  "Medical Care Expenditure",
  "Transportation Expenditure",
  "Education Expenditure",
  "Total Number of Family members"
)

indicator_choices <- c(
  "Comprometimento da renda" = "Comprometimento",
  "Renda mensal domiciliar" = "Renda",
  "Renda per capita" = "RendaPerCapita",
  "Folga financeira" = "FolgaFinanceira",
  "Gasto essencial" = "GastoEssencial",
  "Alimentacao" = "Alimentacao",
  "Moradia" = "Moradia",
  "Transporte" = "Transporte",
  "Saude" = "Saude",
  "Educacao" = "Educacao"
)

locate_income_file <- function() {
  candidates <- c(
    file.path(getwd(), "Income.csv"),
    "C:/Users/Cliente/OneDrive/UFSJ/Prog/R/Income.csv"
  )
  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])
  candidates <- candidates[file.exists(candidates)]

  if (length(candidates) == 0) {
    stop(
      "Nao foi possivel localizar o arquivo Income.csv. ",
      "Coloque o arquivo na pasta do projeto ou mantenha o caminho padrao."
    )
  }

  candidates[1]
}

quintil_scale_x <- function() {
  scale_x_discrete(labels = setNames(quintil_plot_labels, quintil_labels))
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

format_brl <- function(x) {
  ifelse(
    is.na(x),
    "NA",
    paste0(
      "R$ ",
      formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
    )
  )
}

format_pct <- function(x) {
  ifelse(
    is.na(x),
    "NA",
    paste0(formatC(100 * x, format = "f", digits = 1, decimal.mark = ","), "%")
  )
}

calc_stat <- function(x, stat) {
  x <- x[is.finite(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  switch(
    stat,
    mean = mean(x),
    median = median(x),
    sd = if (length(x) > 1) sd(x) else 0,
    iqr = IQR(x),
    mean(x)
  )
}

stat_label <- function(stat) {
  switch(
    stat,
    mean = "Media",
    median = "Mediana",
    sd = "Desvio-padrao",
    iqr = "IQR",
    "Media"
  )
}

indicator_label <- function(variable_name) {
  switch(
    variable_name,
    Comprometimento = "Comprometimento da renda",
    Renda = "Renda mensal domiciliar",
    RendaPerCapita = "Renda per capita",
    FolgaFinanceira = "Folga financeira",
    GastoEssencial = "Gasto essencial",
    Alimentacao = "Alimentacao",
    Moradia = "Moradia",
    Transporte = "Transporte",
    Saude = "Saude",
    Educacao = "Educacao",
    variable_name
  )
}

is_percent_indicator <- function(variable_name) {
  identical(variable_name, "Comprometimento")
}

format_indicator_value <- function(x, variable_name) {
  if (is_percent_indicator(variable_name)) {
    format_pct(x)
  } else {
    format_brl(x)
  }
}

quintil_colors <- function(theme_mode) {
  if (identical(theme_mode, "Escuro")) {
    return(c("#F87171", "#FB923C", "#FACC15", "#4ADE80", "#60A5FA"))
  }

  c("#EF4444", "#F97316", "#F59E0B", "#10B981", "#3B82F6")
}

axis_scale_for <- function(variable_name, axis = "y") {
  if (is_percent_indicator(variable_name)) {
    if (identical(axis, "x")) {
      scale_x_continuous(labels = percent_format(accuracy = 1))
    } else {
      scale_y_continuous(labels = percent_format(accuracy = 1))
    }
  } else {
    if (identical(axis, "x")) {
      scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))
    } else {
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))
    }
  }
}

safe_ratio <- function(numerator, denominator) {
  if (!is.finite(numerator) || !is.finite(denominator) || denominator == 0) {
    return(NA_real_)
  }

  numerator / denominator
}

has_variation <- function(x) {
  x <- x[is.finite(x)]
  length(unique(x)) > 1
}

empty_plot <- function(title, subtitle, theme_mode, x = NULL, y = NULL) {
  pal <- plot_palette(theme_mode)

  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.55,
      label = subtitle,
      colour = pal$text,
      size = 5.1,
      lineheight = 1.2
    ) +
    labs(title = title, x = x, y = y) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    plot_theme(theme_mode) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}

slider_step_for <- function(min_value, max_value, variable_name) {
  if (is_percent_indicator(variable_name)) {
    return(0.01)
  }

  span <- max_value - min_value

  if (!is.finite(span) || span <= 0) {
    return(1)
  }

  if (span <= 10) {
    return(0.1)
  }

  if (span <= 50) {
    return(0.5)
  }

  if (span <= 200) {
    return(1)
  }

  if (span <= 1000) {
    return(5)
  }

  if (span <= 5000) {
    return(10)
  }

  max(signif(span / 100, 1), 1)
}

round_to_step <- function(value, step_value) {
  if (!is.finite(step_value) || step_value <= 0) {
    return(value)
  }

  round(value / step_value) * step_value
}

compute_probability_stats <- function(values, operator, threshold) {
  valid_values <- values[is.finite(values)]
  n <- length(valid_values)

  if (n == 0) {
    return(list(
      n = 0,
      success = 0,
      prob = NA_real_,
      se = NA_real_,
      lower = NA_real_,
      upper = NA_real_
    ))
  }

  success <- switch(
    operator,
    ">=" = sum(valid_values >= threshold),
    "<=" = sum(valid_values <= threshold),
    ">" = sum(valid_values > threshold),
    "<" = sum(valid_values < threshold),
    sum(valid_values >= threshold)
  )

  prob <- success / n
  se <- sqrt(prob * (1 - prob) / n)
  lower <- max(0, prob - 1.96 * se)
  upper <- min(1, prob + 1.96 * se)

  list(
    n = n,
    success = success,
    prob = prob,
    se = se,
    lower = lower,
    upper = upper
  )
}

build_correlation_long <- function(df) {
  corr_df <- df[, c(
    "Renda",
    "RendaPerCapita",
    "GastoEssencial",
    "Comprometimento",
    "FolgaFinanceira",
    "Alimentacao",
    "Moradia",
    "Transporte",
    "Saude",
    "Educacao",
    "Membros"
  )]

  corr_matrix <- cor(corr_df, use = "pairwise.complete.obs")
  labels <- c(
    Renda = "Renda",
    RendaPerCapita = "Renda per\ncapita",
    GastoEssencial = "Gasto\nessencial",
    Comprometimento = "Comprometi-\nmento",
    FolgaFinanceira = "Folga\nfinanceira",
    Alimentacao = "Alimenta-\ncao",
    Moradia = "Moradia",
    Transporte = "Transporte",
    Saude = "Saude",
    Educacao = "Educacao",
    Membros = "Membros"
  )

  corr_long <- expand.grid(
    VarX = rownames(corr_matrix),
    VarY = colnames(corr_matrix),
    stringsAsFactors = FALSE
  )
  corr_long$Correlacao <- as.vector(corr_matrix)
  corr_long$VarX <- factor(corr_long$VarX, levels = names(labels), labels = labels)
  corr_long$VarY <- factor(corr_long$VarY, levels = rev(names(labels)), labels = rev(labels))
  corr_long
}

build_quintiles <- function(renda) {
  groups <- rep(NA_integer_, length(renda))
  valid_index <- which(is.finite(renda))

  if (length(valid_index) == 0) {
    return(factor(quintil_labels[groups], levels = quintil_labels))
  }

  ordered_index <- valid_index[order(renda[valid_index], seq_along(valid_index))]
  ordered_position <- seq_along(ordered_index)
  groups[ordered_index] <- pmin(
    5L,
    floor((ordered_position - 1L) * 5L / length(ordered_index)) + 1L
  )

  factor(quintil_labels[groups], levels = quintil_labels)
}

build_summary_table <- function(df, stat) {
  output <- data.frame(
    Quintil = quintil_labels,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  variable_map <- c(
    "Renda mensal" = "Renda",
    "Renda per capita" = "RendaPerCapita",
    "Alimentacao" = "Alimentacao",
    "Moradia" = "Moradia",
    "Transporte" = "Transporte",
    "Saude" = "Saude",
    "Educacao" = "Educacao",
    "Gasto essencial" = "GastoEssencial",
    "Comprometimento" = "Comprometimento",
    "Folga financeira" = "FolgaFinanceira"
  )

  for (label in names(variable_map)) {
    column_name <- variable_map[[label]]
    output[[label]] <- vapply(
      quintil_labels,
      function(group_label) {
        calc_stat(df[df$Quintil == group_label, column_name], stat)
      },
      numeric(1)
    )
  }

  output
}

build_quintile_ranges <- function(df) {
  do.call(
    rbind,
    lapply(
      quintil_labels,
      function(group_label) {
        subset_df <- df[df$Quintil == group_label, , drop = FALSE]

        if (nrow(subset_df) == 0) {
          return(
            data.frame(
              Quintil = group_label,
              Familias = 0,
              "Renda minima" = NA_real_,
              "Renda mediana" = NA_real_,
              "Renda maxima" = NA_real_,
              check.names = FALSE,
              stringsAsFactors = FALSE
            )
          )
        }

        data.frame(
          Quintil = group_label,
          Familias = nrow(subset_df),
          "Renda minima" = min(subset_df$Renda, na.rm = TRUE),
          "Renda mediana" = median(subset_df$Renda, na.rm = TRUE),
          "Renda maxima" = max(subset_df$Renda, na.rm = TRUE),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

build_cutoff_table <- function(renda) {
  valid_renda <- renda[is.finite(renda)]

  if (length(valid_renda) == 0) {
    return(
      data.frame(
        Percentil = c("0%", "20%", "40%", "60%", "80%", "100%"),
        "Renda mensal" = NA_real_,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }

  quantiles <- quantile(valid_renda, probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)

  data.frame(
    Percentil = c("0%", "20%", "40%", "60%", "80%", "100%"),
    "Renda mensal" = as.numeric(quantiles),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

build_category_share_table <- function(df) {
  do.call(
    rbind,
    lapply(
      quintil_labels,
      function(group_label) {
        subset_df <- df[df$Quintil == group_label, , drop = FALSE]
        values <- c(
          Alimentacao = mean(subset_df$Alimentacao, na.rm = TRUE),
          Moradia = mean(subset_df$Moradia, na.rm = TRUE),
          Transporte = mean(subset_df$Transporte, na.rm = TRUE),
          Saude = mean(subset_df$Saude, na.rm = TRUE),
          Educacao = mean(subset_df$Educacao, na.rm = TRUE)
        )

        total <- sum(values, na.rm = TRUE)
        shares <- if (total > 0) values / total else rep(NA_real_, length(values))

        data.frame(
          Quintil = factor(group_label, levels = quintil_labels),
          Categoria = names(shares),
          Participacao = as.numeric(shares),
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

plot_palette <- function(theme_mode) {
  if (identical(theme_mode, "Claro")) {
    return(list(
      bg = "#F4F7FB",
      panel = "#FFFFFF",
      text = "#182230",
      muted = "#5D6B82",
      grid = "#D8E0EA",
      accent = "#14B8A6",
      accent_alt = "#F59E0B",
      accent_soft = "#0F766E",
      border = "#D0D8E2",
      histogram = "#2563EB",
      box = "#F97316"
    ))
  }

  list(
    bg = "#0B1120",
    panel = "#111827",
    text = "#E5EEF8",
    muted = "#A7B3C8",
    grid = "#334155",
    accent = "#34D399",
    accent_alt = "#FBBF24",
    accent_soft = "#38BDF8",
    border = "#334155",
    histogram = "#22C55E",
    box = "#F59E0B"
  )
}

plot_theme <- function(theme_mode) {
  pal <- plot_palette(theme_mode)

  theme_minimal(base_size = 15) +
    theme(
      plot.background = element_rect(fill = pal$bg, colour = NA),
      panel.background = element_rect(fill = pal$panel, colour = pal$border),
      panel.grid.major = element_line(colour = pal$grid, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      axis.title = element_text(colour = pal$text, face = "bold"),
      axis.text = element_text(colour = pal$text, size = 13),
      plot.title = element_text(colour = pal$text, face = "bold", size = 18),
      plot.subtitle = element_text(colour = pal$muted, size = 13.2),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(colour = pal$text, size = 12.4),
      legend.background = element_rect(fill = pal$bg, colour = NA),
      strip.background = element_rect(fill = pal$panel, colour = pal$border),
      strip.text = element_text(colour = pal$text, face = "bold", size = 13.2)
    )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      :root {
        --bg: #F4F7FB;
        --card: #FFFFFF;
        --card-soft: #EEF4FF;
        --text: #182230;
        --muted: #5D6B82;
        --input-bg: #FFFFFF;
        --input-text: #182230;
        --input-muted: #64748B;
        --accent: #14B8A6;
        --accent-2: #F59E0B;
        --border: rgba(24, 34, 48, 0.12);
        --row-alt: rgba(15, 23, 42, 0.04);
        --shadow: 0 14px 40px rgba(15, 23, 42, 0.08);
      }

      body.dark-mode {
        --bg: #0B1120;
        --card: #111827;
        --card-soft: #172036;
        --text: #E5EEF8;
        --muted: #A7B3C8;
        --input-bg: #0F172A;
        --input-text: #F8FAFC;
        --input-muted: #CBD5E1;
        --accent: #34D399;
        --accent-2: #FBBF24;
        --border: rgba(148, 163, 184, 0.20);
        --row-alt: rgba(255, 255, 255, 0.04);
        --shadow: 0 18px 44px rgba(0, 0, 0, 0.35);
      }

      body {
        background:
          radial-gradient(circle at top left, rgba(20, 184, 166, 0.16), transparent 34%),
          radial-gradient(circle at top right, rgba(245, 158, 11, 0.12), transparent 28%),
          var(--bg);
        color: var(--text);
        font-size: 19px;
        line-height: 1.6;
        position: relative;
        overflow-x: hidden;
        transition: background-color 0.25s ease, color 0.25s ease;
      }

      body::before,
      body::after {
        content: '';
        position: fixed;
        border-radius: 999px;
        filter: blur(70px);
        opacity: 0.22;
        z-index: -1;
        pointer-events: none;
        animation: float-orb 16s ease-in-out infinite;
      }

      body::before {
        width: 260px;
        height: 260px;
        background: var(--accent);
        top: 8%;
        left: -70px;
      }

      body::after {
        width: 220px;
        height: 220px;
        background: var(--accent-2);
        right: -50px;
        bottom: 10%;
        animation-duration: 20s;
      }

      @keyframes float-orb {
        0% { transform: translate3d(0, 0, 0) scale(1); }
        50% { transform: translate3d(25px, -20px, 0) scale(1.08); }
        100% { transform: translate3d(0, 0, 0) scale(1); }
      }

      @keyframes rise-in {
        0% { opacity: 0; transform: translateY(16px); }
        100% { opacity: 1; transform: translateY(0); }
      }

      @keyframes pulse-glow {
        0% { box-shadow: 0 0 0 rgba(0, 0, 0, 0); }
        50% { box-shadow: 0 0 0 6px rgba(20, 184, 166, 0.08); }
        100% { box-shadow: 0 0 0 rgba(0, 0, 0, 0); }
      }

      .container-fluid {
        max-width: 1380px;
        margin: 0 auto;
        padding-bottom: 30px;
      }

      .hero-panel,
      .card-box,
      .metric-card,
      .well {
        background: var(--card);
        color: var(--text);
        border: 1px solid var(--border);
        border-radius: 18px;
        box-shadow: var(--shadow);
        animation: rise-in 0.55s ease-out both;
      }

      .hero-panel {
        padding: 24px 28px;
        margin: 22px 0 18px 0;
      }

      .hero-title {
        margin: 0 0 10px 0;
        font-size: 2.2rem;
        font-weight: 800;
        letter-spacing: 0.02em;
      }

      .hero-subtitle,
      .muted-text {
        color: var(--muted);
      }

      .hero-subtitle {
        font-size: 1.22rem;
        line-height: 1.6;
        margin-bottom: 12px;
      }

      .question-box {
        margin-top: 12px;
        padding: 14px 16px;
        border-radius: 14px;
        background: var(--card-soft);
        border-left: 5px solid var(--accent);
        font-weight: 600;
      }

      .well {
        padding: 18px;
      }

      .well,
      .card-box,
      .metric-card,
      .question-box,
      .note-box,
      table,
      .form-control,
      .selectize-input,
      .selectize-dropdown,
      .nav-tabs > li > a,
      .irs-grid-text,
      .irs-min,
      .irs-max,
      .help-block {
        font-size: 1.12rem;
      }

      .metric-card {
        padding: 16px 18px;
        margin-bottom: 18px;
        min-height: 138px;
        transition: transform 0.25s ease, box-shadow 0.25s ease;
      }

      .metric-card:hover,
      .card-box:hover {
        transform: translateY(-3px);
      }

      .metric-label {
        color: var(--muted);
        font-size: 1.02rem;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.05em;
      }

      .metric-value {
        font-size: 2.18rem;
        font-weight: 800;
        margin: 8px 0;
      }

      .metric-help {
        color: var(--muted);
        font-size: 1rem;
        line-height: 1.5;
      }

      .card-box {
        padding: 20px 22px;
        margin-bottom: 18px;
      }

      .section-title {
        margin-top: 0;
        margin-bottom: 8px;
        font-size: 1.6rem;
        font-weight: 800;
      }

      .note-box {
        margin-top: 12px;
        padding: 14px 16px;
        border-radius: 12px;
        background: var(--card-soft);
        color: var(--text);
      }

      .formula-line {
        margin-bottom: 10px;
        line-height: 1.7;
      }

      .shiny-input-container label,
      .control-label {
        color: var(--text);
        font-weight: 700;
        font-size: 1.16rem;
      }

      .form-control,
      .selectize-input,
      .selectize-dropdown,
      .irs-bar,
      .irs-line,
      .irs-grid-text {
        color: var(--input-text);
      }

      .form-control,
      .selectize-input,
      .selectize-dropdown,
      .selectize-dropdown-content {
        background: var(--input-bg) !important;
      }

      .selectize-input,
      .form-control {
        border: 1px solid var(--border);
        min-height: 54px;
        font-size: 1.1rem !important;
        color: var(--input-text) !important;
      }

      .selectize-input {
        padding: 12px 14px;
        box-shadow: none !important;
      }

      .selectize-control.single .selectize-input,
      .selectize-control.single .selectize-input.input-active,
      .selectize-control.single .selectize-input.full,
      .selectize-control.multi .selectize-input {
        background: var(--input-bg) !important;
        color: var(--input-text) !important;
      }

      .selectize-input input,
      .selectize-input > div,
      .selectize-input .item,
      .selectize-dropdown .option,
      .selectize-dropdown .item,
      .form-control,
      .help-block,
      .irs-grid-text,
      .irs-min,
      .irs-max,
      .irs-single {
        color: var(--input-text) !important;
        font-size: 1.1rem !important;
      }

      .selectize-input input::placeholder,
      .selectize-control.single .selectize-input.not-full::after,
      .selectize-control.single .selectize-input.input-active::after {
        color: var(--input-muted) !important;
      }

      .selectize-control.single .selectize-input:after {
        border-color: var(--input-text) transparent transparent transparent !important;
      }

      .selectize-dropdown {
        border: 1px solid var(--border);
        box-shadow: var(--shadow);
      }

      .selectize-dropdown .active,
      .selectize-dropdown .option:hover {
        background: var(--card-soft);
        color: var(--input-text) !important;
      }

      .selectize-control.multi .selectize-input > div {
        background: var(--card-soft);
        color: var(--input-text);
        border: 1px solid var(--border);
      }

      .checkbox,
      .radio {
        font-size: 1rem;
      }

      .nav-tabs {
        border-bottom: 1px solid var(--border);
        margin-bottom: 18px;
      }

      .nav-tabs > li > a {
        color: var(--muted);
        border: 0;
        border-radius: 12px 12px 0 0;
        font-weight: 700;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: var(--text);
        background: var(--card);
        border: 1px solid var(--border);
        border-bottom-color: var(--card);
      }

      .table-wrapper {
        overflow-x: auto;
      }

      table {
        width: 100%;
        color: var(--text);
        border-collapse: collapse;
        background: transparent;
      }

      th, td {
        border-bottom: 1px solid var(--border);
        padding: 10px 12px;
        text-align: right;
        white-space: nowrap;
        color: var(--text) !important;
        background: transparent !important;
        font-size: 1.06rem;
      }

      th:first-child,
      td:first-child {
        text-align: left;
      }

      thead tr,
      .table > thead > tr > th {
        background: var(--card-soft) !important;
      }

      .table > tbody > tr > td,
      .table > thead > tr > th {
        color: var(--text) !important;
      }

      .table-striped > tbody > tr:nth-of-type(odd) {
        background: var(--row-alt) !important;
      }

      .small-note {
        color: var(--muted);
        font-size: 1.1rem;
        line-height: 1.6;
      }

      .prob-card {
        min-height: 360px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        gap: 12px;
      }

      .prob-title {
        color: var(--muted);
        font-size: 1rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        font-weight: 700;
      }

      .prob-value {
        font-size: 2.6rem;
        font-weight: 800;
        color: var(--text);
      }

      .prob-track {
        width: 100%;
        height: 18px;
        border-radius: 999px;
        background: var(--card-soft);
        overflow: hidden;
        border: 1px solid var(--border);
      }

      .prob-fill {
        height: 100%;
        border-radius: 999px;
        background: linear-gradient(90deg, var(--accent), var(--accent-2));
        animation: pulse-glow 2.4s ease-in-out infinite;
        transition: width 0.35s ease;
      }

      .prob-detail {
        color: var(--muted);
        font-size: 1.04rem;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('apply-theme-mode', function(mode) {
        document.body.classList.toggle('dark-mode', mode === 'Escuro');
      });
    "))
  ),

  div(
    class = "hero-panel",
    h1(class = "hero-title", "Teoria das Botas de Sam Vimes"),
    p(
      class = "hero-subtitle",
      "Dashboard para a Parte 2 da disciplina de Estatistica e Probabilidade, com foco em analise descritiva, contexto, metricas-resumo, filtros e interpretacoes iniciais."
    ),
    p(
      class = "hero-subtitle",
      "A ideia central e observar se familias com menor renda comprometem proporcionalmente mais renda com gastos essenciais, reduzindo sua folga financeira e reforcando a intuicao da teoria das botas."
    ),
    div(
      class = "question-box",
      "Pergunta de pesquisa: familias de menor renda gastam uma parcela maior da renda mensal com itens essenciais do que familias de maior renda?"
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "tema",
        "Tema do dashboard",
        choices = c("Escuro", "Claro"),
        selected = "Escuro"
      ),
      numericInput(
        "taxa_cambio",
        "Taxa PHP -> BRL usada na conversao",
        value = 0.09,
        min = 0.001,
        step = 0.001
      ),
      helpText(
        "Formula adotada no app: valor anual em PHP x taxa de cambio / 12 = valor mensal em BRL."
      ),
      selectInput(
        "base_quintil",
        "Como os quintis sao calculados?",
        choices = c(
          "Recalcular com os dados filtrados" = "filtrada",
          "Usar os quintis da base completa" = "completa"
        ),
        selected = "filtrada"
      ),
      uiOutput("regiao_ui"),
      uiOutput("fonte_ui"),
      uiOutput("familia_ui"),
      selectInput(
        "medida_resumo",
        "Medida-resumo principal",
        choices = c(
          "Media" = "mean",
          "Mediana" = "median",
          "Desvio-padrao" = "sd",
          "IQR" = "iqr"
        ),
        selected = "mean"
      ),
      selectInput(
        "indicador_principal",
        "Indicador principal por quintil",
        choices = indicator_choices,
        selected = "Comprometimento"
      ),
      selectInput(
        "grafico_secundario",
        "Grafico complementar",
        choices = c(
          "Composicao percentual da cesta essencial" = "composicao",
          "Dispersao do indicador pela renda" = "dispersao",
          "Comparacao regional do indicador" = "regiao"
        ),
        selected = "composicao"
      ),
      selectInput(
        "variavel_prob",
        "Variavel para probabilidade empirica",
        choices = indicator_choices,
        selected = "Comprometimento"
      ),
      selectInput(
        "operador_prob",
        "Evento probabilistico",
        choices = c(
          "Maior ou igual ao limiar" = ">=",
          "Menor ou igual ao limiar" = "<="
        ),
        selected = ">="
      ),
      uiOutput("limiar_prob_ui"),
      selectInput(
        "relacao_x",
        "Variavel X para relacoes",
        choices = indicator_choices,
        selected = "Renda"
      ),
      selectInput(
        "relacao_y",
        "Variavel Y para relacoes",
        choices = indicator_choices,
        selected = "GastoEssencial"
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Visao Geral",
          uiOutput("metric_cards"),
          uiOutput("amostra_card"),
          fluidRow(
            column(6, plotOutput("comprometimento_plot", height = "360px")),
            column(6, plotOutput("grafico_secundario_plot", height = "360px"))
          ),
          div(
            class = "card-box",
            h3(class = "section-title", "Insights iniciais"),
            uiOutput("insights_ui")
          )
        ),
        tabPanel(
          "Distribuicao e Probabilidade",
          fluidRow(
            column(8, plotOutput("ecdf_plot", height = "380px")),
            column(4, uiOutput("probabilidade_card"))
          ),
          fluidRow(
            column(6, plotOutput("boxplot_plot", height = "380px")),
            column(6, plotOutput("densidade_plot", height = "380px"))
          ),
          div(
            class = "card-box",
            h3(class = "section-title", "Como ler estes graficos"),
            p(
              class = "small-note",
              "A curva acumulada empirica mostra a probabilidade observada de o indicador ficar abaixo de um valor. O boxplot e a densidade ajudam a comparar centro, dispersao, assimetria e concentracao entre os quintis."
            )
          )
        ),
        tabPanel(
          "Relacoes",
          fluidRow(
            column(6, plotOutput("relacao_plot", height = "430px")),
            column(6, plotOutput("correlacao_plot", height = "430px"))

          ),
          div(
            class = "card-box",
            h3(class = "section-title", "Leitura estatistica"),
            p(
              class = "small-note",
              "O grafico de relacao ajuda a observar tendencias e possiveis padroes entre duas variaveis. O mapa de correlacoes resume a intensidade e o sentido das relacoes lineares entre os principais indicadores do projeto."
            )
          )
        ),
        tabPanel(
          "Tabelas",
          div(
            class = "card-box",
            h3(class = "section-title", "Medidas-resumo por quintil"),
            p(
              class = "small-note",
              textOutput("summary_note", inline = TRUE)
            ),
            div(class = "table-wrapper", tableOutput("resumo_tabela"))
          ),
          div(
            class = "card-box",
            h3(class = "section-title", "Faixas observadas dos quintis"),
            p(
              class = "small-note",
              textOutput("quintil_note", inline = TRUE)
            ),
            div(class = "table-wrapper", tableOutput("quintis_tabela"))
          ),
          div(
            class = "card-box",
            h3(class = "section-title", "Pontos de corte da renda mensal"),
            p(
              class = "small-note",
              "Tabela dos percentis 0%, 20%, 40%, 60%, 80% e 100% da renda mensal usada como referencia descritiva."
            ),
            div(class = "table-wrapper", tableOutput("cutoffs_tabela"))
          )
        ),
        tabPanel(
          "Metodologia",
          div(
            class = "card-box",
            h3(class = "section-title", "Contexto e construcao dos indicadores"),
            p(
              class = "formula-line",
              "1. O conjunto de dados registra informacoes domiciliares anuais. Neste projeto, os valores monetarios sao convertidos de PHP para BRL e depois divididos por 12 para representar uma leitura mensal."
            ),
            p(
              class = "formula-line",
              "2. Gasto essencial = Alimentacao + Moradia/agua + Transporte + Saude + Educacao."
            ),
            p(
              class = "formula-line",
              "3. Comprometimento da renda = Gasto essencial / Renda mensal."
            ),
            p(
              class = "formula-line",
              "4. Folga financeira = Renda mensal - Gasto essencial."
            ),
            p(
              class = "formula-line",
              "5. Renda per capita = Renda mensal / numero de moradores da familia."
            ),
            p(
              class = "formula-line",
              "6. Probabilidade empirica: para um evento definido pelo usuario, o app calcula a proporcao de familias filtradas que satisfazem a condicao."
            ),
            p(
              class = "formula-line",
              "7. Curva acumulada empirica: mostra, para cada valor do indicador, a proporcao observada de familias com resultado menor ou igual a esse valor."
            ),
            div(
              class = "note-box",
              textOutput("metodologia_quintil")
            )
          ),
          div(
            class = "card-box",
            h3(class = "section-title", "Interpretacao e limitacoes"),
            p(
              class = "formula-line",
              "A teoria das botas esta sendo usada como lente interpretativa. O dataset nao mede diretamente qualidade, durabilidade ou necessidade de reposicao dos bens comprados."
            ),
            p(
              class = "formula-line",
              "Os gastos essenciais escolhidos sao uma aproximacao util para o projeto, mas nao esgotam todo o custo de vida. A categoria residual da renda nao separa, neste app, o que foi poupado do que foi gasto em outras rubricas."
            ),
            p(
              class = "formula-line",
              "Como os dados sao das Filipinas, a conversao para BRL facilita a leitura, mas nao transforma automaticamente o contexto em um retrato da economia brasileira."
            ),
            p(
              class = "formula-line",
              "O intervalo de 95% exibido no painel de probabilidade e uma aproximacao baseada na distribuicao normal para a proporcao amostral, servindo como apoio interpretativo."
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    req(input$tema)
    session$sendCustomMessage("apply-theme-mode", input$tema)
  })

  data_path <- reactive({
    locate_income_file()
  })

  base_data <- reactive({
    raw_df <- read.csv(data_path(), check.names = FALSE, stringsAsFactors = FALSE)

    missing_columns <- setdiff(required_columns, names(raw_df))
    validate(
      need(
        length(missing_columns) == 0,
        paste("Colunas ausentes no CSV:", paste(missing_columns, collapse = ", "))
      )
    )

    monthly_factor <- input$taxa_cambio / 12

    df <- data.frame(
      Regiao = raw_df[["Region"]],
      FonteRenda = raw_df[["Main Source of Income"]],
      Membros = safe_numeric(raw_df[["Total Number of Family members"]]),
      Renda = safe_numeric(raw_df[["Total Household Income"]]) * monthly_factor,
      Alimentacao = safe_numeric(raw_df[["Total Food Expenditure"]]) * monthly_factor,
      Moradia = safe_numeric(raw_df[["Housing and water Expenditure"]]) * monthly_factor,
      Transporte = safe_numeric(raw_df[["Transportation Expenditure"]]) * monthly_factor,
      Saude = safe_numeric(raw_df[["Medical Care Expenditure"]]) * monthly_factor,
      Educacao = safe_numeric(raw_df[["Education Expenditure"]]) * monthly_factor,
      stringsAsFactors = FALSE
    )

    df$Membros[df$Membros <= 0] <- NA_real_
    df$GastoEssencial <- rowSums(
      df[, c("Alimentacao", "Moradia", "Transporte", "Saude", "Educacao")],
      na.rm = TRUE
    )
    df$Comprometimento <- ifelse(df$Renda > 0, df$GastoEssencial / df$Renda, NA_real_)
    df$FolgaFinanceira <- df$Renda - df$GastoEssencial
    df$RendaPerCapita <- ifelse(df$Membros > 0, df$Renda / df$Membros, NA_real_)
    df$QuintilCompleto <- build_quintiles(df$Renda)

    df
  })

  output$regiao_ui <- renderUI({
    df <- base_data()
    regions <- sort(unique(df$Regiao))

    selectInput(
      "regioes",
      "Filtro por regiao",
      choices = regions,
      selected = regions,
      multiple = TRUE
    )
  })

  output$fonte_ui <- renderUI({
    df <- base_data()
    sources <- sort(unique(df$FonteRenda))

    selectInput(
      "fontes",
      "Filtro por fonte de renda",
      choices = sources,
      selected = sources,
      multiple = TRUE
    )
  })

  output$familia_ui <- renderUI({
    df <- base_data()
    valid_members <- df$Membros[is.finite(df$Membros)]
    validate(
      need(
        length(valid_members) > 0,
        "Nao ha tamanhos de familia validos no arquivo para montar este filtro."
      )
    )
    range_members <- range(valid_members)

    sliderInput(
      "familia",
      "Filtro por tamanho da familia",
      min = floor(range_members[1]),
      max = ceiling(range_members[2]),
      value = c(floor(range_members[1]), ceiling(range_members[2])),
      step = 1
    )
  })

  filtered_data <- reactive({
    df <- base_data()

    if (!is.null(input$regioes) && length(input$regioes) > 0) {
      df <- df[df$Regiao %in% input$regioes, , drop = FALSE]
    }

    if (!is.null(input$fontes) && length(input$fontes) > 0) {
      df <- df[df$FonteRenda %in% input$fontes, , drop = FALSE]
    }

    if (!is.null(input$familia) && length(input$familia) == 2) {
      df <- df[
        !is.na(df$Membros) &
          df$Membros >= input$familia[1] &
          df$Membros <= input$familia[2],
        ,
        drop = FALSE
      ]
    }

    validate(
      need(
        nrow(df) >= 5,
        "Os filtros selecionados deixaram poucas observacoes. Amplie os filtros para continuar."
      )
    )

    df$Quintil <- if (identical(input$base_quintil, "filtrada")) {
      build_quintiles(df$Renda)
    } else {
      factor(df$QuintilCompleto, levels = quintil_labels)
    }

    df
  })

  summary_df <- reactive({
    build_summary_table(filtered_data(), input$medida_resumo)
  })

  quintile_ranges_df <- reactive({
    build_quintile_ranges(filtered_data())
  })

  cutoff_df <- reactive({
    renda_base <- if (identical(input$base_quintil, "filtrada")) {
      filtered_data()$Renda
    } else {
      base_data()$Renda
    }

    build_cutoff_table(renda_base)
  })

  output$limiar_prob_ui <- renderUI({
    df <- filtered_data()
    variable_name <- input$variavel_prob
    values <- df[[variable_name]]
    values <- values[is.finite(values)]

    validate(
      need(length(values) > 1, "Nao ha valores suficientes para calcular a probabilidade.")
    )

    min_value <- min(values)
    max_value <- max(values)
    median_value <- median(values)

    if (identical(min_value, max_value)) {
      max_value <- min_value + if (is_percent_indicator(variable_name)) 0.01 else 1
    }

    step_value <- slider_step_for(min_value, max_value, variable_name)
    slider_min <- floor(min_value / step_value) * step_value
    slider_max <- ceiling(max_value / step_value) * step_value
    current_value <- if (!is.null(input$limiar_prob)) input$limiar_prob else median_value
    current_value <- round_to_step(current_value, step_value)
    current_value <- min(max(current_value, slider_min), slider_max)

    tagList(
      sliderInput(
        "limiar_prob",
        "Limiar do evento probabilistico",
        min = slider_min,
        max = slider_max,
        value = current_value,
        step = step_value
      ),
      div(
        class = "small-note",
        paste("Valor atual:", format_indicator_value(current_value, variable_name))
      )
    )
  })

  probability_stats <- reactive({
    req(input$variavel_prob, input$operador_prob)
    df <- filtered_data()
    values <- df[[input$variavel_prob]]
    valid_values <- values[is.finite(values)]
    req(length(valid_values) > 0)
    threshold_value <- if (!is.null(input$limiar_prob)) input$limiar_prob else median(valid_values)
    stats <- compute_probability_stats(values, input$operador_prob, threshold_value)
    stats$threshold <- threshold_value
    stats
  })

  output$summary_note <- renderText({
    paste(
      stat_label(input$medida_resumo),
      "calculada para cada indicador dentro de cada quintil de renda."
    )
  })

  output$quintil_note <- renderText({
    if (identical(input$base_quintil, "filtrada")) {
      "Os quintis foram recalculados depois dos filtros, ordenando a renda mensal e dividindo as observacoes em 5 grupos com tamanhos quase iguais."
    } else {
      "Os quintis foram definidos na base completa e mantidos mesmo apos os filtros, preservando a comparacao com a distribuicao geral."
    }
  })

  output$metodologia_quintil <- renderText({
    sample_size <- if (identical(input$base_quintil, "filtrada")) nrow(filtered_data()) else nrow(base_data())

    paste0(
      "Quintis neste app sao grupos de frequencia. A renda mensal e ordenada da menor para a maior e o conjunto analisado e dividido em 5 blocos de tamanho o mais equilibrado possivel. ",
      "Com a configuracao atual, a base usada para definir os quintis tem ",
      format(sample_size, big.mark = ".", decimal.mark = ","),
      " familias."
    )
  })

  output$amostra_card <- renderUI({
    df <- filtered_data()
    selected_regions <- if (!is.null(input$regioes)) length(input$regioes) else 0
    selected_sources <- if (!is.null(input$fontes)) length(input$fontes) else 0
    family_range <- if (!is.null(input$familia) && length(input$familia) == 2) {
      paste0(input$familia[1], " a ", input$familia[2], " moradores")
    } else {
      "Nao definido"
    }

    div(
      class = "card-box",
      h3(class = "section-title", "Leitura atual da amostra"),
      p(
        class = "small-note",
        paste0(
          "A amostra filtrada contem ",
          format(nrow(df), big.mark = ".", decimal.mark = ","),
          " familias, cobrindo ",
          format(selected_regions, big.mark = ".", decimal.mark = ","),
          " regioes selecionadas e ",
          format(selected_sources, big.mark = ".", decimal.mark = ","),
          " fontes de renda. O filtro de tamanho familiar esta em ",
          family_range,
          "."
        )
      ),
      p(
        class = "small-note",
        paste0(
          "Os quintis estao sendo calculados com base ",
          if (identical(input$base_quintil, "filtrada")) {
            "na amostra filtrada"
          } else {
            "na base completa"
          },
          ", usando taxa de cambio PHP -> BRL de ",
          formatC(input$taxa_cambio, format = "f", digits = 3, decimal.mark = ","),
          "."
        )
      )
    )
  })

  output$metric_cards <- renderUI({
    df <- filtered_data()
    stat <- input$medida_resumo
    selected_indicator <- input$indicador_principal
    selected_value <- calc_stat(df[[selected_indicator]], stat)
    renda_value <- calc_stat(df$Renda, stat)
    prob_stats <- probability_stats()

    fluidRow(
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-label", "Familias analisadas"),
          div(class = "metric-value", format(nrow(df), big.mark = ".", decimal.mark = ",")),
          div(class = "metric-help", "Total de observacoes apos aplicar os filtros atuais.")
        )
      ),
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-label", paste(stat_label(stat), "de", indicator_label(selected_indicator))),
          div(class = "metric-value", format_indicator_value(selected_value, selected_indicator)),
          div(class = "metric-help", "Indicador atualmente exibido no grafico principal.")
        )
      ),
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-label", paste(stat_label(stat), "da renda mensal")),
          div(class = "metric-value", format_brl(renda_value)),
          div(class = "metric-help", "Valor mensal domiciliar convertido para BRL.")
        )
      ),
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-label", "Probabilidade do evento"),
          div(class = "metric-value", format_pct(prob_stats$prob)),
          div(
            class = "metric-help",
            paste0(
              indicator_label(input$variavel_prob), " ",
              input$operador_prob, " ",
              format_indicator_value(prob_stats$threshold, input$variavel_prob)
            )
          )
        )
      )
    )
  })

  output$comprometimento_plot <- renderPlot({
    df <- filtered_data()
    stat <- input$medida_resumo
    selected_indicator <- input$indicador_principal
    pal <- plot_palette(input$tema)
    quintil_colors_values <- quintil_colors(input$tema)

    values <- data.frame(
      Quintil = factor(quintil_labels, levels = quintil_labels),
      Valor = vapply(
        quintil_labels,
        function(group_label) {
          calc_stat(df[df$Quintil == group_label, selected_indicator], stat)
        },
        numeric(1)
      )
    )

    if (all(!is.finite(values$Valor))) {
      return(
        empty_plot(
          title = paste(stat_label(stat), "de", indicator_label(selected_indicator), "por quintil"),
          subtitle = "Nao ha dados suficientes para exibir este grafico com os filtros atuais.",
          theme_mode = input$tema,
          x = "Quintil de renda",
          y = indicator_label(selected_indicator)
        )
      )
    }

    base_plot <- ggplot(values, aes(x = Quintil, y = Valor, fill = Quintil)) +
      geom_col(width = 0.72, alpha = 0.9, colour = pal$panel) +
      geom_text(
        aes(label = ifelse(is.na(Valor), "NA", format_indicator_value(Valor, selected_indicator))),
        vjust = -0.45,
        colour = pal$text,
        fontface = "bold",
        size = 4.4
      ) +
      scale_fill_manual(
        values = quintil_colors_values,
        drop = FALSE
      ) +
      labs(
        title = paste(stat_label(stat), "de", indicator_label(selected_indicator), "por quintil"),
        subtitle = "O grafico principal responde dinamicamente ao indicador selecionado na lateral.",
        x = "Quintil de renda",
        y = indicator_label(selected_indicator)
      ) +
      quintil_scale_x() +
      plot_theme(input$tema) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 12.6, lineheight = 0.95, face = "bold", margin = margin(t = 8))
      )

    if (is_percent_indicator(selected_indicator)) {
      base_plot + scale_y_continuous(labels = percent_format(accuracy = 1))
    } else {
      base_plot + scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))
    }
  })

  output$grafico_secundario_plot <- renderPlot({
    df <- filtered_data()
    pal <- plot_palette(input$tema)
    selected_indicator <- input$indicador_principal
    quintil_colors_values <- quintil_colors(input$tema)

    if (identical(input$grafico_secundario, "composicao")) {
      shares_df <- build_category_share_table(df)
      if (!any(is.finite(shares_df$Participacao))) {
        return(
          empty_plot(
            title = "Composicao media da cesta essencial por quintil",
            subtitle = "Nao ha participacoes suficientes para montar a composicao com os filtros atuais.",
            theme_mode = input$tema,
            x = "Quintil de renda",
            y = "Participacao dentro do gasto essencial"
          )
        )
      }

      return(
        ggplot(shares_df, aes(x = Quintil, y = Participacao, fill = Categoria)) +
          geom_col(position = "fill", width = 0.72, colour = pal$panel) +
          scale_y_continuous(labels = percent_format(accuracy = 1)) +
        scale_fill_manual(
          values = c(
            Alimentacao = "#14B8A6",
              Moradia = "#3B82F6",
              Transporte = "#F59E0B",
              Saude = "#EF4444",
              Educacao = "#8B5CF6"
            )
          ) +
          labs(
            title = "Composicao media da cesta essencial por quintil",
            subtitle = "Cada barra soma 100% do gasto essencial medio do quintil.",
            x = "Quintil de renda",
            y = "Participacao dentro do gasto essencial"
          ) +
          quintil_scale_x() +
          plot_theme(input$tema) +
          theme(
            axis.text.x = element_text(size = 12.2, lineheight = 0.95, face = "bold", margin = margin(t = 8))
          )
      )
    }

    if (identical(input$grafico_secundario, "dispersao")) {
      scatter_indicator <- if (identical(selected_indicator, "Renda")) {
        "GastoEssencial"
      } else {
        selected_indicator
      }
      scatter_df <- data.frame(
        Renda = df$Renda,
        Indicador = df[[scatter_indicator]],
        Quintil = df$Quintil
      )
      scatter_df <- scatter_df[is.finite(scatter_df$Renda) & is.finite(scatter_df$Indicador), , drop = FALSE]

      if (nrow(scatter_df) == 0) {
        return(
          empty_plot(
            title = paste("Dispersao entre renda e", tolower(indicator_label(scatter_indicator))),
            subtitle = "Nao ha pontos suficientes para montar a dispersao com os filtros atuais.",
            theme_mode = input$tema,
            x = "Renda mensal domiciliar",
            y = indicator_label(scatter_indicator)
          )
        )
      }

      base_plot <- ggplot(
        scatter_df,
        aes(x = Renda, y = Indicador, colour = Quintil)
      ) +
        geom_point(alpha = 0.55, size = 2) +
        scale_colour_manual(
          values = quintil_colors_values,
          drop = FALSE
        ) +
        labs(
          title = paste("Dispersao entre renda e", tolower(indicator_label(scatter_indicator))),
          subtitle = "Cada ponto representa um domicilio filtrado; a linha destaca a tendencia geral.",
          x = "Renda mensal domiciliar",
          y = indicator_label(scatter_indicator)
        ) +
        plot_theme(input$tema)

      if (nrow(scatter_df) >= 2 && length(unique(scatter_df$Renda)) > 1) {
        base_plot <- base_plot +
          geom_smooth(se = FALSE, linewidth = 1.1, colour = pal$accent_alt)
      }

      if (is_percent_indicator(scatter_indicator)) {
        return(
          base_plot +
            scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
            scale_y_continuous(labels = percent_format(accuracy = 1))
        )
      }

      return(
        base_plot +
          scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
          scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))
      )
    }

    region_summary <- aggregate(
      df[[selected_indicator]],
      by = list(Regiao = df$Regiao),
      FUN = function(x) calc_stat(x, input$medida_resumo)
    )
    names(region_summary)[2] <- "Valor"
    region_summary <- region_summary[is.finite(region_summary$Valor), , drop = FALSE]

    if (nrow(region_summary) == 0) {
      return(
        empty_plot(
          title = paste("Top 10 regioes em", tolower(indicator_label(selected_indicator))),
          subtitle = "Nao ha regioes com dados suficientes para esse indicador.",
          theme_mode = input$tema,
          x = indicator_label(selected_indicator),
          y = "Regiao"
        )
      )
    }

    region_summary <- region_summary[order(region_summary$Valor, decreasing = TRUE), , drop = FALSE]
    region_summary <- head(region_summary, 10)
    region_summary$Regiao <- factor(region_summary$Regiao, levels = rev(region_summary$Regiao))

    base_plot <- ggplot(region_summary, aes(x = Regiao, y = Valor, fill = Valor)) +
      geom_col(width = 0.74, show.legend = FALSE) +
      coord_flip() +
      scale_fill_gradient(low = pal$accent_soft, high = pal$accent_alt) +
      labs(
        title = paste("Top 10 regioes em", tolower(indicator_label(selected_indicator))),
        subtitle = paste("Ordenacao pela", tolower(stat_label(input$medida_resumo)), "do indicador selecionado."),
        x = "Regiao",
        y = indicator_label(selected_indicator)
      ) +
      plot_theme(input$tema)

    if (is_percent_indicator(selected_indicator)) {
      base_plot + scale_y_continuous(labels = percent_format(accuracy = 1))
    } else {
      base_plot + scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))
    }
  })

  output$ecdf_plot <- renderPlot({
    df <- filtered_data()
    stats <- probability_stats()
    pal <- plot_palette(input$tema)
    variable_name <- input$variavel_prob
    quintil_colors_values <- quintil_colors(input$tema)
    ecdf_df <- df[, c("Quintil", variable_name)]
    names(ecdf_df)[2] <- "Valor"
    ecdf_df <- ecdf_df[is.finite(ecdf_df$Valor), , drop = FALSE]

    if (nrow(ecdf_df) == 0) {
      return(
        empty_plot(
          title = paste("Curva acumulada empirica de", indicator_label(variable_name)),
          subtitle = "Nao ha observacoes suficientes para construir a curva acumulada.",
          theme_mode = input$tema,
          x = indicator_label(variable_name),
          y = "Probabilidade acumulada observada"
        )
      )
    }

    base_plot <- ggplot(ecdf_df, aes(x = Valor, colour = Quintil)) +
      stat_ecdf(linewidth = 1.15, alpha = 0.95) +
      geom_vline(
        xintercept = stats$threshold,
        linewidth = 1.1,
        linetype = "dashed",
        colour = pal$accent_alt
      ) +
      scale_colour_manual(
        values = quintil_colors_values,
        drop = FALSE
      ) +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(
        title = paste("Curva acumulada empirica de", indicator_label(variable_name)),
        subtitle = "A linha pontilhada marca o limiar do evento probabilistico selecionado.",
        x = indicator_label(variable_name),
        y = "Probabilidade acumulada observada"
      ) +
      plot_theme(input$tema) +
      theme(legend.position = "bottom")

    base_plot + axis_scale_for(variable_name, "x")
  })

  output$probabilidade_card <- renderUI({
    stats <- probability_stats()
    variable_name <- input$variavel_prob
    event_text <- paste0(
      "P(",
      indicator_label(variable_name), " ",
      input$operador_prob, " ",
      format_indicator_value(stats$threshold, variable_name),
      ")"
    )

    div(
      class = "card-box prob-card",
      div(class = "prob-title", "Probabilidade empirica"),
      div(class = "prob-value", format_pct(stats$prob)),
      div(class = "prob-track", div(class = "prob-fill", style = paste0("width:", round(100 * stats$prob, 1), "%;"))),
      div(class = "prob-detail", event_text),
      div(
        class = "prob-detail",
        paste0(
          stats$success, " de ",
          format(stats$n, big.mark = ".", decimal.mark = ","),
          " familias da amostra filtrada atendem ao evento."
        )
      ),
      div(
        class = "prob-detail",
        paste0(
          "Intervalo aproximado de 95%: ",
          format_pct(stats$lower), " a ", format_pct(stats$upper), "."
        )
      )
    )
  })

  output$boxplot_plot <- renderPlot({
    df <- filtered_data()
    selected_indicator <- input$indicador_principal
    quintil_colors_values <- quintil_colors(input$tema)
    box_df <- data.frame(
      Quintil = df$Quintil,
      Indicador = df[[selected_indicator]]
    )
    box_df <- box_df[is.finite(box_df$Indicador), , drop = FALSE]

    if (nrow(box_df) == 0) {
      return(
        empty_plot(
          title = paste("Dispersao", tolower(indicator_label(selected_indicator))),
          subtitle = "Nao ha observacoes suficientes para montar o boxplot.",
          theme_mode = input$tema,
          x = "Quintil de renda",
          y = indicator_label(selected_indicator)
        )
      )
    }

    base_plot <- ggplot(box_df, aes(x = Quintil, y = Indicador, fill = Quintil)) +
      geom_boxplot(alpha = 0.82, outlier.alpha = 0.22, width = 0.72) +
      scale_fill_manual(
        values = quintil_colors_values,
        drop = FALSE
      ) +
      labs(
        title = paste("Dispersao", tolower(indicator_label(selected_indicator)), "por quintil"),
        subtitle = "O boxplot ajuda a comparar variacao interna e valores extremos.",
        x = "Quintil de renda",
        y = indicator_label(selected_indicator)
      ) +
      quintil_scale_x() +
      plot_theme(input$tema) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 12.4, lineheight = 0.95, face = "bold", margin = margin(t = 8))
      )

    if (is_percent_indicator(selected_indicator)) {
      base_plot + scale_y_continuous(labels = percent_format(accuracy = 1))
    } else {
      base_plot + scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))
    }
  })

  output$densidade_plot <- renderPlot({
    df <- filtered_data()
    variable_name <- input$indicador_principal
    quintil_colors_values <- quintil_colors(input$tema)
    density_df <- df[, c("Quintil", variable_name)]
    names(density_df)[2] <- "Valor"
    density_df <- density_df[is.finite(density_df$Valor), , drop = FALSE]
    density_df <- do.call(
      rbind,
      lapply(
        quintil_labels,
        function(group_label) {
          subset_df <- density_df[density_df$Quintil == group_label, , drop = FALSE]
          if (nrow(subset_df) < 2 || !has_variation(subset_df$Valor)) {
            return(NULL)
          }
          subset_df
        }
      )
    )

    if (is.null(density_df) || nrow(density_df) == 0) {
      return(
        empty_plot(
          title = paste("Densidade", tolower(indicator_label(variable_name))),
          subtitle = "Nao ha variacao suficiente para estimar densidades com os filtros atuais.",
          theme_mode = input$tema,
          x = indicator_label(variable_name),
          y = "Densidade"
        )
      )
    }

    base_plot <- ggplot(density_df, aes(x = Valor, y = after_stat(scaled), fill = Quintil, colour = Quintil)) +
      geom_density(alpha = 0.16, linewidth = 0.9) +
      scale_fill_manual(
        values = quintil_colors_values,
        drop = FALSE
      ) +
      scale_colour_manual(
        values = quintil_colors_values,
        drop = FALSE
      ) +
      labs(
        title = paste("Densidade", tolower(indicator_label(variable_name))),
        subtitle = "As curvas mostram a concentracao relativa do indicador dentro de cada quintil.",
        x = indicator_label(variable_name),
        y = "Densidade relativa"
      ) +
      scale_y_continuous(
        labels = label_number(accuracy = 0.1, decimal.mark = ","),
        limits = c(0, 1.05)
      ) +
      plot_theme(input$tema)

    base_plot + axis_scale_for(variable_name, "x")
  })

  output$relacao_plot <- renderPlot({
    df <- filtered_data()
    pal <- plot_palette(input$tema)
    quintil_colors_values <- quintil_colors(input$tema)
    validate(need(input$relacao_x != input$relacao_y, "Escolha duas variaveis diferentes para analisar a relacao."))

    relation_df <- data.frame(
      X = df[[input$relacao_x]],
      Y = df[[input$relacao_y]],
      Quintil = df$Quintil
    )
    relation_df <- relation_df[is.finite(relation_df$X) & is.finite(relation_df$Y), , drop = FALSE]

    validate(
      need(
        nrow(relation_df) > 0,
        "Nao ha pares de observacoes suficientes para analisar essa relacao."
      )
    )

    corr_value <- cor(relation_df$X, relation_df$Y, use = "complete.obs")

    base_plot <- ggplot(relation_df, aes(x = X, y = Y, colour = Quintil)) +
      geom_point(alpha = 0.58, size = 2.1) +
      scale_colour_manual(
        values = quintil_colors_values,
        drop = FALSE
      ) +
      labs(
        title = paste("Relacao entre", tolower(indicator_label(input$relacao_x)), "e", tolower(indicator_label(input$relacao_y))),
        subtitle = paste0("Correlacao linear observada: ", formatC(corr_value, format = "f", digits = 2, decimal.mark = ","), "."),
        x = indicator_label(input$relacao_x),
        y = indicator_label(input$relacao_y)
      ) +
      plot_theme(input$tema)

    if (nrow(relation_df) >= 2 && length(unique(relation_df$X)) > 1) {
      base_plot <- base_plot +
        geom_smooth(method = "lm", se = FALSE, linewidth = 1.05, colour = pal$accent_alt)
    }

    base_plot +
      axis_scale_for(input$relacao_x, "x") +
      axis_scale_for(input$relacao_y, "y")
  })

  output$correlacao_plot <- renderPlot({
    df <- filtered_data()
    pal <- plot_palette(input$tema)
    corr_long <- build_correlation_long(df)
    corr_long$Correlacao[!is.finite(corr_long$Correlacao)] <- NA_real_

    ggplot(corr_long, aes(x = VarX, y = VarY, fill = Correlacao)) +
      geom_tile(colour = pal$panel, linewidth = 0.55) +
      geom_text(
        aes(label = ifelse(is.na(Correlacao), "NA", formatC(Correlacao, format = "f", digits = 2, decimal.mark = ","))),
        colour = pal$text,
        size = 3.0,
        fontface = "bold"
      ) +
      scale_fill_gradient2(
        low = "#2563EB",
        mid = pal$panel,
        high = "#F59E0B",
        midpoint = 0,
        limits = c(-1, 1),
        na.value = pal$grid
      ) +
      labs(
        title = "Mapa de correlacoes",
        subtitle = "Correlacoes lineares entre os principais indicadores.",
        x = NULL,
        y = NULL
      ) +
      plot_theme(input$tema) +
      theme(
        axis.text.x = element_text(size = 4, face = "bold", lineheight = 0.9, margin = margin(t = 8)),
        axis.text.y = element_text(size = 6, face = "bold", lineheight = 0.9, margin = margin(r = 8)),
        legend.position = "right",
        legend.text = element_text(size = 10.8),
        legend.key.height = grid::unit(20, "pt"),
        panel.grid = element_blank(),
        plot.margin = margin(t = 12, r = 18, b = 8, l = 12)
      )
  })

  output$insights_ui <- renderUI({
    df <- filtered_data()
    selected_indicator <- input$indicador_principal
    prob_stats <- probability_stats()
    mean_comp <- vapply(
      quintil_labels,
      function(group_label) {
        calc_stat(df[df$Quintil == group_label, "Comprometimento"], "mean")
      },
      numeric(1)
    )
    names(mean_comp) <- quintil_labels

    comp_low <- mean_comp[[quintil_labels[1]]]
    comp_high <- mean_comp[[quintil_labels[5]]]
    comp_gap <- comp_high - comp_low
    food_low <- safe_ratio(
      mean(df[df$Quintil == quintil_labels[1], "Alimentacao"], na.rm = TRUE),
      mean(df[df$Quintil == quintil_labels[1], "Renda"], na.rm = TRUE)
    )
    food_high <- safe_ratio(
      mean(df[df$Quintil == quintil_labels[5], "Alimentacao"], na.rm = TRUE),
      mean(df[df$Quintil == quintil_labels[5], "Renda"], na.rm = TRUE)
    )
    selected_low <- calc_stat(df[df$Quintil == quintil_labels[1], selected_indicator], "mean")
    selected_high <- calc_stat(df[df$Quintil == quintil_labels[5], selected_indicator], "mean")

    tagList(
      p(
        class = "small-note",
        paste0(
          "No grupo dos ", quintil_labels[1], ", o comprometimento medio e de ",
          format_pct(comp_low),
          ", enquanto no grupo dos ", quintil_labels[5], " ele fica em ",
          format_pct(comp_high), "."
        )
      ),
      p(
        class = "small-note",
        paste0(
          "A alimentacao pesa mais para os grupos de menor renda: ela representa em media ",
          format_pct(food_low),
          " da renda do primeiro quintil contra ",
          format_pct(food_high),
          " no ultimo quintil."
        )
      ),
      p(
        class = "small-note",
        paste0(
          "A diferenca entre os extremos do comprometimento e de ",
          format_pct(abs(comp_gap)),
          ", o que reforca a ideia de que familias com menor renda tem menos margem para absorver gastos essenciais."
        )
      ),
      p(
        class = "small-note",
        paste0(
          "Na leitura probabilistica da amostra filtrada, a chance empirica de uma familia apresentar ",
          indicator_label(input$variavel_prob), " ",
          input$operador_prob, " ",
          format_indicator_value(prob_stats$threshold, input$variavel_prob),
          " e de ", format_pct(prob_stats$prob), "."
        )
      ),
      if (!identical(selected_indicator, "Comprometimento")) {
        p(
          class = "small-note",
          paste0(
            "No indicador selecionado agora, ",
            indicator_label(selected_indicator),
            ", a media passa de ",
            format_indicator_value(selected_low, selected_indicator),
            " no primeiro grupo para ",
            format_indicator_value(selected_high, selected_indicator),
            " no ultimo grupo de renda."
          )
        )
      }
    )
  })

  output$resumo_tabela <- renderTable({
    summary_table <- summary_df()
    summary_table[money_columns] <- lapply(summary_table[money_columns], format_brl)
    summary_table$Comprometimento <- format_pct(summary_table$Comprometimento)
    summary_table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)

  output$quintis_tabela <- renderTable({
    quintile_table <- quintile_ranges_df()
    quintile_table$Familias <- format(quintile_table$Familias, big.mark = ".", decimal.mark = ",")
    quintile_table[, c("Renda minima", "Renda mediana", "Renda maxima")] <- lapply(
      quintile_table[, c("Renda minima", "Renda mediana", "Renda maxima")],
      format_brl
    )
    quintile_table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)

  output$cutoffs_tabela <- renderTable({
    cutoff_table <- cutoff_df()
    cutoff_table$`Renda mensal` <- format_brl(cutoff_table$`Renda mensal`)
    cutoff_table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)
}

shinyApp(ui = ui, server = server)
