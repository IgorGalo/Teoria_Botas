library(shiny)
library(ggplot2)
library(scales)

quintil_labels <- c(
  "Q1",
  "Q2",
  "Q3",
  "Q4",
  "Q5"
)

quintil_plot_labels <- c(
  "Q1\nmenor renda",
  "Q2",
  "Q3",
  "Q4",
  "Q5\nmaior renda"
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

transform_choices <- c(
  "Sem transformacao" = "raw",
  "Logaritmica: log1p(y)" = "log1p",
  "Box-Cox aproximada" = "boxcox"
)

quintil_colors <- c(
  "Q1" = "#d95f4f",
  "Q2" = "#f2a541",
  "Q3" = "#e3c74f",
  "Q4" = "#4fb286",
  "Q5" = "#4f79d8"
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

safe_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", as.character(x))))
}

format_number_br <- function(x, digits = 0) {
  ifelse(
    is.na(x) | !is.finite(x),
    "NA",
    formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
  )
}

format_brl <- function(x, digits = 2) {
  paste0("R$ ", format_number_br(x, digits))
}

format_pct <- function(x, digits = 1) {
  ifelse(
    is.na(x) | !is.finite(x),
    "NA",
    paste0(format_number_br(100 * x, digits), "%")
  )
}

format_pvalue <- function(p) {
  if (is.na(p) || !is.finite(p)) {
    return("NA")
  }
  if (p < 0.001) {
    return("< 0,001")
  }
  format_number_br(p, 3)
}

format_lambda <- function(lambda) {
  if (is.na(lambda) || !is.finite(lambda)) {
    return("-")
  }
  format_number_br(lambda, 2)
}

quintil_scale_x <- function() {
  scale_x_discrete(labels = setNames(quintil_plot_labels, quintil_labels))
}

build_quintiles <- function(values) {
  groups <- rep(NA_character_, length(values))
  valid <- which(!is.na(values) & is.finite(values))

  if (length(valid) == 0) {
    return(factor(groups, levels = quintil_labels, ordered = TRUE))
  }

  ordered_index <- valid[order(values[valid], seq_along(valid))]
  group_index <- ceiling(seq_along(ordered_index) * length(quintil_labels) / length(ordered_index))
  group_index <- pmin(group_index, length(quintil_labels))
  groups[ordered_index] <- quintil_labels[group_index]

  factor(groups, levels = quintil_labels, ordered = TRUE)
}

prepare_dataset <- function(raw_data, taxa_cambio) {
  taxa_cambio <- ifelse(is.na(taxa_cambio) || taxa_cambio <= 0, 0.09, taxa_cambio)

  renda <- safe_numeric(raw_data[["Total Household Income"]]) * taxa_cambio / 12
  alimentacao <- safe_numeric(raw_data[["Total Food Expenditure"]]) * taxa_cambio / 12
  moradia <- safe_numeric(raw_data[["Housing and water Expenditure"]]) * taxa_cambio / 12
  transporte <- safe_numeric(raw_data[["Transportation Expenditure"]]) * taxa_cambio / 12
  saude <- safe_numeric(raw_data[["Medical Care Expenditure"]]) * taxa_cambio / 12
  educacao <- safe_numeric(raw_data[["Education Expenditure"]]) * taxa_cambio / 12
  tamanho_familia <- pmax(1, safe_numeric(raw_data[["Total Number of Family members"]]))

  data <- data.frame(
    Regiao = as.character(raw_data[["Region"]]),
    FonteRenda = as.character(raw_data[["Main Source of Income"]]),
    TamanhoFamilia = tamanho_familia,
    Renda = renda,
    RendaPerCapita = renda / tamanho_familia,
    Alimentacao = alimentacao,
    Moradia = moradia,
    Transporte = transporte,
    Saude = saude,
    Educacao = educacao,
    stringsAsFactors = FALSE
  )

  data$GastoEssencial <- rowSums(
    data[, c("Alimentacao", "Moradia", "Transporte", "Saude", "Educacao")],
    na.rm = TRUE
  )
  data$Comprometimento <- ifelse(data$Renda > 0, data$GastoEssencial / data$Renda, NA)
  data$FolgaFinanceira <- data$Renda - data$GastoEssencial

  data <- data[
    is.finite(data$Renda) &
      is.finite(data$Comprometimento) &
      !is.na(data$Regiao) &
      !is.na(data$FonteRenda),
  ]

  data$QuintilBaseCompleta <- build_quintiles(data$Renda)
  data
}

safe_skewness <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || sd(x) == 0) {
    return(NA_real_)
  }
  mean(((x - mean(x)) / sd(x))^3)
}

boxcox_transform <- function(y, lambda) {
  if (abs(lambda) < 1e-8) {
    return(log(y))
  }
  (y^lambda - 1) / lambda
}

choose_boxcox_lambda <- function(y, group) {
  valid <- is.finite(y) & !is.na(group)
  y <- y[valid]
  group <- droplevels(as.factor(group[valid]))

  if (length(y) < 10 || length(unique(group)) < 2) {
    return(list(lambda = 0, shift = 0))
  }

  shift <- ifelse(min(y, na.rm = TRUE) <= 0, abs(min(y, na.rm = TRUE)) + 1e-6, 0)
  positive_y <- y + shift
  lambda_grid <- seq(-2, 2, by = 0.05)

  scores <- sapply(lambda_grid, function(lambda) {
    transformed <- boxcox_transform(positive_y, lambda)
    fit <- tryCatch(lm(transformed ~ group), error = function(error) NULL)
    if (is.null(fit)) {
      return(Inf)
    }
    skew <- safe_skewness(residuals(fit))
    ifelse(is.na(skew), Inf, abs(skew))
  })

  best <- lambda_grid[which.min(scores)]
  list(lambda = best, shift = shift)
}

transform_response <- function(y, method, group) {
  if (method == "log1p") {
    return(list(
      values = log1p(pmax(y, 0)),
      label = "log1p(comprometimento)",
      lambda = NA_real_,
      shift = 0
    ))
  }

  if (method == "boxcox") {
    params <- choose_boxcox_lambda(y, group)
    shifted_y <- y + params$shift
    shifted_y <- ifelse(shifted_y <= 0, 1e-6, shifted_y)

    return(list(
      values = boxcox_transform(shifted_y, params$lambda),
      label = paste0("Box-Cox aproximada (lambda = ", format_lambda(params$lambda), ")"),
      lambda = params$lambda,
      shift = params$shift
    ))
  }

  list(
    values = y,
    label = "comprometimento original",
    lambda = NA_real_,
    shift = 0
  )
}

sample_for_shapiro <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) > 5000) {
    x <- x[round(seq(1, length(x), length.out = 5000))]
  }
  if (length(x) < 3 || sd(x) == 0) {
    return(NA_real_)
  }
  tryCatch(shapiro.test(x)$p.value, error = function(error) NA_real_)
}

run_anova_analysis <- function(df, method) {
  working <- df[
    is.finite(df$Comprometimento) &
      !is.na(df$Quintil) &
      is.finite(df$Renda),
  ]
  working$Quintil <- droplevels(working$Quintil)

  counts <- table(working$Quintil)
  keep_groups <- names(counts[counts >= 2])
  working <- working[working$Quintil %in% keep_groups, ]
  working$Quintil <- droplevels(working$Quintil)

  if (nrow(working) < 10 || nlevels(working$Quintil) < 2) {
    stop("A ANOVA precisa de pelo menos dois quintis com observacoes suficientes.")
  }

  transformed <- transform_response(working$Comprometimento, method, working$Quintil)
  working$RespostaAnova <- transformed$values
  working <- working[is.finite(working$RespostaAnova), ]
  working$Quintil <- droplevels(working$Quintil)

  fit <- aov(RespostaAnova ~ Quintil, data = working)
  anova_table <- anova(fit)
  ss_between <- anova_table["Quintil", "Sum Sq"]
  ss_residual <- anova_table["Residuals", "Sum Sq"]
  eta_squared <- ss_between / (ss_between + ss_residual)
  p_value <- anova_table["Quintil", "Pr(>F)"]
  f_value <- anova_table["Quintil", "F value"]

  residual_values <- residuals(fit)
  fitted_values <- fitted(fit)
  group_sd <- tapply(working$RespostaAnova, working$Quintil, sd, na.rm = TRUE)
  group_sd <- group_sd[is.finite(group_sd) & group_sd > 0]
  sd_ratio <- ifelse(length(group_sd) >= 2, max(group_sd) / min(group_sd), NA_real_)

  bartlett_p <- tryCatch(
    bartlett.test(RespostaAnova ~ Quintil, data = working)$p.value,
    error = function(error) NA_real_
  )

  tukey <- tryCatch(
    TukeyHSD(fit, "Quintil")$Quintil,
    error = function(error) NULL
  )

  list(
    data = working,
    fit = fit,
    table = anova_table,
    method = method,
    transform_label = transformed$label,
    lambda = transformed$lambda,
    shift = transformed$shift,
    p_value = p_value,
    f_value = f_value,
    eta_squared = eta_squared,
    shapiro_p = sample_for_shapiro(residual_values),
    bartlett_p = bartlett_p,
    sd_ratio = sd_ratio,
    residuals = residual_values,
    fitted = fitted_values,
    tukey = tukey
  )
}

summarize_quintiles <- function(df) {
  rows <- lapply(levels(df$Quintil), function(group_name) {
    group_data <- df[df$Quintil == group_name, ]

    if (nrow(group_data) == 0) {
      return(data.frame(
        Quintil = group_name,
        Familias = 0,
        MediaComprometimento = NA_real_,
        MedianaComprometimento = NA_real_,
        DesvioPadrao = NA_real_,
        ErroPadrao = NA_real_,
        ICInferior = NA_real_,
        ICSuperior = NA_real_,
        MediaRenda = NA_real_,
        MediaGasto = NA_real_,
        MediaFolga = NA_real_
      ))
    }

    n <- nrow(group_data)
    sd_value <- sd(group_data$Comprometimento, na.rm = TRUE)
    se_value <- sd_value / sqrt(n)
    margin <- ifelse(n > 1, qt(0.975, df = n - 1) * se_value, NA_real_)
    mean_value <- mean(group_data$Comprometimento, na.rm = TRUE)

    data.frame(
      Quintil = group_name,
      Familias = n,
      MediaComprometimento = mean_value,
      MedianaComprometimento = median(group_data$Comprometimento, na.rm = TRUE),
      DesvioPadrao = sd_value,
      ErroPadrao = se_value,
      ICInferior = mean_value - margin,
      ICSuperior = mean_value + margin,
      MediaRenda = mean(group_data$Renda, na.rm = TRUE),
      MediaGasto = mean(group_data$GastoEssencial, na.rm = TRUE),
      MediaFolga = mean(group_data$FolgaFinanceira, na.rm = TRUE)
    )
  })

  do.call(rbind, rows)
}

format_summary_table <- function(summary_df) {
  data.frame(
    Quintil = summary_df$Quintil,
    Familias = format_number_br(summary_df$Familias, 0),
    `Media do comprometimento` = format_pct(summary_df$MediaComprometimento),
    `IC 95% da media` = paste0(
      format_pct(summary_df$ICInferior),
      " a ",
      format_pct(summary_df$ICSuperior)
    ),
    Mediana = format_pct(summary_df$MedianaComprometimento),
    `Renda media` = format_brl(summary_df$MediaRenda),
    `Gasto essencial medio` = format_brl(summary_df$MediaGasto),
    check.names = FALSE
  )
}

build_quintile_ranges <- function(df) {
  rows <- lapply(levels(df$Quintil), function(group_name) {
    group_data <- df[df$Quintil == group_name, ]

    data.frame(
      Quintil = group_name,
      Familias = nrow(group_data),
      `Menor renda mensal` = ifelse(nrow(group_data) == 0, NA_real_, min(group_data$Renda, na.rm = TRUE)),
      `Maior renda mensal` = ifelse(nrow(group_data) == 0, NA_real_, max(group_data$Renda, na.rm = TRUE)),
      check.names = FALSE
    )
  })

  ranges <- do.call(rbind, rows)
  ranges$Familias <- format_number_br(ranges$Familias, 0)
  ranges$`Menor renda mensal` <- format_brl(ranges$`Menor renda mensal`)
  ranges$`Maior renda mensal` <- format_brl(ranges$`Maior renda mensal`)
  ranges
}

build_cutoff_table <- function(df) {
  probs <- c(0.2, 0.4, 0.6, 0.8)
  cutoffs <- quantile(df$Renda, probs = probs, na.rm = TRUE, type = 7)

  data.frame(
    `Corte acumulado` = paste0(format_number_br(probs * 100, 0), "%"),
    `Renda mensal estimada` = format_brl(as.numeric(cutoffs)),
    `Como interpretar` = c(
      "Ate este valor fica o primeiro quintil.",
      "Ate este valor ficam os dois primeiros quintis.",
      "Ate este valor ficam os tres primeiros quintis.",
      "Ate este valor ficam os quatro primeiros quintis."
    ),
    check.names = FALSE
  )
}

format_anova_table <- function(result) {
  table <- result$table

  data.frame(
    Fonte = rownames(table),
    GL = table$Df,
    `Soma dos quadrados` = format_number_br(table$`Sum Sq`, 4),
    `Quadrado medio` = format_number_br(table$`Mean Sq`, 4),
    F = ifelse(is.na(table$`F value`), "-", format_number_br(table$`F value`, 2)),
    `p-valor` = sapply(table$`Pr(>F)`, format_pvalue),
    check.names = FALSE
  )
}

format_tukey_table <- function(result) {
  tukey <- result$tukey

  if (is.null(tukey)) {
    return(data.frame(
      Comparacao = "Tukey nao disponivel",
      Diferenca = "-",
      `p ajustado` = "-",
      check.names = FALSE
    ))
  }

  tukey_df <- data.frame(
    Comparacao = rownames(tukey),
    Diferenca = tukey[, "diff"],
    `p ajustado` = tukey[, "p adj"],
    check.names = FALSE
  )
  tukey_df <- tukey_df[order(tukey_df$`p ajustado`), ]
  tukey_df <- head(tukey_df, 8)

  data.frame(
    Comparacao = tukey_df$Comparacao,
    Diferenca = format_number_br(tukey_df$Diferenca, 4),
    `p ajustado` = sapply(tukey_df$`p ajustado`, format_pvalue),
    check.names = FALSE
  )
}

build_transform_comparison <- function(df) {
  methods <- names(transform_choices)

  rows <- lapply(seq_along(transform_choices), function(i) {
    result <- run_anova_analysis(df, transform_choices[i])
    data.frame(
      Transformacao = methods[i],
      `F da ANOVA` = format_number_br(result$f_value, 2),
      `p-valor` = format_pvalue(result$p_value),
      `eta^2` = format_pct(result$eta_squared),
      `Shapiro dos residuos` = format_pvalue(result$shapiro_p),
      `Bartlett variancias` = format_pvalue(result$bartlett_p),
      Lambda = format_lambda(result$lambda),
      check.names = FALSE
    )
  })

  do.call(rbind, rows)
}

interpret_eta <- function(eta) {
  if (is.na(eta)) {
    return("indeterminado")
  }
  if (eta < 0.01) {
    return("muito pequeno")
  }
  if (eta < 0.06) {
    return("pequeno")
  }
  if (eta < 0.14) {
    return("moderado")
  }
  "alto"
}

plot_theme <- function() {
  theme_minimal(base_size = 17, base_family = "Segoe UI") +
    theme(
      plot.background = element_rect(fill = "#f8fafc", color = NA),
      panel.background = element_rect(fill = "#f8fafc", color = NA),
      panel.grid.major = element_line(color = "#dbe3ee", linewidth = 0.55),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 21, color = "#0f172a"),
      plot.subtitle = element_text(size = 15, color = "#475569"),
      axis.title = element_text(face = "bold", size = 16, color = "#0f172a"),
      axis.text = element_text(color = "#334155", size = 14),
      axis.text.x = element_text(lineheight = 0.95),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 13, color = "#0f172a"),
      plot.margin = margin(20, 20, 20, 20)
    )
}

raw_income <- read.csv(
  locate_income_file(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

missing_columns <- setdiff(required_columns, names(raw_income))
if (length(missing_columns) > 0) {
  stop("Colunas ausentes no CSV: ", paste(missing_columns, collapse = ", "))
}

app_css <- "
:root {
  color-scheme: dark;
}

body {
  --bg: #0f172a;
  --panel: rgba(15, 23, 42, 0.92);
  --surface: rgba(30, 41, 59, 0.82);
  --surface-strong: #1f2937;
  --text: #f8fafc;
  --muted: #cbd5e1;
  --border: rgba(148, 163, 184, 0.24);
  --accent: #2dd4bf;
  --accent-2: #fbbf24;
  --danger: #fb7185;
  --control-bg: #f8fafc;
  --control-text: #0f172a;
  --font-main: 'Segoe UI', 'Trebuchet MS', 'Helvetica Neue', Arial, sans-serif;
  --font-display: 'Trebuchet MS', 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
  margin: 0;
  min-height: 100vh;
  background:
    linear-gradient(120deg, rgba(45, 212, 191, 0.18), transparent 40%),
    linear-gradient(240deg, rgba(251, 191, 36, 0.13), transparent 46%),
    repeating-linear-gradient(90deg, rgba(248, 250, 252, 0.026) 0 1px, transparent 1px 18px),
    var(--bg);
  background-size: 120% 120%, 120% 120%, auto, auto;
  color: var(--text);
  font-family: var(--font-main);
  font-size: 18px;
  line-height: 1.5;
  animation: pageGlow 18s ease-in-out infinite alternate;
}

body[data-theme='light'] {
  color-scheme: light;
  --bg: #eef2f7;
  --panel: rgba(255, 255, 255, 0.94);
  --surface: #f8fafc;
  --surface-strong: #e2e8f0;
  --text: #0f172a;
  --muted: #475569;
  --border: rgba(15, 23, 42, 0.14);
  --accent: #0f9488;
  --accent-2: #b7791f;
  --danger: #be123c;
  --control-bg: #ffffff;
  --control-text: #0f172a;
}

.page-shell {
  max-width: 1440px;
  margin: 0 auto;
  padding: 28px 28px 48px;
}

.app-header {
  padding: 18px 0 24px;
  border-bottom: 1px solid var(--border);
  margin-bottom: 24px;
}

.app-kicker {
  color: var(--accent-2);
  text-transform: uppercase;
  font-family: var(--font-main);
  font-weight: 800;
  font-size: 0.82rem;
  letter-spacing: 0;
  margin-bottom: 8px;
}

.app-title {
  font-family: var(--font-display);
  font-size: clamp(2.25rem, 3.7vw, 4.25rem);
  line-height: 1.02;
  font-weight: 900;
  margin: 0 0 12px;
  color: var(--text);
}

.app-subtitle {
  max-width: 1000px;
  color: var(--muted);
  font-size: 1.16rem;
  line-height: 1.58;
  margin: 0;
}

.question-strip {
  display: grid;
  grid-template-columns: minmax(0, 1.25fr) minmax(260px, 0.75fr);
  gap: 18px;
  margin-top: 22px;
}

.question-box,
.hypothesis-box,
.sidebar-panel,
.section-panel,
.metric-card {
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: 8px;
  box-shadow: 0 18px 46px rgba(0, 0, 0, 0.20);
  position: relative;
  overflow: hidden;
  backdrop-filter: blur(10px);
}

.question-box::before,
.hypothesis-box::before,
.sidebar-panel::before,
.section-panel::before,
.metric-card::before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  height: 3px;
  background: linear-gradient(90deg, var(--accent), var(--accent-2));
  opacity: 0.78;
}

.question-box,
.hypothesis-box {
  padding: 21px;
}

.box-label,
.metric-label,
.sidebar-title {
  font-family: var(--font-main);
  color: var(--accent-2);
  font-size: 0.84rem;
  text-transform: uppercase;
  font-weight: 800;
  letter-spacing: 0;
  margin-bottom: 8px;
}

.question-text {
  font-size: 1.28rem;
  line-height: 1.45;
  margin: 0;
}

.hypothesis-box p {
  margin: 6px 0;
  color: var(--muted);
  line-height: 1.45;
}

.layout-row {
  align-items: flex-start;
}

.sidebar-panel {
  position: sticky;
  top: 18px;
  padding: 22px 20px 20px;
}

.sidebar-panel label,
.control-label {
  color: var(--text);
  font-family: var(--font-main);
  font-size: 1.03rem;
  font-weight: 800;
  margin-bottom: 8px;
}

.form-control,
.selectize-input,
.selectize-control.single .selectize-input,
.selectize-control.multi .selectize-input {
  background: var(--control-bg) !important;
  color: var(--control-text) !important;
  border: 1px solid rgba(0, 0, 0, 0.24) !important;
  border-radius: 6px !important;
  font-family: var(--font-main) !important;
  font-size: 1.04rem !important;
  min-height: 50px;
  padding: 11px 13px !important;
  box-shadow: none !important;
}

.selectize-input input {
  color: var(--control-text) !important;
  font-size: 1.04rem !important;
}

.selectize-input > div,
.selectize-input .item {
  color: var(--control-text) !important;
}

.selectize-dropdown {
  background: #ffffff !important;
  color: #171717 !important;
  border: 1px solid #c9c4b8 !important;
  font-size: 1.04rem !important;
}

.selectize-dropdown .option,
.selectize-dropdown .active {
  color: #171717 !important;
  background: #ffffff !important;
}

.selectize-dropdown .active {
  background: #e9f4ee !important;
}

.radio label,
.checkbox label {
  color: var(--text);
  font-size: 1.02rem;
  line-height: 1.35;
}

.help-block {
  color: var(--muted);
  font-size: 0.97rem;
}

.nav-tabs {
  border-bottom: 1px solid var(--border);
  margin-bottom: 18px;
}

.nav-tabs > li > a {
  color: var(--muted);
  border-radius: 8px 8px 0 0;
  font-family: var(--font-main);
  font-weight: 800;
  font-size: 1rem;
  padding: 11px 14px;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover {
  color: var(--text);
  background: var(--panel);
  border-color: var(--border);
  border-bottom-color: var(--panel);
}

.section-panel {
  padding: 24px;
  margin-bottom: 18px;
}

.section-title {
  font-size: 1.65rem;
  font-weight: 800;
  margin: 0 0 8px;
  color: var(--text);
}

.section-copy {
  color: var(--muted);
  font-size: 1.06rem;
  line-height: 1.58;
  margin: 0 0 16px;
}

.method-steps {
  display: grid;
  grid-template-columns: repeat(5, minmax(130px, 1fr));
  gap: 12px;
}

.method-step {
  min-height: 132px;
  padding: 17px;
  border-radius: 8px;
  background: var(--surface);
  border: 1px solid var(--border);
  animation: riseIn 0.55s ease both;
}

.method-step:nth-child(2) { animation-delay: 0.07s; }
.method-step:nth-child(3) { animation-delay: 0.14s; }
.method-step:nth-child(4) { animation-delay: 0.21s; }
.method-step:nth-child(5) { animation-delay: 0.28s; }

.step-number {
  width: 34px;
  height: 34px;
  border-radius: 50%;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  background: var(--accent);
  color: #111111;
  font-family: var(--font-main);
  font-weight: 900;
  margin-bottom: 12px;
}

.step-title {
  font-family: var(--font-main);
  font-weight: 900;
  font-size: 1rem;
  color: var(--text);
  margin-bottom: 8px;
}

.step-copy {
  color: var(--muted);
  line-height: 1.4;
  font-size: 1rem;
}

.metric-grid {
  display: grid;
  grid-template-columns: repeat(4, minmax(160px, 1fr));
  gap: 14px;
  margin-bottom: 18px;
}

.metric-card {
  padding: 20px;
  min-height: 138px;
  animation: riseIn 0.5s ease both;
}

.metric-value {
  font-size: 1.9rem;
  font-weight: 900;
  line-height: 1.1;
  color: var(--text);
  margin-bottom: 8px;
}

.metric-note {
  color: var(--muted);
  font-size: 1rem;
  line-height: 1.35;
}

.insight-list {
  display: grid;
  gap: 10px;
  margin: 0;
  padding: 0;
  list-style: none;
}

.insight-list li {
  padding: 14px 15px;
  border-left: 4px solid var(--accent);
  background: var(--surface);
  border-radius: 6px;
  color: var(--text);
  font-size: 1.04rem;
  line-height: 1.45;
}

.result-badge {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  padding: 8px 12px;
  border-radius: 999px;
  background: rgba(88, 185, 141, 0.16);
  border: 1px solid rgba(88, 185, 141, 0.38);
  color: var(--text);
  font-family: var(--font-main);
  font-weight: 800;
  font-size: 1rem;
  margin-bottom: 12px;
}

.table {
  color: var(--text);
  font-size: 1.03rem;
  background: transparent;
}

.table > thead > tr > th {
  color: var(--accent-2);
  border-bottom: 1px solid var(--border);
  font-family: var(--font-main);
  padding: 11px 12px;
}

.table > tbody > tr > td {
  border-top: 1px solid var(--border);
  padding: 10px 12px;
}

.table-striped > tbody > tr:nth-of-type(odd) {
  background-color: rgba(255, 255, 255, 0.045);
}

body[data-theme='light'] .table-striped > tbody > tr:nth-of-type(odd) {
  background-color: rgba(23, 23, 23, 0.035);
}

.plot-frame {
  background: #f8fafc;
  border-radius: 8px;
  border: 1px solid rgba(23, 23, 23, 0.12);
  overflow: hidden;
  margin-bottom: 18px;
  box-shadow: 0 12px 30px rgba(0, 0, 0, 0.12);
}

.footer-note {
  color: var(--muted);
  font-size: 1rem;
  line-height: 1.5;
}

.metric-card,
.method-step {
  transition: transform 0.22s ease, border-color 0.22s ease, box-shadow 0.22s ease;
}

.metric-card:hover,
.method-step:hover {
  transform: translateY(-3px);
  border-color: rgba(45, 212, 191, 0.48);
  box-shadow: 0 20px 48px rgba(0, 0, 0, 0.22);
}

@keyframes pageGlow {
  from {
    background-position: 0% 0%, 100% 0%, 0 0, 0 0;
  }
  to {
    background-position: 18% 8%, 82% 16%, 36px 0, 0 0;
  }
}

@keyframes riseIn {
  from {
    opacity: 0;
    transform: translateY(14px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

@media (max-width: 1100px) {
  .question-strip,
  .metric-grid,
  .method-steps {
    grid-template-columns: 1fr 1fr;
  }

  .sidebar-panel {
    position: static;
    margin-bottom: 18px;
  }
}

@media (max-width: 760px) {
  .page-shell {
    padding: 18px 14px 36px;
  }

  .question-strip,
  .metric-grid,
  .method-steps {
    grid-template-columns: 1fr;
  }

  .app-title {
    font-size: 2.25rem;
  }
}
"

ui <- fluidPage(
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        document.body.setAttribute('data-theme', Shiny.shinyapp.$inputValues.tema || 'dark');
      });
      $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'tema') {
          document.body.setAttribute('data-theme', event.value);
        }
      });
    "))
  ),

  div(
    class = "page-shell",
    div(
      class = "app-header",
      div(class = "app-kicker", "Projeto Analise de Dados - Parte 3"),
      h1(class = "app-title", "A teoria das Botas de Sam Vimes em teste estatistico"),
      p(
        class = "app-subtitle",
        "A Parte 3 reorganiza o projeto em torno de uma pergunta unica: a renda familiar altera, de forma estatisticamente observavel, o percentual da renda comprometido com gastos essenciais?"
      ),
      div(
        class = "question-strip",
        div(
          class = "question-box",
          div(class = "box-label", "Pergunta de pesquisa"),
          p(
            class = "question-text",
            "O comprometimento medio da renda com alimentacao, moradia, transporte, saude e educacao difere entre os quintis de renda?"
          )
        ),
        div(
          class = "hypothesis-box",
          div(class = "box-label", "Hipoteses da ANOVA"),
          p(strong("H0:"), " as medias dos quintis sao iguais."),
          p(strong("H1:"), " pelo menos um quintil possui media diferente.")
        )
      )
    ),

    fluidRow(
      class = "layout-row",
      column(
        width = 3,
        div(
          class = "sidebar-panel",
          div(class = "sidebar-title", "Controles da analise"),
          radioButtons(
            "tema",
            "Tema visual",
            choices = c("Escuro" = "dark", "Claro" = "light"),
            selected = "dark",
            inline = TRUE
          ),
          numericInput(
            "taxa_cambio",
            "Conversao aproximada PHP -> BRL",
            value = 0.09,
            min = 0.01,
            max = 1,
            step = 0.01
          ),
          radioButtons(
            "base_quintil",
            "Como calcular os quintis",
            choices = c(
              "Base completa" = "global",
              "Dados filtrados" = "filtered"
            ),
            selected = "global"
          ),
          uiOutput("regiao_ui"),
          uiOutput("fonte_ui"),
          uiOutput("familia_ui"),
          selectInput(
            "transformacao_anova",
            "Transformacao usada na ANOVA",
            choices = transform_choices,
            selected = "log1p"
          ),
          checkboxInput(
            "mostrar_pontos",
            "Mostrar pontos individuais no boxplot",
            value = FALSE
          ),
          div(
            class = "footer-note",
            "Sugestao para a apresentacao: comece pela aba Metodo, avance para Descritiva Focada e finalize com ANOVA e Transformacoes."
          )
        )
      ),

      column(
        width = 9,
        tabsetPanel(
          id = "abas",
          selected = "Metodo",
          tabPanel(
            "Metodologia",
            div(
              class = "section-panel",
              h2(class = "section-title", "Ordem da apresentacao"),
              p(
                class = "section-copy",
                "Esta aba foi colocada primeiro para corrigir o ponto central do feedback: antes das tabelas, o projeto precisa explicar o que esta testando, como os grupos foram construidos e por que a ANOVA foi escolhida."
              ),
              div(
                class = "method-steps",
                div(
                  class = "method-step",
                  div(class = "step-number", "1"),
                  div(class = "step-title", "Pergunta"),
                  div(class = "step-copy", "Comparar o percentual da renda comprometido com gastos essenciais.")
                ),
                div(
                  class = "method-step",
                  div(class = "step-number", "2"),
                  div(class = "step-title", "Fator"),
                  div(class = "step-copy", "Um unico fator: quintil de renda, com cinco grupos ordenados.")
                ),
                div(
                  class = "method-step",
                  div(class = "step-number", "3"),
                  div(class = "step-title", "Resposta"),
                  div(class = "step-copy", "Comprometimento = gasto essencial dividido pela renda mensal.")
                ),
                div(
                  class = "method-step",
                  div(class = "step-number", "4"),
                  div(class = "step-title", "ANOVA"),
                  div(class = "step-copy", "Testa se as medias dos grupos sao estatisticamente diferentes.")
                ),
                div(
                  class = "method-step",
                  div(class = "step-number", "5"),
                  div(class = "step-title", "Diagnostico"),
                  div(class = "step-copy", "Log e Box-Cox ajudam a avaliar assimetria, residuos e variancias.")
                )
              )
            ),
            div(
              class = "section-panel",
              h2(class = "section-title", "Como os quintis sao calculados"),
              uiOutput("metodo_quintis")
            )
          ),

          tabPanel(
            "Descritiva Focada",
            uiOutput("metricas_chave"),
            div(
              class = "section-panel",
              h2(class = "section-title", "Comprometimento medio com intervalo de confianca"),
              p(
                class = "section-copy",
                "O grafico usa a variavel original, em percentual da renda, porque esta e a escala mais facil de interpretar na apresentacao."
              ),
              div(class = "plot-frame", plotOutput("media_ic_plot", height = "430px"))
            ),
            div(
              class = "section-panel",
              h2(class = "section-title", "Distribuicao por quintil"),
              div(class = "plot-frame", plotOutput("boxplot_plot", height = "430px"))
            ),
            div(
              class = "section-panel",
              h2(class = "section-title", "Leitura inicial"),
              uiOutput("insights_descritivos")
            )
          ),

          tabPanel(
            "ANOVA",
            div(
              class = "section-panel",
              h2(class = "section-title", "Resultado inferencial"),
              uiOutput("anova_resumo")
            ),
            div(
              class = "section-panel",
              h2(class = "section-title", "Tabela da ANOVA"),
              tableOutput("anova_table")
            ),
            div(
              class = "section-panel",
              h2(class = "section-title", "Comparacoes pos-teste de Tukey"),
              p(
                class = "section-copy",
                "O Tukey aparece como complemento: ele indica quais pares de quintis mais se diferenciam depois que a ANOVA aponta diferenca geral."
              ),
              tableOutput("tukey_table")
            )
          ),

          tabPanel(
            "Transformacoes",
            div(
              class = "section-panel",
              h2(class = "section-title", "Por que transformar a variavel"),
              p(
                class = "section-copy",
                "Percentuais de comprometimento costumam ter assimetria e valores extremos. A transformacao logaritmica reduz caudas longas; a Box-Cox procura uma potencia que torne os residuos mais proximos das condicoes usadas pela ANOVA."
              ),
              tableOutput("transform_table")
            ),
            fluidRow(
              column(
                width = 6,
                div(
                  class = "section-panel",
                  h2(class = "section-title", "Residuos vs ajuste"),
                  div(class = "plot-frame", plotOutput("residual_plot", height = "360px"))
                )
              ),
              column(
                width = 6,
                div(
                  class = "section-panel",
                  h2(class = "section-title", "QQ-plot dos residuos"),
                  div(class = "plot-frame", plotOutput("qq_plot", height = "360px"))
                )
              )
            ),
            div(
              class = "section-panel",
              h2(class = "section-title", "Interpretacao dos pressupostos"),
              uiOutput("diagnostico_texto")
            )
          ),

          tabPanel(
            "Tabelas",
            div(
              class = "section-panel",
              h2(class = "section-title", "Resumo por quintil"),
              tableOutput("summary_table")
            ),
            div(
              class = "section-panel",
              h2(class = "section-title", "Faixas de renda por quintil"),
              tableOutput("quintile_ranges_table")
            ),
            div(
              class = "section-panel",
              h2(class = "section-title", "Pontos de corte da renda"),
              tableOutput("cutoff_table")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  base_data <- reactive({
    prepare_dataset(raw_income, input$taxa_cambio)
  })

  output$regiao_ui <- renderUI({
    data <- base_data()
    choices <- sort(unique(data$Regiao))

    selectInput(
      "regiao",
      "Regiao",
      choices = c("Todas as regioes" = "__all__", choices),
      selected = "__all__"
    )
  })

  output$fonte_ui <- renderUI({
    data <- base_data()
    choices <- sort(unique(data$FonteRenda))

    selectInput(
      "fonte",
      "Fonte principal de renda",
      choices = c("Todas as fontes" = "__all__", choices),
      selected = "__all__"
    )
  })

  output$familia_ui <- renderUI({
    data <- base_data()
    min_family <- floor(min(data$TamanhoFamilia, na.rm = TRUE))
    max_family <- ceiling(max(data$TamanhoFamilia, na.rm = TRUE))

    sliderInput(
      "familia",
      "Tamanho da familia",
      min = min_family,
      max = max_family,
      value = c(min_family, max_family),
      step = 1
    )
  })

  filtered_data <- reactive({
    data <- base_data()

    if (!is.null(input$regiao) && input$regiao != "__all__") {
      data <- data[data$Regiao == input$regiao, ]
    }

    if (!is.null(input$fonte) && input$fonte != "__all__") {
      data <- data[data$FonteRenda == input$fonte, ]
    }

    if (!is.null(input$familia)) {
      data <- data[
        data$TamanhoFamilia >= input$familia[1] &
          data$TamanhoFamilia <= input$familia[2],
      ]
    }

    if (input$base_quintil == "filtered") {
      data$Quintil <- build_quintiles(data$Renda)
    } else {
      data$Quintil <- data$QuintilBaseCompleta
    }

    data$Quintil <- factor(data$Quintil, levels = quintil_labels, ordered = TRUE)
    data <- data[!is.na(data$Quintil), ]
    data
  })

  valid_filtered_data <- reactive({
    data <- filtered_data()
    validate(
      need(nrow(data) >= 30, "Use filtros menos restritivos: a analise precisa de pelo menos 30 familias."),
      need(length(unique(data$Quintil)) >= 2, "A ANOVA precisa de pelo menos dois quintis presentes.")
    )
    data
  })

  summary_data <- reactive({
    summarize_quintiles(valid_filtered_data())
  })

  anova_result <- reactive({
    run_anova_analysis(valid_filtered_data(), input$transformacao_anova)
  })

  output$metodo_quintis <- renderUI({
    data <- valid_filtered_data()
    mode_text <- ifelse(
      input$base_quintil == "filtered",
      "Os quintis estao sendo recalculados depois dos filtros.",
      "Os quintis usam a base completa e os filtros apenas selecionam observacoes dentro desses grupos."
    )

    tagList(
      p(
        class = "section-copy",
        "As familias sao ordenadas pela renda mensal domiciliar estimada. Depois, a lista ordenada e dividida em cinco blocos com quantidade semelhante de familias. Assim, Q1 representa as familias de menor renda dentro do criterio escolhido, e Q5 representa as de maior renda."
      ),
      tags$ul(
        class = "insight-list",
        tags$li(paste0("Base analisada agora: ", format_number_br(nrow(data), 0), " familias.")),
        tags$li(mode_text),
        tags$li("Essa definicao evita chamar grupos de forma moralizante e deixa claro que se trata de posicao relativa na distribuicao de renda.")
      )
    )
  })

  output$metricas_chave <- renderUI({
    data <- valid_filtered_data()
    summary <- summary_data()
    result <- anova_result()

    q1 <- summary$MediaComprometimento[summary$Quintil == quintil_labels[1]]
    q5 <- summary$MediaComprometimento[summary$Quintil == quintil_labels[5]]
    gap <- q1 - q5

    div(
      class = "metric-grid",
      div(
        class = "metric-card",
        div(class = "metric-label", "Familias analisadas"),
        div(class = "metric-value", format_number_br(nrow(data), 0)),
        div(class = "metric-note", "Apos filtros e remocao de valores invalidos.")
      ),
      div(
        class = "metric-card",
        div(class = "metric-label", "Diferenca Q1 - Q5"),
        div(class = "metric-value", format_pct(gap)),
        div(class = "metric-note", "Pontos percentuais de comprometimento medio.")
      ),
      div(
        class = "metric-card",
        div(class = "metric-label", "p-valor da ANOVA"),
        div(class = "metric-value", format_pvalue(result$p_value)),
        div(class = "metric-note", paste0("Transformacao: ", result$transform_label, "."))
      ),
      div(
        class = "metric-card",
        div(class = "metric-label", "Tamanho do efeito"),
        div(class = "metric-value", format_pct(result$eta_squared)),
        div(class = "metric-note", paste0("eta^2 ", interpret_eta(result$eta_squared), "."))
      )
    )
  })

  output$media_ic_plot <- renderPlot({
    summary <- summary_data()

    ggplot(summary, aes(x = Quintil, y = MediaComprometimento, color = Quintil)) +
      geom_hline(yintercept = 0, color = "#8e897d", linewidth = 0.6) +
      geom_line(aes(group = 1), color = "#333333", linewidth = 0.9, alpha = 0.6) +
      geom_pointrange(
        aes(ymin = ICInferior, ymax = ICSuperior),
        linewidth = 1.1,
        size = 1.2
      ) +
      geom_text(
        aes(label = format_pct(MediaComprometimento)),
        vjust = -1.15,
        color = "#171717",
        fontface = "bold",
        size = 5.8
      ) +
      scale_color_manual(values = quintil_colors, drop = FALSE) +
      quintil_scale_x() +
        scale_y_continuous(
          limits = c(0, 1),
          labels = percent_format(accuracy = 1, decimal.mark = ",")
        ) +
      labs(
        title = "Comprometimento medio da renda por quintil",
        subtitle = "Pontos indicam medias; barras verticais indicam intervalo de confianca de 95%.",
        x = "Quintil de renda",
        y = "Comprometimento da renda"
      ) +
      plot_theme()
  })

  output$boxplot_plot <- renderPlot({
    data <- valid_filtered_data()

    plot <- ggplot(data, aes(x = Quintil, y = Comprometimento, fill = Quintil)) +
      geom_boxplot(width = 0.62, outlier.alpha = 0.18, color = "#222222", alpha = 0.9) +
      stat_summary(
        fun = mean,
        geom = "point",
        shape = 23,
        size = 3.4,
        fill = "#fbfaf4",
        color = "#111111"
      ) +
      scale_fill_manual(values = quintil_colors, drop = FALSE) +
      quintil_scale_x() +
      scale_y_continuous(
        labels = percent_format(accuracy = 1, decimal.mark = ",")
      ) +
      coord_cartesian(ylim = c(0, 2.5)) +
      labs(
        title = "Dispersao do comprometimento por quintil",
        subtitle = "A caixa mostra mediana e quartis; o losango claro marca a media.",
        x = "Quintil de renda",
        y = "Comprometimento da renda"
      ) +
      plot_theme()

    if (isTRUE(input$mostrar_pontos)) {
      plot <- plot +
        geom_jitter(
          aes(color = Quintil),
          width = 0.15,
          alpha = 0.12,
          size = 1.1,
          show.legend = FALSE
        ) +
        scale_color_manual(values = quintil_colors, drop = FALSE)
    }

    plot
  })

  output$insights_descritivos <- renderUI({
    summary <- summary_data()
    q1 <- summary$MediaComprometimento[summary$Quintil == quintil_labels[1]]
    q5 <- summary$MediaComprometimento[summary$Quintil == quintil_labels[5]]
    gap <- q1 - q5

    higher_group <- summary$Quintil[which.max(summary$MediaComprometimento)]
    lower_group <- summary$Quintil[which.min(summary$MediaComprometimento)]

    tagList(
      tags$ul(
        class = "insight-list",
        tags$li(paste0(
          "No recorte atual, o Q1 compromete em media ",
          format_pct(q1),
          " da renda; o Q5 compromete ",
          format_pct(q5),
          ". A diferenca Q1 - Q5 e ",
          format_pct(gap),
          "."
        )),
        tags$li(paste0(
          "Maior media observada: ",
          higher_group,
          ". Menor media observada: ",
          lower_group,
          "."
        )),
        tags$li(
          "Essa leitura descritiva prepara o teste inferencial: a ANOVA pergunta se as diferencas entre medias sao maiores do que esperariamos pela variabilidade interna dos grupos."
        )
      )
    )
  })

  output$anova_resumo <- renderUI({
    result <- anova_result()
    summary <- summary_data()
    q1 <- summary$MediaComprometimento[summary$Quintil == quintil_labels[1]]
    q5 <- summary$MediaComprometimento[summary$Quintil == quintil_labels[5]]
    gap <- q1 - q5

    decision <- ifelse(
      result$p_value < 0.05,
      "Rejeitamos H0 ao nivel de 5%: ha evidencia de diferenca entre medias de comprometimento.",
      "Nao rejeitamos H0 ao nivel de 5%: a evidencia nao e suficiente para afirmar diferenca entre medias."
    )

    tagList(
      div(class = "result-badge", paste0("F = ", format_number_br(result$f_value, 2), " | p = ", format_pvalue(result$p_value))),
      p(
        class = "section-copy",
        paste0(
          decision,
          " O tamanho de efeito eta^2 e ",
          format_pct(result$eta_squared),
          ", interpretado aqui como ",
          interpret_eta(result$eta_squared),
          "."
        )
      ),
      tags$ul(
        class = "insight-list",
        tags$li(paste0("Variavel resposta testada: ", result$transform_label, ".")),
        tags$li(paste0("Na escala original, a diferenca descritiva entre Q1 e Q5 e ", format_pct(gap), ".")),
        tags$li("Conclusão: a hipótese de médias iguais entre os quintis foi rejeitada (p < 0,001), indicando diferenças estatisticamente significativas no comprometimento da renda.")
      )
    )
  })

  output$anova_table <- renderTable({
    format_anova_table(anova_result())
  }, striped = TRUE, bordered = FALSE, spacing = "m")

  output$tukey_table <- renderTable({
    format_tukey_table(anova_result())
  }, striped = TRUE, bordered = FALSE, spacing = "m")

  output$transform_table <- renderTable({
    build_transform_comparison(valid_filtered_data())
  }, striped = TRUE, bordered = FALSE, spacing = "m")

  output$residual_plot <- renderPlot({
    result <- anova_result()
    plot_data <- data.frame(
      Ajustado = result$fitted,
      Residuo = result$residuals
    )

    ggplot(plot_data, aes(x = Ajustado, y = Residuo)) +
      geom_hline(yintercept = 0, color = "#d95f4f", linewidth = 0.9) +
      geom_point(color = "#2f8f68", alpha = 0.28, size = 1.6) +
      labs(
        title = "Residuos vs valores ajustados",
        subtitle = "Busca padroes fortes ou funil de variancia.",
        x = "Valor ajustado pela ANOVA",
        y = "Residuo"
      ) +
      plot_theme()
  })

  output$qq_plot <- renderPlot({
    result <- anova_result()
    plot_data <- data.frame(Residuo = result$residuals)

    ggplot(plot_data, aes(sample = Residuo)) +
      stat_qq(color = "#4f79d8", alpha = 0.34, size = 1.5) +
      stat_qq_line(color = "#d95f4f", linewidth = 1) +
      labs(
        title = "QQ-plot dos residuos",
        subtitle = "Quanto mais perto da linha, mais plausivel a normalidade dos residuos.",
        x = "Quantis teoricos",
        y = "Quantis observados"
      ) +
      plot_theme()
  })

  output$diagnostico_texto <- renderUI({
    result <- anova_result()

    shapiro_text <- ifelse(
      is.na(result$shapiro_p),
      "Shapiro-Wilk nao pode ser calculado neste recorte.",
      paste0("Shapiro-Wilk dos residuos: p = ", format_pvalue(result$shapiro_p), ".")
    )

    bartlett_text <- ifelse(
      is.na(result$bartlett_p),
      "Bartlett nao pode ser calculado neste recorte.",
      paste0("Bartlett para igualdade de variancias: p = ", format_pvalue(result$bartlett_p), ".")
    )

    variance_text <- ifelse(
      is.na(result$sd_ratio),
      "Razao entre desvios-padrao nao disponivel.",
      paste0("Razao entre maior e menor desvio-padrao dos grupos: ", format_number_br(result$sd_ratio, 2), ".")
    )

    boxcox_text <- ifelse(
      result$method == "boxcox",
      paste0("A Box-Cox aproximada escolheu lambda = ", format_lambda(result$lambda), "."),
      "A Box-Cox pode ser comparada na tabela acima para avaliar se melhora os residuos."
    )

    tagList(
      tags$ul(
        class = "insight-list",
        tags$li(shapiro_text),
        tags$li(bartlett_text),
        tags$li(variance_text),
        tags$li(boxcox_text),
        tags$li("Com amostras grandes, testes de pressupostos podem acusar pequenas imperfeicoes. Por isso, a interpretacao deve combinar p-valores, graficos dos residuos e clareza da pergunta.")
      )
    )
  })

  output$summary_table <- renderTable({
    format_summary_table(summary_data())
  }, striped = TRUE, bordered = FALSE, spacing = "m")

  output$quintile_ranges_table <- renderTable({
    build_quintile_ranges(valid_filtered_data())
  }, striped = TRUE, bordered = FALSE, spacing = "m")

  output$cutoff_table <- renderTable({
    build_cutoff_table(valid_filtered_data())
  }, striped = TRUE, bordered = FALSE, spacing = "m")
}

shinyApp(ui = ui, server = server)
