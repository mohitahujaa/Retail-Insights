# Shared presentation helpers: palette, plot theme, tables, KPI cards ---------

RP_COLORS <- list(
  ink = "#1d2733",
  muted = "#5b6b7b",
  grid = "#e3e8ee",
  primary = "#1f4e79",
  partial = "#b8c4d0",
  personalized = "#b5532a",
  fallback = "#d9a441",
  none = "#aab4be",
  segments = c("#1f4e79", "#d9822b", "#3a8d5c", "#8e5ea2", "#c94f4f", "#6c7a89")
)

rp_alert_colors <- function() {
  stats::setNames(
    c(RP_COLORS$personalized, RP_COLORS$fallback, RP_COLORS$none),
    c(RP_ALERT_PERSONALIZED, RP_ALERT_FALLBACK, RP_ALERT_NONE)
  )
}

rp_segment_colors <- function(segments) {
  segments <- sort(unique(segments))
  stats::setNames(rep(RP_COLORS$segments, length.out = length(segments)), segments)
}

rp_gg_theme <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = RP_COLORS$ink),
      axis.text = ggplot2::element_text(colour = RP_COLORS$muted),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = RP_COLORS$grid),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(colour = RP_COLORS$muted, hjust = 0),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

# Interactive table with consistent defaults. `currency`, `integer`, `decimal`
# and `percent` name columns to format (by display name).
# `wide` names text columns that get a fixed, generous width (e.g. explanations).
rp_dt <- function(df, currency = NULL, integer = NULL, decimal = NULL,
                  percent = NULL, page_length = 10, dom = "frtip", wide = NULL, ...) {
  col_defs <- if (length(wide)) {
    list(list(width = "460px", targets = match(wide, names(df)) - 1L))
  } else {
    list()
  }
  dt <- DT::datatable(
    df,
    rownames = FALSE, filter = "none", selection = "none",
    class = "compact stripe hover",
    options = list(
      pageLength = page_length, scrollX = TRUE, dom = dom,
      autoWidth = length(wide) > 0, columnDefs = col_defs
    ),
    ...
  )
  if (length(currency)) dt <- DT::formatCurrency(dt, currency, currency = "\u00a3", digits = 0)
  if (length(integer)) dt <- DT::formatRound(dt, integer, digits = 0)
  if (length(decimal)) dt <- DT::formatRound(dt, decimal, digits = 2)
  if (length(percent)) dt <- DT::formatPercentage(dt, percent, digits = 1)
  dt
}

# Table output that keeps its natural height inside fillable cards.
rp_dt_output <- function(id) DT::DTOutput(id, fill = FALSE)

# Theme rules shared by the whole app (navbar contrast, compact KPI values).
rp_theme_rules <- function() {
  "
  .navbar.navbar-default, .navbar.navbar-static-top { background-color: $primary !important; }
  .navbar.navbar-default .navbar-brand, .navbar.navbar-default .nav-link, .navbar.navbar-default .navbar-text { color: rgba(255,255,255,.85) !important; }
  .navbar.navbar-default .nav-link.active, .navbar.navbar-default .nav-link:hover, .navbar.navbar-default .navbar-brand { color: #fff !important; }
  .navbar.navbar-default .nav-link.active { box-shadow: inset 0 -3px 0 #fff; }
  .navbar.navbar-default .navbar-toggler { border-color: rgba(255,255,255,.6); filter: invert(1); }
  .bslib-value-box .value-box-area .value-box-value { font-size: 1.55rem !important; }
  .bslib-value-box .value-box-area .value-box-title { font-size: .9rem !important; }
  table.dataTable { font-size: .875rem; }
  "
}

rp_kpi <- function(title, value_output, caption) {
  bslib::value_box(
    title = title,
    value = value_output,
    shiny::p(class = "small mb-0", caption),
    theme = bslib::value_box_theme(bg = "#ffffff", fg = RP_COLORS$ink),
    class = "border"
  )
}

rp_note <- function(...) {
  shiny::div(class = "small text-muted", style = "line-height:1.45;", ...)
}

rp_yes_no <- function(x) ifelse(is.na(x), "n/a", ifelse(x, "Yes", "No"))
