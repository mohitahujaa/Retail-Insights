# RetailPulse: Customer Segmentation and Personalized Purchase-Lapse Intelligence
#
# Run from the project root with shiny::runApp(). The first start reads the
# workbook and runs the pipeline (about a minute); results are cached in
# data/processed/ and reused while the data, code and configuration are
# unchanged. Run scripts/run_analysis.R beforehand for a fast first start.

library(shiny)
library(bslib)
library(ggplot2)
library(DT)

source(file.path("R", "pipeline.R"))
rp_source_all(".", modules = TRUE)

cfg <- rp_config()
results <- tryCatch(rp_load_results(cfg, "."), error = function(e) e)

theme <- bs_theme(
  version = 5,
  bg = "#ffffff", fg = RP_COLORS$ink,
  primary = RP_COLORS$primary, secondary = RP_COLORS$muted,
  base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif")
)
theme <- bs_add_rules(theme, rp_theme_rules())

if (inherits(results, "error")) {
  ui <- page_fillable(
    theme = theme,
    card(
      card_header("RetailPulse could not load its data"),
      card_body(
        p(conditionMessage(results)),
        p("No substitute data is shown. Fix the problem above and restart the app with shiny::runApp().")
      )
    )
  )
  server <- function(input, output, session) {}
} else {
  valid_k <- results$segmentation$candidates$k[results$segmentation$candidates$valid]
  view_choices <- stats::setNames(valid_k, ifelse(
    valid_k == results$segmentation$k,
    paste0("k = ", valid_k, " (best silhouette)"), paste0("k = ", valid_k)
  ))

  ui <- page_navbar(
    title = "RetailPulse",
    theme = theme,
    window_title = "RetailPulse",
    fillable = FALSE,
    nav_panel("Overview", mod_overview_ui("overview", results)),
    nav_panel("Customer Segments", mod_segments_ui("segments", results)),
    nav_panel("Lapse Signals", mod_lapse_ui("lapse", results)),
    nav_panel("Methodology & Data Quality", mod_methodology_ui("methodology", results)),
    nav_spacer(),
    nav_item(div(
      class = "rp-view-select d-flex align-items-center gap-2",
      tags$label(`for` = "view_k", class = "navbar-text small mb-0", "Segmentation view"),
      selectInput("view_k", NULL, choices = view_choices, selected = results$segmentation$k, width = "200px")
    )),
    nav_item(span(
      class = "navbar-text small",
      paste("Reference date", rp_fmt_date(results$reference_date))
    ))
  )

  server <- function(input, output, session) {
    # The segmentation view (k) chosen in the top bar drives every tab.
    view <- reactive({
      k <- if (is.null(input$view_k)) results$segmentation$k else input$view_k
      rp_segment_view(results$segmentation, results$customers, k)
    })
    lapse_state <- mod_lapse_server("lapse", results, view)
    mod_overview_server("overview", results, lapse_state, view)
    mod_segments_server("segments", results, view, lapse_state)
    mod_methodology_server("methodology", results)
  }
}

shinyApp(ui, server)
