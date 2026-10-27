# Lapse Signals tab -------------------------------------------------------------

mod_lapse_ui <- function(id, res) {
  ns <- shiny::NS(id)
  cfg <- res$config
  segments <- sort(unique(res$customers$segment))

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::h6("Rule thresholds"),
      shiny::sliderInput(ns("ratio_threshold"), "Personalized: inactivity ratio \u2265",
        min = 1, max = 5, value = cfg$ratio_threshold, step = 0.25
      ),
      shiny::sliderInput(ns("fixed_days"), "Fixed rule and fallback: inactive days \u2265",
        min = 14, max = 180, value = cfg$fixed_days, step = 1
      ),
      rp_note(
        "Inactivity ratio = days since last purchase \u00f7 the customer's median ",
        "gap between distinct purchase dates. Needs \u2265", cfg$min_invoices,
        " invoices and \u2265", cfg$min_purchase_dates, " distinct purchase dates; ",
        "otherwise the fixed-day fallback applies. Defaults (2.0, ",
        cfg$fixed_days, " days) are starting heuristics, not calibrated values."
      ),
      shiny::hr(),
      shiny::h6("Customer list filters"),
      shiny::selectInput(ns("segment"), "Segment", choices = segments,
        selected = segments, multiple = TRUE
      ),
      shiny::checkboxGroupInput(ns("category"), "Alert category",
        choices = c(RP_ALERT_PERSONALIZED, RP_ALERT_FALLBACK, RP_ALERT_NONE),
        selected = c(RP_ALERT_PERSONALIZED, RP_ALERT_FALLBACK)
      ),
      shiny::downloadButton(ns("download"), "Download filtered list (CSV)",
        class = "btn-sm btn-outline-primary"
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 4, fill = FALSE,
      rp_kpi("Qualified for personalized cadence", shiny::textOutput(ns("kpi_qualified")), "customers with enough history"),
      rp_kpi("Assessed by fallback rule", shiny::textOutput(ns("kpi_fallback")), "customers with too little history"),
      rp_kpi("Purchase-lapse signals", shiny::textOutput(ns("kpi_flagged")), shiny::textOutput(ns("kpi_flagged_split"), inline = TRUE)),
      rp_kpi("Historical value of flagged customers", shiny::textOutput(ns("kpi_value")), "descriptive exposure, not expected loss")
    ),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel(
        "Customer list",
        rp_note(
          "Each row states the rule applied and the values behind the decision. ",
          "A signal means 'consider reviewing', not that the customer has churned."
        ),
        rp_dt_output(ns("table"))
      ),
      bslib::nav_panel(
        "A: Same-population comparison",
        rp_note(shiny::textOutput(ns("a_caption"), inline = TRUE)),
        bslib::layout_columns(
          col_widths = c(7, 5),
          rp_dt_output(ns("a_table")),
          shiny::plotOutput(ns("a_plot"), height = "330px")
        )
      ),
      bslib::nav_panel(
        "B: Hybrid policy",
        rp_note(
          "All customers: personalized cadence where history is sufficient, ",
          "fixed-day fallback otherwise. The fixed rule applied to everyone is shown for reference."
        ),
        rp_dt_output(ns("b_table"))
      ),
      bslib::nav_panel(
        "Cadence chart",
        rp_note(
          "Qualified customers only. Points below the solid line exceed the ratio threshold ",
          "(short usual interval, long inactivity); points right of the dashed line exceed the fixed-day threshold. Log scales."
        ),
        shiny::plotOutput(ns("cadence_plot"), height = "460px")
      ),
      bslib::nav_panel(
        "Threshold sensitivity",
        rp_note(
          "Number of customers each rule flags across a grid of thresholds ",
          "(computed once at the default settings; independent of the sliders)."
        ),
        rp_dt_output(ns("sensitivity"))
      )
    )
  )
}

mod_lapse_server <- function(id, res, view) {
  shiny::moduleServer(id, function(input, output, session) {
    # Segment labels follow the segmentation view; keep the filter in step.
    shiny::observeEvent(view(), {
      segments <- view()$profile$segment
      shiny::updateSelectInput(session, "segment", choices = segments, selected = segments)
    })

    ratio <- shiny::debounce(shiny::reactive({
      shiny::req(is.numeric(input$ratio_threshold))
      input$ratio_threshold
    }), 300)
    days <- shiny::debounce(shiny::reactive({
      shiny::req(is.numeric(input$fixed_days))
      input$fixed_days
    }), 300)

    lapse <- shiny::reactive(rp_apply_lapse_rules(view()$customers, ratio(), days()))
    eval_a <- shiny::reactive(rp_evaluate_rules_same_population(lapse()))
    eval_b <- shiny::reactive(rp_evaluate_hybrid_policy(lapse()))

    filtered <- shiny::reactive({
      x <- lapse()
      x[x$segment %in% input$segment & x$alert_category %in% input$category, , drop = FALSE]
    })

    output$kpi_qualified <- shiny::renderText(rp_fmt_int(sum(res$customers$cadence_qualified)))
    output$kpi_fallback <- shiny::renderText(rp_fmt_int(sum(!res$customers$cadence_qualified)))
    output$kpi_flagged <- shiny::renderText({
      x <- lapse()
      paste0(rp_fmt_int(sum(x$hybrid_flag)), " (", rp_fmt_pct(mean(x$hybrid_flag)), ")")
    })
    output$kpi_flagged_split <- shiny::renderText({
      x <- lapse()
      paste0(
        rp_fmt_int(sum(x$alert_category == RP_ALERT_PERSONALIZED)), " personalized, ",
        rp_fmt_int(sum(x$alert_category == RP_ALERT_FALLBACK)), " fallback"
      )
    })
    output$kpi_value <- shiny::renderText({
      x <- lapse()
      rp_fmt_gbp(sum(x$monetary[x$hybrid_flag]))
    })

    output$table <- DT::renderDT({
      x <- filtered()
      x <- x[order(x$alert_category == RP_ALERT_NONE, -x$monetary), , drop = FALSE]
      rp_dt(
        data.frame(
          Customer = x$CustomerID,
          Segment = x$segment,
          Alert = x$alert_category,
          `Rule applied` = x$rule_applied,
          `Last purchase` = x$last_purchase,
          `Days inactive` = x$recency,
          `Typical interval (days)` = x$typical_interval,
          `Inactivity ratio` = x$inactivity_ratio,
          `Historical value` = x$monetary,
          Invoices = x$frequency,
          `Fixed rule flag` = rp_yes_no(x$fixed_flag),
          Explanation = x$explanation,
          check.names = FALSE
        ),
        currency = "Historical value", decimal = c("Typical interval (days)", "Inactivity ratio"),
        page_length = 15, wide = "Explanation"
      )
    })

    output$download <- shiny::downloadHandler(
      filename = function() paste0("retailpulse_lapse_signals_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        x <- filtered()
        out <- data.frame(
          CustomerID = x$CustomerID, segment = x$segment,
          alert_category = x$alert_category, rule_applied = x$rule_applied,
          last_purchase = x$last_purchase, days_since_last_purchase = x$recency,
          typical_interval_days = x$typical_interval,
          inactivity_ratio = round(x$inactivity_ratio, 4),
          historical_purchase_value = round(x$monetary, 2),
          invoices = x$frequency, distinct_purchase_dates = x$n_purchase_dates,
          personalized_flag = x$personalized_flag, fixed_rule_flag = x$fixed_flag,
          hybrid_flag = x$hybrid_flag, explanation = x$explanation,
          ratio_threshold = ratio(), fixed_days_threshold = days(),
          reference_date = res$reference_date
        )
        utils::write.csv(out, file, row.names = FALSE, na = "")
      }
    )

    output$a_caption <- shiny::renderText({
      a <- eval_a()
      sprintf(
        "Population: %s customers qualified for personalized cadence; both rules use reference date %s. Jaccard overlap of the two flagged sets: %s. This is a sensitivity comparison: there are no verified churn labels, so precision and recall cannot be computed.",
        rp_fmt_int(a$population), rp_fmt_date(res$reference_date),
        rp_fmt_num(a$jaccard, 2)
      )
    })
    output$a_table <- DT::renderDT({
      t <- eval_a()$table
      rp_dt(
        stats::setNames(t, c("Group", "Customers", "Share of population", "Historical value", "Share of value")),
        currency = "Historical value", integer = "Customers",
        percent = c("Share of population", "Share of value"), dom = "t", page_length = 10
      )
    })
    output$a_plot <- shiny::renderPlot(res = 96, {
      t <- eval_a()$table
      groups <- c("Flagged only by personalized rule", "Flagged by both", "Flagged only by fixed rule")
      d <- t[match(groups, t$group), ]
      d$group <- factor(c("Personalized\nrule only", "Both rules", "Fixed rule\nonly"),
        levels = c("Personalized\nrule only", "Both rules", "Fixed rule\nonly")
      )
      ggplot2::ggplot(d, ggplot2::aes(group, customers)) +
        ggplot2::geom_col(fill = c(RP_COLORS$personalized, "#7a5c8f", RP_COLORS$primary), width = 0.65) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(rp_fmt_int(customers), "\n", rp_fmt_gbp(historical_value))),
          vjust = -0.25, size = 3.4, lineheight = 0.9
        ) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.3))) +
        ggplot2::labs(x = NULL, y = "Customers", caption = "Labels: customers and their historical purchase value") +
        rp_gg_theme(12)
    })

    output$b_table <- DT::renderDT({
      t <- eval_b()
      rp_dt(
        stats::setNames(t, c("Group", "Customers", "Share of customers", "Historical value", "Share of value")),
        currency = "Historical value", integer = "Customers",
        percent = c("Share of customers", "Share of value"), dom = "t", page_length = 10
      )
    })

    output$cadence_plot <- shiny::renderPlot(res = 96, {
      x <- lapse()
      x <- x[x$cadence_qualified, , drop = FALSE]
      x$group <- ifelse(x$personalized_flag & x$fixed_flag, "Both rules",
        ifelse(x$personalized_flag, "Personalized only",
          ifelse(x$fixed_flag, "Fixed rule only", "Neither")
        )
      )
      x$group <- factor(x$group, levels = c("Personalized only", "Both rules", "Fixed rule only", "Neither"))
      ggplot2::ggplot(x, ggplot2::aes(recency, typical_interval, colour = group)) +
        ggplot2::geom_point(alpha = 0.65, size = 1.6) +
        ggplot2::geom_abline(slope = 1, intercept = -log10(ratio()), colour = RP_COLORS$ink) +
        ggplot2::geom_vline(xintercept = days(), linetype = "dashed", colour = RP_COLORS$ink) +
        ggplot2::scale_x_log10(labels = scales::label_comma()) +
        ggplot2::scale_y_log10(labels = scales::label_comma()) +
        ggplot2::scale_colour_manual(values = c(
          "Personalized only" = RP_COLORS$personalized, "Both rules" = "#7a5c8f",
          "Fixed rule only" = RP_COLORS$primary, "Neither" = RP_COLORS$none
        ), drop = FALSE) +
        ggplot2::labs(
          x = "Days since last purchase (log scale)",
          y = "Median purchase interval, days (log scale)",
          caption = sprintf("Solid line: inactivity ratio = %.2f. Dashed line: %d days.", ratio(), as.integer(days()))
        ) +
        rp_gg_theme(12)
    })

    output$sensitivity <- DT::renderDT({
      s <- res$default_lapse$sensitivity
      rp_dt(
        stats::setNames(s, c("Rule", "Threshold", "Flagged", "Population", "Share flagged")),
        integer = c("Flagged", "Population"), percent = "Share flagged", dom = "t", page_length = 20
      )
    })

    list(lapse = lapse, ratio = ratio, days = days, filtered = filtered)
  })
}
