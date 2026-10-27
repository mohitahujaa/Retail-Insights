# Overview tab -----------------------------------------------------------------

mod_overview_ui <- function(id, res) {
  ns <- shiny::NS(id)
  s <- res$cleaning$summary
  a <- res$raw_audit
  partial <- res$monthly$label[res$monthly$partial]

  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1 / 5, fill = FALSE,
      rp_kpi("Identifiable customers", rp_fmt_int(s$final_customers), "with at least one eligible purchase"),
      rp_kpi("Eligible purchase invoices", rp_fmt_int(s$final_invoices), "distinct invoices, cancellations excluded"),
      rp_kpi("Eligible historical purchase value", rp_fmt_gbp(s$final_value), "sum of quantity \u00d7 unit price"),
      rp_kpi("Analytical transaction rows", rp_fmt_int(s$final_rows), paste0("of ", rp_fmt_int(a$n_rows), " rows in the workbook")),
      rp_kpi("Flagged for lapse review", shiny::textOutput(ns("kpi_flagged")), shiny::textOutput(ns("kpi_flagged_caption"), inline = TRUE))
    ),
    bslib::card(
      class = "mt-3",
      bslib::card_body(
        rp_note(
          shiny::strong("Scope. "),
          paste0(
            "UCI Online Retail dataset (one UK-based online retailer), transactions from ",
            rp_fmt_date(a$first_timestamp), " to ", rp_fmt_date(a$last_timestamp), ". "
          ),
          shiny::strong("Analysis reference date: ", rp_fmt_date(res$reference_date)),
          " (the day after the last transaction in the workbook); recency is measured from this date, not today. ",
          "Customer metrics use eligible purchase lines only: identified customers, positive quantity and price, ",
          "excluding cancellation and adjustment invoices, purchase lines matched to a later cancellation, and non-product service or charge lines. ",
          if (length(partial)) shiny::strong(paste0(partial, " is incomplete and is not comparable with full months."))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5), fill = FALSE,
      bslib::card(
        bslib::card_header("Monthly eligible purchase value"),
        shiny::plotOutput(ns("monthly"), height = "300px"),
        bslib::card_footer(rp_note("Paler bar: partial month. Value is gross eligible purchase value, not net revenue or profit."))
      ),
      bslib::card(
        bslib::card_header("Customers and purchase value by segment"),
        shiny::plotOutput(ns("segments"), height = "300px"),
        bslib::card_footer(rp_note(sprintf(
          "K-Means on log-transformed, standardised RFM; k = %d chosen by mean silhouette width (%.3f).",
          res$segmentation$k, res$segmentation$silhouette
        )))
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6), fill = FALSE,
      bslib::card(
        bslib::card_header("Data-quality indicators"),
        rp_dt_output(ns("quality"))
      ),
      bslib::card(
        bslib::card_header("Lapse signals by segment (current thresholds)"),
        rp_dt_output(ns("segment_lapse"))
      )
    )
  )
}

mod_overview_server <- function(id, res, lapse_state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$kpi_flagged <- shiny::renderText({
      x <- lapse_state$lapse()
      paste0(rp_fmt_int(sum(x$hybrid_flag)), " (", rp_fmt_pct(mean(x$hybrid_flag)), ")")
    })
    output$kpi_flagged_caption <- shiny::renderText({
      sprintf(
        "hybrid policy: ratio \u2265 %.2f, or \u2265 %d days without enough history",
        lapse_state$ratio(), as.integer(lapse_state$days())
      )
    })

    output$monthly <- shiny::renderPlot(res = 96, {
      m <- res$monthly
      m$label <- factor(m$label, levels = m$label)
      ggplot2::ggplot(m, ggplot2::aes(label, purchase_value, fill = partial)) +
        ggplot2::geom_col(width = 0.75) +
        ggplot2::scale_fill_manual(values = c(`FALSE` = RP_COLORS$primary, `TRUE` = RP_COLORS$partial), guide = "none") +
        ggplot2::scale_y_continuous(labels = scales::label_dollar(prefix = "\u00a3", scale_cut = scales::cut_short_scale())) +
        ggplot2::scale_x_discrete(labels = function(x) sub(" \\(", "\n(", x)) +
        ggplot2::labs(x = NULL, y = NULL) +
        rp_gg_theme(11) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8))
    })

    output$segments <- shiny::renderPlot(res = 96, {
      p <- res$segmentation$profile
      d <- rbind(
        data.frame(segment = p$segment, measure = "Share of customers", share = p$share_customers),
        data.frame(segment = p$segment, measure = "Share of purchase value", share = p$share_value)
      )
      ggplot2::ggplot(d, ggplot2::aes(share, measure, fill = segment)) +
        ggplot2::geom_col(width = 0.6, position = ggplot2::position_stack(reverse = TRUE)) +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(share >= 0.06, rp_fmt_pct(share, 0), "")),
          position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE), colour = "white", size = 3.6
        ) +
        ggplot2::scale_fill_manual(values = rp_segment_colors(p$segment)) +
        ggplot2::scale_x_continuous(labels = scales::label_percent()) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
        rp_gg_theme(11)
    })

    output$quality <- DT::renderDT({
      q <- res$data_quality
      keep <- q$measure %in% c(
        "Transaction rows in workbook", "Rows missing CustomerID",
        "Cancellation rows (invoice starts with C)",
        "Purchase lines removed as later cancelled",
        "Non-product service or charge lines removed", "Rows",
        "Identifiable customers"
      )
      q <- q[keep, ]
      q$measure[q$measure == "Rows"] <- "Final analytical rows"
      rp_dt(stats::setNames(q[, c("measure", "count", "note")], c("Measure", "Count", "Note")),
        integer = "Count", dom = "t", page_length = 20
      )
    })

    output$segment_lapse <- DT::renderDT({
      s <- rp_segment_lapse_summary(lapse_state$lapse())
      rp_dt(
        stats::setNames(s, c("Segment", "Customers", "Qualified for cadence", "Flagged", "Value of flagged", "Share flagged")),
        integer = c("Customers", "Qualified for cadence", "Flagged"),
        currency = "Value of flagged", percent = "Share flagged", dom = "t"
      )
    })
  })
}
