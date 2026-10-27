# Customer Segments tab ---------------------------------------------------------

mod_segments_ui <- function(id, res) {
  ns <- shiny::NS(id)
  seg <- res$segmentation
  valid_k <- seg$candidates$k[seg$candidates$valid]

  bslib::navset_card_tab(
    full_screen = TRUE,
    bslib::nav_panel(
      "Segment profiles",
      rp_note(
        sprintf("%d segments from K-Means (k chosen by %s). ", seg$k, tolower(seg$selection_rule)),
        "Segments are numbered by descending median purchase value for display; the numbers are not a ranking of customer quality. ",
        "Names are derived from each segment's median recency, frequency and value compared with the median customer.",
        if (length(seg$warnings)) shiny::tags$ul(lapply(seg$warnings, shiny::tags$li))
      ),
      rp_dt_output(ns("profile")),
      shiny::plotOutput(ns("boxplots"), height = "320px")
    ),
    bslib::nav_panel(
      "RFM distributions",
      rp_note(
        "Recency = days from last eligible purchase to the reference date; frequency = distinct invoices; ",
        "monetary = sum of quantity × unit price over eligible lines. Skewness after log1p shows why the transform is used before clustering."
      ),
      rp_dt_output(ns("rfm_summary")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        shiny::plotOutput(ns("histograms"), height = "300px"),
        shiny::plotOutput(ns("scatter"), height = "300px")
      ),
      shiny::h6("Spearman rank correlations"),
      shiny::tableOutput(ns("correlations"))
    ),
    bslib::nav_panel(
      "Model selection",
      bslib::layout_columns(
        col_widths = c(5, 7),
        shiny::plotOutput(ns("silhouette"), height = "280px"),
        rp_dt_output(ns("candidates"))
      ),
      rp_note(
        "Silhouette width measures how much closer customers are to their own cluster than to the nearest other cluster (−1 to 1). ",
        "It is one aid to choosing k, not proof that the segments are commercially meaningful. ",
        "Use the selector to inspect how the profiles change at other k; the rest of the dashboard uses the selected k."
      ),
      shiny::selectInput(ns("alt_k"), "Inspect profiles for k =", choices = valid_k, selected = seg$k, width = "200px"),
      rp_dt_output(ns("alt_profile"))
    ),
    bslib::nav_panel(
      "Customer table",
      shiny::div(
        class = "d-flex justify-content-between align-items-center mb-2",
        rp_note("One row per identifiable customer. Search by customer ID, segment or country."),
        shiny::downloadButton(ns("download_customers"), "Download (CSV)", class = "btn-sm btn-outline-primary")
      ),
      rp_dt_output(ns("customers"))
    ),
    bslib::nav_panel(
      "RFM rules vs K-Means",
      shiny::uiOutput(ns("rules_intro")),
      bslib::layout_columns(
        col_widths = c(6, 6),
        rp_dt_output(ns("rule_profile")),
        shiny::tableOutput(ns("crosstab"))
      )
    )
  )
}

mod_segments_server <- function(id, res) {
  shiny::moduleServer(id, function(input, output, session) {
    seg <- res$segmentation
    cust <- res$customers
    colors <- rp_segment_colors(cust$segment)

    profile_table <- function(p) {
      data.frame(
        Segment = p$segment, Customers = p$customers, `Share of customers` = p$share_customers,
        `Median recency (days)` = p$median_recency, `Mean recency` = p$mean_recency,
        `Median frequency` = p$median_frequency, `Mean frequency` = p$mean_frequency,
        `Median value` = p$median_monetary, `Mean value` = p$mean_monetary,
        `Total value` = p$total_value, `Share of value` = p$share_value,
        check.names = FALSE
      )
    }
    format_profile <- function(df) {
      rp_dt(df,
        currency = c("Median value", "Mean value", "Total value"),
        integer = c("Customers", "Median recency (days)"),
        decimal = c("Mean recency", "Median frequency", "Mean frequency"),
        percent = c("Share of customers", "Share of value"), dom = "t"
      )
    }

    output$profile <- DT::renderDT(format_profile(profile_table(seg$profile)))

    output$boxplots <- shiny::renderPlot(res = 96, {
      d <- rbind(
        data.frame(segment = cust$segment, feature = "Recency (days)", value = cust$recency),
        data.frame(segment = cust$segment, feature = "Frequency (invoices, log scale)", value = cust$frequency),
        data.frame(segment = cust$segment, feature = "Monetary (£, log scale)", value = cust$monetary)
      )
      d$value_plot <- ifelse(grepl("log", d$feature), log10(d$value), d$value)
      ggplot2::ggplot(d, ggplot2::aes(value_plot, segment, fill = segment)) +
        ggplot2::geom_boxplot(outlier.alpha = 0.25, outlier.size = 0.8, width = 0.6) +
        ggplot2::facet_wrap(~feature, scales = "free_x") +
        ggplot2::scale_fill_manual(values = colors, guide = "none") +
        ggplot2::scale_x_continuous(labels = function(x) {
          if (max(x, na.rm = TRUE) < 7) scales::label_comma(accuracy = 1)(10^x) else scales::label_comma()(x)
        }) +
        ggplot2::labs(x = NULL, y = NULL) +
        rp_gg_theme(11)
    })

    output$rfm_summary <- DT::renderDT({
      s <- res$rfm_summary
      rp_dt(
        stats::setNames(s, c("Feature", "N", "Mean", "SD", "Min", "P5", "P25", "Median", "P75", "P95", "Max", "Skewness", "Skewness after log1p")),
        integer = "N", decimal = c("Mean", "SD", "Min", "P5", "P25", "Median", "P75", "P95", "Max", "Skewness", "Skewness after log1p"),
        dom = "t"
      )
    })

    output$histograms <- shiny::renderPlot(res = 96, {
      d <- rbind(
        data.frame(feature = "Recency (days)", value = cust$recency),
        data.frame(feature = "Frequency (log scale)", value = log10(cust$frequency)),
        data.frame(feature = "Monetary £ (log scale)", value = log10(cust$monetary))
      )
      ggplot2::ggplot(d, ggplot2::aes(value)) +
        ggplot2::geom_histogram(bins = 40, fill = RP_COLORS$primary, colour = "white", linewidth = 0.2) +
        ggplot2::facet_wrap(~feature, scales = "free", ncol = 3) +
        ggplot2::scale_x_continuous(labels = function(x) {
          if (max(x, na.rm = TRUE) < 7) scales::label_comma(accuracy = 1)(10^x) else scales::label_comma()(x)
        }) +
        ggplot2::labs(x = NULL, y = "Customers") +
        rp_gg_theme(11)
    })

    output$scatter <- shiny::renderPlot(res = 96, {
      ggplot2::ggplot(cust, ggplot2::aes(recency, monetary, colour = segment)) +
        ggplot2::geom_point(alpha = 0.45, size = 1) +
        ggplot2::scale_y_log10(labels = scales::label_dollar(prefix = "£")) +
        ggplot2::scale_colour_manual(values = colors) +
        ggplot2::labs(x = "Recency (days)", y = "Monetary (log scale)") +
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, override.aes = list(size = 3, alpha = 1))) +
        rp_gg_theme(11)
    })

    output$correlations <- shiny::renderTable(
      {
        m <- round(res$rfm_correlations, 3)
        data.frame(Feature = rownames(m), m, check.names = FALSE)
      },
      digits = 3
    )

    output$silhouette <- shiny::renderPlot(res = 96, {
      c <- seg$candidates
      ggplot2::ggplot(c[c$valid, ], ggplot2::aes(k, silhouette)) +
        ggplot2::geom_line(colour = RP_COLORS$muted) +
        ggplot2::geom_point(ggplot2::aes(colour = k == seg$k), size = 3) +
        ggplot2::scale_colour_manual(values = c(`TRUE` = RP_COLORS$personalized, `FALSE` = RP_COLORS$primary), guide = "none") +
        ggplot2::scale_x_continuous(breaks = c$k) +
        ggplot2::labs(x = "Number of clusters (k)", y = "Mean silhouette width", caption = "Highlighted: selected k") +
        rp_gg_theme(11)
    })

    output$candidates <- DT::renderDT({
      c <- seg$candidates
      rp_dt(
        data.frame(
          k = c$k, Valid = rp_yes_no(c$valid), `Mean silhouette` = c$silhouette,
          `Between-cluster / total SS` = c$between_ss_ratio, `Within-cluster SS` = c$tot_withinss,
          `Smallest cluster` = c$min_cluster_size, Note = c$note, check.names = FALSE
        ),
        decimal = c("Mean silhouette", "Between-cluster / total SS", "Within-cluster SS"),
        integer = "Smallest cluster", dom = "t"
      )
    })

    output$alt_profile <- DT::renderDT({
      p <- seg$candidate_profiles
      format_profile(profile_table(p[p$k == as.integer(input$alt_k), ]))
    })

    customer_table <- function() {
      data.frame(
        CustomerID = cust$CustomerID, segment = cust$segment, country = cust$country,
        recency_days = cust$recency, frequency_invoices = cust$frequency,
        historical_purchase_value = round(cust$monetary, 2),
        average_invoice_value = round(cust$avg_invoice_value, 2),
        first_purchase = cust$first_purchase, last_purchase = cust$last_purchase,
        distinct_purchase_dates = cust$n_purchase_dates,
        typical_interval_days = cust$typical_interval
      )
    }

    output$customers <- DT::renderDT({
      t <- customer_table()
      names(t) <- c(
        "Customer", "Segment", "Country", "Recency (days)", "Frequency (invoices)",
        "Historical value", "Average invoice value", "First purchase", "Last purchase",
        "Purchase dates", "Typical interval (days)"
      )
      rp_dt(t,
        currency = c("Historical value", "Average invoice value"),
        decimal = "Typical interval (days)", page_length = 15
      )
    })

    output$download_customers <- shiny::downloadHandler(
      filename = function() "retailpulse_customer_segments.csv",
      content = function(file) utils::write.csv(customer_table(), file, row.names = FALSE, na = "")
    )

    rc <- res$rule_comparison
    output$rules_intro <- shiny::renderUI({
      if (is.null(rc)) return(rp_note("The rule-based comparison could not be computed."))
      rp_note(
        "Extension: a transparent rule-based alternative. Each customer receives R, F and M scores of 1–5 from quintiles ",
        "(R reversed so 5 = most recent; tied values share a score), then a group from the R score and the average of F and M. ",
        shiny::strong(sprintf("Adjusted Rand Index between the rule groups and the K-Means segments: %.3f", rc$adjusted_rand)),
        " (1 = identical partitions, about 0 = no more agreement than chance). The two methods answer different questions: ",
        "rules impose fixed, explainable cut-offs; K-Means finds groups from the data's own structure."
      )
    })
    output$rule_profile <- DT::renderDT({
      shiny::req(rc)
      p <- rc$rule_profile
      rp_dt(
        stats::setNames(p, c("Rule group", "Customers", "Median recency", "Median frequency", "Median value", "Total value", "Share of customers")),
        currency = c("Median value", "Total value"), integer = c("Customers", "Median recency"),
        percent = "Share of customers", dom = "t"
      )
    })
    output$crosstab <- shiny::renderTable(
      {
        shiny::req(rc)
        data.frame(`Rule group` = rownames(rc$crosstab), rc$crosstab, check.names = FALSE)
      },
      digits = 0
    )
  })
}
