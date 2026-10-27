# Customer Segments tab ---------------------------------------------------------
#
# Every segment output follows the segmentation view chosen in the top bar
# (`view`, a reactive returning rp_segment_view()).

mod_segments_ui <- function(id, res) {
  ns <- shiny::NS(id)
  seg <- res$segmentation
  valid_k <- seg$candidates$k[seg$candidates$valid]
  compare_k <- if (4 %in% valid_k && seg$k != 4) 4 else setdiff(valid_k, seg$k)[1]

  bslib::navset_card_tab(
    full_screen = TRUE,
    bslib::nav_panel(
      "Segment profiles",
      shiny::uiOutput(ns("profile_note")),
      rp_dt_output(ns("profile")),
      shiny::plotOutput(ns("boxplots"), height = "340px")
    ),
    bslib::nav_panel(
      "Business utility: compare k",
      rp_note(
        "Silhouette tells you how cleanly the data separates; it does not tell you whether the segments are useful. ",
        "This view puts two segmentations side by side: what each segment contains, how its lapse-signal rate differs, ",
        "where its customers came from in the other view, and what the profile suggests doing. ",
        "Suggested uses are interpretations of the profiles, not effects measured in this data. ",
        "Lapse rates use the thresholds currently set on the Lapse Signals tab."
      ),
      shiny::div(
        class = "d-flex gap-3 align-items-end",
        shiny::selectInput(ns("k_a"), "Compare k =", choices = valid_k, selected = seg$k, width = "140px"),
        shiny::selectInput(ns("k_b"), "with k =", choices = valid_k, selected = compare_k, width = "140px"),
        shiny::downloadButton(ns("download_utility"), "Download comparison (CSV)", class = "btn-sm btn-outline-primary mb-3")
      ),
      bslib::layout_columns(
        col_widths = c(5, 7),
        shiny::div(shiny::h6("Utility measures"), shiny::tableOutput(ns("utility_summary"))),
        shiny::div(shiny::h6("What the comparison shows"), shiny::uiOutput(ns("insights")))
      ),
      shiny::plotOutput(ns("utility_plot"), height = "360px"),
      shiny::h6("Segments and suggested use"),
      rp_dt_output(ns("utility_segments")),
      shiny::h6(shiny::textOutput(ns("crosswalk_title"), inline = TRUE)),
      shiny::tableOutput(ns("crosswalk"))
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
        sprintf("The statistical selection rule (%s) picks k = %d. ", tolower(seg$selection_rule), seg$k),
        "It is one aid to choosing k, not proof that segments are commercially meaningful: see 'Business utility: compare k', ",
        "and use the Segmentation view selector in the top bar to switch the whole dashboard to another k."
      )
    ),
    bslib::nav_panel(
      "Customer table",
      shiny::div(
        class = "d-flex justify-content-between align-items-center mb-2",
        rp_note("One row per identifiable customer, with segments under the current view. Search by customer ID, segment or country."),
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

mod_segments_server <- function(id, res, view, lapse_state) {
  shiny::moduleServer(id, function(input, output, session) {
    seg <- res$segmentation
    cust <- shiny::reactive(view()$customers)
    colors <- shiny::reactive(rp_segment_colors(view()$profile$segment))

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

    output$profile_note <- shiny::renderUI({
      v <- view()
      rp_note(
        sprintf(
          "Current view: k = %d (mean silhouette %.3f)%s. ", v$k, v$silhouette,
          if (v$is_selected) ", the statistically selected solution" else sprintf("; the statistically selected solution is k = %d", seg$k)
        ),
        "Segments are numbered by descending median purchase value for display; the numbers are not a ranking of customer quality. ",
        "Names are derived from each segment's median recency, frequency and value compared with the median customer.",
        if (any(duplicated(v$profile$segment_name))) {
          shiny::tags$ul(shiny::tags$li("Two or more segments share a descriptive name; they differ in degree (see the medians)."))
        }
      )
    })

    output$profile <- DT::renderDT({
      rp_dt(profile_table(view()$profile),
        currency = c("Median value", "Mean value", "Total value"),
        integer = c("Customers", "Median recency (days)"),
        decimal = c("Mean recency", "Median frequency", "Mean frequency"),
        percent = c("Share of customers", "Share of value"), dom = "t"
      )
    })

    output$boxplots <- shiny::renderPlot(res = 96, {
      x <- cust()
      d <- rbind(
        data.frame(segment = x$segment, feature = "Recency (days)", value = x$recency),
        data.frame(segment = x$segment, feature = "Frequency (invoices, log scale)", value = x$frequency),
        data.frame(segment = x$segment, feature = "Monetary (£, log scale)", value = x$monetary)
      )
      d$value_plot <- ifelse(grepl("log", d$feature), log10(d$value), d$value)
      ggplot2::ggplot(d, ggplot2::aes(value_plot, segment, fill = segment)) +
        ggplot2::geom_boxplot(outlier.alpha = 0.25, outlier.size = 0.8, width = 0.6) +
        ggplot2::facet_wrap(~feature, scales = "free_x") +
        ggplot2::scale_y_discrete(limits = rev) +
        ggplot2::scale_fill_manual(values = colors(), guide = "none") +
        # The log-scale panels hold log10 values (all below 7); label them in
        # original units.
        ggplot2::scale_x_continuous(labels = function(x) {
          if (max(x, na.rm = TRUE) < 7) scales::label_comma(accuracy = 1)(10^x) else scales::label_comma()(x)
        }) +
        ggplot2::labs(x = NULL, y = NULL) +
        rp_gg_theme(11)
    })

    # Business utility comparison ----------------------------------------------
    ks <- shiny::reactive({
      shiny::req(input$k_a, input$k_b)
      unique(as.integer(c(input$k_a, input$k_b)))
    })
    utility <- shiny::reactive({
      rp_segmentation_utility(seg, res$customers, ks(), lapse_state$ratio(), lapse_state$days())
    })

    output$utility_summary <- shiny::renderTable({
      u <- utility()$summary
      wide <- data.frame(Measure = unique(u$measure), check.names = FALSE)
      for (k in ks()) {
        wide[[paste0("k = ", k)]] <- u$display[u$k == k][match(wide$Measure, u$measure[u$k == k])]
      }
      wide
    })

    output$insights <- shiny::renderUI({
      if (length(ks()) < 2) return(rp_note("Choose two different values of k to compare."))
      items <- rp_segmentation_insights(seg, res$customers, ks()[1], ks()[2], utility())
      shiny::tags$ul(class = "small", lapply(items, shiny::tags$li))
    })

    output$utility_plot <- shiny::renderPlot(res = 96, {
      s <- utility()$segments
      s$panel <- factor(paste0("k = ", s$k), levels = paste0("k = ", sort(unique(s$k))))
      d <- rbind(
        data.frame(panel = s$panel, segment = s$segment, measure = "Share of customers", value = s$share_customers),
        data.frame(panel = s$panel, segment = s$segment, measure = "Share of purchase value", value = s$share_value),
        data.frame(panel = s$panel, segment = s$segment, measure = "Lapse-signal rate", value = s$flagged_share)
      )
      d$measure <- factor(d$measure, levels = c("Share of customers", "Share of purchase value", "Lapse-signal rate"))
      d$segment <- factor(d$segment, levels = rev(unique(s$segment[order(s$k, s$segment_id)])))
      ggplot2::ggplot(d, ggplot2::aes(value, segment, fill = measure)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
        ggplot2::geom_text(ggplot2::aes(label = rp_fmt_pct(value, 0)),
          position = ggplot2::position_dodge(width = 0.8), hjust = -0.15, size = 3
        ) +
        ggplot2::facet_wrap(~panel, scales = "free_y", ncol = 2) +
        ggplot2::scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1.1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
        ggplot2::scale_fill_manual(values = c(
          "Share of customers" = RP_COLORS$partial, "Share of purchase value" = RP_COLORS$primary,
          "Lapse-signal rate" = RP_COLORS$personalized
        )) +
        ggplot2::labs(x = NULL, y = NULL) +
        rp_gg_theme(11)
    })

    output$utility_segments <- DT::renderDT({
      s <- utility()$segments
      rp_dt(
        data.frame(
          k = s$k, Segment = s$segment, Customers = s$customers, `Share of customers` = s$share_customers,
          `Median recency` = s$median_recency, `Median frequency` = s$median_frequency,
          `Median value` = s$median_monetary, `Share of value` = s$share_value,
          `Lapse-signal rate` = s$flagged_share, `Flagged value` = s$flagged_value,
          `Suggested use (interpretation)` = s$suggested_use, check.names = FALSE
        ),
        currency = c("Median value", "Flagged value"), integer = c("Customers", "Median recency"),
        percent = c("Share of customers", "Share of value", "Lapse-signal rate"),
        dom = "t", page_length = 20, wide = "Suggested use (interpretation)"
      )
    })

    output$crosswalk_title <- shiny::renderText({
      if (length(ks()) < 2) return("")
      sprintf("Where the k = %d segments come from in the k = %d view (customers)", max(ks()), min(ks()))
    })
    output$crosswalk <- shiny::renderTable(
      {
        shiny::req(length(ks()) == 2)
        cw <- rp_segment_crosswalk(seg, res$customers, min(ks()), max(ks()))
        wide <- tidy_crosswalk(cw)
        wide
      },
      digits = 0
    )
    tidy_crosswalk <- function(cw) {
      froms <- sort(unique(cw$from))
      tos <- sort(unique(cw$to))
      out <- data.frame(Segment = tos, check.names = FALSE)
      for (f in froms) {
        out[[paste("from", f)]] <- vapply(tos, function(t) {
          n <- cw$customers[cw$to == t & cw$from == f]
          if (length(n)) rp_fmt_int(n) else "0"
        }, character(1))
      }
      out
    }

    output$download_utility <- shiny::downloadHandler(
      filename = function() paste0("retailpulse_segmentation_k", paste(ks(), collapse = "_vs_k"), ".csv"),
      content = function(file) {
        out <- utility()$segments
        out$ratio_threshold <- lapse_state$ratio()
        out$fixed_days_threshold <- lapse_state$days()
        utils::write.csv(out, file, row.names = FALSE)
      }
    )

    # RFM distributions and model selection (independent of the view) ---------
    output$rfm_summary <- DT::renderDT({
      s <- res$rfm_summary
      rp_dt(
        stats::setNames(s, c("Feature", "N", "Mean", "SD", "Min", "P5", "P25", "Median", "P75", "P95", "Max", "Skewness", "Skewness after log1p")),
        integer = "N", decimal = c("Mean", "SD", "Min", "P5", "P25", "Median", "P75", "P95", "Max", "Skewness", "Skewness after log1p"),
        dom = "t"
      )
    })

    output$histograms <- shiny::renderPlot(res = 96, {
      x <- res$customers
      d <- rbind(
        data.frame(feature = "Recency (days)", value = x$recency),
        data.frame(feature = "Frequency (log scale)", value = log10(x$frequency)),
        data.frame(feature = "Monetary £ (log scale)", value = log10(x$monetary))
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
      ggplot2::ggplot(cust(), ggplot2::aes(recency, monetary, colour = segment)) +
        ggplot2::geom_point(alpha = 0.45, size = 1) +
        ggplot2::scale_y_log10(labels = scales::label_dollar(prefix = "£")) +
        ggplot2::scale_colour_manual(values = colors()) +
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
      v <- view()$k
      c$role <- ifelse(c$k == seg$k, "Statistically selected", ifelse(c$k == v, "Current view", "Other"))
      ggplot2::ggplot(c[c$valid, ], ggplot2::aes(k, silhouette)) +
        ggplot2::geom_line(colour = RP_COLORS$muted) +
        ggplot2::geom_point(ggplot2::aes(colour = role), size = 3) +
        ggplot2::scale_colour_manual(values = c(
          "Statistically selected" = RP_COLORS$personalized, "Current view" = RP_COLORS$fallback, "Other" = RP_COLORS$primary
        )) +
        ggplot2::scale_x_continuous(breaks = c$k) +
        ggplot2::labs(x = "Number of clusters (k)", y = "Mean silhouette width") +
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

    # Customer table ------------------------------------------------------------
    customer_table <- function() {
      x <- cust()
      data.frame(
        CustomerID = x$CustomerID, segment = x$segment, segmentation_k = view()$k, country = x$country,
        recency_days = x$recency, frequency_invoices = x$frequency,
        historical_purchase_value = round(x$monetary, 2),
        average_invoice_value = round(x$avg_invoice_value, 2),
        first_purchase = x$first_purchase, last_purchase = x$last_purchase,
        distinct_purchase_dates = x$n_purchase_dates,
        typical_interval_days = x$typical_interval
      )
    }

    output$customers <- DT::renderDT({
      t <- customer_table()
      t$segmentation_k <- NULL
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
      filename = function() paste0("retailpulse_customer_segments_k", view()$k, ".csv"),
      content = function(file) utils::write.csv(customer_table(), file, row.names = FALSE, na = "")
    )

    # Rule-based RFM comparison (against the current view) ----------------------
    rc <- shiny::reactive(tryCatch(rp_compare_rfm_rules(cust()), error = function(e) NULL))
    output$rules_intro <- shiny::renderUI({
      r <- rc()
      if (is.null(r)) return(rp_note("The rule-based comparison could not be computed."))
      rp_note(
        "Extension: a transparent rule-based alternative. Each customer receives R, F and M scores of 1–5 from quintiles ",
        "(R reversed so 5 = most recent; tied values share a score), then a group from the R score and the average of F and M. ",
        shiny::strong(sprintf("Adjusted Rand Index between the rule groups and the K-Means segments (k = %d): %.3f", view()$k, r$adjusted_rand)),
        " (1 = identical partitions, about 0 = no more agreement than chance). The two methods answer different questions: ",
        "rules impose fixed, explainable cut-offs; K-Means finds groups from the data's own structure."
      )
    })
    output$rule_profile <- DT::renderDT({
      shiny::req(rc())
      p <- rc()$rule_profile
      rp_dt(
        stats::setNames(p, c("Rule group", "Customers", "Median recency", "Median frequency", "Median value", "Total value", "Share of customers")),
        currency = c("Median value", "Total value"), integer = c("Customers", "Median recency"),
        percent = "Share of customers", dom = "t"
      )
    })
    output$crosstab <- shiny::renderTable(
      {
        shiny::req(rc())
        data.frame(`Rule group` = rownames(rc()$crosstab), rc()$crosstab, check.names = FALSE)
      },
      digits = 0
    )
  })
}
