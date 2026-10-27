# Methodology and Data Quality tab ----------------------------------------------

mod_methodology_ui <- function(id, res) {
  ns <- shiny::NS(id)
  cfg <- res$config
  a <- res$raw_audit
  s <- res$cleaning$summary
  seg <- res$segmentation
  q <- res$customers$cadence_qualified
  status_counts <- table(res$customers$cadence_status)
  partial <- res$monthly$label[res$monthly$partial]

  section <- function(title, ...) {
    bslib::card(bslib::card_header(title), bslib::card_body(...))
  }

  bslib::layout_columns(
    col_widths = c(6, 6),
    shiny::div(
      section(
        "1. Data source and reference date",
        shiny::p(
          "UCI Machine Learning Repository, ",
          shiny::a("Online Retail (id 352)", href = "https://archive.ics.uci.edu/dataset/352/online+retail", target = "_blank"),
          sprintf(
            ": %s transaction lines from one UK-based online retailer, %s to %s, %s countries. File: %s (MD5 %s).",
            rp_fmt_int(a$n_rows), rp_fmt_date(a$first_timestamp), rp_fmt_date(a$last_timestamp),
            a$n_countries, res$source$path %||% cfg$data_path, res$source$md5 %||% "n/a"
          )
        ),
        shiny::p(sprintf(
          "Analysis reference date: %s, the calendar day after the latest timestamp in the workbook (%s), fixed before any exclusion. All recency and inactivity values are measured from this date.",
          rp_fmt_date(res$reference_date), format(a$last_timestamp, "%d %b %Y %H:%M")
        )),
        if (length(partial)) shiny::p(shiny::strong(paste0(partial, " is a partial month; monthly comparisons should not treat it as complete.")))
      ),
      section(
        "2. Cleaning and exclusion policy",
        shiny::p(
          "Every row receives exactly one status, the first reason that applies in this order. ",
          "Overlapping conditions (for example a cancellation with no customer ID) are counted under the first reason only."
        ),
        rp_dt_output(ns("funnel")),
        shiny::tags$ul(
          shiny::tags$li("Cancellation records (invoice starting with C) and adjustment entries (starting with A) are never treated as purchases."),
          shiny::tags$li(sprintf(
            "Cancellation matching: each cancellation line is matched one-to-one to an earlier eligible purchase line with the same CustomerID, StockCode, absolute quantity and unit price (tolerance %g), timestamped strictly before the cancellation. If several qualify, the most recent unmatched line is used (equal timestamps: later row). %s of %s cancellation lines matched (%s with several candidates); %s purchase lines worth %s were excluded.",
            cfg$price_tolerance, rp_fmt_int(s$cancellations_matched), rp_fmt_int(s$cancellation_lines),
            rp_fmt_int(s$cancellations_ambiguous), rp_fmt_int(s$matched_rows_removed), rp_fmt_gbp(s$matched_value_removed)
          )),
          shiny::tags$li(sprintf(
            "Unmatched cancellations (%s) are left unresolved: partial cancellations, price changes and purchases made before December 2010 cannot be linked reliably, so no purchase is removed for them. Purchase value may therefore remain slightly overstated.",
            rp_fmt_int(s$cancellations_unmatched)
          )),
          shiny::tags$li(sprintf(
            "Non-product lines are excluded by exact, normalised StockCode match: %s. %s lines worth %s were excluded. No description-based filtering is applied.",
            paste(cfg$non_product_codes, collapse = ", "), rp_fmt_int(s$non_product_rows_removed), rp_fmt_gbp(s$non_product_value_removed)
          ))
        ),
        shiny::h6("Cancellation matching outcomes"),
        shiny::tableOutput(ns("matching")),
        shiny::h6("Non-product codes"),
        shiny::tableOutput(ns("non_product")),
        shiny::div(
          class = "d-flex gap-2",
          shiny::downloadButton(ns("download_quality"), "Data-quality table (CSV)", class = "btn-sm btn-outline-primary"),
          shiny::downloadButton(ns("download_matching"), "Cancellation audit (CSV)", class = "btn-sm btn-outline-primary")
        )
      )
    ),
    shiny::div(
      section(
        "3. RFM features",
        shiny::tags$ul(
          shiny::tags$li("Recency: calendar days from the customer's last eligible purchase date to the reference date (lower is more recent)."),
          shiny::tags$li("Frequency: number of distinct eligible purchase invoices (not product lines)."),
          shiny::tags$li("Monetary: sum of quantity \u00d7 unit price over eligible lines. This is eligible historical purchase value, not profit, net revenue or lifetime value.")
        ),
        shiny::p(sprintf(
          "%s customers with at least one eligible purchase are analysed; rows without a CustomerID (%s in the workbook) cannot be attributed and are excluded rather than imputed.",
          rp_fmt_int(nrow(res$customers)), rp_fmt_int(a$n_missing_customer)
        ))
      ),
      section(
        "4. K-Means segmentation",
        shiny::tags$ul(
          shiny::tags$li("Features: recency, frequency, monetary. All three are right-skewed (see the RFM distributions tab), so log1p is applied, then each is standardised to mean 0 and SD 1."),
          shiny::tags$li(sprintf(
            "K-Means (base R kmeans, Hartigan-Wong) for k = %d to %d, nstart = %d, iter.max = %d, seed %d for every fit.",
            min(cfg$k_range), max(cfg$k_range), cfg$nstart, cfg$iter_max, cfg$seed
          )),
          shiny::tags$li(sprintf(
            "Selection: %s. Candidates with a cluster smaller than %d customers are treated as degenerate. Selected k = %d (mean silhouette %.3f).",
            tolower(seg$selection_rule), cfg$min_cluster_size, seg$k, seg$silhouette
          )),
          shiny::tags$li("Segments are profiled in original units and named from their medians relative to the median customer. Cluster numbers carry no meaning; display order is by median value."),
          shiny::tags$li(sprintf(
            "Segmentation view: every valid k is fitted once and kept. The selector in the top bar switches the whole dashboard to another k (for example k = 4 for finer business groups); the statistical choice (k = %d) stays the default. 'Business utility: compare k' on the Segments tab sets separation against usefulness: segment sizes, value concentration, lapse-signal rates, groups with a distinct suggested use, and how segments split between the two views.",
            seg$k
          ))
        )
      ),
      section(
        "5. Purchase-lapse signals",
        shiny::tags$ul(
          shiny::tags$li("Typical purchase interval = median of the positive day gaps between a customer's distinct purchase dates."),
          shiny::tags$li("Inactivity ratio = days since last purchase \u00f7 typical purchase interval."),
          shiny::tags$li(sprintf(
            "Personalized rule (default): ratio \u2265 %.1f. Requires \u2265 %d invoices, \u2265 %d distinct purchase dates and \u2265 %d intervals; %s customers qualify.",
            cfg$ratio_threshold, cfg$min_invoices, cfg$min_purchase_dates, cfg$min_intervals, rp_fmt_int(sum(q))
          )),
          shiny::tags$li(sprintf(
            "Fixed rule (default): days since last purchase \u2265 %d. Used as the fallback for the %s customers without enough history (%s).",
            cfg$fixed_days, rp_fmt_int(sum(!q)),
            paste(sprintf("%s: %s", names(status_counts)[names(status_counts) != "Sufficient history"],
              rp_fmt_int(status_counts[names(status_counts) != "Sufficient history"])), collapse = "; ")
          )),
          shiny::tags$li("Comparison A applies both rules to the same qualified customers. Comparison B evaluates the full hybrid policy on all customers."),
          shiny::tags$li("Both thresholds are adjustable on the Lapse Signals tab. They are starting heuristics, not calibrated or industry-standard values.")
        )
      ),
      section(
        "6. Limitations",
        shiny::tags$ul(
          shiny::tags$li("Historical data from a single retailer covering about 12 months; results describe this period, not current customers."),
          shiny::tags$li("No verified churn labels: lapse signals cannot be scored with precision, recall or accuracy, and are not predictions."),
          shiny::tags$li("Purchase value is gross eligible spend; it is not profit and does not equal recoverable revenue."),
          shiny::tags$li(sprintf("About %s of rows lack a CustomerID (guest or unrecorded buyers), so customer coverage is incomplete.", rp_fmt_pct(a$n_missing_customer / a$n_rows, 0))),
          shiny::tags$li("Customers first seen late in the period have short histories and mostly fall under the fallback rule."),
          shiny::tags$li("Some cancellations could not be linked to their original purchase lines (see section 2)."),
          shiny::tags$li("Segment names are interpretations of cluster medians; K-Means assumes roughly spherical groups in the transformed space, and silhouette indicates only moderate separation."),
          shiny::tags$li("The dataset records no marketing actions, so it cannot show whether contacting flagged customers changes behaviour.")
        )
      )
    )
  )
}

mod_methodology_server <- function(id, res) {
  shiny::moduleServer(id, function(input, output, session) {
    output$funnel <- DT::renderDT({
      f <- res$cleaning$funnel
      rp_dt(
        data.frame(
          Order = f$step, Status = f$status, Rows = f$rows,
          `Share of rows` = f$share_of_rows, `Purchase value` = f$purchase_value,
          check.names = FALSE
        ),
        integer = "Rows", percent = "Share of rows", currency = "Purchase value", dom = "t"
      )
    })
    output$matching <- shiny::renderTable({
      a <- res$cleaning$cancellation_audit
      t <- as.data.frame(table(Outcome = a$status), stringsAsFactors = FALSE)
      amb <- as.data.frame(table(Outcome = paste0("  of which: ", a$ambiguity[a$status == "Matched"])), stringsAsFactors = FALSE)
      out <- rbind(t, amb)
      names(out) <- c("Outcome", "Cancellation lines")
      out$`Cancellation lines` <- rp_fmt_int(out$`Cancellation lines`)
      out
    })
    output$non_product <- shiny::renderTable({
      n <- res$cleaning$non_product
      data.frame(
        StockCode = n$StockCode, Description = n$description,
        `All rows` = rp_fmt_int(n$all_rows), `Eligible rows` = rp_fmt_int(n$eligible_rows),
        `Excluded rows` = rp_fmt_int(n$excluded_rows), `Excluded value` = rp_fmt_gbp(n$excluded_value),
        check.names = FALSE
      )
    })
    output$download_quality <- shiny::downloadHandler(
      filename = function() "retailpulse_data_quality.csv",
      content = function(file) utils::write.csv(res$data_quality, file, row.names = FALSE)
    )
    output$download_matching <- shiny::downloadHandler(
      filename = function() "retailpulse_cancellation_audit.csv",
      content = function(file) utils::write.csv(res$cleaning$cancellation_audit, file, row.names = FALSE, na = "")
    )
  })
}
