# RetailPulse

**Customer Segmentation and Personalized Purchase-Lapse Intelligence**

RetailPulse is an R/Shiny analytics application built on the UCI Online Retail
dataset. It turns raw transaction lines into customer-level evidence:

- RFM (recency, frequency, monetary) profiles for each customer.
- K-Means behavioural segments.
- Explainable purchase-lapse signals that compare each customer's current
  inactivity with their own purchase rhythm.

It is an undergraduate MLDA course project. It favours transparent, defensible
methods over complexity, and every number in the app is computed from the
supplied data.

---

## 1. Problem

A retailer can hold a year of transactions and still be unable to answer simple
questions:
- Which customers carry most of the purchase value?
- How do customers group by behaviour?
- Who has gone unusually quiet?

A fixed rule such as "no purchase for 60 days" treats a weekly buyer and a
bimonthly buyer the same. RetailPulse adds a **personalized** check. It divides
each customer's days since their last purchase by their own median gap between
purchases, then flags customers who are well past their usual rhythm. It also
compares this rule with the fixed rule on the same customers.

The flags are a heuristic to support decisions. They are **not** a validated
churn model: the dataset has no churn labels.

## 2. Core features

| Area | What it does |
|---|---|
| Ingestion and data quality | Reads the workbook and validates its columns and types. Every row receives exactly one documented status (an exclusion funnel). |
| Cancellation matching | Matches each cancellation one-to-one to the earlier purchase line it reverses, using the timestamps, and excludes only matched lines. Every match and non-match is recorded. |
| Non-product exclusion | Removes postage, carriage, manual, bank-charge and similar lines by exact StockCode match. |
| RFM features | Customer recency, frequency (distinct invoices) and eligible historical purchase value, with descriptive statistics. |
| K-Means segmentation | log1p transform and standardisation, then k = 2 to 6 with a fixed seed. k is chosen by silhouette width, and segments are profiled and named from their medians. |
| Segmentation view and business utility | A switch in the top bar re-segments the whole dashboard at any valid k (for example k = 4); k = 2 stays the default. A "Business utility: compare k" tab sets two views side by side: value concentration, lapse-rate separation, suggested use per segment, how segments split, and generated findings. |
| Purchase-lapse signals | A personalized inactivity-ratio rule, a fixed-day rule and a hybrid policy, with a plain-language explanation for each customer. |
| Evaluation | Comparison A (both rules on the same population), Comparison B (the hybrid policy on all customers), threshold sensitivity, cluster diagnostics and data-quality counts. |
| Dashboard | Four tabs: Overview, Customer Segments, Lapse Signals (adjustable thresholds, filters, CSV export) and Methodology & Data Quality. |
| Extension | Rule-based RFM quintile groups compared with the K-Means segments (cross-tabulation and Adjusted Rand Index). |

## 3. Dataset

- **Source:** UCI Machine Learning Repository, *Online Retail* (id 352),
  <https://archive.ics.uci.edu/dataset/352/online+retail>.
- **Contents:** 541,909 transaction lines from one UK-based online retailer,
  covering 1 Dec 2010 to 9 Dec 2011, with 8 columns: `InvoiceNo`, `StockCode`,
  `Description`, `Quantity`, `InvoiceDate`, `UnitPrice`, `CustomerID` and
  `Country`.
- **Location in this project:** `data/raw/online_retail.xlsx`. This is the
  workbook from the supplied `online+retail.zip`, unchanged. It is set to
  read-only, and its MD5 checksum is shown in the app.
- **No churn labels and no marketing-intervention data.** The coverage is
  historical, and December 2011 is a partial month (1–9 Dec).

## 4. Technology

The project uses R (4.1 or later, because the code uses the native `|>` pipe)
with these packages:

| Purpose | Packages |
|---|---|
| Data handling | `readxl`, `dplyr`, `lubridate` (`tidyr` was not needed: no reshaping step required it) |
| Analysis | base R `kmeans()`, `cluster` (silhouette) |
| Interface and charts | `shiny`, `bslib`, `ggplot2`, `DT`, `scales` |
| Testing | `testthat` |

There is no database, API, cloud service or LLM.

Verified with R 4.4.1 and these package versions: shiny 1.9.1, bslib 0.8.0,
DT 0.33, ggplot2 3.5.1, dplyr 1.1.4, readxl 1.4.3, testthat 3.2.1.1,
cluster 2.1.6.

## 5. Project structure

```text
RetailPulse/
├── app.R                      Shiny entry point (sources R/, loads results, wires modules)
├── R/
│   ├── config.R               all settings: paths, exclusion codes, seed, k range, thresholds
│   ├── data_io.R              workbook reading (identifiers as text, conversion warnings captured)
│   ├── data_validation.R      schema checks, type normalisation, raw audit, reference date
│   ├── data_cleaning.R        row classification, cancellation matching, exclusion funnel
│   ├── features_rfm.R         customer RFM, purchase intervals, cadence eligibility, statistics
│   ├── segmentation.R         preprocessing, K-Means candidates, k selection, profiles, RFM rules
│   ├── lapse_signals.R        personalized / fixed / hybrid rules and explanations
│   ├── evaluation.R           Comparison A and B, sensitivity grid, data-quality table
│   ├── summaries.R            monthly trend (partial months marked), segment lapse summary
│   ├── formatting.R           £, %, integer and date formatting
│   ├── pipeline.R             end-to-end pipeline, results cache, source helper
│   └── modules/               Shiny modules: overview, segments, lapse, methodology, ui_helpers
├── data/
│   ├── raw/online_retail.xlsx original workbook (read only)
│   └── processed/             derived outputs (results cache + CSV tables), regenerable
├── scripts/
│   ├── validate_data.R        audit of the workbook against the recorded audit figures
│   └── run_analysis.R         full analysis without the UI; writes data/processed/
├── tests/
│   ├── testthat.R             test runner
│   └── testthat/              fixtures and tests (validation, cleaning, RFM, segmentation,
│                              lapse rules, pipeline and Shiny modules)
├── docs/
│   ├── methodology.md         detailed method description for the report and viva
│   └── evaluation.md          actual results of the pipeline run
├── Dockerfile                 container image: R 4.4.1, packages, analysis run at build time
└── DOCKER.md                  step-by-step instructions for running in Docker locally
```

## 6. Installation

1. Install R (4.1 or later) and, optionally, RStudio.
2. Copy the whole `RetailPulse/` folder to your laptop. Keep
   `data/raw/online_retail.xlsx` where it is.
3. In R, from any directory:

```r
install.packages(c(
  "readxl", "dplyr", "lubridate", "ggplot2", "shiny",
  "bslib", "DT", "scales", "testthat"
))
# The cluster package ships with R; if it is missing: install.packages("cluster")
R.version.string   # check the R version
```

## 7. Running

> Prefer not to install R? **[DOCKER.md](DOCKER.md)** explains how to build
> and run everything in a Docker container on your PC
> (`docker build -t retailpulse .` then
> `docker run --rm -p 3838:3838 retailpulse`).

Run these from the project root (the folder that contains `app.R`). In RStudio,
use *Session → Set Working Directory → To Project Directory*, or `setwd()`.

```r
# Optional: check the workbook (about 30 s)
source("scripts/validate_data.R")

# Optional: precompute everything and write derived CSVs (about 30-60 s)
source("scripts/run_analysis.R")

# Start the dashboard
shiny::runApp()

# Run the tests
source("tests/testthat.R")
```

From a terminal, the equivalent commands are `Rscript scripts/run_analysis.R`,
`Rscript -e "shiny::runApp()"` and `Rscript tests/testthat.R`.

The first time the app starts without a cache, it reads the workbook and runs
the pipeline, which takes about a minute. The results go to
`data/processed/pipeline_results.rds` and are reused while the workbook, the
analytical code in `R/*.R` and the configuration stay the same. If any of those
change, the cache is rebuilt automatically. If the workbook is missing, the app
shows an explanatory error and does not substitute other data.

## 8. Analytical methods (summary)

See `docs/methodology.md` for full detail.

1. **Reference date.** The day after the latest timestamp in the workbook
   (**10 Dec 2011**). It is fixed before any exclusion, and recency never uses
   today's date.
2. **Eligibility.** A row is eligible when:
   - its `CustomerID` is present;
   - its invoice number does not start with `C` (cancellation) or `A`
     (adjustment);
   - quantity is above 0 and unit price is above 0;
   - it is not a purchase line matched to a later cancellation;
   - it is not a non-product code (`POST`, `M`, `DOT`, `C2`, `BANK CHARGES`,
     `PADS`).
3. **RFM.**
   - Recency: days from the last purchase date to the reference date.
   - Frequency: number of distinct invoices.
   - Monetary: sum of quantity × unit price (eligible historical purchase
     value).
4. **K-Means.** log1p, then z-scores, then `kmeans()` for k = 2 to 6
   (`nstart = 25`, seed 42). The k with the highest mean silhouette is chosen,
   with ties going to the smaller k. Segments are profiled in original units.
5. **Lapse rules.**
   - Typical interval: the median gap between a customer's distinct purchase
     dates.
   - Inactivity ratio: recency ÷ typical interval.
   - Personalized signal: ratio ≥ 2.0. It requires at least 3 invoices and at
     least 3 distinct purchase dates.
   - Fixed signal: recency ≥ 60 days.
   - Hybrid policy: the personalized rule where history is sufficient, and the
     fixed rule as a fallback otherwise.

## 9. Evaluation (what is measured)

See `docs/evaluation.md` for the actual numbers.

- **Data quality:** raw diagnostics, the exclusion funnel, cancellation-matching
  outcomes and non-product lines, plus preliminary and final counts.
- **Clustering:** silhouette and between/total sum of squares for each k,
  cluster sizes, profiles, and the trade-off at other k values. There is no
  "accuracy", because there are no ground-truth labels.
- **Lapse rules:**
  - Comparison A: the personalized and fixed rules on the same qualified
    customers, with overlap, Jaccard index and the value of each group.
  - Comparison B: the hybrid policy on all customers.
  - A sensitivity grid across thresholds.

  These are sensitivity analyses, **not** precision or recall.

## 10. Course-outcome mapping

| Outcome | Evidence in the project |
|---|---|
| **CO2:** statistical analysis and visualisation | RFM descriptive statistics, skewness before and after log1p, Spearman correlations, distribution plots, monthly trend with the partial month marked, segment box plots, cadence scatter chart |
| **CO3:** analysis of data-science methods | Rationale for the transform and scaling; silhouette-based choice of k compared with business utility at k = 4 (separation against actionability); the personalized rule versus the fixed rule (Comparisons A and B, sensitivity grid); rule-based RFM versus K-Means (ARI) |
| **CO4:** a data-science solution in R | A modular R pipeline (I/O, validation, cleaning, features, models, evaluation), a Shiny app with reactive thresholds and exports, automated tests, reproducible scripts |
| CO1 (supporting) | Transaction classification policy, cancellation matching, the end-to-end workflow |

## 11. Limitations

- Data from one retailer over about 12 months. The results describe that
  period, not current customers.
- No verified churn labels, so lapse signals cannot be scored as predictions.
- Purchase value is gross eligible spend. It is not profit, net revenue or
  lifetime value.
- About 25% of rows have no `CustomerID`, so those buyers cannot be analysed.
- 6,504 of 9,288 cancellation lines could not be linked one-to-one to an
  earlier purchase line: partial cancellations, price differences, purchases
  before December 2010, or no customer ID. Purchase value may still be slightly
  overstated.
- 56% of customers do not have enough history for the personalized rule and
  are assessed with the fallback.
- Silhouette is moderate (0.43 at k = 2), so segments are broad tendencies
  rather than sharply separated groups.
- No data on marketing actions, so the project cannot show whether contacting
  flagged customers changes their behaviour.

## 12. Suggested demonstration workflow

1. **Overview:** state the scope, the reference date and the partial
   December. Walk through the KPI definitions and the data-quality indicators.
2. **Methodology & Data Quality:** explain the exclusion funnel and the
   cancellation-matching rule, including the matched £168k and £77k bulk-order
   reversals.
3. **Customer Segments → RFM distributions:** show the skew and why log1p is
   used.
4. **Customer Segments → Model selection:** explain why silhouette picks
   k = 2.
5. **Customer Segments → Business utility: compare k:** compare k = 2 with
   k = 4. Point out that k = 2's "High-value active" group hides 699 customers
   (£1.45m) who have gone quiet, that lapse rates spread from 0% to 90% at
   k = 4, and that k = 4 produces a 1,164-customer win-back-review list. Then
   set **Segmentation view = k = 4** in the top bar and show every tab update.
6. **Customer Segments → RFM rules vs K-Means:** compare the two methods using
   the ARI and the cross-tabulation.
7. **Lapse Signals → Cadence chart:** explain the ratio line and the 60-day
   line visually.
8. **Lapse Signals → A:** show that the two rules flag different customers, and
   point out the high-value "personalized only" group.
9. Move the sliders, show the numbers update, filter to one segment, read one
   explanation, and download the CSV.
10. Close on the limitations: these are signals for review, not churn
   predictions.

## 13. Verification status

- **Done in development:** the full pipeline was run on the real workbook, the
  test suite passed (207 expectations), the app was started headlessly, and all
  four tabs were rendered in a headless browser with no output errors. This used
  R 4.4.1 on Linux in a disposable Docker container (`rocker/r-ver:4.4.1`).
- **Not yet done:** a run on your own laptop's R installation.
