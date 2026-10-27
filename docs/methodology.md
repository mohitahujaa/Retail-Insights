# RetailPulse: Methodology

This document describes exactly what the code does. The function names refer
to files in `R/`. The actual results are in `evaluation.md`.

## 1. Processing sequence

```
online_retail.xlsx
  → rp_read_transactions()      read; identifiers as text; conversion warnings captured
  → rp_standardize()            schema check; normalise IDs; parse numbers/dates; flag unparseable values
  → rp_audit_raw()              raw diagnostics; analysis reference date
  → rp_clean_transactions()     classify rows; match cancellations; exclusion funnel
  → rp_customer_features()      one row per customer: R, F, M and supporting fields
  → rp_purchase_intervals() + rp_add_cadence()   gaps between purchase dates; cadence eligibility
  → rp_segment_customers()      log1p + scale → K-Means (k = 2..6) → silhouette → profiles/names
  → rp_apply_lapse_rules()      personalized / fixed / hybrid rules + explanations
  → rp_evaluate_*()             Comparison A, Comparison B, sensitivity grid
```

`rp_run_pipeline_df()` runs every step. The dashboard, `scripts/run_analysis.R`
and the tests all call this same function, so there is only one
implementation.

## 2. Reading and validation

- `InvoiceNo`, `StockCode`, `Description`, `CustomerID` and `Country` are read
  as text. This keeps cancellation prefixes such as `C536379` and alphanumeric
  stock codes intact.
- `Quantity` and `UnitPrice` are read as numbers and `InvoiceDate` as a
  date-time (UTC). Any cell that cannot be converted is counted and flagged
  `invalid_value`, and its row is excluded with the status *Invalid or
  unparseable value*. No silent coercion happens.
- Missing required columns stop the pipeline with a message naming them.
- `CustomerID` is trimmed, and a trailing `.0` from numeric cells is removed.
  Stock codes and invoice numbers get upper-cased copies (`stock_norm`,
  `invoice_norm`) for matching, and the originals are kept.
- Every row gets a stable `row_id`, which is its position in the sheet.

## 3. Analysis reference date

The reference date is `as.Date(max(InvoiceDate)) + 1`, computed on the **raw**
input before any exclusion. For this workbook it is **10 December 2011**, the
day after the last transaction at 09 Dec 2011 12:50. All recency and inactivity
values are measured from this date. The system date is never used. Keeping the
date fixed before cleaning means an exclusion can't move it.

December 2011 has only nine days of data. The monthly trend marks it as partial
(`rp_monthly_trend()`), and the dashboard says it is not comparable with full
months.

## 4. Cleaning policy

### 4.1 Row statuses

Each row receives exactly one status: the first that applies in this order.

| Order | Status | Rule |
|---|---|---|
| 1 | Invalid or unparseable value | invoice number, quantity, price or date missing or unconvertible |
| 2 | Cancellation record (C invoice) | invoice number starts with `C` |
| 3 | Adjustment entry (A invoice) | invoice number starts with `A` (bad-debt adjustments) |
| 4 | Missing CustomerID | no customer identifier |
| 5 | Non-positive quantity | `Quantity <= 0` |
| 6 | Non-positive unit price | `UnitPrice <= 0` |
| 7 | Purchase line later cancelled (matched) | see 4.2 |
| 8 | Non-product service or charge line | see 4.3 |
| 9 | Eligible | none of the above |

Rows that fail none of rules 1–6 form the **preliminary baseline**. Rows with
status 9 form the **final analytical dataset**. Overlapping conditions are
reported separately as raw diagnostics. For example, a cancellation without a
customer ID counts under "rows missing CustomerID" in the raw audit, but only
under status 2 in the funnel.

Negative quantities are not assumed to be errors. In this workbook every
negative-quantity row that has a customer ID is on a `C` invoice, so returns
are handled through the cancellation logic.

### 4.2 One-to-one cancellation matching (`rp_match_cancellations()`)

If cancellation rows are only dropped, the original purchase they reverse stays
in the data. The largest case in this workbook is an 80,995-unit, £168,470 line
that was cancelled 12 minutes after it was placed. Leaving it in would badly
distort Monetary.

**Candidate purchases:** preliminary-eligible lines.

**Cancellations:** every `C` line. Cancellations without a CustomerID cannot be
attributed to anyone and are recorded as unmatched.

A cancellation line *c* can match a purchase line *p* when all of these hold:

1. `CustomerID(p) == CustomerID(c)`
2. `stock_norm(p) == stock_norm(c)`
3. `Quantity(p) == |Quantity(c)|`
4. `|UnitPrice(p) − UnitPrice(c)| <= 1e-6`
5. `InvoiceDate(p) < InvoiceDate(c)` (strictly earlier)
6. *p* has not already been matched

Cancellations are processed in order of (timestamp, row_id). If several
purchase lines qualify, the **most recent** is selected. If they share a
timestamp, the larger `row_id` (the later row in the file) wins. A selected line
cannot be matched again. Only matched purchase lines are excluded. The rest of
the original invoice stays, and fuzzy matching or inferred partial cancellations
are not attempted.

The audit table (`cancellation_audit`, exported as CSV) records one row per
cancellation line with these fields:
- cancellation row and invoice, customer, stock code, quantity, price and time;
- status: matched, or the reason it is unmatched;
- the number of qualifying candidates;
- an ambiguity note: single candidate, most recent selected, or tie resolved by
  row order;
- the matched purchase row, invoice, time and value.

There are two unmatched reasons:
- *no earlier line with the same quantity and price*. This usually means a
  partial cancellation or a price difference.
- *no earlier purchase of this item by this customer*. This usually means the
  original purchase came before December 2010.

### 4.3 Non-product lines

These are excluded by exact match on the normalised `StockCode`, set in
`R/config.R`:

| Code | Meaning (most common description) |
|---|---|
| `POST` | postage |
| `M` | manual entry |
| `DOT` | DOTCOM postage |
| `C2` | carriage |
| `BANK CHARGES` | bank charges |
| `PADS` | "PADS TO MATCH ALL CUSHIONS" (3 eligible lines, total £0.003) |

Exact matching means a product code such as `POSTCARD` is never removed by
accident. No description-based filtering is applied. Other service codes in the
workbook (`D` discount, `S` samples, `AMAZONFEE`, `CRUK`, `B`,
`DCGSSBOY`/`DCGSSGIRL`, `GIFT_0001_*`) appear only on rows already excluded by
rules 1–6. After cleaning, every eligible line has a stock code beginning with
five digits, which was checked on the actual data.

## 5. RFM features (`rp_customer_features()`)

For each customer *i*, using final eligible lines only:

- **Recency** `R_i = reference_date − last_purchase_date_i`, in calendar days.
  It is at least 1 by construction.
- **Frequency** `F_i` = the number of **distinct invoices**. A 30-line order
  counts once.
- **Monetary** `M_i = Σ Quantity × UnitPrice`, which is *eligible historical
  purchase value*. It is gross spend after the exclusions above. It is not
  profit, net revenue or customer lifetime value.

Supporting fields are first and last purchase date, distinct purchase dates,
line count, average invoice value and the modal country. Rows without a
CustomerID are never imputed or pooled into a synthetic customer.

`rp_describe()` reports the mean, SD, minimum, 5th/25th/50th/75th/95th
percentiles, maximum and moment skewness, both raw and after `log1p`.
`rp_rfm_correlations()` reports Spearman rank correlations, which are robust to
the heavy skew.

## 6. K-Means segmentation (`R/segmentation.R`)

### 6.1 Preprocessing

All three RFM features are strongly right-skewed (see `evaluation.md`).
K-Means uses Euclidean distance, so without a transform a handful of very large
customers would dominate the fit. The steps are:

1. `log1p(x)` for each feature. All values are non-negative, and log1p handles
   0 safely.
2. Standardise to mean 0 and SD 1 (`scale()`), so each feature carries equal
   weight.
3. Stop with an error on missing or non-finite values, negative inputs or
   zero-variance features.

### 6.2 Fitting and selection

- `kmeans()` uses the Hartigan-Wong algorithm, with `nstart = 25`,
  `iter.max = 100` and `set.seed(42)` before **every** k, for k = 2 to 6.
- A candidate is **invalid** if k is not smaller than the number of customers,
  if K-Means fails, or if any cluster has fewer than 10 customers.
- **Mean silhouette width** is computed with `cluster::silhouette()` on the full
  Euclidean distance matrix. For more than 8,000 customers it would use a
  reproducible sample instead; this dataset has 4,324 customers, so no sampling
  is applied.
- **Selection rule:** the highest mean silhouette among valid candidates, with
  ties within 1e-9 going to the smaller k. `k_override` in `config.R` can force
  a valid k. It is not used.
- The profiles of every valid k are kept (`candidate_profiles`), so the
  trade-off between separation and granularity can be discussed. Only the
  selected k is used for the rest of the analysis.

### 6.3 Profiling and naming

K-Means cluster numbers are arbitrary. For display, segments are renumbered
S1…Sk in descending order of median monetary value. This order is a
presentation choice, not a quality ranking. Each segment is profiled in
original units: the customer count and share; the median and mean of R, F and
M; total value; and share of value.

Names come from a transparent rule that compares each segment's medians with
the overall customer medians:

| Recent? (median R ≤ overall) | Frequent? (median F > overall) | Higher value? (median M > overall) | Name |
|---|---|---|---|
| yes | yes | yes | High-value active customers |
| yes | yes | no | Recent frequent, lower-value buyers |
| yes | no | yes | Recent occasional, higher-value buyers |
| yes | no | no | Recent, low-activity customers |
| no | yes, or higher value | | Historically valuable, currently inactive |
| no | no | no | Infrequent, inactive customers |

The names are interpretations of observed medians, not predefined categories.
Each segment's basis (its medians compared with the overall medians) is stored
alongside its name.

### 6.4 Extension: rule-based RFM compared with K-Means

`rp_rfm_rule_segments()` gives each customer quintile scores of 1–5 for R, F
and M. R is reversed so that 5 means most recent, and tied values always share
a score. Customers are then grouped by their R score and the average of their F
and M scores:

| Group | Rule |
|---|---|
| R1 | R ≥ 4 and FM ≥ 4 |
| R2 | R ≥ 3 and FM ≥ 3 |
| R3 | R ≥ 4 and FM < 3 |
| R4 | R ≤ 2 and FM ≥ 3 |
| R5 | R ≤ 2 and FM < 3 |
| R6 | the rest (R = 3 and FM < 3) |

Agreement with K-Means is measured by the **Adjusted Rand Index** (implemented
in base R in `rp_adjusted_rand()`) and shown as a cross-tabulation.

## 7. Purchase-lapse signals (`R/lapse_signals.R`)

### 7.1 Cadence

For each customer, the distinct eligible purchase **dates** are sorted and the
day gaps between consecutive dates are computed. Several invoices on one day
count as one purchase date, so every gap is a positive integer. The **typical
purchase interval** `G_i` is the median gap.

A customer **qualifies** for personalized analysis only when they have:
- at least 3 distinct invoices,
- at least 3 distinct purchase dates, and
- at least 2 positive intervals, with a median above 0.

Customers who don't qualify keep `G_i = NA`. No interval is ever imputed.

### 7.2 Rules

| Rule | Definition | Applies to |
|---|---|---|
| Personalized | `I_i = R_i / G_i ≥ ratio_threshold` (default 2.0) | qualified customers only (otherwise NA) |
| Fixed | `R_i ≥ fixed_days` (default 60) | all customers |
| Hybrid policy | personalized where qualified, otherwise fixed (labelled "fallback") | all customers |

Both comparisons are inclusive (≥), and both use the same reference date. The
thresholds are configurable in `config.R` and live in the dashboard. They are
starting heuristics, not industry standards or calibrated probabilities.

**Explanations** (`rp_explain_lapse()`) are built from the same values that
produced the decision. For example:

> Potentially overdue: inactive for 39 days compared with a median purchase
> interval of 8.0 days. Inactivity ratio 4.88 (threshold 2.00).

> Fixed-threshold signal: inactive for 183 days (threshold 60 days).
> Insufficient purchase history for personalized cadence analysis (fewer than
> 3 invoices); the 60-day fallback was applied.

The wording never says a customer has churned or will not return.

### 7.3 Evaluations

- **Comparison A** (`rp_evaluate_rules_same_population()`) uses only qualified
  customers. It reports the population, the customers flagged by each rule,
  both, only one, or neither, the Jaccard overlap
  |P ∩ F| / |P ∪ F|, and the historical value of each group.
- **Comparison B** (`rp_evaluate_hybrid_policy()`) uses all customers. It
  reports how many customers each rule evaluated, the flags from each rule, the
  total flagged and their share and value. The fixed rule applied to everyone
  is shown for reference.
- **Sensitivity** (`rp_lapse_sensitivity()`): flag counts for ratios of 1.5–4
  and for 30–180 days.

There are no churn labels, so these comparisons describe **how the rules
differ**, not which is more accurate. Precision, recall, ROC-AUC and accuracy
are deliberately not reported. Value associated with flagged customers is
descriptive exposure, not expected or recoverable revenue.

## 8. Reproducibility

- Every setting is in `R/config.R`. The seed is fixed for every K-Means fit,
  and cancellation matching is deterministic, including on ties.
- The raw workbook is read-only and never written to. Derived outputs go to
  `data/processed/` and can be regenerated with `scripts/run_analysis.R`.
- The results cache is keyed on the workbook's MD5, the MD5 of every analytical
  source file and the configuration, so stale results are never reused.

## 9. Assumptions and limitations

1. The data comes from one retailer over 12 months, so the results are
   historical and descriptive.
2. There are no churn labels or intervention records. Lapse signals are
   heuristics, not predictions, and do not show the effect of contacting
   anyone.
3. Unmatched cancellations (partial cancellations, price changes, or purchases
   before the data begins) are left in place, so purchase value may be
   slightly overstated for some customers.
4. Rows without a CustomerID (about 25%) are excluded, so guest buyers are
   invisible.
5. Customers who are new late in the period, or who buy rarely, can't have a
   reliable cadence and fall to the fixed fallback.
6. A median interval from only 2–3 gaps is noisy. Thresholds should be read as
   a prompt for review, not a verdict.
7. K-Means assumes compact, roughly spherical clusters in the transformed
   space. Silhouette is one guide among several. Segment names interpret
   medians and are not ground truth.
8. Purchase value is gross spend in GBP as recorded, with no adjustment for
   discounts, returns that could not be matched, or margin.
