# RetailPulse: Evaluation Results

**Source of these figures.** They are actual outputs of
`scripts/run_analysis.R`, run on the supplied `data/raw/online_retail.xlsx`
(MD5 `8f8e6d94ba88f976f4d8290cb2dea7fd`). The run used R 4.4.1 in a Linux
container with the default configuration in `R/config.R`. The same tables are
written to `data/processed/*.csv` and shown in the dashboard. If the code or
configuration changes, re-run the script; this document is a snapshot of that
run.

Analysis reference date: **10 December 2011**.

---

## 1. Dataset quality

### 1.1 Raw input (diagnostics; categories overlap)

| Measure | Count | Note |
|---|---:|---|
| Transaction rows | 541,909 | |
| Cells that failed type conversion | 0 | |
| Rows missing CustomerID | 135,080 | 24.9% of rows |
| Rows missing Description | 1,454 | Description is not used in any calculation |
| Cancellation rows (invoice starts with C) | 9,288 | 3,836 cancellation invoices |
| Adjustment rows (invoice starts with A) | 3 | bad-debt adjustments |
| Negative-quantity rows | 10,624 | 1,336 are not on C invoices, and none of those has a CustomerID |
| Non-positive unit price | 2,517 | 2 negative |
| Distinct invoices (all types) | 25,900 | |
| Date range | 01 Dec 2010 08:26 to 09 Dec 2011 12:50 | |

Every figure recorded in the pre-implementation audit was reproduced exactly
by `scripts/validate_data.R`.

### 1.2 Exclusion funnel (mutually exclusive; first applicable reason)

| Order | Status | Rows | Share | Purchase value |
|---:|---|---:|---:|---:|
| 1 | Invalid or unparseable value | 0 | 0.0% | |
| 2 | Cancellation record (C invoice) | 9,288 | 1.7% | |
| 3 | Adjustment entry (A invoice) | 3 | <0.1% | |
| 4 | Missing CustomerID | 134,694 | 24.9% | |
| 5 | Non-positive quantity | 0 | 0.0% | |
| 6 | Non-positive unit price | 40 | <0.1% | |
| 7 | Purchase line later cancelled (matched) | 2,784 | 0.5% | £402,896 |
| 8 | Non-product service or charge line | 1,503 | 0.3% | £135,874 |
| 9 | **Eligible** | **393,597** | 72.6% | **£8,372,638** |

Missing CustomerID shows 134,694 rows here, not 135,080. The other 386
ID-less rows are cancellations or adjustments, which are counted earlier in the
order. Non-positive quantity shows 0 for a similar reason: every
negative-quantity row is either a cancellation or lacks a customer ID.

### 1.3 Baseline compared with the final dataset

| Measure | Preliminary baseline | Final analytical dataset | Change |
|---|---:|---:|---:|
| Rows | 397,884 | 393,597 | −4,287 |
| Customers | 4,338 | 4,324 | −14 |
| Distinct invoices | 18,532 | 18,283 | −249 |
| Eligible purchase value | £8,911,408 | £8,372,638 | −£538,770 (−6.0%) |
| Countries | 37 | 37 | 0 |
| Customers with one invoice | 1,493 | n/a | |

The 14 customers who disappear had only cancelled purchases or only non-product
lines. One example is customer 12346, whose single 74,215-unit order was
reversed 16 minutes later.

### 1.4 Cancellation matching

| Outcome | Cancellation lines |
|---|---:|
| Matched one-to-one to an earlier purchase line | 2,784 |
| of which: single candidate | 2,191 |
| of which: several candidates, most recent selected | 547 |
| of which: several candidates, equal timestamps resolved by row order | 46 |
| Unmatched: missing CustomerID | 383 |
| Unmatched: no earlier line with the same quantity and price | 4,815 |
| Unmatched: no earlier purchase of this item by this customer | 1,306 |

The two largest matched reversals are £168,470 (80,995 units, customer 16446,
cancelled 12 minutes later) and £77,184 (customer 12346).

Before implementation, a naive many-to-one estimate gave 5,558 lines worth
about £591k. The one-to-one, time-ordered rule is stricter: it matches 2,784
lines worth £402,896.

The 4,815 cancellations with "no earlier line with the same quantity and price"
are mostly partial cancellations or cancellations at a different price. They
are deliberately left unresolved, which is a known source of remaining
overstatement.

### 1.5 Non-product lines excluded

| StockCode | Description | All rows | Eligible rows | Excluded | Excluded value |
|---|---|---:|---:|---:|---:|
| POST | POSTAGE | 1,256 | 1,099 | 1,083 | £77,122 |
| M | Manual | 572 | 284 | 257 | £40,044 |
| DOT | DOTCOM POSTAGE | 710 | 16 | 16 | £11,906 |
| C2 | CARRIAGE | 144 | 133 | 132 | £6,636 |
| BANK CHARGES | Bank Charges | 37 | 12 | 12 | £165 |
| PADS | PADS TO MATCH ALL CUSHIONS | 4 | 3 | 3 | £0 |

Excluded rows are fewer than eligible rows for `POST`, `M` and `C2` because 44
of those lines had already been excluded as matched cancellations, which come
earlier in the order.

---

## 2. RFM statistics (4,324 customers)

| Feature | Mean | SD | Min | P25 | Median | P75 | P95 | Max | Skewness | Skewness after log1p |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Recency (days) | 93.3 | 100.3 | 1 | 18 | 51 | 144 | 312.9 | 374 | 1.24 | −0.33 |
| Frequency (invoices) | 4.23 | 7.57 | 1 | 1 | 2 | 5 | 13 | 203 | 11.84 | 1.22 |
| Monetary (£) | 1,936 | 8,332 | 2.90 | 303 | 659 | 1,622 | 5,719 | 278,778 | 21.36 | 0.35 |

Frequency and Monetary are extremely right-skewed: their means are far above
their medians. log1p brings Monetary close to symmetric and greatly reduces the
skew in Frequency, which supports the preprocessing choice.

### Monthly eligible purchase value

| Month | Value | Invoices | Customers |
|---|---:|---:|---:|
| Dec 2010 | £558,837 | 1,386 | 883 |
| Jan 2011 | £458,846 | 975 | 737 |
| Feb 2011 | £440,505 | 989 | 756 |
| Mar 2011 | £577,366 | 1,303 | 970 |
| Apr 2011 | £449,468 | 1,130 | 849 |
| May 2011 | £653,298 | 1,528 | 1,051 |
| Jun 2011 | £645,643 | 1,377 | 986 |
| Jul 2011 | £587,244 | 1,314 | 945 |
| Aug 2011 | £627,931 | 1,264 | 933 |
| Sep 2011 | £930,214 | 1,729 | 1,254 |
| Oct 2011 | £970,176 | 1,884 | 1,356 |
| Nov 2011 | £1,128,938 | 2,631 | 1,657 |
| Dec 2011 (1–9 Dec, **partial**) | £344,174 | 773 | 612 |

Activity rises strongly from September to November, consistent with seasonal
(pre-Christmas) demand. December 2011 covers nine days and is not comparable
with full months.

---

## 3. Clustering evaluation

### 3.1 Candidate models (log1p and z-scored RFM, nstart = 25, seed 42)

| k | Mean silhouette | Between SS / total SS | Smallest cluster | Valid |
|---:|---:|---:|---:|---|
| **2** | **0.432** | 0.503 | 1,658 | yes |
| 3 | 0.340 | 0.628 | 728 | yes |
| 4 | 0.337 | 0.700 | 708 | yes |
| 5 | 0.318 | 0.750 | 361 | yes |
| 6 | 0.313 | 0.783 | 344 | yes |

**Selected: k = 2.** It has the highest mean silhouette, by a clear margin of
0.09 over k = 3. A silhouette of 0.43 indicates moderate separation. The
segments are broad tendencies rather than sharply distinct groups. The
between-to-total sum-of-squares ratio rises with every extra k, as it always
does, so it can't choose k by itself.

### 3.2 Selected segments (k = 2)

| Segment | Customers | Share | Median recency | Median frequency | Median value | Mean value | Total value | Share of value |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| S1: High-value active customers | 1,658 | 38.3% | 17 days | 6 | £2,022 | £4,263 | £7,067,294 | 84.4% |
| S2: Infrequent, inactive customers | 2,666 | 61.7% | 97 days | 1 | £356 | £490 | £1,305,344 | 15.6% |

**Interpretation.** The data divides most clearly into a minority of repeat,
recent customers who account for 84% of eligible purchase value, and a majority
of one-off or lapsed buyers. This is a strong concentration of value. It is
also a coarse split.

### 3.3 Business utility: k = 2 compared with k = 4

Silhouette measures statistical separation only. The dashboard's
**Segmentation view** switch (top bar) and the **Business utility: compare k**
tab let the evaluator judge usefulness as well. All figures below are computed
from the data. Lapse rates use the default thresholds (ratio 2.0, 60 days).

| Measure | k = 2 | k = 4 |
|---|---|---|
| Mean silhouette (separation) | **0.432** | 0.337 |
| Segments / distinct suggested actions | 2 / 2 | 4 / 4 |
| Smallest and largest segment (share of customers) | 38.3% and 61.7% | 16.4% and 36.4% |
| Highest-value segment | 38.3% of customers hold 84.4% of value | 16.4% of customers hold 64.6% of value |
| Lapse-signal rate range across segments | 7% to 62% | **0% to 90%** |
| Customers in a "review for win-back" segment | none isolated | **1,164 (£1,966,965 historical value)** |
| Flagged customers inside the high-value active segment | 116 | 16 |

**k = 4 segments:**

| Segment | Customers | Median R / F / value | Share of value | Lapse-signal rate | Suggested use (interpretation) |
|---|---:|---|---:|---:|---|
| S1: High-value active customers | 708 | 9 days / 10 / £3,618 | 64.6% | 2.3% | Protect and retain; watch personalized lapse signals |
| S2: Historically valuable, currently inactive | 1,164 | 54 days / 4 / £1,341 | 23.5% | 28.6% | Review for win-back |
| S3: Recent, low-activity customers | 877 | 19 days / 2 / £443 | 5.4% | 0.5% | Nurture a second or third purchase |
| S4: Infrequent, inactive customers | 1,575 | 183 days / 1 / £292 | 6.5% | 90.4% | Low-cost reactivation or deprioritise |

**How the k = 2 segments split at k = 4:**

| k = 4 segment | from k = 2 S1 (High-value active) | from k = 2 S2 (Infrequent, inactive) |
|---|---:|---:|
| S1: High-value active | 708 | 0 |
| S2: Historically valuable, currently inactive | 699 (£1,445,208) | 465 (£521,757) |
| S3: Recent, low-activity | 251 | 626 |
| S4: Infrequent, inactive | 0 | 1,575 |

**Interpretation.**

1. k = 2 is the statistically cleaner split, and a valid headline: a minority of
   repeat, recent customers holds most of the value.
2. Its "High-value active" label is too broad for action. 699 of its 1,658
   customers (42%), holding £1.45m, are by their own medians no longer active.
   They have a median recency of 54 days, against 9 days for the core group.
3. k = 4 separates lapse risk far more sharply (0% to 90%, against 7% to 62%).
   It isolates three groups that call for different handling, plus a
   win-back-review list of 1,164 customers with £1.97m of past value. That
   group doesn't exist as a unit at k = 2.
4. The cost is weaker separation (silhouette 0.337, about 0.10 lower), so k = 4
   segment boundaries should be treated as softer.

**Recommended presentation:**
- Report k = 2 as the statistical result. The selection rule is unchanged and
  remains the default.
- Use k = 4 as the business view and say so explicitly.
- These suggested uses are interpretations of the profiles. The data contains
  no campaign outcomes, so their effect is untested.

At **k = 3**, two segments both meet the "high-value active" naming rule, with
medians of £3,641 and £974 at frequencies of 10 and 3. The dashboard flags the
duplicated name. It shows that names are coarse descriptions of medians, and
k = 3 adds less interpretive value than k = 4.

### 3.4 Extension: rule-based RFM groups compared with K-Means

Quintile-based RFM rule groups:

| Rule group | Customers | Median R | Median F | Median value |
|---|---:|---:|---:|---:|
| R1: Recent, high frequency/value | 937 | 11 | 7 | £2,633 |
| R2: Fairly recent, mid-high frequency/value | 707 | 37 | 4 | £1,105 |
| R3: Recent, low frequency/value | 500 | 19 | 2 | £339 |
| R4: Not recent, mid-high frequency/value | 411 | 116 | 3 | £1,240 |
| R5: Not recent, low frequency/value | 1,313 | 206 | 1 | £300 |
| R6: Middle recency, low frequency/value | 456 | 53 | 1 | £339 |

How the rule groups fall into the K-Means segments:

| Rule group | S1 | S2 |
|---|---:|---:|
| R1 | 937 | 0 |
| R2 | 542 | 165 |
| R3 | 57 | 443 |
| R4 | 122 | 289 |
| R5 | 0 | 1,313 |
| R6 | 0 | 456 |

**Adjusted Rand Index = 0.303.** The two methods agree at the extremes: every
R1 customer is in S1, and every R5 and R6 customer is in S2. They disagree in
the middle (R2, R3 and R4). Part of the low ARI is structural, because six
rule groups are being compared with two clusters.

In short, the rules give fixed, explainable cut-offs at chosen quantiles, while
K-Means finds the dominant split in the data's own structure. Both separate
"recent repeat buyers" from "one-off or lapsed buyers".

---

## 4. Purchase-lapse evaluation

Defaults: ratio threshold 2.0 and fixed threshold 60 days. Cadence
qualification requires at least 3 invoices and at least 3 distinct purchase
dates.

Cadence eligibility across the 4,324 customers:

| Status | Customers |
|---|---:|
| Sufficient history | 1,901 (44.0%) |
| Fewer than 3 invoices | 2,333 |
| Fewer than 3 distinct purchase dates | 90 |

Among qualified customers, the typical purchase interval has a median of 45
days, with P25 = 26.5, P75 = 73.5 and a range of 2 to 185.5 days. Only 21
customers have a typical interval under 7 days.

### 4.1 Comparison A: same population (1,901 qualified customers)

| Group | Customers | Share | Historical value | Share of value |
|---|---:|---:|---:|---:|
| Flagged by personalized rule | 233 | 12.3% | £589,630 | 8.3% |
| Flagged by fixed rule | 436 | 22.9% | £676,967 | 9.5% |
| Flagged by both | 202 | 10.6% | £313,828 | 4.4% |
| Flagged only by personalized rule | 31 | 1.6% | £275,802 | 3.9% |
| Flagged only by fixed rule | 234 | 12.3% | £363,139 | 5.1% |
| Flagged by neither | 1,434 | 75.4% | £6,139,655 | 86.6% |

The Jaccard overlap of the two flagged sets is **0.43**.

**What differs:**
- The 31 customers flagged **only by the personalized rule** are frequent
  buyers. Their median typical interval is 12 days, median frequency 8
  invoices, and median value £1,967. They have been quiet for a median of 36
  days. That is well past their own rhythm, but still below 60 days, so the
  fixed rule misses them. They hold £275,802 of historical value, nearly as much
  as the 234 fixed-only customers.
- The 234 customers flagged **only by the fixed rule** are slow-cadence buyers.
  Their median typical interval is 81 days, and they have been quiet for a
  median of 80 days. That is about their normal rhythm, so the personalized rule
  does not consider them overdue.

This is the behaviour the personalized rule was designed for. It shifts
attention towards frequent buyers who have unusually stopped, and away from
naturally slow buyers. Whether those signals are *more correct* can't be
established without outcome labels.

### 4.2 Comparison B: hybrid policy (4,324 customers)

| Group | Customers | Share | Historical value | Share of value |
|---|---:|---:|---:|---:|
| Evaluated by personalized cadence | 1,901 | 44.0% | £7,092,423 | 84.7% |
| Evaluated by fixed fallback | 2,423 | 56.0% | £1,280,215 | 15.3% |
| Flagged: personalized cadence signal | 233 | 5.4% | £589,630 | 7.0% |
| Flagged: fallback threshold signal | 1,544 | 35.7% | £800,457 | 9.6% |
| **Flagged in total (hybrid policy)** | **1,777** | **41.1%** | **£1,390,087** | **16.6%** |
| Reference: fixed 60-day rule applied to everyone | 1,980 | 45.8% | £1,477,424 | 17.6% |
| Hybrid and all-customer fixed rule agree | 4,059 | 93.9% | | |

Most hybrid flags (1,544 of 1,777) come from the fallback, because most
customers have too little history for a personal cadence. The hybrid policy
flags 203 fewer customers than the fixed rule applied to everyone. The two
policies disagree on 265 customers, which are exactly the "only" groups from
Comparison A.

### 4.3 Threshold sensitivity

| Rule (population) | Threshold | Flagged | Share |
|---|---|---:|---:|
| Personalized (1,901) | ratio ≥ 1.5 | 332 | 17.5% |
| | ratio ≥ 2.0 | 233 | 12.3% |
| | ratio ≥ 2.5 | 170 | 8.9% |
| | ratio ≥ 3.0 | 130 | 6.8% |
| | ratio ≥ 4.0 | 93 | 4.9% |
| Fixed (1,901 qualified) | ≥ 30 days | 811 | 42.7% |
| | ≥ 45 days | 575 | 30.2% |
| | ≥ 60 days | 436 | 22.9% |
| | ≥ 90 days | 252 | 13.3% |
| | ≥ 120 days | 156 | 8.2% |
| | ≥ 180 days | 67 | 3.5% |
| Fixed (4,324 all) | ≥ 30 days | 2,757 | 63.8% |
| | ≥ 60 days | 1,980 | 45.8% |
| | ≥ 90 days | 1,464 | 33.9% |
| | ≥ 180 days | 871 | 20.1% |

Flag counts change smoothly with the threshold, and no particular cut-off is
singled out by the data. The defaults are therefore policy choices about review
capacity, not empirically optimal values.

### 4.4 What these results do **not** show

- **No precision, recall, accuracy or ROC-AUC.** There is no verified churn
  label to compare against.
- The historical value of flagged customers is descriptive exposure. It is not
  revenue at risk and not recoverable revenue.
- Nothing here shows that contacting flagged customers changes their behaviour.

---

## 5. Key interpretations

1. Cleaning matters. One-to-one cancellation matching and the non-product
   exclusions remove 6.0% of preliminary purchase value, including two single
   bulk orders worth £245k that were fully reversed.
2. Purchase value is highly concentrated. 38% of customers (S1) account for 84%
   of eligible value.
3. Silhouette favours a two-group structure, but k = 4 is the more useful
   business view. It splits the broad "high-value active" group, separates
   lapse risk much more sharply (0–90% against 7–62%) and isolates a
   win-back-review group of 1,164 customers (£1.97m). It costs about 0.10 of
   silhouette. The dashboard lets you switch between the two views.
4. The personalized lapse rule and the fixed rule overlap only partly (Jaccard
   0.43). The personalized rule highlights a small, high-value set of frequent
   buyers that the fixed rule misses, and it stops flagging slow-cadence buyers
   who are behaving normally.
5. 56% of customers lack the history for personalized analysis. Any lapse
   policy for this retailer therefore relies heavily on a simple fixed rule for
   the majority.

## 6. Limitations of this evaluation

These are covered in full in `methodology.md` §9:
- one retailer and about 12 months;
- no churn labels;
- cancellations left unmatched (6,504 lines);
- 25% of rows without a customer ID;
- moderate silhouette;
- heuristic thresholds;
- no intervention data.
