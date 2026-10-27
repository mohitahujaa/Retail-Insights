# Consistent number, currency, percentage and date formatting -----------------

rp_fmt_int <- function(x) {
  ifelse(is.na(x), "-", formatC(round(x), format = "d", big.mark = ","))
}

rp_fmt_num <- function(x, digits = 1) {
  ifelse(is.na(x), "-", formatC(x, format = "f", digits = digits, big.mark = ","))
}

rp_fmt_gbp <- function(x, digits = 0) {
  ifelse(is.na(x), "-", paste0(
    ifelse(x < 0, "-", ""), "\u00a3",
    formatC(abs(x), format = "f", digits = digits, big.mark = ",")
  ))
}

rp_fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "-", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

rp_fmt_date <- function(x) {
  ifelse(is.na(x), "-", format(as.Date(x), "%d %b %Y"))
}
