# Reading the source workbook ----------------------------------------------

# Column types used when reading the UCI workbook. Identifiers are read as
# text so that invoice numbers, stock codes and customer IDs are never treated
# as quantities.
RP_COL_TYPES <- c(
  InvoiceNo = "text", StockCode = "text", Description = "text",
  Quantity = "numeric", InvoiceDate = "date", UnitPrice = "numeric",
  CustomerID = "text", Country = "text"
)

# Resolve a path relative to the project root unless it is already absolute.
rp_path <- function(path, root = ".") {
  if (grepl("^(/|[A-Za-z]:[/\\\\]|~)", path)) path else file.path(root, path)
}

# Read the raw transaction workbook.
#
# Returns a data frame with the original columns plus `row_id`, a stable
# identifier equal to the row's position in the worksheet (1 = first data row).
# Any cell that readxl could not convert to the requested type is counted and
# returned in the "read_warnings" attribute instead of being silently dropped.
rp_read_transactions <- function(path, sheet = 1) {
  if (!file.exists(path)) {
    stop(
      "Transaction workbook not found at '", path, "'.\n",
      "Place the UCI Online Retail workbook at data/raw/online_retail.xlsx ",
      "(relative to the project root), or set data_path in R/config.R.",
      call. = FALSE
    )
  }

  header <- names(readxl::read_excel(path, sheet = sheet, n_max = 0))
  missing <- setdiff(names(RP_COL_TYPES), header)
  if (length(missing) > 0) {
    stop(
      "The workbook is missing required column(s): ",
      paste(missing, collapse = ", "), ". Found: ",
      paste(header, collapse = ", "), call. = FALSE
    )
  }
  col_types <- unname(ifelse(header %in% names(RP_COL_TYPES),
    RP_COL_TYPES[header], "guess"
  ))

  read_warnings <- character()
  raw <- withCallingHandlers(
    readxl::read_excel(path, sheet = sheet, col_types = col_types),
    warning = function(w) {
      read_warnings <<- c(read_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  raw$row_id <- seq_len(nrow(raw))
  attr(raw, "read_warnings") <- read_warnings
  attr(raw, "source_path") <- path
  raw
}
