# Run all tests from the project root:  Rscript tests/testthat.R
library(testthat)
root <- if (file.exists("app.R")) "." else ".."
test_dir(file.path(root, "tests", "testthat"), reporter = "summary", stop_on_failure = TRUE)
