library(testthat)

#-----------------------------------------------------------------------------#
# Basic usage
#
test_that("compare.synds returns expected data structure from minimal input", {
  # Inputs
  data("SD2011", package = "synthpop")
  selected_vars <- c("sex", "age", "edu", "marital", "ls", "income")
  ods <- SD2011[, selected_vars]
  s1 <- syn(ods, cont.na = list(income = -8))

  # Function call
  result <- synthpop::compare(object = s1, data = ods)

  # Check output
  expect_s3_class(result, "compare.synds")
  expect_true("tables" %in% names(result))
  expect_true("plots" %in% names(result))
  expect_true("stat" %in% names(result))
  expect_true("vars" %in% names(result))
  expect_true("tab.utility" %in% names(result))
  expect_true("table" %in% names(result))
  expect_true("plot" %in% names(result))

  # If vars is NULL all synthesised variables are compared
  expect_equal(length(result$vars), length(selected_vars))
  expect_equal(result$vars, selected_vars)
})

#-----------------------------------------------------------------------------#
# Permutate arguments
#
test_that("compare.synds works with arguments", {
  # Inputs
  data("SD2011", package = "synthpop")
  ods <- SD2011[, c("sex", "age", "edu", "marital", "ls", "income")]
  s1 <- syn(ods, cont.na = list(income = -8))

  # Function call
  result <- synthpop::compare(
    s1, ods,
    vars = "income", stat = "counts", table = TRUE, breaks = 10
  )

  # Check output
  expect_s3_class(result, "compare.synds")
  expect_true("tables" %in% names(result))
  expect_true("plots" %in% names(result))
  expect_true("stat" %in% names(result))
  expect_true("vars" %in% names(result))
  expect_true("tab.utility" %in% names(result))
  expect_true("table" %in% names(result))
  expect_true("plot" %in% names(result))
})

test_that("compare.synds(utility.stats = 'all') prints all utility stats", {
  # Inputs
  data("SD2011", package = "synthpop")
  ods <- SD2011[, c("sex", "age", "edu", "marital", "ls", "income")]
  s1 <- syn(ods, cont.na = list(income = -8))

  # Function call
  result <- synthpop::compare(s1, ods, utility.stats = "all")

  # Check output
  expect_s3_class(result, "compare.synds")
  utility_measures <- c(
    "VW", "FT", "JSD", "SPECKS", "WMabsDD", "U", "G", "pMSE", "PO50",
    "MabsDD", "dBhatt", "S_VW", "S_FT", "S_JSD", "S_WMabsDD", "S_G",
    "S_pMSE", "df"
  )
  expect_equal(colnames(result$tab.utility), utility_measures)
})

#-----------------------------------------------------------------------------#
# Test edge cases
# TODO

#-----------------------------------------------------------------------------#
# Test expected errors from invalid input
# TODO

#-----------------------------------------------------------------------------#
# Insert into R/compare.synds.R as quick link to this test file:
# usethis::use_test("compare_synds")
