#-----------------------------------------------------------------------------#
# Basic usage
#
test_that("codebook.syn returns correct output for a simple data frame", {
  # Create a simple data frame for testing, factor levels are <= maxlevs
  # (default: 3)
  test_data <- data.frame(
    A = c(1, 2, 3),
    B = c("a", "b", "a"),
    C = c(NA, 5, 6)
  )

  # Test if codebook.syn() returns a list with two components
  result <- codebook.syn(test_data)
  expect_type(result, "list")
  expect_named(result, c("tab", "labs"))

  # Test if tab component has the correct structure
  expect_type(result$tab, "list")
  expect_named(
    result$tab,
    c("variable", "class", "nmiss", "perctmiss", "ndistinct", "details")
  )

  # Test if labs component is NULL for this simple data frame
  expect_type(result$labs, "NULL")
})

test_that("codebook.syn works with factor levels above maxlevs", {
  # Create a more complex data frame for testing
  test_data <- data.frame(
    A = c(1, 2, 3, NA, 5),
    B = c("a", "b", "a", "c", "b"),
    C = factor(c("low", "medium", "high", "medium", "extreme"))
  )

  # Test if codebook.syn() returns correct statistics
  result <- codebook.syn(test_data)

  # Test missing values statistics
  expect_equal(result$tab$nmiss[1], 1)
  expect_equal(result$tab$perctmiss[1], 20)

  # Test factor level statistics
  expect_equal(result$tab$ndistinct[2], 3)
  expect_equal(result$tab$details[2], "Max length: 1")

  # Test labs component for factors with more levels than maxlevs
  expect_type(result$labs$C, "list")
  expect_named(result$labs$C, "label")
  expect_equal(result$labs$C$label, c("extreme", "high", "low", "medium"))
})
#-----------------------------------------------------------------------------#
# Permutate arguments
#

#-----------------------------------------------------------------------------#
# Edge cases
#

#-----------------------------------------------------------------------------#
# Expected errors from invalid input
#
test_that("codebook.syn handles invalid input", {
  # Test if codebook.syn() raises an error for non-data frame input
  x <- testthat::capture_error(
    codebook.syn(123),
    entrace = FALSE
  )
  testthat::expect_s3_class(x, "error")
})

#-----------------------------------------------------------------------------#
# Insert into R/codebook.syn.R as quick link to this test file:
# usethis::use_test("codebook_syn")
