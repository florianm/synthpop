#-----------------------------------------------------------------------------#
# Basic usage
# Examples from reference manual, calls from other functions in codebase
test_that("Example tests", {
  # Inputs
  set.seed(123)
  ods <- SD2011[1:1000, c("sex", "age", "edu", "marital", "ls", "smoke")]
  s1 <- syn(ods, m = 3)

  ods <- SD2011[1:1000, c("sex", "age", "income", "marital", "depress")]
  ods$income[ods$income == -8] <- NA
  s2 <- syn(ods, m = 3)

  # Test glm.synds with logistic regression
  f1 <- glm.synds(smoke ~ sex + age + edu + marital + ls, data = s1, family = "binomial")
  expect_s3_class(f1, "fit.synds")

  # Test lm.synds with linear regression
  f2 <- lm.synds(depress ~ sex + age + log(income) + marital, data = s2)
  expect_s3_class(f2, "fit.synds")
})
#-----------------------------------------------------------------------------#
# Advanced usage
# Arguments with options: Each option, none, non-existing option
# Arguments with scalars: Lowest, highest, expected, none, ludicrous values
test_that("Different inputs tests", {
  # Inputs
  set.seed(123)
  ods <- SD2011[1:1000, c("sex", "age", "edu", "marital", "ls", "smoke")]
  s1 <- syn(ods, m = 3)

  ods <- SD2011[1:1000, c("sex", "age", "income", "marital", "depress")]
  ods$income[ods$income == -8] <- NA
  s2 <- syn(ods, m = 3)

  # Test with default family argument
  f1_default_family <- glm.synds(smoke ~ sex + age + edu + marital + ls, data = s1)
  expect_s3_class(f1_default_family, "fit.synds")

  # Test with different synthetic data sets, TODO: duplicate synth data
  f1_multiple_syn <- glm.synds(smoke ~ sex + age + edu + marital + ls, data = s1)
  f2_multiple_syn <- lm.synds(depress ~ sex + age + log(income) + marital, data = s2)

  expect_s3_class(f1_multiple_syn, "fit.synds")
  expect_s3_class(f2_multiple_syn, "fit.synds")

  # Test with non-existing argument
  expect_error(glm.synds(smoke ~ sex + age + edu + marital + ls, data = s1, non_existing_arg = 0), "unused argument")
})
#-----------------------------------------------------------------------------#
# Edge cases
# From bug reports

#-----------------------------------------------------------------------------#
# Expected errors from invalid input
test_that("Edge cases and gate checks tests", {
  set.seed(123)
  ods <- SD2011[1:1000, c("sex", "age", "edu", "marital", "ls", "smoke")]
  s1 <- syn(ods, m = 3)

  # Test with no data argument
  expect_error(
    glm.synds(smoke ~ sex + age + edu + marital + ls, data = list()),
    "Data must have class synds"
  )
})

#-----------------------------------------------------------------------------#
# Insert into R/xxx.R as quick link to this test file:
# usethis::use_test("glm_synds")
