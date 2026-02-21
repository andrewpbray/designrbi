# Shared test data: 6 units, two treatment levels
make_schedule <- function() {
  data <- data.frame(
    id = 1:6,
    treatment = factor(c("control", "control", "treatment", "treatment", "control", "treatment")),
    response = c(10, 12, 15, 18, 11, 17)
  )
  as_schedule(data, treatment, response)
}

test_that("errors on non-schedule input", {
  expect_error(impute_unobserved(data.frame(x = 1), tau = 0), "schedule")
})

test_that("tau = 0 fills NA potential outcomes with observed value", {
  result <- impute_unobserved(make_schedule(), tau = 0)
  po_cols <- attr(result, "potential_outcome_cols")

  # No NAs remain
  expect_false(anyNA(result[po_cols]))

  # Units assigned to control: Ytreatment should equal Ycontrol
  control_rows <- result[!is.na(result$Ycontrol) & is.na(result$Ytreatment) |
                           !is.na(result$Ycontrol), ]
  expect_equal(result$Ycontrol, result$Ytreatment)
})

test_that("tau != 0 imputes Y1 = Y0 + tau for control units", {
  result <- impute_unobserved(make_schedule(), tau = 3)

  # Units originally in control had NA Ytreatment; now Ytreatment = Ycontrol + 3
  schedule <- make_schedule()
  control_rows <- !is.na(schedule$Ycontrol)
  expect_equal(result$Ytreatment[control_rows], result$Ycontrol[control_rows] + 3)
})

test_that("tau != 0 imputes Y0 = Y1 - tau for treated units", {
  result <- impute_unobserved(make_schedule(), tau = 3)

  schedule <- make_schedule()
  treated_rows <- !is.na(schedule$Ytreatment)
  expect_equal(result$Ycontrol[treated_rows], result$Ytreatment[treated_rows] - 3)
})

test_that("tau != 0 leaves no NAs in potential outcome columns", {
  result <- impute_unobserved(make_schedule(), tau = 5)
  po_cols <- attr(result, "potential_outcome_cols")
  expect_false(anyNA(result[po_cols]))
})

test_that("tau != 0 errors for more than two treatment levels", {
  data <- data.frame(
    id = 1:6,
    treatment = factor(c("a", "b", "c", "a", "b", "c")),
    response = c(1, 2, 3, 4, 5, 6)
  )
  schedule <- as_schedule(data, treatment, response)
  expect_error(impute_unobserved(schedule, tau = 1), "two treatment levels")
})
