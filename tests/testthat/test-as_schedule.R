test_that("attributes get populated correctly", {
  data <- data.frame(
    id = 1:6,
    treatment = factor(c("control", "control", "treatment", "treatment", "control", "treatment")),
    response = c(10, 12, 15, 18, 11, 17)
  )

  schedule <- as_schedule(data, treatment, response)

  expect_equal(attr(schedule, "treatment"), "treatment")
  expect_equal(attr(schedule, "response"), "response")
  expect_equal(attr(schedule, "treatment_levels"), c("control", "treatment"))
  expect_equal(attr(schedule, "potential_outcome_cols"), c("Ycontrol", "Ytreatment"))
})

test_that("append = FALSE returns only the schedule columns", {
  data <- data.frame(
    id = 1:6,
    treatment = factor(c("control", "control", "treatment", "treatment", "control", "treatment")),
    response = c(10, 12, 15, 18, 11, 17)
  )

  schedule <- as_schedule(data, treatment, response, append = FALSE)

  expect_equal(colnames(schedule), c("Ycontrol", "Ytreatment"))
})

test_that("errors are thrown for missing columns", {
  data <- data.frame(
    id = 1:6,
    treatment = factor(c("control", "control", "treatment", "treatment", "control", "treatment")),
    response = c(10, 12, 15, 18, 11, 17)
  )

  expect_error(as_schedule(data, nonexistent_treatment, response))
  expect_error(as_schedule(data, treatment, nonexistent_response))
})