make_long_df <- function() {
  data.frame(
    id        = 1:6,
    treatment = factor(c("control", "control", "treatment",
                         "treatment", "control", "treatment")),
    response  = c(10, 12, 15, 18, 11, 17)
  )
}

# ── class and attributes ───────────────────────────────────────────────────────

test_that("experiment_to_schedule() returns a schedule object", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response)
  expect_s3_class(sched, "schedule")
})

test_that("experiment_to_schedule() sets treatment attribute", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response)
  expect_equal(attr(sched, "treatment"), "treatment")
})

test_that("experiment_to_schedule() sets response attribute", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response)
  expect_equal(attr(sched, "response"), "response")
})

test_that("experiment_to_schedule() sets treatment_levels attribute", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response)
  expect_equal(attr(sched, "treatment_levels"), c("control", "treatment"))
})

test_that("experiment_to_schedule() sets treatment_counts attribute", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response)
  expect_equal(as.integer(attr(sched, "treatment_counts")), c(3L, 3L))
})

test_that("experiment_to_schedule() sets potential_outcome_cols attribute", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response)
  expect_equal(attr(sched, "potential_outcome_cols"), c("Ycontrol", "Ytreatment"))
})

# ── output structure ───────────────────────────────────────────────────────────

test_that("experiment_to_schedule() preserves original columns", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response)
  expect_true(all(c("id", "treatment", "response") %in% colnames(sched)))
})

test_that("experiment_to_schedule() appends one potential outcome column per treatment level", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response)
  expect_true(all(c("Ycontrol", "Ytreatment") %in% colnames(sched)))
})

test_that("experiment_to_schedule() observed values land in correct outcome column", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response)
  # control units have observed Ycontrol, treatment units have observed Ytreatment
  control_rows   <- sched$treatment == "control"
  treatment_rows <- sched$treatment == "treatment"
  expect_equal(sched$Ycontrol[control_rows],     make_long_df()$response[control_rows])
  expect_equal(sched$Ytreatment[treatment_rows], make_long_df()$response[treatment_rows])
})

test_that("experiment_to_schedule() unobserved potential outcomes are NA", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response)
  control_rows   <- sched$treatment == "control"
  treatment_rows <- sched$treatment == "treatment"
  expect_true(all(is.na(sched$Ytreatment[control_rows])))
  expect_true(all(is.na(sched$Ycontrol[treatment_rows])))
})

# ── outcome_prefix ─────────────────────────────────────────────────────────────

test_that("experiment_to_schedule() outcome_prefix changes column names", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response,
                                  outcome_prefix = "PO_")
  expect_true(all(c("PO_control", "PO_treatment") %in% colnames(sched)))
})

test_that("experiment_to_schedule() outcome_prefix is reflected in potential_outcome_cols attribute", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response,
                                  outcome_prefix = "PO_")
  expect_equal(attr(sched, "potential_outcome_cols"), c("PO_control", "PO_treatment"))
})

# ── input validation ───────────────────────────────────────────────────────────

test_that("experiment_to_schedule() errors when treatment column does not exist", {
  expect_error(experiment_to_schedule(make_long_df(), nonexistent, response))
})

test_that("experiment_to_schedule() errors when response column does not exist", {
  expect_error(experiment_to_schedule(make_long_df(), treatment, nonexistent))
})

test_that("experiment_to_schedule() errors when treatment column is not a factor", {
  df <- make_long_df()
  df$treatment <- as.character(df$treatment)
  expect_error(experiment_to_schedule(df, treatment, response), "factor")
})

# ── edge cases ─────────────────────────────────────────────────────────────────

test_that("experiment_to_schedule() works with three treatment levels", {
  df <- data.frame(
    treatment = factor(c("a", "b", "c", "a", "b", "c")),
    response  = c(1, 2, 3, 4, 5, 6)
  )
  sched <- experiment_to_schedule(df, treatment, response)
  expect_equal(attr(sched, "treatment_levels"), c("a", "b", "c"))
  expect_true(all(c("Ya", "Yb", "Yc") %in% colnames(sched)))
})
