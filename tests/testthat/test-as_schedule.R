# ── Shared fixtures ───────────────────────────────────────────────────────────

make_wide_df <- function() {
  data.frame(
    id         = 1:6,
    treatment  = factor(c("control", "control", "treatment",
                          "treatment", "control", "treatment")),
    Ycontrol   = c(10, 12, NA, NA, 11, NA),
    Ytreatment = c(NA, NA, 15, 18, NA, 17)
  )
}

# ── class and attributes ───────────────────────────────────────────────────────

test_that("as_schedule() returns a schedule object", {
  sched <- as_schedule(make_wide_df(), treatment, c(Ycontrol, Ytreatment))
  expect_s3_class(sched, "schedule")
})

test_that("as_schedule() sets treatment attribute", {
  sched <- as_schedule(make_wide_df(), treatment, c(Ycontrol, Ytreatment))
  expect_equal(attr(sched, "treatment"), "treatment")
})

test_that("as_schedule() sets treatment_levels attribute", {
  sched <- as_schedule(make_wide_df(), treatment, c(Ycontrol, Ytreatment))
  expect_equal(attr(sched, "treatment_levels"), c("control", "treatment"))
})

test_that("as_schedule() sets treatment_counts attribute", {
  sched <- as_schedule(make_wide_df(), treatment, c(Ycontrol, Ytreatment))
  expect_equal(as.integer(attr(sched, "treatment_counts")), c(3L, 3L))
})

test_that("as_schedule() sets potential_outcome_cols attribute", {
  sched <- as_schedule(make_wide_df(), treatment, c(Ycontrol, Ytreatment))
  expect_equal(attr(sched, "potential_outcome_cols"), c("Ycontrol", "Ytreatment"))
})

test_that("as_schedule() does not set a response attribute", {
  sched <- as_schedule(make_wide_df(), treatment, c(Ycontrol, Ytreatment))
  expect_null(attr(sched, "response"))
})

# ── data is unchanged ─────────────────────────────────────────────────────────

test_that("as_schedule() does not add or remove columns", {
  df    <- make_wide_df()
  sched <- as_schedule(df, treatment, c(Ycontrol, Ytreatment))
  expect_equal(colnames(sched), colnames(df))
})

test_that("as_schedule() does not modify cell values", {
  df    <- make_wide_df()
  sched <- as_schedule(df, treatment, c(Ycontrol, Ytreatment))
  expect_equal(sched$Ycontrol,   df$Ycontrol)
  expect_equal(sched$Ytreatment, df$Ytreatment)
})

# ── tidyselect expressions ─────────────────────────────────────────────────────

test_that("as_schedule() accepts starts_with() tidyselect expression", {
  sched <- as_schedule(make_wide_df(), treatment, starts_with("Y"))
  expect_equal(attr(sched, "potential_outcome_cols"), c("Ycontrol", "Ytreatment"))
})

# ── input validation ───────────────────────────────────────────────────────────

test_that("as_schedule() errors when treatment column does not exist", {
  expect_error(
    as_schedule(make_wide_df(), nonexistent, c(Ycontrol, Ytreatment))
  )
})

test_that("as_schedule() errors when treatment column is not a factor", {
  df <- make_wide_df()
  df$treatment <- as.character(df$treatment)
  expect_error(as_schedule(df, treatment, c(Ycontrol, Ytreatment)), "factor")
})

test_that("as_schedule() errors when fewer than two outcome columns are selected", {
  expect_error(
    as_schedule(make_wide_df(), treatment, Ycontrol),
    "two"
  )
})

# ── experiment_to_schedule() — Type A (long format) ───────────────────────────

make_long_df <- function() {
  data.frame(
    id        = 1:6,
    treatment = factor(c("control", "control", "treatment",
                         "treatment", "control", "treatment")),
    response  = c(10, 12, 15, 18, 11, 17)
  )
}



test_that("experiment_to_schedule() sets attributes correctly", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response)

  expect_equal(attr(sched, "treatment"),              "treatment")
  expect_equal(attr(sched, "response"),               "response")
  expect_equal(attr(sched, "treatment_levels"),       c("control", "treatment"))
  expect_equal(attr(sched, "potential_outcome_cols"), c("Ycontrol", "Ytreatment"))
  expect_s3_class(sched, "schedule")
})

test_that("experiment_to_schedule() appends potential outcome columns to original data", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response)

  expect_true(all(c("id", "treatment", "response", "Ycontrol", "Ytreatment") %in%
                    colnames(sched)))
})

test_that("experiment_to_schedule() outcome_prefix changes column names", {
  sched <- experiment_to_schedule(make_long_df(), treatment, response,
                                  outcome_prefix = "PO_")

  expect_equal(attr(sched, "potential_outcome_cols"), c("PO_control", "PO_treatment"))
  expect_true(all(c("PO_control", "PO_treatment") %in% colnames(sched)))
})

test_that("experiment_to_schedule() errors when treatment column is missing", {
  expect_error(experiment_to_schedule(make_long_df(), nonexistent, response))
})

test_that("experiment_to_schedule() errors when response column is missing", {
  expect_error(experiment_to_schedule(make_long_df(), treatment, nonexistent))
})

test_that("experiment_to_schedule() errors when treatment column is not a factor", {
  df <- make_long_df()
  df$treatment <- as.character(df$treatment)
  expect_error(experiment_to_schedule(df, treatment, response), "factor")
})

test_that("experiment_to_schedule() handles duplicate rows without error", {
  df <- data.frame(
    id        = c(1, 1, 2, 2, 3, 3),
    treatment = factor(c("control", "control", "treatment",
                         "treatment", "control", "treatment")),
    response  = c(10, 10, 15, 15, 11, 17)
  )
  expect_no_error(experiment_to_schedule(df, treatment, response))
})