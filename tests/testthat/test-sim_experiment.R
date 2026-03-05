# ── Shared fixtures ───────────────────────────────────────────────────────────

# Type A: schedule from long-format experimental data (has "response" attr)
make_type_a_schedule <- function(tau = 0) {
  data.frame(
    id        = 1:6,
    treatment = factor(c("control", "control", "treatment",
                         "treatment", "control", "treatment")),
    response  = c(10, 12, 15, 18, 11, 17)
  ) |>
    experiment_to_schedule(treatment, response) |>
    impute_unobserved(tau = tau)
}

# Type B: schedule from wide-format data (no "response" attr)
make_type_b_schedule <- function() {
  data.frame(
    id         = 1:6,
    treatment  = factor(c("control", "control", "treatment",
                          "treatment", "control", "treatment")),
    Ycontrol   = c(10, 12, 15, 18, 11, 17),
    Ytreatment = c(10, 12, 18, 21, 11, 17)
  ) |>
    as_schedule(treatment, c(Ycontrol, Ytreatment))
}

# ── output structure ───────────────────────────────────────────────────────────

test_that("sim_experiment() returns a data frame", {
  expect_s3_class(sim_experiment(make_type_a_schedule()), "data.frame")
})

test_that("sim_experiment() returns the correct number of rows for reps = 1", {
  sched <- make_type_a_schedule()
  out   <- sim_experiment(sched, reps = 1)
  expect_equal(nrow(out), nrow(sched))
})

test_that("sim_experiment() returns reps * n rows for reps > 1", {
  sched <- make_type_a_schedule()
  out   <- sim_experiment(sched, reps = 5)
  expect_equal(nrow(out), nrow(sched) * 5)
})

test_that("sim_experiment() output has replicate, treatment, and response columns", {
  sched <- make_type_a_schedule()
  out   <- sim_experiment(sched)
  # treatment col is named after the schedule's treatment attribute
  expect_true(all(c("replicate", attr(sched, "treatment"), "response") %in% colnames(out)))
})

test_that("sim_experiment() replicate column contains correct values", {
  out <- sim_experiment(make_type_a_schedule(), reps = 3)
  expect_equal(sort(unique(out$replicate)), 1:3)
})

test_that("sim_experiment() replicate column is integer", {
  out <- sim_experiment(make_type_a_schedule(), reps = 2)
  expect_type(out$replicate, "integer")
})

# ── treatment assignment ───────────────────────────────────────────────────────

test_that("sim_experiment() preserves treatment group sizes", {
  sched <- make_type_a_schedule()
  out   <- sim_experiment(sched, reps = 1)
  counts <- table(out$treatment)
  expect_equal(as.integer(counts[["control"]]),   3L)
  expect_equal(as.integer(counts[["treatment"]]), 3L)
})

test_that("sim_experiment() treatment column is a factor", {
  out <- sim_experiment(make_type_a_schedule())
  expect_s3_class(out$treatment, "factor")
})

test_that("sim_experiment() treatment levels are preserved", {
  sched  <- make_type_a_schedule()
  out    <- sim_experiment(sched)
  expect_equal(levels(out$treatment), attr(sched, "treatment_levels"))
})

# ── response values ────────────────────────────────────────────────────────────

test_that("sim_experiment() response values come from the schedule", {
  sched         <- make_type_a_schedule()
  valid_values  <- unlist(sched[attr(sched, "potential_outcome_cols")])
  out          <- sim_experiment(sched, reps = 10)
  response_col <- attr(sched, "response")  # "response" for Type A
  expect_true(all(out[[response_col]] %in% valid_values))
})

# ── response_name argument ─────────────────────────────────────────────────────

test_that("sim_experiment() uses 'response' as response column name for Type A schedules", {
  out <- sim_experiment(make_type_a_schedule())
  expect_true("response" %in% colnames(out))
})

test_that("sim_experiment() uses 'Y' as response column name for Type B schedules", {
  out <- sim_experiment(make_type_b_schedule())
  expect_true("Y" %in% colnames(out))
})

test_that("sim_experiment() response_name overrides the default for Type A", {
  out <- sim_experiment(make_type_a_schedule(), response_name = "outcome")
  expect_true("outcome" %in% colnames(out))
  expect_false("response" %in% colnames(out))
})

test_that("sim_experiment() response_name overrides the default for Type B", {
  out <- sim_experiment(make_type_b_schedule(), response_name = "outcome")
  expect_true("outcome" %in% colnames(out))
  expect_false("Y" %in% colnames(out))
})

# ── input validation ───────────────────────────────────────────────────────────

test_that("sim_experiment() errors when schedule is not a schedule object", {
  expect_error(sim_experiment(data.frame(x = 1)), "schedule")
})

test_that("sim_experiment() errors when reps is not a positive integer", {
  sched <- make_type_a_schedule()
  expect_error(sim_experiment(sched, reps = 0),    "positive integer")
  expect_error(sim_experiment(sched, reps = -1),   "positive integer")
  expect_error(sim_experiment(sched, reps = 1.5),  "positive integer")
  expect_error(sim_experiment(sched, reps = "one"), "positive integer")
})
