#' Simulate a randomized experiment
#'
#' Recreates the process of randomly assigning each unit to a treatment group
#' and observing the corresponding potential outcome, given a complete schedule
#' of potential outcomes.
#'
#' @param schedule A data frame of class \code{schedule} with no missing
#'   potential outcomes, as returned by \code{impute_unobserved()}.
#' @param reps A positive integer giving the number of simulations to run.
#'   Results from each simulation are stacked row-wise and identified by the
#'   \code{replicate} column. Defaults to \code{1}.
#'
#' @return A data frame with the same columns as the original input to
#'   \code{as_schedule()} (i.e. the treatment and response columns), plus a
#'   \code{replicate} integer column indicating which simulation each row
#'   belongs to.
#'
#' @examples
#' data <- data.frame(
#'   treatment = factor(c("control", "control", "treatment", "treatment", "control", "treatment")),
#'   response  = c(10, 12, 15, 18, 11, 17)
#' )
#' schedule <- as_schedule(data, treatment, response)
#' imputed  <- impute_unobserved(schedule, tau = 0)
#' sim_experiment(imputed, reps = 5)
#' @export
sim_experiment <- function(schedule, reps = 1) {
  if (!inherits(schedule, "schedule")) {
    stop("'schedule' must be an object of class 'schedule'.")
  }
  if (!is.numeric(reps) ||
      length(reps) != 1L ||
      is.na(reps) ||
      !is.finite(reps) ||
      reps < 1L ||
      reps != as.integer(reps)) {
    stop("'reps' must be a positive integer.")
  }

  treatment_nm           <- attr(schedule, "treatment")
  response_nm            <- attr(schedule, "response")
  treatment_levels       <- attr(schedule, "treatment_levels")
  potential_outcome_cols <- attr(schedule, "potential_outcome_cols")
  group_sizes            <- as.integer(attr(schedule, "treatment_counts"))

  n <- nrow(schedule)

  # Build a lookup: potential_outcome_cols[i] corresponds to treatment_levels[i]
  po_lookup <- stats::setNames(potential_outcome_cols, treatment_levels)

  purrr::map(
    seq_len(reps),
    function(rep_i) {
      # Draw a random treatment assignment with the same group sizes
      new_treatment <- factor(
        sample(rep(treatment_levels, times = group_sizes)),
        levels = treatment_levels
      )

      # Observe the potential outcome matching each unit's assigned treatment
      new_response <- purrr::map2_dbl(
        seq_len(n),
        as.character(new_treatment),
        \(row, trt) schedule[[po_lookup[trt]]][row]
      )

      tibble::tibble(
        replicate        = rep_i,
        !!treatment_nm  := new_treatment,
        !!response_nm   := new_response
      )
    }
  ) |>
    purrr::list_rbind()
}
