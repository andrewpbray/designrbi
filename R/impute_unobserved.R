#' Impute Unobserved Potential Outcomes
#' 
#' Impute the unobserved potential outcomes in a schedule of potential outcomes using a specified value of the Individual Treatment Effect (ITE).
#' 
#' @param schedule A data frame containing the schedule of potential outcomes, with one row per unit and one column for each treatment level.
#' @param tau A numeric value of the Individual Treatment Effect to use to impute for the unobserved potential outcomes.
#' @return A data frame with the same structure as \code{schedule} but with the unobserved potential outcomes imputed using the specified value of \code{tau}.
#' @examples
#' #' data <- data.frame(
#'  id = 1:6,
#'  treatment = factor(c("control", "control", "treatment", "treatment", "control", "treatment")),
#'  response = c(10, 12, 15, 18, 11, 17)
#' )
#' schedule <- as_schedule(data, treatment, response)
#' 
#' # the fisher sharp null: tau = 0
#' imputed_schedule_0 <- impute_unobserved(schedule, tau = 0)
#' print(imputed_schedule_0)
#' 
#' # a constant shift effect size of 3
#' imputed_schedule_3 <- impute_unobserved(schedule, tau = 3)
#' print(imputed_schedule_3)
#' @export
impute_unobserved <- function(schedule, tau) {
  # Check that the schedule is a data frame of class schedule
  if (!inherits(schedule, "schedule")) {
    stop("The schedule must be a data frame of class 'schedule'.")
  }

  # Get the names of the potential outcome columns from the schedule
  potential_outcome_cols <- attr(schedule, "potential_outcome_cols")

  # Impute the unobserved potential outcomes
  if (tau == 0) { # the Fisher Sharp Null
    observed <- apply(schedule[potential_outcome_cols], 1, \(x) x[!is.na(x)][1])
    schedule <- dplyr::mutate(
      schedule,
      dplyr::across(dplyr::all_of(potential_outcome_cols), \(x) dplyr::coalesce(x, observed))
    )
  } else {
    treatment_levels <- attr(schedule, "treatment_levels")
    if (length(treatment_levels) > 2) {
      stop("Non-zero tau imputation is only supported for two treatment levels.")
    }

    # Y0 = lower level, Y1 = higher level; tau = Y1 - Y0
    col0 <- potential_outcome_cols[1]
    col1 <- potential_outcome_cols[2]

    schedule <- dplyr::mutate(
      schedule,
      "{col0}" := dplyr::coalesce(.data[[col0]], .data[[col1]] - tau),
      "{col1}" := dplyr::coalesce(.data[[col1]], .data[[col0]] + tau)
    )
  }

  schedule
}  