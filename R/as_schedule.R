#' Coerce a wide data frame to a schedule of potential outcomes
#'
#' Attaches the \code{"schedule"} class and metadata attributes to a data frame
#' that is already structured as a schedule of potential outcomes — i.e., one
#' column per treatment level containing the potential outcomes for each unit.
#' To convert long-format experimental data instead, see
#' \code{\link{experiment_to_schedule}}.
#'
#' @param data A data frame structured as a schedule of potential outcomes, with
#'   one column per treatment level.
#' @param treatment The unquoted name of the column in \code{data} that contains
#'   the treatment assignments, which must be a factor.
#' @param potential_outcomes A tidyselect expression selecting the columns that
#'   contain the potential outcomes, one per treatment level (e.g.,
#'   \code{c(Ycontrol, Ytreatment)} or \code{starts_with("Y")}).
#'
#' @return A data frame with class \code{"schedule"} and metadata attributes:
#'   \code{treatment}, \code{treatment_levels}, \code{treatment_counts}, and
#'   \code{potential_outcome_cols}.
#'
#' @seealso \code{\link{experiment_to_schedule}}
#'
#' @examples
#' sched <- data.frame(
#'   id = 1:6,
#'   treatment = factor(c("control", "control", "treatment",
#'                        "treatment", "control", "treatment")),
#'   Ycontrol   = c(10, 12, 15, 18, 11, 17),
#'   Ytreatment = c(11, 13, 16, 19, 10, 17)
#' )
#' as_schedule(sched, treatment, c(Ycontrol, Ytreatment))
#' @export
as_schedule <- function(data, treatment, potential_outcomes) {
  treatment_nm <- rlang::as_name(rlang::enquo(treatment))
  potential_outcome_cols <- names(
    tidyselect::eval_select(rlang::enquo(potential_outcomes), data)
  )

  if (!treatment_nm %in% colnames(data)) {
    stop("The specified treatment column does not exist in the data frame.")
  }
  if (!is.factor(data[[treatment_nm]])) {
    stop("The treatment column must be a factor.")
  }
  if (length(potential_outcome_cols) < 2) {
    stop(
      "At least two potential outcome columns must be selected. ",
      "To convert long-format experimental data, use `experiment_to_schedule()`."
    )
  }

  treatment_levels <- levels(data[[treatment_nm]])
  treatment_counts <- table(data[[treatment_nm]])

  out <- data
  class(out) <- c("schedule", class(out))
  attr(out, "treatment")              <- treatment_nm
  attr(out, "treatment_levels")       <- treatment_levels
  attr(out, "treatment_counts")       <- treatment_counts
  attr(out, "potential_outcome_cols") <- potential_outcome_cols

  out
}
