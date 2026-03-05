#' Convert experimental data to a schedule of potential outcomes
#'
#' Takes a data frame in the long format typical of experimental data — one row
#' per unit, with a single response column and a treatment assignment column —
#' and pivots it into a wide schedule of potential outcomes before attaching the
#' \code{"schedule"} class and metadata. The observed response for each unit
#' fills its assigned treatment column; all other potential outcome columns are
#' \code{NA} (unobserved). Use \code{\link{impute_unobserved}} to fill them in.
#'
#' @param data A data frame with one row per experimental unit.
#' @param treatment The unquoted name of the column in \code{data} that contains
#'   the treatment assignments, which must be a factor.
#' @param response The unquoted name of the column in \code{data} that contains
#'   the observed response variable.
#' @param outcome_prefix A character string prepended to each treatment level
#'   name to form the potential outcome column names. Defaults to \code{"Y"}.
#'
#' @return A data frame with class \code{"schedule"}, containing the original
#'   columns plus one new column per treatment level (named
#'   \code{<outcome_prefix><level>}), and metadata attributes:
#'   \code{treatment}, \code{treatment_levels}, \code{treatment_counts},
#'   \code{potential_outcome_cols}, and \code{response}.
#'
#' @seealso \code{\link{as_schedule}}, \code{\link{impute_unobserved}}
#'
#' @examples
#' data <- data.frame(
#'   id = 1:6,
#'   treatment = factor(c("control", "control", "treatment",
#'                        "treatment", "control", "treatment")),
#'   response = c(10, 12, 15, 18, 11, 17)
#' )
#' experiment_to_schedule(data, treatment, response)
#' @export
experiment_to_schedule <- function(data, treatment, response, outcome_prefix = "Y") {
  treatment_nm <- rlang::as_name(rlang::enquo(treatment))
  response_nm  <- rlang::as_name(rlang::enquo(response))

  if (!treatment_nm %in% colnames(data)) {
    stop("The specified treatment column does not exist in the data frame.")
  }
  if (!is.factor(data[[treatment_nm]])) {
    stop("The treatment column must be a factor.")
  }
  if (!response_nm %in% colnames(data)) {
    stop("The specified response column does not exist in the data frame.")
  }

  treatment_levels       <- levels(data[[treatment_nm]])
  potential_outcome_cols <- paste0(outcome_prefix, treatment_levels)

  # Pivot to wide: one potential outcome column per treatment level.
  # The observed value lands in the unit's assigned column; others are NA.
  wide <- data |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    tidyr::pivot_wider(
      names_from   = dplyr::all_of(treatment_nm),
      values_from  = dplyr::all_of(response_nm),
      names_prefix = outcome_prefix,
      names_sort   = TRUE
    ) |>
    dplyr::select(dplyr::all_of(potential_outcome_cols))

  out <- dplyr::bind_cols(data, wide)

  class(out) <- c("schedule", class(out))
  attr(out, "treatment")              <- treatment_nm
  attr(out, "response")               <- response_nm
  attr(out, "treatment_levels")       <- treatment_levels
  attr(out, "treatment_counts")       <- table(data[[treatment_nm]])
  attr(out, "potential_outcome_cols") <- potential_outcome_cols

  out
}