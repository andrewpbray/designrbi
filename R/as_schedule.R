#' Convert data frame to a schedule of potential outcomes
#' 
#' @param data A data frame of data from a randomized experiment.
#' @param treatment Name of the column in \code{data} that contains the treatment assignments, a factor.
#' @param response Name of the column in \code{data} that contains the response variable.
#' @param append A logical value indicating whether to append the schedule of potential outcomes to the original data frame. 
#' @param outcome_prefix A character string to prefix the names of the columns containing the potential outcomes.
#' 
#' @return A data frame with the schedule of potential outcomes appended as new columns.
#' @export
as_schedule <- function(data, treatment, response, append = TRUE, outcome_prefix = "Y") {
  treatment_nm <- rlang::as_name(rlang::enquo(treatment))
  response_nm  <- rlang::as_name(rlang::enquo(response))

  # Check that the specified columns exist in the data frame
  if (!all(c(treatment_nm, response_nm) %in% colnames(data))) {
    stop("The specified columns do not all exist in the data frame.")
  }

  # Check that the treatment column is a factor
  if (!is.factor(data[[treatment_nm]])) {
    stop("The treatment column must be a factor.")
  }
  
  # Create the schedule of potential outcomes
  schedule <- tidyr::pivot_wider(data, 
    names_from = {{ treatment }}, 
    values_from = {{ response }}, 
    names_prefix = outcome_prefix,
    names_sort = TRUE)
  
  # Add "schedule" class
  class(schedule) <- c("schedule", class(schedule))

  # Store attributes
  attr(schedule, "treatment") <- treatment_nm
  attr(schedule, "response")  <- response_nm
  attr(schedule, "treatment_levels") <- levels(data[[treatment_nm]])
  attr(schedule, "potential_outcome_cols") <- paste0(outcome_prefix, levels(data[[treatment_nm]]))

  if (append) {
    return(schedule)
  } else {
    dplyr::select(schedule, -dplyr::any_of(colnames(data)))
  }
}