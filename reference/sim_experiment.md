# Simulate a randomized experiment

Recreates the process of randomly assigning each unit to a treatment
group and observing the corresponding potential outcome, given a
complete schedule of potential outcomes.

## Usage

``` r
sim_experiment(schedule, reps = 1, response_name = NULL)
```

## Arguments

- schedule:

  A data frame of class `schedule` with no missing potential outcomes,
  as returned by
  [`impute_unobserved()`](https://github.com/andrewpbray/designrbi/reference/impute_unobserved.md).

- reps:

  A positive integer giving the number of simulations to run. Results
  from each simulation are stacked row-wise and identified by the
  `replicate` column. Defaults to `1`.

- response_name:

  A character string giving the name of the response column in the
  output. For schedules created with
  [`experiment_to_schedule()`](https://github.com/andrewpbray/designrbi/reference/experiment_to_schedule.md),
  this defaults to the original response column name. For schedules
  created with
  [`as_schedule()`](https://github.com/andrewpbray/designrbi/reference/as_schedule.md)
  (wide format), there is no original response column name, so this
  defaults to `"Y"`.

## Value

A data frame with the treatment column, the response column (named
according to `response_name`), and a `replicate` integer column
indicating which simulation each row belongs to.

## Examples

``` r
data <- data.frame(
  treatment = factor(c("control", "control", "treatment", "treatment", "control", "treatment")),
  response  = c(10, 12, 15, 18, 11, 17)
)
schedule <- experiment_to_schedule(data, treatment, response)
imputed  <- impute_unobserved(schedule, tau = 0)
sim_experiment(imputed, reps = 5)
#> # A tibble: 30 × 3
#>    replicate treatment response
#>        <int> <fct>        <dbl>
#>  1         1 treatment       10
#>  2         1 control         12
#>  3         1 treatment       15
#>  4         1 treatment       18
#>  5         1 control         11
#>  6         1 control         17
#>  7         2 treatment       10
#>  8         2 control         12
#>  9         2 treatment       15
#> 10         2 control         18
#> # ℹ 20 more rows
```
