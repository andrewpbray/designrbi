# Coerce a wide data frame to a schedule of potential outcomes

Attaches the `"schedule"` class and metadata attributes to a data frame
that is already structured as a schedule of potential outcomes — i.e.,
one column per treatment level containing the potential outcomes for
each unit. To convert long-format experimental data instead, see
[`experiment_to_schedule`](https://github.com/andrewpbray/designrbi/reference/experiment_to_schedule.md).

## Usage

``` r
as_schedule(data, treatment, potential_outcomes)
```

## Arguments

- data:

  A data frame structured as a schedule of potential outcomes, with one
  column per treatment level.

- treatment:

  The unquoted name of the column in `data` that contains the treatment
  assignments, which must be a factor.

- potential_outcomes:

  A tidyselect expression selecting the columns that contain the
  potential outcomes, one per treatment level (e.g.,
  `c(Ycontrol, Ytreatment)` or `starts_with("Y")`).

## Value

A data frame with class `"schedule"` and metadata attributes:
`treatment`, `treatment_levels`, `treatment_counts`, and
`potential_outcome_cols`.

## See also

[`experiment_to_schedule`](https://github.com/andrewpbray/designrbi/reference/experiment_to_schedule.md)

## Examples

``` r
sched <- data.frame(
  id = 1:6,
  treatment = factor(c("control", "control", "treatment",
                       "treatment", "control", "treatment")),
  Ycontrol   = c(10, 12, 15, 18, 11, 17),
  Ytreatment = c(11, 13, 16, 19, 10, 17)
)
as_schedule(sched, treatment, c(Ycontrol, Ytreatment))
#>   id treatment Ycontrol Ytreatment
#> 1  1   control       10         11
#> 2  2   control       12         13
#> 3  3 treatment       15         16
#> 4  4 treatment       18         19
#> 5  5   control       11         10
#> 6  6 treatment       17         17
```
