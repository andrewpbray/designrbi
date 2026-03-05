# Convert experimental data to a schedule of potential outcomes

Takes a data frame in the long format typical of experimental data — one
row per unit, with a single response column and a treatment assignment
column — and pivots it into a wide schedule of potential outcomes before
attaching the `"schedule"` class and metadata. The observed response for
each unit fills its assigned treatment column; all other potential
outcome columns are `NA` (unobserved). Use
[`impute_unobserved`](https://github.com/andrewpbray/designrbi/reference/impute_unobserved.md)
to fill them in.

## Usage

``` r
experiment_to_schedule(data, treatment, response, outcome_prefix = "Y")
```

## Arguments

- data:

  A data frame with one row per experimental unit.

- treatment:

  The unquoted name of the column in `data` that contains the treatment
  assignments, which must be a factor.

- response:

  The unquoted name of the column in `data` that contains the observed
  response variable.

- outcome_prefix:

  A character string prepended to each treatment level name to form the
  potential outcome column names. Defaults to `"Y"`.

## Value

A data frame with class `"schedule"`, containing the original columns
plus one new column per treatment level (named
`<outcome_prefix><level>`), and metadata attributes: `treatment`,
`treatment_levels`, `treatment_counts`, `potential_outcome_cols`, and
`response`.

## See also

[`as_schedule`](https://github.com/andrewpbray/designrbi/reference/as_schedule.md),
[`impute_unobserved`](https://github.com/andrewpbray/designrbi/reference/impute_unobserved.md)

## Examples

``` r
data <- data.frame(
  id = 1:6,
  treatment = factor(c("control", "control", "treatment",
                       "treatment", "control", "treatment")),
  response = c(10, 12, 15, 18, 11, 17)
)
experiment_to_schedule(data, treatment, response)
#>   id treatment response Ycontrol Ytreatment
#> 1  1   control       10       10         NA
#> 2  2   control       12       12         NA
#> 3  3 treatment       15       NA         15
#> 4  4 treatment       18       NA         18
#> 5  5   control       11       11         NA
#> 6  6 treatment       17       NA         17
```
