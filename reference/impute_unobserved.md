# Impute Unobserved Potential Outcomes

Impute the unobserved potential outcomes in a schedule of potential
outcomes using a specified value of the Individual Treatment Effect
(ITE).

## Usage

``` r
impute_unobserved(schedule, tau)
```

## Arguments

- schedule:

  A data frame containing the schedule of potential outcomes, with one
  row per unit and one column for each treatment level.

- tau:

  A numeric value of the Individual Treatment Effect to use to impute
  for the unobserved potential outcomes.

## Value

A data frame with the same structure as `schedule` but with the
unobserved potential outcomes imputed using the specified value of
`tau`.

## Examples

``` r
data <- data.frame(
 id = 1:6,
 treatment = factor(c("control", "control", "treatment", "treatment", "control", "treatment")),
 response = c(10, 12, 15, 18, 11, 17)
)
schedule <- experiment_to_schedule(data, treatment, response)

# the fisher sharp null: tau = 0
imputed_schedule_0 <- impute_unobserved(schedule, tau = 0)
print(imputed_schedule_0)
#>   id treatment response Ycontrol Ytreatment
#> 1  1   control       10       10         10
#> 2  2   control       12       12         12
#> 3  3 treatment       15       15         15
#> 4  4 treatment       18       18         18
#> 5  5   control       11       11         11
#> 6  6 treatment       17       17         17

# a constant shift effect size of 3
imputed_schedule_3 <- impute_unobserved(schedule, tau = 3)
print(imputed_schedule_3)
#>   id treatment response Ycontrol Ytreatment
#> 1  1   control       10       10         13
#> 2  2   control       12       12         15
#> 3  3 treatment       15       12         15
#> 4  4 treatment       18       15         18
#> 5  5   control       11       11         14
#> 6  6 treatment       17       14         17
```
