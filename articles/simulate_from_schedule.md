# Simulate an Experiment from a Schedule

``` r
library(designrbi)
```

This article demonstrates how to simulate an experiment from a
pre-existing schedule of potential outcomes. This is useful for
calculating power or quantifying the coverage probability of confidence
intervals under a particular data generating process.

### Create a schedule object

The key function for simulating an experiment is . This function takes
as input a *schedule* object. These objects can be created using
[`experiment_to_schedule()`](https://github.com/andrewpbray/designrbi/reference/experiment_to_schedule.md)
if you have an observed dataset with a treatment variable and an outcome
variable. If instead you have a data frame that contains the potential
outcomes for each unit under each treatment condition, you can use
[`as_schedule()`](https://github.com/andrewpbray/designrbi/reference/as_schedule.md)
to convert it to a schedule object.

Let’s start with an example schedule:

``` r
library(readr)

anchoring_sched <- read_csv("https://stat158.berkeley.edu/spring-2026/data/anchoring/godlike_schedule_A.csv")
#> Rows: 62 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> dbl (3): d, Y11, Y73
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
anchoring_sched$d <- factor(anchoring_sched$d)
anchoring_sched
#> # A tibble: 62 × 3
#>    d       Y11   Y73
#>    <fct> <dbl> <dbl>
#>  1 73       15    30
#>  2 73       45    60
#>  3 73       25    40
#>  4 11       25    40
#>  5 11       20    35
#>  6 11       21    36
#>  7 73        5    20
#>  8 11        8    23
#>  9 11       20    35
#> 10 73        5    20
#> # ℹ 52 more rows
```

This data frame contains the potential outcomes for 62 units in the
Anchoring Experiment, where they were assigned to answer a question that
referenced the number `11` or `73` and then we asked to estimated a
percentage of African countries in the United Nations. The columns `Y11`
and `Y73` represent the potential outcomes for each unit under the
control and treatment conditions, respectively. Note these are not the
observed outcomes, but rather the potential outcomes that would be
observed under each treatment condition.

This data frame also contains a factor column that indicates the
treatment assignments. This is useful because it represents the total
number of units that were assigned to each group, and essential piece of
information for simulating experiments that mirror the original design.

We can convert this data frame to a schedule object using
[`as_schedule()`](https://github.com/andrewpbray/designrbi/reference/as_schedule.md).

``` r
schedule <- as_schedule(anchoring_sched, treatment = d, potential_outcomes = c(Y11, Y73))
schedule
#> # A tibble: 62 × 3
#>    d       Y11   Y73
#>    <fct> <dbl> <dbl>
#>  1 73       15    30
#>  2 73       45    60
#>  3 73       25    40
#>  4 11       25    40
#>  5 11       20    35
#>  6 11       21    36
#>  7 73        5    20
#>  8 11        8    23
#>  9 11       20    35
#> 10 73        5    20
#> # ℹ 52 more rows
```

### Simulate an experiment from the schedule

Now that we have a schedule object, we can simulate an experiment using
the
[`sim_experiment()`](https://github.com/andrewpbray/designrbi/reference/sim_experiment.md)
function. This function takes as input a schedule object and the number
of simulations to run.

``` r
set.seed(5024) # for reproducibility
simulated_experiment <- schedule |>
  sim_experiment(reps = 1)
simulated_experiment
#> # A tibble: 62 × 3
#>    replicate d         Y
#>        <int> <fct> <dbl>
#>  1         1 73       30
#>  2         1 11       45
#>  3         1 73       40
#>  4         1 73       40
#>  5         1 73       35
#>  6         1 73       36
#>  7         1 11        5
#>  8         1 11        8
#>  9         1 73       35
#> 10         1 11        5
#> # ℹ 52 more rows
```

This data frame contains the observed outcomes for each unit under the
simulated treatment assignments. Each row corresponds to a single unit,
and the columns represent the treatment assignment and the observed
outcome for that unit in the simulated experiment.
