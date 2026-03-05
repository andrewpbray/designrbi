
## `designrbi`

\
\
\

#### Overview

`designrbi` is an R package for conducting statistical inference on randomized experiments using the randomization-based framework. It offers a series of helpful functions.

- `experiment_to_schedule()` will transform the data from an experiment into a schedule of potential outcomes with the missing outcomes showing up as `NA`.
- `impute_unobserved()` will impute the missing potential outcomes in a schedule using a constant effect of your choice.
- `sim_experiment()` will simulate one or more experiments based on a given schedule of potential outcomes.
- `as_schedule()` will take a pre-existing data frame with all of the potential outcomes present and convert it into a schedule object.


#### Installation

Install the package directly from github using the `remotes` package.

```r
# install.packages("remotes")
library(remotes)
install_github("andrewpbray/designrbi")
```

See the **Articles** link in the top menu for guidance on how to use the package.



