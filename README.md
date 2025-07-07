# RoAM

Package supporting the implementation of the Root/Additional Metric construction framework.

Contains functions for constructing a RoAM and calculating RoAM values, weights and effect sample sizes.

For more details on RoAM framework methodology and a full worked example, see  
https://arxiv.org/abs/2507.01526

## Installation

You can install the development version of RoAM from GitHub with:

```R
# install.packages('devtools')
devtools::install_github('lebgoodyear/RoAM')
```

## Examples of use  

```R
library("roam")
```

Metric is calculated for each data entry separately, where the first parameter is a vector of root variables, the second parameter is a vector of additional variables and the third parameter is a vector of betas.

```R
# For example
calc_metric(c(0.2, 0.3), c(1, 0), c(0.5, 0.3, 0.2))
```

Here 0.2 and 0.3 are the values of the two root variables, 0 and 1 are the values of the two additional variables and 0.5 is the weight of the first additional variable, 0.3 the weight of the second, and 0.2 the baseline weight.  

If data is aggregated from another source, artificial errors and confidence intervals can be constructed.

```R
# For example, for artificial errors based only on metric value 
# and sample size
calc_std_error(metric_value = 0.7, sample_size = 100)

# Other uncertainty variables can also be used, such as usability,
# which contributes to weighting the sample size
# For example
calc_grade_weight(grade=2, max_grade=0, min_grade=5, min_weight=0.5)
# Here our data entry is graded 2 (where the best grade is 0 and the worst grade is 5) and we want the minimum weight (weight for worst grade of 5) to be 0.5

# We can then calculate our error and include this grade weight (which was 0.8)
calc_std_error(metric_value = 0.7, sample_size = 100, grade_weight = 0.8)

# Confidence intervals can be calculated using the metric value and this artifical error
# For example
calc_ci(metric_value = 0.7, std_error = 0.2)
```
