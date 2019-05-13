# bayesmmm

Pooled Marketing Mix Modelling using RStan.

This project was born out of a desire to reimagine traditional frequentist MMM in a hierarchical Bayesian framework. Much of the credit here goes to Richard McElreath and his fantastic book Statistical Rethinking for introducing me to Bayesian statistics and philosophy.

## Examples

```r
library(tidyverse)
library(bayesmmm)

model_data <- bayesmmm::demo_dataset

y <- "Sales"
variables <- c("Media",
               "Seasonality",
               "CCI",
               "Competitor") %>% variable_grid

model <- bayesmmm::bayesmodel(model_data,
                            y,
                            variables)

# View model fit
model$plots$fit

# View coefficient distributions:
model$plots$coeffs$Seasonality

# Get decomp
model$decomp

# View variable coefficient convergence
model %>% trace_plot("Seasonality")
```

## Media transformations

```r
variables <- c("Media",
               "Seasonality",
               "CCI",
               "Competitor") %>% variable_grid
variables$transformation[1] <- "media"

model <- bayesmmm::bayesmodel(model_data,
                            y,
                            variables)

# View media transformation distributions:
model$plots$adstocks
model$plots$denominators

# View trace plots
model %>% trace_plot("Media")
```
