# bayesmmm

Pooled MMM using RStan.

## Example

```r
library(tidyverse)
library(rbayes)

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
