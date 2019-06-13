# bayesmmm

Pooled Marketing Mix Modelling using RStan.

This project was born out of a desire to reimagine traditional frequentist MMM in a hierarchical Bayesian framework. Much of the credit here goes to Richard McElreath and the fantastic Statistical Rethinking for introducing me to Bayesian statistics and associated philosophy.

## Examples


```r
library(tidyverse)
library(bayesmmm)

model_data <- bayesmmm::demo_dataset
```

Use the `variable_grid()` function to create a data frame to contain the model specification

```r
y <- "Sales"
variables <- c("Media",
               "Seasonality",
               "CCI",
               "Competitor") %>% variable_grid
```
Run the model

```r
model <- bayesmmm::bayesmodel(model_data,
                            y,
                            variables)
```

### View model fit
```r
model$plots$fit
```
<p align="center">
  <img height="200" src="https://github.com/nrhodes1451/bayesmmm/blob/master/img/model.png">
</p>

### View coefficient distributions:
```r
model$plots$coeffs$Seasonality
```
<p align="center">
  <img height="200" src="https://github.com/nrhodes1451/bayesmmm/blob/master/img/coeffs.png">
</p>

### Get decomp
```r
model$decomp
```

### View variable coefficient convergence
```r
model %>% trace_plot("Seasonality")
```
<p align="center">
  <img height="200" src="https://github.com/nrhodes1451/bayesmmm/blob/master/img/chains.png">
</p>

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
