# bayesmmm

Pooled Marketing Mix Modelling using RStan.

This project was born out of a desire to reimagine traditional frequentist MMM in a hierarchical Bayesian framework. Much of the credit here goes to Richard McElreath and the fantastic Statistical Rethinking for introducing me to Bayesian statistics and associated philosophy.

### Update

Please note that this package is currently incomplete. I hope to complete/refine as and when I have time, but this is currently taking the back seat between work/study/moving house.

## Installation

```r
library(devtools)
install_github("https://github.com/nrhodes1451/bayesmmm")
```
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
  <img height="300" src="https://github.com/nrhodes1451/bayesmmm/blob/master/img/model.png">
</p>

### View coefficient posterior distributions:
```r
model$plots$coeffs$Seasonality
```
<p align="center">
  <img height="300" src="https://github.com/nrhodes1451/bayesmmm/blob/master/img/coeffs.png">
</p>

### Get model decomposition calculated from MAP estimates
```r
model$decomp %>% write_csv("decomp.csv")
```

## Media transformations

```r
variables <- c("Media",
               "Seasonality",
               "CCI",
               "Competitor") %>% variable_grid
```
Set a variable's transformation to "media" within the `variable_grid` in order to calculate media Adstocks/diminishing returns rates
```r
variables$transformation[1] <- "media"

model <- bayesmmm::bayesmodel(model_data,
                            y,
                            variables)
```

### View media transformation posterior distributions:

```r
model$plots$adstocks
```

<p align="center">
  <img height="300" src="https://github.com/nrhodes1451/bayesmmm/blob/master/img/adstock.png">
</p>

```r
model$plots$denominators
```

<p align="center">
  <img height="300" src="https://github.com/nrhodes1451/bayesmmm/blob/master/img/denominator.png">
</p>

### View trace plots for convergence
```r
model %>% trace_plot("Media")
```
<p align="center">
  <img height="300" src="https://github.com/nrhodes1451/bayesmmm/blob/master/img/chains.png">
</p>

### View model decomposition
```r
model$plots$decomp
```
<p align="center">
  <img height="300" src="https://github.com/nrhodes1451/bayesmmm/blob/master/img/decomp.png">
</p>
