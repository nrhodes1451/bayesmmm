## code to prepare `DATASET` dataset goes here

stanmodel <- rstan::stan_model("stan/model.stan")

usethis::use_data(stanmodel, overwrite = T)
