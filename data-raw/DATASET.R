## code to prepare `DATASET` dataset goes here

stanmodel <- rstan::stan_model("stan/priors model.stan")
demo_dataset <- readr::read_csv("data-raw/demo.csv")

usethis::use_data(stanmodel, overwrite = T)
usethis::use_data(demo_dataset, overwrite = T)
