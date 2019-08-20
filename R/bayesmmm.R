#' @import rstan
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @import zoo

#' @title variable_grid
#' @description A utility function to create a template variable data frame from a list of variables.
#' @param variables A character vector of variable names.
#' @return A data frame containing one row for each independent variable and the following columns: ' \describe{
#'   \item{variable}{The variable name}
#'   \item{expected sign}{Either "+" or "-" for half-normal prior distributions. Any other values will be ignored and treated as NA. Default NA}
#'   \item{transformation}{Choose between "standard", "standard pooled", or "media".}
#'   \item{reference.point}{Choose between "0", "min", or "max".}
#'   \item{global}{An optional boolean to specify whether coefficients should vary across pools. If FALSE, a separate coefficient will be estimated for each pool using hierarchical partial-pooling, otherwise a single coefficient will be estimated. Default FALSE.}
#'   \item{prior.mean}{The prior mean. NA assumes a weakly informative prior distribution (i.e. zero after standardisation).}
#'   \item{prior.sd}{The prior standard deviation. NA assumes a weakly informative prior distribution (i.e. 1 after standardisation).}
#' }
#' @examples variables <- variable_grid(c("media", "seasonality"))
#' @export
variable_grid <- function(variables){
  tibble::tibble("variable"=variables,
                 "expected.sign"=NA,
                 "transformation"="Standard",
                 "reference.point"=0,
                 "global"=FALSE,
                 "prior.mean"=NA,
                 "prior.sd"=NA)
}

#' @title bayesmodel
#' @description Creates a bayesian model using rstan.
#' @param data A data frame containing a column of observations (labelled "obs"), an optional column of pool names (labelled "pools"), the dependent variable, and indepedendent variables.
#' @param y A string identifying the dependent variable.
#' @param variables a data frame (optionally created by the \code{\link{variable_grid}} function) containing one row for each independent variable and the following columns: ' \describe{
#'   \item{variable}{The variable name}
#'   \item{expected sign}{Either "+" or "-" for half-normal prior distributions. Any other values will be ignored and treated as NA. Default NA}
#'   \item{transformation}{Choose between "standard", "standard pooled", or "media".}
#'   \item{reference.point}{Choose between "0", "min", or "max".}
#'   \item{global}{An optional boolean to specify whether coefficients should vary across pools. If FALSE, a separate coefficient will be estimated for each pool using hierarchical partial-pooling, otherwise a single coefficient will be estimated. Default FALSE.}
#'   \item{prior.mean}{The prior mean. NA assumes a weakly informative prior distribution (i.e. zero after standardisation).}
#'   \item{prior.sd}{The prior standard deviation. NA assumes a weakly informative prior distribution (i.e. 1 after standardisation).}
#' }
#' @param observations an optional string vector of observation names, e.g. c("2014-01-01", "2014-01-08", ...). Default NULL
#' @param cores The number of cores to be used for the markov chain estimation. Default 4.
#' @return \describe{
#'   \item{model}{the Stan model}
#'   \item{summary}{a data frame of untransformed MAP estimates}
#'   \item{script}{the generated Stan code}
#'   \item{scale}{the scaling factors used for standardisation}
#'   \item{y}{the y vector}
#'   \item{x}{the x data frame}
#'   \item{priors}{the priors (if any)}
#'   \item{observations}{the observation names (if any)}
#' }
#' @examples model <- bayesmodel(dep, indeps, priors, observations=obs)
#' @export
bayesmodel <- function(data, y, variables,
                       observations=NULL,
                       cores=4,
                       chains=4,
                       adstock_range=c(0.1, 0.7),
                       chainlength=2000) {
  # Error handling ----
  # Missing date field
  if(!("date" %in% names(data))){
    stop("date column not found in data. Please supply a column with name 'date' containing a list of observation labels.")
  }
  if(is.null(observations)){
    observations <- data$date
  }
  observations <- unique(observations)
  # Missing KPI
  if(!(y %in% names(data))){
    stop(paste0("Dependent variable: ", y, " not found in data."))
  }
  # Missing reference point
  if(is.null(variables$reference.point)){
    variables$reference.point <- 0
  }
  variables$reference.point[is.na(variables$reference.point)] <- 0
  variables$reference.point <- tolower(variables$reference.point)
  # Missing Priors
  if(is.null(variables$prior.mean)){
    variables$prior.mean <- NA
  }
  if(is.null(variables$prior.sd)){
    variables$prior.sd <- NA
  }
  # Convert variable names to character
  if(is.factor(variables$variable)){
    variables$variable <- as.character(variables$variable)
  }
  # Ignore intercept(s)
  intercepts <- variables$variable[data[data$date %in% observations,
                                        variables$variable] %>%
                                     sapply(sd) == 0]
  if(length(intercepts) > 0){
    message(
      paste("The following variables have no variance for the observation range and will be ignored:",
            intercepts))
    variables <- variables %>%
      filter(!(variable %in% intercepts))
  }

  if(!"expected.sign" %in% names(variables)){
    variables$expected.sign <- NA
  }
  if(!("transformation" %in% names(variables))){
    variables$transformation <- "Standard"
  }
  if(!"global" %in% names(variables)){
    variables$global <- FALSE
  }
  if(!("poolname" %in% names(data))){
    data$poolname <- "total"
  }

  data[is.na(data)] <- 0

  # Data processing ----
  message("Transforming data")

  cnames <- c("poolname", "date", y, variables$variable)
  poolnames <- unique(data$poolname)

  model_data <- list(raw = data[cnames])

  # Transform data
  accepted_transformations <- c(
    "standard",
    "standard pooled",
    "media"
  )
  variables$transformation <- tolower(variables$transformation)
  variables$transformation[
    !(variables$transformation %in% accepted_transformations)] <-
    accepted_transformations[1]

  # transformation grid
  means <- data %>% filter(date %in% observations) %>%
    select(-date) %>%
    group_by(poolname) %>%
    summarise_all(list(mean)) %>%
    gather(variable, mean, -poolname)
  sds <- data %>% filter(date %in% observations) %>%
    select(-date) %>%
    group_by(poolname) %>%
    summarise_all(list(sd)) %>%
    gather(variable, sd, -poolname)
  maxes <- data %>% filter(date %in% observations) %>%
    select(-date) %>%
    group_by(poolname) %>%
    summarise_all(list(max)) %>%
    gather(variable, max, -poolname)
  global_means <- data %>% filter(date %in% observations) %>%
    select(-date, -poolname) %>%
    sapply(mean) %>%
    enframe(name="variable") %>%
    rename(gmean=value)
  global_sds <- data %>% filter(date %in% observations) %>%
    select(-date, -poolname) %>%
    sapply(sd) %>%
    enframe(name="variable") %>%
    rename(gsd=value)
  global_maxes <- data %>% filter(date %in% observations) %>%
    select(-date, -poolname) %>%
    sapply(max) %>%
    enframe(name="variable") %>%
    rename(max=value)
  model_data$transformations <- merge(means, sds) %>%
    merge(maxes) %>%
    # merge(global_means) %>%
    # merge(global_sds) %>%
    filter(variable %in% cnames) %>%
    merge(select(variables, variable, transformation),
          all.x=T)
  model_data$transformations[is.na(model_data$transformations)] <-
    "dependent"
  model_data$transformations <- model_data$transformations %>%
    merge(model_data$transformations %>%
            filter(transformation=="dependent") %>%
            select(poolname, sd) %>%
            rename(ysd=sd)) %>%
    as_tibble %>%
    mutate(scale=ysd)

  model_data$transformed <- model_data$raw %>%
    gather(variable, value, -poolname, -date) %>%
    merge(model_data$transformations) %>%
    as_tibble

  # Always standard normalise dependent variable
  df_dep <- model_data$transformed %>%
    filter(transformation=="dependent") %>%
    mutate(value = (value-mean)/sd)

  # Standard transformed variables are mean centered
  df_sta <- model_data$transformed %>%
    filter(transformation=="standard") %>%
    mutate(value = (value-mean))
  # and then divided by the resulting standard deviation
  df_sta_sd <- df_sta %>% group_by(variable) %>%
    summarise(gsd = sd(value))
  df_sta <- df_sta %>% merge(df_sta_sd) %>% as_tibble %>%
    mutate(value = value / gsd,
           scale = scale / gsd) %>%
    select(-gsd)

  # Standard pooled variables are pooled mean centered
  df_sta_p <- model_data$transformed %>%
    filter(transformation=="standard pooled") %>%
    mutate(value = (value-mean) / sd,
           scale = scale / sd)

  # Media variables are divided by the maximum value
  df_med <- model_data$transformed %>%
    filter(transformation=="media") %>%
    mutate(value = value / max)

  # Combine
  model_data$transformed <- df_dep %>%
    rbind(df_sta) %>%
    rbind(df_sta_p) %>%
    rbind(df_med)

  # Fixed negative variables are multiplied by -1
  model_data$transformed <- model_data$transformed %>%
    merge(select(variables, variable, expected.sign), all.x=T) %>%
    mutate(expected.sign = expected.sign=="-")
  model_data$transformed$expected.sign[
    is.na(model_data$transformed$expected.sign)] <- FALSE
  model_data$transformed$value[
    model_data$transformed$expected.sign] <- model_data$transformed$value[
      model_data$transformed$expected.sign] * -1
  model_data$transformed$scale[
    model_data$transformed$expected.sign] <- model_data$transformed$scale[
      model_data$transformed$expected.sign] * -1

  model_data$transformations <- model_data$transformed %>%
    select(poolname, variable, transformation, scale) %>%
    unique

  model_data$transformed <- model_data$transformed %>%
    select(poolname, variable, date, value) %>%
    spread(variable, value)

  # Fill NA values
  model_data$transformed[is.na(model_data$transformed)] <- 0

  results <- list()
  stan_data <- list()

  # modelling ----

  stan_data$num_pools <- length(poolnames)
  stan_data$total_obs <- length(data$date %>% unique)
  stan_data$model_obs <- length(observations)
  # Create vector of modelled observation indices
  # We provide stan with the full list as these are used for adstocks etc.
  # but only the modelled obs are used for calculating parameters
  stan_data$obs_ix <- rep(which(unique(data$date) %in% observations),
                          length(poolnames))

  # Create vector of pool indices
  stan_data$pool_ix <- match(model_data$transformed[model_data$transformed$date %in% observations,]$poolname, poolnames)

  stan_data$num_vars <- nrow(variables)
  # 2-D independent variables data frame, dims -> [dates, pools]
  stan_data$X <- model_data$transformed[variables$variable] %>% t
  # Dependent variable vector
  stan_data$y <- model_data$transformed[model_data$transformed$date %in% observations,][[y]]

  # Add scaling factors for relative coefficient weights by pools
  stan_data$pool_scale <- model_data$transformations %>%
    filter(transformation!="dependent") %>%
    select(poolname, variable, scale) %>%
    spread(poolname, scale)
  stan_data$pool_scale <- cbind(stan_data$pool_scale[1], stan_data$pool_scale[order(poolnames)+1])
  stan_data$pool_scale <- stan_data$pool_scale[order(variables$variable),]
  stan_data$pool_scale$global <- rowMeans(stan_data$pool_scale[-1]) %>%
    as.vector
  for(i in 2:(length(stan_data$pool_scale)-1)){
    stan_data$pool_scale[[i]] <- stan_data$pool_scale[[i]] /
      stan_data$pool_scale$global
  }
  variables$prior.mean[is.na(variables$prior.mean)] <- 0
  variables$prior.sd[which(is.na(variables$prior.sd))] <-
    stan_data$pool_scale$global[which(is.na(variables$prior.sd))]
  # Shape = [variables, pools]
  stan_data$pool_scale <- stan_data$pool_scale[-1] %>% as_tibble %>% as.matrix

  # Matrix of identifiers for null variable/pool combinations
  # i.e. variable has no data for this pool
  # Shape = [variables, pools]
  stan_data$is_null <- model_data$transformed[c("poolname", "date", variables$variable)] %>%
    filter(date %in% observations) %>%
    group_by(poolname) %>%
    select(-date) %>%
    summarise_all(list(sd)) %>%
    select(-poolname)
  stan_data$is_null <- 1 * (stan_data$is_null==0)
  stan_data$is_null[is.na(stan_data$is_null)] <- 1

  # Add priors to stan data list
  stan_data$pmeans <- variables$prior.mean %>% as.array()
  stan_data$psds <- variables$prior.sd %>% as.array()

  # Group variables by category
  # Floating Variables
  stan_data$flo_ix <- which(is.na(variables$expected.sign)) %>% as.array()
  stan_data$num_vars_flo <- length(stan_data$flo_ix)
  # Fixed Variables
  stan_data$fix_ix <- which(!is.na(variables$expected.sign)) %>% as.array()
  stan_data$num_vars_fix <- length(stan_data$fix_ix)

  # Media Variables
  stan_data$med_ix <- which(variables$transformation=="media") %>% as.array()
  stan_data$num_vars_med <- length(stan_data$med_ix)

  # Scaled Variables
  stan_data$scaled_ix <- which(!variables$global) %>% as.array()
  stan_data$num_vars_scaled <- length(stan_data$scaled_ix)

  stan_data$adstock_range <- adstock_range

  # Finally run the Stan model
  message("Running stan code...")

  results$stanmodel <- rstan::sampling(stanmodel,
                                       data=stan_data,
                                       cores=cores,
                                       iter=chainlength,
                                       chains=chains,
                                       control=list(max_treedepth=15))

  # Generate model summary ----
  message("Generating untransformed model summary")
  model_summary <- rstan::summary(results$stanmodel)
  model_summary <- model_summary$summary %>% data.frame
  model_summary$id <- rownames(model_summary)
  model_summary <- as_tibble(model_summary, .name_repair="minimal")
  stan_ids <- list()

  # Fitted values
  fitted_summary <- model_summary %>% filter(substring(id, 1, 3) == "mu[")
  fitted_summary$poolname <- stan_data$pool_ix
  fitted_summary <- fitted_summary %>% mutate(poolname = poolnames[poolname]) %>%
    merge(select(filter(model_data$transformations, variable==y), poolname, scale))
  fitted_summary$date <- model_data$transformed$date[stan_data$obs_ix]
  fitted_summary[2:9] <- fitted_summary[2:9] * fitted_summary$scale

  stan_ids$fitted <- fitted_summary %>% select(poolname, date, id) %>%
    as_tibble

  fitted_summary <- fitted_summary[c(1,14,2:9)] %>% as_tibble %>%
    rename(fitted=mean)
  fitted_summary <- fitted_summary %>%
    merge(means %>% filter(variable==y)) %>%
    select(-variable)
  fitted_summary[c(3,6:10)] <- fitted_summary[c(3,6:10)] + fitted_summary$mean
  fitted_summary <- fitted_summary %>% as_tibble

  # Coefficients
  beta_summary <- model_summary %>%
    filter(substring(id, 1, 10) == "pool_beta[" |
             id == "a" |
             substring(id, 1, 2) == "a[")
  beta_labels <- expand.grid(poolnames, variables$variable, stringsAsFactors = F)
  names(beta_labels) <- c("poolname", "variable")
  beta_labels <- rbind(beta_labels[1:length(poolnames),], beta_labels)
  beta_labels$variable[1:length(poolnames)] <- "Intercept"
  beta_summary <- cbind(beta_labels, beta_summary) %>%
    merge(model_data$transformations, by=c("poolname", "variable"), all.x=T) %>%
    select(-transformation)

  stan_ids$variables <- beta_summary %>% select(
    poolname, variable, id
  ) %>% as_tibble

  beta_summary$scale[is.na(beta_summary$scale)] <- 1
  beta_summary <- as_tibble(beta_summary) %>% select(-id)
  results$scaled_summary <- beta_summary
  results$summary <- beta_summary
  results$summary[3:10] <- results$summary[3:10] * results$summary$scale

  # Add transformations
  transformed_variables <- variables$variable[variables$transformation=="media"]
  transformations <- do.call(rbind, lapply(seq(transformed_variables), function(i){
    variable <- transformed_variables[i]
    ads <- model_summary[model_summary$id==paste0("ads_tra[",i,"]"),]
    ads$Variable <- paste(variable, "ads")
    ads$scale <- 1

    denominator <- model_summary[model_summary$id==paste0("den_tra[",i,"]"),]
    denominator$Variable <- paste(variable, "denominator")
    # Rescale denominator
    denominator$scale <- model_data$raw[[variable]][model_data$raw$date %in%
                                                      observations] %>% max
    trans <- ads %>% rbind(denominator)
    return(trans)
  }))

  if(!is.null(transformations)){
    stan_ids$transformations <- transformations %>%
      select(Variable, id) %>%
      rename("variable" = Variable)

    transformations <- transformations %>% select(-id)
    transformations$MAP <- transformations$mean
    transformations <- transformations[c(11, 13, 1:10, 12)]
    results$scaled_transformations <- transformations
    results$transformations <- transformations
    results$transformations[2:10] <- results$transformations[2:10] * results$transformations$scale
  }

  results$stan_ids <- stan_ids

  results$significance <- beta_summary %>%
    filter(variable != "Intercept") %>%
    select(poolname, variable, mean, sd) %>%
    mutate(T = abs(mean/sd)) %>%
    as_tibble()

  # Create posterior distribution plots ----
  message("Creating posterior distribution plots")
  samples <- rstan::extract(results$stanmodel)

  plots <- list(
    coeffs=list(),
    adstocks=list(),
    denominators=list()
  )
  maps <- data.frame()

  # Intercepts
  coeffs <- samples$a
  colnames(coeffs) <- poolnames
  coeffs <- coeffs %>% as_tibble
  post <- posterior_plot(coeffs)
  plots$coeffs$intercept <- post$chart
  maps <- post$MAP %>%
    mutate(variable="Intercept") %>%
    rename(poolname=key) %>%
    select(poolname, variable, MAP)

  # Rescale variable coefficients
  coeffs <- samples$pool_beta

  # Variables
  for(i in 1:nrow(variables)){
    v <- variables$variable[i]
    coeff <- coeffs[,i,]
    media <- variables$transformation[i]=="media"
    scale <- results$summary %>% filter(variable==v) %>%
      select(poolname, scale)
    scale <- scale[order(poolnames),]$scale
    coeff <- (coeff %*% diag(scale)) %>% as_tibble
    names(coeff) <- poolnames
    post_i <- posterior_plot(coeff)

    plots$coeffs[[v]] <- post_i$chart

    post_i$MAP <- post_i$MAP %>%
      mutate(variable=v) %>%
      rename(poolname=key) %>%
      select(poolname, variable, MAP)
    maps <- maps %>% rbind(post_i$MAP)

    # Media Variables
    if(media){
      j <- which(variables$variable[
        variables$transformation=="media"]==v)
      # Adstock
      ads <- samples$ads_tra[, j] %>% data.frame
      names(ads) <- paste(v, "ads")
      ads_post <- posterior_plot(ads)
      plots$adstocks[[v]] <- ads_post$chart

      # Denominator
      den <- data.frame(
        max(model_data$raw[model_data$raw$date %in% observations, v]) *
          samples$den_tra[, j])
      names(den) <- paste(v, "denominator")
      den_post <- posterior_plot(den)
      plots$denominators[[v]] <- den_post$chart

      # Add MAP to transformations data frame
      results$transformations$MAP[results$transformations$Variable==
                                    ads_post$MAP$key] <-
        ads_post$MAP$MAP
      results$transformations$MAP[results$transformations$Variable==
                                    den_post$MAP$key] <-
        den_post$MAP$MAP
    }
  }

  results$summary <- maps %>%
    merge(results$summary, sort = F) %>%
    as_tibble

  # Update results
  results$plots <- plots

  # Generate basic decomp ----
  results$decomp <- model_data$raw
  results$fit <- results$decomp[c("poolname", "date", y)]

  # Transformations
  for(v in variables$variable[variables$transformation=="media"]){
    ads <- results$transformations$MAP[
      results$transformations$Variable==paste(v, "ads")
      ]
    den <- results$transformations$MAP[
      results$transformations$Variable==paste(v, "denominator")
      ]
    for(p in poolnames){
      results$decomp[[v]][results$decomp$poolname==p] <-
        results$decomp[[v]][results$decomp$poolname==p] %>% adstock(ads)
      results$decomp[[v]][results$decomp$poolname==p] <- 1 -
        exp(-results$decomp[[v]][results$decomp$poolname==p] / den)
    }
  }

  results$decomp <- results$decomp %>%
    gather(variable, value, -poolname, -date) %>%
    filter(variable %in% variables$variable) %>%
    merge(results$summary %>%
            select(poolname, variable, MAP),
          by=c("poolname", "variable"),
          all.x=T)
  results$decomp$MAP[is.na(results$decomp$MAP)] <- 0
  results$decomp <- results$decomp %>%
    mutate(value = MAP * value) %>%
    filter(date %in% observations) %>%
    select(-MAP) %>%
    as_tibble
  results$fit <- results$fit %>% merge(fitted_summary) %>% select(
    -se_mean, -sd, -mean
  ) %>% as_tibble

  # Reference points
  ref <- results$decomp %>% group_by(poolname, date) %>%
    summarise(value = sum(value)) %>%
    group_by(poolname) %>%
    summarise(value = mean(value)) %>%
    merge(means %>% filter(variable==y)) %>%
    mutate(value = mean-value) %>%
    select(poolname, value) %>%
    merge(results$decomp %>% select(poolname, date) %>% unique) %>%
    as_tibble %>%
    mutate(variable="C")
  ref <- ref[names(results$decomp)]
  results$decomp <- results$decomp %>%
    rbind(ref) %>% as_tibble

  # Min
  minvars <- variables$variable[variables$reference.point=="min"]
  for(v in minvars){
    mins <- results$decomp %>% filter(variable==v) %>%
      group_by(poolname, variable) %>%
      summarise(min = -min(value)) %>%
      as_tibble
    mins_const <- mins %>% mutate(
      variable="C",
      min = -min)
    mins <- mins %>% rbind(mins_const)
    results$decomp <- results$decomp %>% merge(mins, all.x=TRUE) %>% as_tibble
    results$decomp$min[is.na(results$decomp$min)] <- 0
    results$decomp$value <- results$decomp$value + results$decomp$min
    results$decomp <- results$decomp %>% select(-min)
  }

  # Max
  maxvars <- variables$variable[variables$reference.point=="max"]
  for(v in maxvars){
    maxes <- results$decomp %>% filter(variable==v) %>%
      group_by(poolname, variable) %>%
      summarise(maxes = -max(value)) %>%
      as_tibble
    maxes_const <- maxes %>% mutate(
      variable="C",
      maxes = -maxes)
    maxes <- maxes %>% rbind(maxes_const)
    results$decomp <- results$decomp %>% merge(maxes, all.x=TRUE) %>% as_tibble
    results$decomp$maxes[is.na(results$decomp$maxes)] <- 0
    results$decomp$value <- results$decomp$value + results$decomp$maxes
    results$decomp <- results$decomp %>% select(-maxes)
  }

  results$fit$Residuals <- results$fit[[y]] - results$fit$fitted

  act_col <- "rgb(0,0,0)"
  fit_col <- "rgb(255,0,0)"
  fit_col_75 <- "rgba(255,0,0,0.4)"
  fit_col_97.5 <- "rgba(255,0,0,0.2)"

  # Create actual/fitted plots ----
  fittotal <- results$fit[-1] %>%
    group_by(date) %>% summarise_all(list(sum))
  results$plots$fit <- plotly::plot_ly(fittotal, x = ~date) %>%
    plotly::add_trace(y = fittotal[[y]], name = y,
                      type='scatter', mode = 'lines',
                      line = list(color = "rgb(0,0,0)")) %>%
    plotly::add_trace(y = ~fitted, name = 'Fitted Values',
                      type='scatter', mode = 'lines',
                      line = list(color = "rgb(255,0,0)")) %>%
    plotly::add_trace(y = ~Residuals, name="Residuals",
                      type="bar", marker = list(color = "rgb(100,100,100)")) %>%
    plotly::add_trace(y = ~X2.5., name = "95% lb", type = 'scatter', mode = 'lines',
                      line = list(color = "rgba(0,0,0,0)"),
                      showlegend = FALSE) %>%
    plotly::add_trace(y = ~X97.5., name = "95%", type = 'scatter', mode = 'lines',
                      line = list(color = "rgba(0,0,0,0)"),
                      fill = 'tonexty', fillcolor=fit_col_97.5,
                      name = '97.5%') %>%
    plotly::add_trace(y = ~X25., name = "50% lb", type = 'scatter', mode = 'lines',
                      line = list(color = "rgba(0,0,0,0)"),
                      showlegend = FALSE, name = '97.5%') %>%
    plotly::add_trace(y = ~X75., name = "50%", type = 'scatter', mode = 'lines',
                      line = list(color = "rgba(0,0,0,0)"),
                      fill = 'tonexty', fillcolor=fit_col_75,
                      name = '75%')
  cseries <- length(results$plots$fit$x$attrs)-1

  # Act/Fit/Residual by pool
  for(p in poolnames){
    fitx <- results$fit %>% filter(poolname==p) %>%
      select(-poolname)
    results$plots$fit <- results$plots$fit %>%
      plotly::add_trace(x=fitx$date, y = fitx[[y]], name = y,
                        type='scatter', mode = 'lines',
                        line = list(color = "rgb(0,0,0)"), visible = FALSE) %>%
      plotly::add_trace(x=fitx$date, y = fitx$fitted, name = 'Fitted Values',
                        type='scatter', mode = 'lines',
                        line = list(color = "rgb(255,0,0)"), visible = FALSE) %>%
      plotly::add_trace(x=fitx$date, y = fitx$Residuals, name="Residuals",
                        type="bar", marker = list(color = "rgb(100,100,100)"),
                        visible = FALSE) %>%
      plotly::add_trace(x=fitx$date, y = fitx$X2.5., name = "95% lb",
                        type = 'scatter', mode = 'lines',
                        line = list(color = "rgba(0,0,0,0)"),
                        showlegend = FALSE, visible = FALSE) %>%
      plotly::add_trace(x=fitx$date, y = fitx$X97.5., name = "95%",
                        type = 'scatter', mode = 'lines',
                        line = list(color = "rgba(0,0,0,0)"),
                        fill = 'tonexty', fillcolor=fit_col_97.5,
                        name = '97.5%', visible = FALSE) %>%
      plotly::add_trace(x=fitx$date, y = fitx$X25., name = "50% lb",
                        type = 'scatter', mode = 'lines',
                        line = list(color = "rgba(0,0,0,0)"),
                        showlegend = FALSE, name = '97.5%', visible = FALSE) %>%
      plotly::add_trace(x=fitx$date, y = fitx$X75., name = "50%",
                        type = 'scatter', mode = 'lines',
                        line = list(color = "rgba(0,0,0,0)"),
                        fill = 'tonexty', fillcolor=fit_col_75,
                        name = '75%', visible = FALSE)
  }

  # Dropdown buttons
  plotbuttons <- lapply(1:(length(poolnames)+1), function(p){
    vis <- rep(F, cseries*(length(poolnames)+1))
    for(i in (seq(cseries)-1)){
      vis[p*cseries-i] <- T
    }
    lab <- "Total"
    if(p>1) lab <- poolnames[p-1]
    list(
      method = "restyle",
      args = list("visible", vis),
      label = lab
    )
  })

  results$plots$fit <- results$plots$fit %>% plotly::layout(
    legend = list(orientation = 'h'),
    xaxis = list(title = "Date"),
    yaxis = list(title = y),
    updatemenus = list(
      list(x=0, y=1.085, yref='paper', align='left', showarrow=F,
           buttons = plotbuttons)
    )
  )

  # Create decomp plots ----

  # Default plotly colours:
  colour_list=c('rgb(31, 119, 180)', 'rgb(255, 127, 14)',
                'rgb(44, 160, 44)', 'rgb(214, 39, 40)',
                'rgb(148, 103, 189)', 'rgb(140, 86, 75)',
                'rgb(227, 119, 194)', 'rgb(127, 127, 127)',
                'rgb(188, 189, 34)', 'rgb(23, 190, 207)')

  decomptotal <- results$decomp %>%
    group_by(variable, date) %>% summarise(value = sum(value)) %>%
    ungroup
  decompvars <- decomptotal %>% group_by(variable) %>%
    summarise(value = sum(value)) %>% ungroup
  decompvars <- decompvars$variable[order(abs(decompvars$value), decreasing = T)]
  decomptotal <- decomptotal %>% spread(variable, value)
  decomptotal[[y]] <- fittotal[[y]]
  results$plots$decomp <- plotly::plot_ly(decomptotal, x = ~date) %>%
    plotly::add_trace(y = fittotal[[y]], name = y,
                      type='scatter', mode = 'lines',
                      line = list(color = "rgb(0,0,0)")) %>%
    layout(barmode = 'relative')
  for(v in decompvars){
    results$plots$decomp <- results$plots$decomp %>%
      plotly::add_trace(y = decomptotal[[v]], name=v, type="bar",
          marker = list(color = which(decompvars==v) %% length(colour_list)))
  }

  # Decomp plots by pool
  for(p in poolnames){
    dcx <- results$decomp %>% filter(poolname==p) %>%
      select(-poolname) %>%
      spread(variable, value)
    dcx[[y]] <- results$fit %>% filter(poolname==p) %>%
      .[[y]]
    results$plots$decomp <- results$plots$decomp %>%
      plotly::add_trace(x=dcx$date, y = dcx[[y]], name = y,
                        type='scatter', mode = 'lines',
                        line = list(color = "rgb(0,0,0)"), visible = FALSE)
    for(v in decompvars){
      results$plots$decomp <- results$plots$decomp %>%
        plotly::add_trace(y = dcx[[v]], name=v, type="bar",
          marker = list(color = which(decompvars==v) %% length(colour_list)),
          visible = FALSE)
    }
  }

  # Dropdown buttons
  plotbuttons <- lapply(1:(length(poolnames)+1), function(p){
    vis <- rep(F, (length(decompvars)+1)*(length(poolnames)+1))
    for(i in 1:(length(decompvars)+1)){
      vis[p*(length(decompvars)+1)+1-i] <- T
    }
    lab <- "Total"
    if(p>1) lab <- poolnames[p-1]
    list(
      method = "restyle",
      args = list("visible", vis),
      label = lab
    )
  })

  results$plots$decomp <- results$plots$decomp %>% plotly::layout(
    legend = list(orientation = 'h'),
    xaxis = list(title = "Date"),
    yaxis = list(title = y),
    updatemenus = list(
      list(x=0, y=1.085, yref='paper', align='left', showarrow=F,
           buttons = plotbuttons)
    )
  )

  # Calculate R.Squared ----
  results$rsq <- list(
    "total" = rsq(fittotal[[y]], fittotal$fitted)
  )
  for(p in poolnames){
    fitx <- results$fit %>% filter(poolname==p)
    results$rsq[[p]] <- rsq(
      fitx[[y]], fitx$fitted
    )
  }
  results$rsq <- results$rsq %>% data.frame %>% t %>% data.frame
  names(results$rsq) <- "r.sq"

  # Return results ----
  return(results)
}

#' @title trace_plot
#' @description Create trace plots from a model object and variable name using ggplot.
#' @param model A bayesian model, created by the \code{\link{bayesmodel}} function.
#' @param var The variable name.
#' @examples trace_plot(my_model, "Variable X")
#' @export
trace_plot <- function(model, var){

  if(!(var %in% model$stan_ids$variables$variable)){
    stop(paste0("Variable: \"",var,"\" not found in model."))
  }

  # Get IDs for variable from stan model
  stan_id <- model$stan_ids$variables %>% filter(
    variable==var
  ) %>%
    mutate(name = paste(variable, poolname))
  ids <- stan_id$id

  # Create traceplots
  plots <- rstan::traceplot(model$stanmodel,
                            ids,
                            inc_warmup = TRUE)

  # Relabel series in ggplot output
  plots$data <- plots$data %>%
    merge(stan_id, by.x="parameter", by.y="id", sort = FALSE) %>%
    mutate(parameter=as_factor(name)) %>%
    select(value, parameter, chain, iteration)
  plots$data <- plots$data[order(plots$data$parameter,
                                 plots$data$chain,
                                 plots$data$iteration),]

  # Add media transformations (if any)
  if(!is.null(dim(model$stan_ids$transformations))){
    if(paste(var, "ads") %in% model$stan_ids$transformations$variable){
      ids_t <- c(
        model$stan_ids$transformations$id[
          model$stan_ids$transformations$variable==paste(var, "ads")
          ],
        model$stan_ids$transformations$id[
          model$stan_ids$transformations$variable==paste(var, "denominator")
          ]
      )
      plots_t <- rstan::traceplot(model$stanmodel,
                                  ids_t,
                                  inc_warmup = TRUE)
      plots_t$data$parameter <- as.character(plots_t$data$parameter)
      plots_t$data$parameter[plots_t$data$parameter==ids_t[1]] <- "adstock"
      plots_t$data$parameter[plots_t$data$parameter==ids_t[2]] <- "denominator"
      plots_t$data$parameter <- as_factor(plots_t$data$parameter)

      plots$data <- rbind(plots$data, plots_t$data)
    }
  }

  return(plots)
}

#' @title update_model
#' @description Creates a new model from a given dataset, where the priors are taken from the posterior distribution of an existing model.
#' @param model A bayesian model created by the \code{\link{bayesmodel}} function
#' @param y A string identifying the dependent variable.
#' @param data A data frame containing a column of observations (labelled "obs"), the dependent variable, and indepedendent variables
#' @param observations An optional string vector of observation names, e.g. c("2014-01-01", "2014-01-08", ...)
#' @param is_linear An optional boolean to specify whether whether a linear model is provided. If TRUE, the model argument is taken to be the output from the lm function and the priors are set to be normally distributed, with mean=coefficient and s.d.=standard error. Default FALSE
#' @return A bayesian model
#' @examples update_model(bayesian_model, df, df$obs, FALSE)
update_model <- function(model, y, data, observations, is_linear=FALSE){
  if(is_linear){
    priors <- data.frame(summary(model)$coefficients)

    variables <- str_remove_all(rownames(priors), "`")
    # Exclude intercept from variable names
    variables <- variables[variables != "(Intercept)"]

    missing_vars <- variables[!(variables %in% names(data))]
    if(length(missing_vars)==1){
      message(paste("The following variable is missing from the dataset and will be excluded from the model:", missing_vars))
    }
    else if(length(missing_vars)>1){
      message(paste("The following variables are missing from the dataset and will be excluded from the model:", missing_vars))
    }

    variables <- variables[variables %in% names(data)]

    priors$variable <- str_remove_all(rownames(priors), "`")
    priors <- priors[priors$variable %in% variables, ]

    variables <- priors[c(5,1,2)]
    names(variables) <- c("variable", "prior.mean", "prior.sd")

    new_model <- rbayes::bayesmodel(data, y, variables, observations)

    return(new_model)
  }
  else{
    stop("function incomplete")
  }
}

#' @title Moving Average
#' @description Create moving averages of pooled data.
#' @param data A data frame containing a column of poolnames, a column of observations (labelled "date"), and variables.
#' @param series A string identifying the variable to be "mav"d.
#' @param obs The moving average observation length.
#' @param name An optional argument to specify the name of the new seres. If NULL, the original data will be overwritten with the new values. Default NULL.
#' @param center An optional boolean to specify centered moving average. Default FALSE.
#' @return A transformed data frame.
#' @examples df <- df_raw %>% mav("seasonality", 13, center = T)
#' @export
mav <- function(data, series, obs, name = NULL, center = F){

  if(!"poolname" %in% names(data)){
    message("Column 'poolname' not found in data. Assuming non-pooled data.")
    data$poolname <- "total"
  }
  if(!"date" %in% names(data)){
    stop("Column 'date' missing from data. Please supply a column of date values.")
  }

  df_mav <- data[c("poolname", "date", series)]
  names(df_mav)[3] <- "mavcol"
  df_mav <- df_mav %>% spread(poolname, mavcol)
  for(i in 2:length(df_mav)){
    arr <- df_mav[[i]]
    df_mav[[i]] <- zoo::rollmean(arr,
                                 obs,
                                 fill = c(
                                   mean(head(arr, obs)),
                                   mean(arr),
                                   mean(tail(arr, obs))
                                 ),
                                 align = if(center) "center" else "left")
  }
  df_mav <- df_mav %>% gather(poolname, mav, -date)

  data <- data %>% left_join(df_mav)
  if(is.null(name)) data[[series]] <- data$mav
  else data[[name]] <- data$mav

  data <- data %>% select(-mav)
  return(data)
}

#' @title Lag Variables
#' @description Create lags and leads of pooled data.
#' @param data A data frame containing a column of poolnames, a column of observations (labelled "date"), and variables.
#' @param series A string identifying the variable to be lagged.
#' @param obs The number of weeks to lag by. Negative numbers will result in a lead.
#' @param name An optional argument to specify the name of the new seres. If NULL, the original data will be overwritten with the new values. Default NULL
#' @param rep_nearest An optional boolean to fill NA values with nearest neighbour. If FALSE, NA values will be filled with zero. Default TRUE.
#' @return A transformed data frame.
#' @examples df <- df_raw %>% mav("Christmas", 2, rep_nearest = F)
#' @export
lag <- function(data, series, obs, name = NULL, rep_nearest = T){
  if(!"poolname" %in% names(data)){
    message("Column 'poolname' not found in data. Assuming non-pooled data.")
    data$poolname <- "total"
  }
  if(!"date" %in% names(data)){
    stop("Column 'date' missing from data. Please supply a column of date values.")
  }

  df_lag <- data[c("poolname", "date", series)]
  names(df_lag)[3] <- "lagcol"
  df_lag <- df_lag %>% spread(poolname, lagcol)
  for(i in 2:length(df_lag)){
    arr <- df_lag[[i]]
    if(obs<0){
      if(rep_nearest){
        df_lag[[i]] <- c(tail(arr, obs), rep(tail(arr,1), abs(obs)))
      }
      else{
        df_lag[[i]] <- c(tail(arr, obs), rep(0, abs(obs)))
      }
    }
    else{
      if(rep_nearest){
        df_lag[[i]] <- c(rep(arr[1], obs), head(arr, -obs))
      }
      else{
        df_lag[[i]] <- c(rep(0, obs), head(arr, -obs))
      }
    }
  }
  df_lag <- df_lag %>% gather(poolname, lag, -date)

  data <- data %>% left_join(df_lag)

  if(is.null(name)) data[[series]] <- data$lag
  else data[[name]] <- data$lag

  data <- data %>% select(-lag)
  return(data)
}

#' @title Line chart
#' @description Create a line chart from pooled data. One series per pool.
#' @param data A data frame containing a column of poolnames, a column of observations (labelled "date"), and variables.
#' @param series The name of the series to be charted
#' @param series2 An optional second series to be charted on a secondary axis. Default NULL.
#' @return A plotly chart object
#' @examples lplot(model_data, "revenue")
#' @export
lplot <- function(data, series, series2 = NULL){
  chart_data <- data[c("poolname", "date", series)] %>%
    spread(poolname, series)
  names(chart_data)[-1] <- paste(series, names(chart_data)[-1])

  p <- plotly::plot_ly(chart_data, x = ~date)
  for(s in names(chart_data)[-1]){
    p <- p %>% plotly::add_trace(y = chart_data[[s]], name = s,
                                 type='scatter', mode = 'lines')
  }

  p <- p %>% plotly::layout(
    legend = list(orientation = 'h'),
    yaxis = list(title = series))

  if(!is.null(series2)){
    chart_data <- data[c("poolname", "date", series2)] %>%
      spread(poolname, series2)
    names(chart_data)[-1] <- paste(series2, names(chart_data)[-1])

    for(s in names(chart_data)[-1]){
      p <- p %>% plotly::add_trace(y = chart_data[[s]], name = s,
                                   type='scatter', mode = 'lines',
                                   yaxis = "y2")
      p <- p %>% plotly::layout(yaxis2 = list(
        tickfont = list(color = "red"),
        overlaying = "y",
        side = "right",
        title = series2,
        showgrid = F
      ))
    }
  }
  return(p)
}