posterior_plot <- function(data){
  means <- sapply(data, mean) %>%
    data.frame %>%
    as_tibble(rownames="key")
  names(means)[2] <- "mean"
  maps <- sapply(data, function(x){
    dens <- density(x)
    dens$x[dens$y==max(dens$y)]
  }) %>%
    data.frame %>%
    as_tibble(rownames="key")
  names(maps)[2] <- "MAP"

  data <- data %>% gather

  cht <- ggplot(data,aes(x=value, color=key)) +
    geom_density() +
    geom_vline(data=maps, aes(xintercept=MAP, color=key),
               linetype="dashed") +
    theme_classic()

  return(list(
    chart=plotly::ggplotly(cht),
    MAP=maps))
}

rsq <- function(x, y){
  cor(x, y)^2
}

adstock <- function(data, ads){
  adata <- data
  for(i in 2:length(adata)){
    adata[i] <- adata[i] + adata[i-1] * ads
  }
  return(adata)
}
