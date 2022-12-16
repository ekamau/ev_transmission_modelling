fit_model <- function(stan_data, modelfile) {
  fit <- stan(
    file = modelfile, data = stan_data, chains = 4, 
    init = "random",  warmup = 3000, iter = 10000, refresh = 5000, 
    control = list(adapt_delta = 0.9999, max_treedepth = 25))
  
  fit
}
