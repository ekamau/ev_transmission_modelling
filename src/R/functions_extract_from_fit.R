fit_extract_summary <- function(fit) {
  fit.summary <- data.frame(rstan::summary(fit)$summary)
  fit.summary$median <- rstan::summary(fit)$summary[ , "50%"]
  
  fit.summary
} 

fit_extract_lambda <- function(fit) {
  fit_lambda <- rstan::extract(fit, c("lambda"))
  
  fit_lambda
}

fit_extract_lambda_rho <- function(fit) {
  fit_lambda_rho <- rstan::extract(fit, c("lambda", "rho"))
  
  fit_lambda_rho
}

fit_extract_lambda_beta <- function(fit) {
  fit_lambda_beta <- rstan::extract(fit, c("lambda", "beta"))
  
  fit_lambda_beta
}

fit_extract_lambda_beta_rho <- function(fit) {
  fit_lambda_beta_rho <- rstan::extract(fit, c("lambda", "beta", "rho"))
  
  fit_lambda_beta_rho
}
