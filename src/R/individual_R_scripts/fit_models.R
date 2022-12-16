options(mc.cores = parallel::detectCores())

source("src/R/functions_extract_from_fit.R")

### EV-A71:
source("src/R/process_eva71_data.R")
datE71 <- process_eva71_data()

# create stan data:
source("src/R/create_stan_data_constFOI_EVA71.R")
source("src/R/create_stan_data_timeFOI_EVA71.R")
e71_stan_data_const <- create_stan_data_constFOI_EVA71(datE71)
e71_stan_data_time <- create_stan_data_timeFOI_EVA71(datE71)
e71_stan_data_model4a <- create_stan_data_timeFOI_EVA71(datE71, rho = 2)
e71_stan_data_model4b <- create_stan_data_timeFOI_EVA71(datE71, rho = 0.2)
e71_stan_data_model4c <- create_stan_data_timeFOI_EVA71(datE71, rho = 0.5)

# fit data:
fit_e71_model1 <- fit_model(e71_stan_data_const, "src/stan/model1.stan")
saveRDS(fit_e71_model1, file = "results/EVA71/EVA71_model1_fit.rds")
fit_lambda <- fit_extract_lambda(fit_e71_model1)
saveRDS(fit_lambda, file = "results/EVA71/EVA71_model1_lambda.rds")

fit_e71_model2 <- fit_model(e71_stan_data_const, "src/stan/model2.stan")
saveRDS(fit_e71_model2, file = "results/EVA71/EVA71_model2_fit.rds")
fit_lambda_rho <- fit_extract_lambda_rho(fit_e71_model2)
saveRDS(fit_lambda_rho, file = "results/EVA71/EVA71_model2_lambda_rho.rds")

fit_e71_model3 <- fit_model(e71_stan_data_time, "src/stan/model3.stan")
saveRDS(fit_e71_model3, file = "results/EVA71/EVA71_model3_fit.rds")

fit_e71_model4a <- fit_model(e71_stan_data_model4a, "src/stan/model4.stan")
saveRDS(fit_e71_model4a, file = "results/EVA71/EVA71_model4a_fit.rds")
fit_e71_model4b <- fit_model(e71_stan_data_model4b, "src/stan/model4.stan")
saveRDS(fit_e71_model4b, file = "results/EVA71/EVA71_model4b_fit.rds")
fit_e71_model4c <- fit_model(e71_stan_data_model4c, "src/stan/model4.stan")
saveRDS(fit_e71_model4c, file = "results/EVA71/EVA71_model4c_fit.rds")

fit_e71_model5 <- fit_model(e71_stan_data_const, "src/stan/model5.stan")
saveRDS(fit_e71_model5, file = "results/EVA71/EVA71_model5_fit.rds")
fit_lambda_beta <- fit_extract_lambda_beta(fit_e71_model5)
saveRDS(fit_lambda_beta, file = "results/EVA71/EVA71_model5_lambda_beta.rds")

fit_e71_model6 <- fit_model(e71_stan_data_const, "src/stan/model6.stan")
saveRDS(fit_e71_model6, file = "results/EVA71/EVA71_model6_fit.rds")
fit_lambda_beta_rho <- fit_extract_lambda_beta_rho(fit_e71_model6)
saveRDS(fit_lambda_beta_rho, file = "results/EVA71/EVA71_model6_lambda_beta_rho.rds")

### CVA6:
source("src/R/process_cva6_data.R")
datCA6 <- process_cva6_data()

# create stan data:
source("src/R/create_stan_data_constFOI_CVA6.R")
source("src/R/create_stan_data_timeFOI_CVA6.R")
ca6_stan_data_const <- create_stan_data_constFOI_EVA71(datCA6)
ca6_stan_data_time <- create_stan_data_timeFOI_EVA71(datCA6)
ca6_stan_data_model4a <- create_stan_data_timeFOI_EVA71(datCA6, rho = 2)
ca6_stan_data_model4b <- create_stan_data_timeFOI_EVA71(datCA6, rho = 0.2)
ca6_stan_data_model4c <- create_stan_data_timeFOI_EVA71(datCA6, rho = 0.5)

# fit model to data and extract parameters:
fit_ca6_model1 <- fit_model(ca6_stan_data_const, "src/stan/model1.stan")
saveRDS(fit_ca6_model1, file = "results/CVA6/CVA6_model1_fit.rds")
fit_lambda <- fit_extract_lambda(fit_ca6_model1)
saveRDS(fit_lambda, file = "results/CVA6_model1_lambda.rds")

fit_ca6_model2 <- fit_model(ca6_stan_data_const, "src/stan/model2.stan")
saveRDS(fit_ca6_model2, file = "results/CVA6/CVA6_model2_fit.rds")
fit_lambda_rho <- fit_extract_lambda_rho(fit_ca6_model2)
saveRDS(fit_lambda_rho, file = "results/CVA6_model2_lambda_rho.rds")

fit_ca6_model3 <- fit_model(ca6_stan_data_time, "src/stan/model3.stan")
saveRDS(fit_ca6_model3, file = "results/CVA6/CVA6_model3_fit.rds")

fit_ca6_model4a <- fit_model(ca6_stan_data_model4a, "src/stan/model4.stan")
saveRDS(fit_ca6_model4a, file = "results/CVA6/CVA6_model4a_fit.rds")
fit_ca6_model4b <- fit_model(ca6_stan_data_model4b, "src/stan/model4.stan")
saveRDS(fit_ca6_model4b, file = "results/CVA6/CVA6_model4b_fit.rds")
fit_ca6_model4c <- fit_model(ca6_stan_data_model4c, "src/stan/model4.stan")
saveRDS(fit_ca6_model4c, file = "results/CVA6/CVA6_model4c_fit.rds")

fit_ca6_model5 <- fit_model(ca6_stan_data_const, "src/stan/model5.stan")
saveRDS(fit_ca6_model5, file = "results/CVA6/CVA6_model5_fit.rds")
fit_lambda_beta <- fit_extract_lambda_beta(fit_ca6_model5)
saveRDS(fit_lambda_beta, file = "results/CVA6_model5_lambda_beta.rds")

fit_ca6_model6 <- fit_model(ca6_stan_data_const, "src/stan/model6.stan")
saveRDS(fit_ca6_model6, file = "results/CVA6/CVA6_model6_fit.rds")
fit_lambda_beta_rho <- fit_extract_lambda_beta_rho(fit_ca6_model6)
saveRDS(fit_lambda_beta_rho, file = "results/CVA6_model6_lambda_beta_rho.rds")
