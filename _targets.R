# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("rstan", "tidyverse", "lubridate", "reshape2", "patchwork", "cowplot", "viridis", "viridisLite"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
#lapply(list.files("src/R", full.names = TRUE), source)
source("src/R/create_stan_data_constFOI_CVA6.R") # Source other scripts as needed. # nolint
source("src/R/create_stan_data_constFOI_EVA71.R")
source("src/R/create_stan_data_timeFOI_CVA6.R")
source("src/R/create_stan_data_timeFOI_EVA71.R")

source("src/R/Figures1_S7.R")
source("src/R/Figures2_S10_S11.R")
source("src/R/Figure3.R")
source("src/R/Figure4.R")
source("src/R/Figure5.R")
source("src/R/FigureS1.R")
source("src/R/FigureS2.R")
source("src/R/FigureS3.R")
source("src/R/FigureS4.R")
source("src/R/FiguresS5_S6.R")
source("src/R/FigureS8.R")
source("src/R/FigureS9.R")
source("src/R/FigureS12.R")
source("src/R/FigureS13.R")

source("src/R/functions_extract_from_fit.R")
source("src/R/process_cva6_data.R")
source("src/R/process_cva6_fit_summary.R")
source("src/R/process_eva71_data.R")
source("src/R/process_eva71_fit_summary.R")

source("src/R/fit_stan_model.R")
source("src/R/functions_extract_from_fit.R")

# Replace the target list below with your own:
list(
  # CVA6 model fitting:
  tar_target(ca6_dat, process_cva6_data()),
  tar_target(stan_data_ca6, create_stan_data_constFOI_CVA6(ca6_dat)),
  tar_target(fit_ca6_model1_exp1, fit_model(stan_data_ca6, "src/stan/model1.stan")),
  tar_target(fit_ca6_model1_exp1_summary, fit_extract_summary(fit_ca6_model1_exp1)),
  tar_target(file_fit_summary1a,
             {
               write.csv(fit_ca6_model1_exp1_summary, "results/CVA6/fitSummary_CVA6_model1.csv");
               "results/CVA6/fitSummary_CVA6_model1.csv"
             },
             format = "file"),
  
  tar_target(stan_data_ca6, create_stan_data_constFOI_CVA6(ca6_dat)),
  tar_target(fit_ca6_model1_exp10, fit_model(stan_data_ca6, "src/stan/model1_exponential10.stan")),
  tar_target(fit_ca6_model1_exp10_summary, fit_extract_summary(fit_ca6_model1_exp10)),
  tar_target(file_fit_summary1b,
             {
               write.csv(fit_ca6_model1_exp10_summary, "results/CVA6/fitSummary_CVA6_model1_exponential10.csv");
               "results/CVA6/fitSummary_CVA6_model1_exponential10.csv"
             },
             format = "file"),
  
  tar_target(stan_data_ca6, create_stan_data_constFOI_CVA6(ca6_dat)),
  tar_target(fit_ca6_model2_exp1, fit_model(stan_data_ca6, "src/stan/model2.stan")),
  tar_target(fit_lambda_rho, fit_extract_lambda_rho(fit_ca6_model2_exp1)),
  tar_target(file_fit_lambda_rho, 
             {
               saveRDS(fit_lambda_rho, file = "results/CVA6/CVA6_model2_lambda_rho.rds");
               "results/CVA6/CVA6_model2_lambda_rho.rds"
             },
             format = "file"),
  tar_target(fit_ca6_model2_exp1_summary, fit_extract_summary(fit_ca6_model2_exp1)),
  tar_target(file_fit_summary2a,
             {
               write.csv(fit_ca6_model2_exp1_summary, "results/CVA6/fitSummary_CVA6_model2.csv");
               "results/CVA6/fitSummary_CVA6_model2.csv"
             },
             format = "file"),
  
  tar_target(stan_data_ca6, create_stan_data_constFOI_CVA6(ca6_dat)),
  tar_target(fit_ca6_model2_exp10, fit_model(stan_data_ca6, "src/stan/model2_exponential10.stan")),
  tar_target(fit_ca6_model2_exp10_summary, fit_extract_summary(fit_ca6_model2_exp10)),
  tar_target(file_fit_summary2b,
             {
               write.csv(fit_ca6_model2_exp10_summary, "results/CVA6/fitSummary_CVA6_model2_exponential10.csv");
               "results/CVA6/fitSummary_CVA6_model2_exponential10.csv"
             },
             format = "file"),
  
  tar_target(stan_data_ca6, create_stan_data_timeFOI_CVA6(ca6_dat)),
  tar_target(fit_ca6_model3, fit_model(stan_data_ca6, "src/stan/model3.stan")),
  tar_target(fit_ca6_model3_summary, fit_extract_summary(fit_ca6_model3)),
  tar_target(file_fit_summary,
             {
               write.csv(fit_ca6_model3_summary, "results/CVA6/fitSummary_CVA6_model3.csv");
               "results/CVA6/fitSummary_CVA6_model3.csv"
             },
             format = "file"),
  
  tar_target(stan_data_ca6, create_stan_data_timeFOI_CVA6(ca6_dat, rho = 2)),
  tar_target(fit_ca6_model4a, fit_model(stan_data_ca6, "src/stan/model4.stan")),
  tar_target(fit_ca6_model4a_summary, fit_extract_summary(fit_ca6_model4a)),
  tar_target(file_fit_summary4a,
             {
               write.csv(fit_ca6_model4a_summary, "results/CVA6/fitSummary_CVA6_model4_rate2.csv");
               "results/CVA6/fitSummary_CVA6_model4_rate2.csv"
             },
             format = "file"),
  
  tar_target(stan_data_ca6, create_stan_data_timeFOI_CVA6(ca6_dat, rho = 0.2)),
  tar_target(fit_ca6_model4b, fit_model(stan_data_ca6, "src/stan/model4.stan")),
  tar_target(fit_ca6_model4b_summary, fit_extract_summary(fit_ca6_model4b)),
  tar_target(file_fit_summary4b,
             {
               write.csv(fit_ca6_model4b_summary, "results/CVA6/fitSummary_CVA6_model4_rate0.2.csv");
               "results/CVA6/fitSummary_CVA6_model4_rate0.2.csv"
             },
             format = "file"),
  
  tar_target(stan_data_ca6, create_stan_data_timeFOI_CVA6(ca6_dat, rho = 0.05)),
  tar_target(fit_ca6_model4c, fit_model(stan_data_ca6, "src/stan/model4.stan")),
  tar_target(fit_ca6_model4c_summary, fit_extract_summary(fit_ca6_model4c)),
  tar_target(file_fit_summary4c,
             {
               write.csv(fit_ca6_model4c_summary, "results/CVA6/fitSummary_CVA6_model4_rate0.05.csv");
               "results/CVA6/fitSummary_CVA6_model4_rate0.05.csv"
             },
             format = "file"),
  
  tar_target(stan_data_ca6, create_stan_data_constFOI_CVA6(ca6_dat)),
  tar_target(fit_ca6_model5_exp1, fit_model(stan_data_ca6, "src/stan/model5.stan")),
  tar_target(fit_lambda_beta, fit_extract_lambda_rho(fit_ca6_model5_exp1)),
  tar_target(file_fit_lambda_beta, 
             {
               saveRDS(fit_lambda_beta, file = "results/CVA6/CVA6_model5_lambda_beta.rds");
               "results/CVA6/CVA6_model5_lambda_beta.rds"
             },
             format = "file"),
  tar_target(fit_ca6_model5_exp1_summary, fit_extract_summary(fit_ca6_model5_exp1)),
  tar_target(file_fit_summary5a,
             {
               write.csv(fit_ca6_model5_exp1_summary, "results/CVA6/fitSummary_CVA6_model5.csv");
               "results/CVA6/fitSummary_CVA6_model5.csv"
             },
             format = "file"),
  
  tar_target(stan_data_ca6, create_stan_data_constFOI_CVA6(ca6_dat)),
  tar_target(fit_ca6_model5_exp10, fit_model(stan_data_ca6, "src/stan/model5_exponential10.stan")),
  tar_target(fit_ca6_model5_exp10_summary, fit_extract_summary(fit_ca6_model5_exp10)),
  tar_target(file_fit_summary5b,
             {
               write.csv(fit_ca6_model5_exp10_summary, "results/CVA6/fitSummary_CVA6_model5_exponential10.csv");
               "results/CVA6/fitSummary_CVA6_model5_exponential10.csv"
             },
             format = "file"),
  
  tar_target(stan_data_ca6, create_stan_data_constFOI_CVA6(ca6_dat)),
  tar_target(fit_ca6_model6_exp1, fit_model(stan_data_ca6, "src/stan/model6.stan")),
  tar_target(fit_lambda_beta_rho, fit_extract_lambda_beta_rho(fit_ca6_model6_exp1)),
  tar_target(file_fit_lambda_beta_rho, 
             {
               saveRDS(fit_lambda_beta_rho, file = "results/CVA6/CVA6_model6_lambda_rho_beta.rds");
               "results/CVA6/CVA6_model6_lambda_rho_beta.rds"
             },
             format = "file"),
  tar_target(fit_ca6_model6_exp1_summary, fit_extract_summary(fit_ca6_model6_exp1)),
  tar_target(file_fit_summary6a,
             {
               write.csv(fit_ca6_model6_exp1_summary, "results/CVA6/fitSummary_CVA6_model6.csv");
               "results/CVA6/fitSummary_CVA6_model6.csv"
             },
             format = "file"),
  
  tar_target(stan_data_ca6, create_stan_data_constFOI_CVA6(ca6_dat)),
  tar_target(fit_ca6_model6_exp10, fit_model(stan_data_ca6, "src/stan/model6_exponential10.stan")),
  tar_target(fit_ca6_model6_exp10_summary, fit_extract_summary(fit_ca6_model6_exp10)),
  tar_target(file_fit_summary6b,
             {
               write.csv(fit_ca6_model6_exp10_summary, "results/CVA6/fitSummary_CVA6_model6_exponential10.csv");
               "results/CVA6/fitSummary_CVA6_model6_exponential10.csv"
             },
             format = "file"),
  
  # EV-A71 model fitting:
  tar_target(e71_dat, process_cva6_data()),
  tar_target(stan_data_e71, create_stan_data_constFOI_EVA71(e71_dat)),
  tar_target(fit_e71_model1_exp1, fit_model(stan_data_e71, "src/stan/model1.stan")),
  tar_target(fit_e71_model1_exp1_summary, fit_extract_summary(fit_e71_model1_exp1)),
  tar_target(file_fit_summary1a,
             {
               write.csv(fit_e71_model1_exp1_summary, "results/EVA71/fitSummary_EVA71_model1.csv");
               "results/EVA71/fitSummary_EVA71_model1.csv"
             },
             format = "file"),
  
  tar_target(stan_data_e71, create_stan_data_constFOI_EVA71(e71_dat)),
  tar_target(fit_e71_model1_exp10, fit_model(stan_data_e71, "src/stan/model1_exponential10.stan")),
  tar_target(fit_e71_model1_exp10_summary, fit_extract_summary(fit_e71_model1_exp10)),
  tar_target(file_fit_summary1b,
             {
               write.csv(fit_e71_model1_exp10_summary, "results/EVA71/fitSummary_EVA71_model1_exponential10.csv");
               "results/EVA71/fitSummary_EVA71_model1_exponential10.csv"
             },
             format = "file"),
  
  tar_target(stan_data_e71, create_stan_data_constFOI_EVA71(e71_dat)),
  tar_target(fit_e71_model2_exp1, fit_model(stan_data_e71, "src/stan/model2.stan")),
  tar_target(fit_lambda_rho, fit_extract_lambda_rho(fit_e71_model2_exp1)),
  tar_target(file_fit_lambda_rho, 
             {
               saveRDS(fit_lambda_rho, file = "results/EVA71/EVA71_model2_lambda_rho.rds");
               "results/EVA71/EVA71_model2_lambda_rho.rds"
             },
             format = "file"),
  tar_target(fit_e71_model2_exp1_summary, fit_extract_summary(fit_e71_model2_exp1)),
  tar_target(file_fit_summary2a,
             {
               write.csv(fit_e71_model2_exp1_summary, "results/EVA71/fitSummary_EVA71_model2.csv");
               "results/EVA71/fitSummary_EVA71_model2.csv"
             },
             format = "file"),
  
  tar_target(stan_data_e71, create_stan_data_constFOI_EVA71(e71_dat)),
  tar_target(fit_e71_model2_exp10, fit_model(stan_data_e71, "src/stan/model2_exponential10.stan")),
  tar_target(fit_e71_model2_exp10_summary, fit_extract_summary(fit_e71_model2_exp10)),
  tar_target(file_fit_summary2b,
             {
               write.csv(fit_e71_model2_exp10_summary, "results/EVA71/fitSummary_EVA71_model2_exponential10.csv");
               "results/EVA71/fitSummary_EVA71_model2_exponential10.csv"
             },
             format = "file"),
  
  tar_target(stan_data_e71, create_stan_data_timeFOI_EVA71(e71_dat)),
  tar_target(fit_e71_model3, fit_model(stan_data_e71, "src/stan/model3.stan")),
  tar_target(fit_e71_model3_summary, fit_extract_summary(fit_e71_model3)),
  tar_target(file_fit_summary,
             {
               write.csv(fit_e71_model3_summary, "results/EVA71/fitSummary_EVA71_model3.csv");
               "results/EVA71/fitSummary_EVA71_model3.csv"
             },
             format = "file"),
  
  tar_target(stan_data_e71, create_stan_data_timeFOI_EVA71(e71_dat, rho = 2)),
  tar_target(fit_e71_model4a, fit_model(stan_data_e71, "src/stan/model4.stan")),
  tar_target(fit_e71_model4a_summary, fit_extract_summary(fit_e71_model4a)),
  tar_target(file_fit_summary4a,
             {
               write.csv(fit_e71_model4a_summary, "results/EVA71/fitSummary_EVA71_model4_rate2.csv");
               "results/EVA71/fitSummary_EVA71_model4_rate2.csv"
             },
             format = "file"),
  
  tar_target(stan_data_e71, create_stan_data_timeFOI_EVA71(e71_dat, rho = 0.2)),
  tar_target(fit_e71_model4b, fit_model(stan_data_e71, "src/stan/model4.stan")),
  tar_target(fit_e71_model4b_summary, fit_extract_summary(fit_e71_model4b)),
  tar_target(file_fit_summary4b,
             {
               write.csv(fit_e71_model4b_summary, "results/EVA71/fitSummary_EVA71_model4_rate0.2.csv");
               "results/EVA71/fitSummary_EVA71_model4_rate0.2.csv"
             },
             format = "file"),
  
  tar_target(stan_data_e71, create_stan_data_timeFOI_EVA71(e71_dat, rho = 0.05)),
  tar_target(fit_e71_model4c, fit_model(stan_data_e71, "src/stan/model4.stan")),
  tar_target(fit_e71_model4c_summary, fit_extract_summary(fit_e71_model4c)),
  tar_target(file_fit_summary4c,
             {
               write.csv(fit_e71_model4c_summary, "results/EVA71/fitSummary_EVA71_model4_rate0.05.csv");
               "results/EVA71/fitSummary_EVA71_model4_rate0.05.csv"
             },
             format = "file"),
  
  tar_target(stan_data_e71, create_stan_data_constFOI_EVA71(e71_dat)),
  tar_target(fit_e71_model5_exp1, fit_model(stan_data_e71, "src/stan/model5.stan")),
  tar_target(fit_lambda_beta, fit_extract_lambda_rho(fit_e71_model5_exp1)),
  tar_target(file_fit_lambda_beta, 
             {
               saveRDS(fit_lambda_beta, file = "results/EVA71/EVA71_model5_lambda_beta.rds");
               "results/EVA71/EVA71_model5_lambda_beta.rds"
             },
             format = "file"),
  tar_target(fit_e71_model5_exp1_summary, fit_extract_summary(fit_e71_model5_exp1)),
  tar_target(file_fit_summary5a,
             {
               write.csv(fit_e71_model5_exp1_summary, "results/EVA71/fitSummary_EVA71_model5.csv");
               "results/EVA71/fitSummary_EVA71_model5.csv"
             },
             format = "file"),
  
  tar_target(stan_data_e71, create_stan_data_constFOI_EVA71(e71_dat)),
  tar_target(fit_e71_model5_exp10, fit_model(stan_data_e71, "src/stan/model5_exponential10.stan")),
  tar_target(fit_e71_model5_exp10_summary, fit_extract_summary(fit_e71_model5_exp10)),
  tar_target(file_fit_summary5b,
             {
               write.csv(fit_e71_model5_exp10_summary, "results/EVA71/fitSummary_EVA71_model5_exponential10.csv");
               "results/EVA71/fitSummary_EVA71_model5_exponential10.csv"
             },
             format = "file"),
  
  tar_target(stan_data_e71, create_stan_data_constFOI_EVA71(e71_dat)),
  tar_target(fit_e71_model6_exp1, fit_model(stan_data_e71, "src/stan/model6.stan")),
  tar_target(fit_lambda_beta_rho, fit_extract_lambda_beta_rho(fit_e71_model6_exp1)),
  tar_target(file_fit_lambda_beta_rho, 
             {
               saveRDS(fit_lambda_beta_rho, file = "results/EVA71/EVA71_model6_lambda_rho_beta.rds");
               "results/EVA71/EVA71_model6_lambda_rho_beta.rds"
             },
             format = "file"),
  tar_target(fit_e71_model6_exp1_summary, fit_extract_summary(fit_e71_model6_exp1)),
  tar_target(file_fit_summary6a,
             {
               write.csv(fit_e71_model6_exp1_summary, "results/EVA71/fitSummary_EVA71_model6.csv");
               "results/EVA71/fitSummary_EVA71_model6.csv"
             },
             format = "file"),
  
  tar_target(stan_data_e71, create_stan_data_constFOI_EVA71(e71_dat)),
  tar_target(fit_e71_model6_exp10, fit_model(stan_data_e71, "src/stan/model6_exponential10.stan")),
  tar_target(fit_e71_model6_exp10_summary, fit_extract_summary(fit_e71_model6_exp10)),
  tar_target(file_fit_summary6b,
             {
               write.csv(fit_e71_model6_exp10_summary, "results/EVA71/fitSummary_EVA71_model6_exponential10.csv");
               "results/EVA71/fitSummary_EVA71_model6_exponential10.csv"
             },
             format = "file"),
  
  # Figures:
  tar_target(figs1_S7, plot_fig1_figS7()),
  tar_target(fig2_S10_11, plot_fig2_figS10_figS11()),
  tar_target(fig3, plot_fig3()),
  tar_target(fig4, plot_fig4()),
  tar_target(fig5, plot_fig5()),
  tar_target(figS1, plot_figS1()),
  tar_target(figS2, plot_figS2()),
  tar_target(figS3, plot_figS3()),
  tar_target(figS4, plot_figS4()),
  tar_target(figS5_S6, plot_figS5_S6()),
  tar_target(figS8, plot_figS8()),
  tar_target(figS9, plot_figS9()),
  tar_target(figS12, plot_fig_S12()),
  tar_target(figS13, plot_fig_S13())
)
