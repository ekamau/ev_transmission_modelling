require(tidyverse)
require(patchwork)

plot_fig5 <- function() {
  age_seropositivity_curves_subsample <- function(
    df, age_range, sero_function, n_iterations = 1000) {
    for(i in 1:n_iterations) {
      index <- sample(nrow(df), 1)
      df_tmp <- df[index, ]
      foi_tmp <- sero_function(
        df_tmp, age_range) %>% mutate(iteration=i)
      if(i == 1)
        big_df <- foi_tmp
      else
        big_df <- big_df %>% bind_rows(foi_tmp)
    }
    big_df
  }
  
  summarise_age_curve_uncertainty <- function(filename, age_range, sero_function, n_iterations = 1000) {
    df <- readRDS(filename) %>% as.data.frame()
    tmp <- age_seropositivity_curves_subsample(df, age_range, sero_function, n_iterations)
    tmp %>% group_by(age) %>% 
      summarise(middle = median(seropositivity),
                lower = quantile(seropositivity, 0.025),
                upper = quantile(seropositivity, 0.975))
  }
  
  both_viruses <- function(filename_cva6, filename_eva71, age_range, sero_function) {
    cva6 <- summarise_age_curve_uncertainty(filename_cva6, age_range, sero_function, 1000) %>% 
      mutate(virus = "CVA6")
    
    eva71 <- summarise_age_curve_uncertainty(filename_eva71, age_range, sero_function, 1000) %>% 
      mutate(virus = "EV-A71")
    
    both <- cva6 %>% bind_rows(eva71)
    both
  }
  
  
  # model 2 -----
  z <- function(lambda, rho, age_range){
    sero <- (lambda / (lambda + rho)) * (1 - exp(- age_range * (lambda + rho))) 
    tibble(age = age_range, seropositivity = sero)
  }
  
  sero_function_seroreversion <- function(df_single, age_range) {
    z(df_single$lambda[1], df_single$gamma[1], age_range)
  }
  
  age_range <- seq(0, 85, 0.1)
  df_2 <- both_viruses("results/CVA6/CVA6_model2_lambda_rho.rds",
                       "results/EVA71/EVA71_model2_lambda_rho.rds",
                       age_range, sero_function_seroreversion)
  
  # model 5 -----
  z1 <- function(lambda, beta, age_range){
    sero <- 1 - exp(-lambda / beta * (1 - exp(-age_range * beta))) 
    tibble(age = age_range, seropositivity = sero)
  }
  
  sero_function_age <- function(df_single, age_range) {
    z1(df_single$lambda[1], df_single$beta[1], age_range)
  }
  
  df_5 <- both_viruses("results/CVA6/CVA6_model5_lambda_beta.rds",
                       "results/EVA71/EVA71_model5_lambda_beta.rds",
                       age_range, sero_function_age)
  
  # combine and plot versus real data ----
  df_both <- df_2 %>% mutate(model = "Age-constant FOI & seroreversion") %>% 
    bind_rows(df_5 %>% mutate(model = "Age-dependent FOI & no seroreversion")) %>% 
    mutate(type = "simulated")
  
  process_data <- function(filename, age_bins) {
    read.csv(filename, header = TRUE, sep = ",") %>%
      rename_all(tolower) %>% filter(age >= 1) %>% 
      mutate(age_bin = cut(age, age_bins, include.lowest = TRUE)) %>% 
      mutate(serostatus = case_when(final_titer >= 8 ~ 'Positive', 
                                    final_titer < 8 ~ 'Negative')) %>% 
      rename(year=year_collection) %>% group_by(year, age_bin) %>% 
      summarise(n = n(), n_positive = sum(serostatus == "Positive"), age = mean(age)) %>% 
      mutate(first = 1 + n_positive, second = 1 + n - n_positive) %>% 
      mutate(middle = qbeta(0.5, first, second),
             lower = qbeta(0.05, first, second),
             upper = qbeta(0.975, first, second)) %>% 
      mutate(type = "real")
  }
  
  age_bins <- c(1, 4, 9, 19, 29, 39, 49, 59, 69, 79, 109)
  df_cva6 <- process_data("data/titers_CVA6.csv", age_bins) %>% mutate(virus = "CVA6")
  df_eva71 <- process_data("data/titers_EVA71.csv", age_bins) %>% mutate(virus = "EV-A71")
  df_real <- df_cva6 %>% bind_rows(df_eva71)
  
  ggplot(df_both, aes(x = age, y = middle)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    geom_line() +
    geom_pointrange(data = df_real, aes(ymin = lower, ymax = upper, colour = as.factor(year))) +
    scale_color_brewer("Year", palette = "Dark2") +
    labs(x = "Age, years", y = "Seropositivity") +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    theme(axis.title = element_text(size = 11),
          axis.text = element_text(size = 9),
          panel.grid = element_blank(),
          plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 11),
          legend.position = "bottom") +
    facet_grid(vars(virus), vars(model))
  
  # Add binomial sampling noise -----
  
  summarise_age_curve_uncertainty_all <- function(
    filename, age_range, sero_function, n_mcmc_iterations, n_rbinom_iterations, sample_size) {
    
    df <- readRDS(filename) %>% as.data.frame()
    tmp <- age_seropositivity_curves_subsample(df, age_range, sero_function, n_mcmc_iterations)
    n_iterations <- n_distinct(tmp$iteration)
    for(i in 1:n_iterations) {
      df_short <- tmp %>% filter(iteration == i)
      n_age <- length(df_short$age)
      lowers <- vector(length = n_age)
      mids <- vector(length = n_age)
      uppers <- vector(length = n_age)
      for(j in seq_along(df_short$age)) {
        x_rvs <- rbinom(n_rbinom_iterations, sample_size, df_short$seropositivity[j])
        x_rvs <- x_rvs / sample_size
        lower <- quantile(x_rvs, 0.025)
        middle <- quantile(x_rvs, 0.5)
        upper <- quantile(x_rvs, 0.975)
        lowers[j] <- lower
        mids[j] <- middle
        uppers[j] <- upper
      }
      df_short <- df_short %>% mutate(lower = lowers, middle = mids, upper = uppers)
      if(i == 1)
        big_df <- df_short
      else
        big_df <- big_df %>% bind_rows(df_short)
    }
    
    big_df %>% group_by(age) %>% 
      summarise(middle = mean(middle), lower = mean(lower), upper = mean(upper))
  }
  
  both_viruses_all <- function(filename_cva6, filename_eva71, age_range, 
                               sero_function, n_mcmc_iterations, n_rbinom_iterations, 
                               sample_size_cva6, sample_size_eva71) {
    
    cva6 <- summarise_age_curve_uncertainty_all(filename_cva6, age_range, 
                                                sero_function, n_mcmc_iterations, n_rbinom_iterations,
                                                sample_size_cva6) %>% 
      mutate(virus = "CVA6")
    
    eva71 <- summarise_age_curve_uncertainty_all(filename_eva71, age_range, 
                                                 sero_function, n_mcmc_iterations, n_rbinom_iterations,
                                                 sample_size_eva71) %>% 
      mutate(virus = "EV-A71")
    
    both <- cva6 %>% bind_rows(eva71)
    both
  }
  
  n_cva6 <- round(median(df_cva6$n))
  n_eva71 <- round(median(df_eva71$n))
  n_mcmc <- 1000
  n_rbinom <- 10000
  df_2 <- both_viruses_all("results/CVA6/CVA6_model2_lambda_rho.rds",
                           "results/EVA71/EVA71_model2_lambda_rho.rds",
                           age_range, sero_function_seroreversion, n_mcmc, n_rbinom, n_cva6, n_eva71)
  
  df_5 <- both_viruses_all("results/CVA6/CVA6_model5_lambda_beta.rds",
                           "results/EVA71/EVA71_model5_lambda_beta.rds",
                           age_range, sero_function_age, n_mcmc, n_rbinom, n_cva6, n_eva71)
  
  df_both <- df_2 %>% mutate(model = "Age-constant FOI & seroreversion") %>% 
    bind_rows(df_5 %>% mutate(model = "Age-dependent FOI & no seroreversion")) %>% 
    mutate(type = "simulated") %>% mutate(virus = as.factor(virus)) %>% 
    mutate(virus = fct_rev(virus))
  
  #########
  
  df_real <- df_real %>% mutate(virus = as.factor(virus)) %>% mutate(virus = fct_rev(virus))
  
  g <- ggplot(df_both, aes(x = age, y = middle)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    geom_line() +
    geom_pointrange(data = df_real, aes(ymin = lower, ymax = upper, colour = as.factor(year))) +
    scale_color_brewer("Year", palette = "Dark2") +
    labs(x = "Age, years", y = "Seropositivity") +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    theme(axis.title = element_text(size = 11),
          axis.text = element_text(size = 9),
          panel.grid = element_blank(),
          plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 10),
          legend.position = "bottom") +
    facet_grid(vars(virus), vars(model))
  
  ggsave("results/figures/Figure5.pdf", g, height = 150, width = 183, units = 'mm', dpi = 300)
  
}

plot_fig5()
