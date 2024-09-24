lapply(c("rstan", "tidyverse", "lubridate", "reshape2", "patchwork", "cowplot", "viridis", "viridisLite"), require, character.only = TRUE)

plot_fig6 <- function() {
  # (A)
  # probability of being infected by age ---- model 2:
  prob_infected_m2 <- function(lambda, age_range) { # version 1
    tibble(age = age_range, prob = 1 - exp(-(lambda * age_range)))
  }
  
  infection_curves_subsample_m2 <- function(df, age_range, n_iterations = 1000) {
    for(i in 1:n_iterations) {
      index <- sample(nrow(df), 1)
      df_tmp <- df[index, ]
      infected_tmp <- prob_infected_m2(df_tmp[1, "lambda"], age_range) %>% mutate(iteration = i)
      if(i == 1)
        big_df <- infected_tmp
      else
        big_df <- big_df %>% bind_rows(infected_tmp)
    }
    big_df
  }
  
  summarise_infected_curve_uncertainty_m2 <- function(filename, age_range, n_iterations = 1000) {
    df <- readRDS(filename) %>% as.data.frame()
    
    tmp <- infection_curves_subsample_m2(df, age_range, n_iterations)
    tmp %>% 
      group_by(age) %>% 
      summarise(middle = median(prob),
                lower = quantile(prob, 0.025),
                upper = quantile(prob, 0.975))
  }
  
  age_range <- seq(0, 20, 0.1)
  cva6_m2 <- summarise_infected_curve_uncertainty_m2("results/CVA6/CVA6_model2_lambda_rho.rds",
                                                     age_range, 1000) %>% mutate(Serotype = "CVA6")
  eva71_m2 <- summarise_infected_curve_uncertainty_m2("results/EVA71/EVA71_model2_lambda_rho.rds",
                                                      age_range, 1000) %>% mutate(Serotype = "EV-A71")
  both_m2 <- eva71_m2 %>% bind_rows(cva6_m2) %>% mutate(model = "M2")
  
  both_m2 %>% 
    ggplot(aes(x = age, y = middle)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Serotype), alpha = 0.5) +
    geom_line(aes(colour = Serotype)) +
    scale_colour_manual(values = c("#D95F02", "#1B9E77"), name = "") +
    scale_fill_manual(values = c("#D95F02", "#1B9E77"), name = "") +
    labs(x = "Age, years", y = "Probability of having been infected", title = "(A)") +
    theme_bw() +
    theme(axis.text = element_text(size = 9),
          plot.title = element_text(face = 'bold', size = 14),
          axis.title = element_text(size = 11),
          legend.position = c(0.8, 0.8))
  
  # probability of being infected by age ---- Model 5 and 6:
  prob_infected_age <- function(lambda, beta, age_range) { # version 1
    tibble(age = age_range, prob = 1 - exp(- lambda / beta * (1 - exp(-age_range * lambda))))
  }
  
  infection_curves_subsample_age <- function(df, age_range, n_iterations = 1000) {
    for(i in 1:n_iterations) {
      index <- sample(nrow(df), 1)
      df_tmp <- df[index, ]
      infected_tmp <- prob_infected_age(df_tmp[1, "lambda"], df_tmp[1, "beta"], age_range) %>% 
        mutate(iteration = i)
      if(i == 1)
        big_df <- infected_tmp
      else
        big_df <- big_df %>% bind_rows(infected_tmp)
    }
    big_df
  }
  
  summarise_infected_curve_uncertainty_age <- function(filename, age_range, n_iterations = 1000) {
    df <- readRDS(filename) %>% as.data.frame()
    
    tmp <- infection_curves_subsample_age(df, age_range, n_iterations)
    tmp %>% 
      group_by(age) %>% 
      summarise(middle = median(prob),
                lower = quantile(prob, 0.025),
                upper = quantile(prob, 0.975))
  }
  
  age_range <- seq(0, 20, 0.1)
  cva6_m5 <- summarise_infected_curve_uncertainty_age("results/CVA6/CVA6_model5_lambda_beta.rds",
                                                      age_range, 1000) %>% mutate(Serotype = "CVA6")
  eva71_m5<- summarise_infected_curve_uncertainty_age("results/EVA71/EVA71_model5_lambda_beta.rds",
                                                      age_range, 1000) %>% mutate(Serotype = "EV-A71")
  both_m5 <- eva71_m5 %>% bind_rows(cva6_m5) %>% mutate(model = "M5")
  
  both_m5 %>% 
    ggplot(aes(x = age, y = middle)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Serotype), alpha = 0.5) +
    geom_line(aes(colour = Serotype)) +
    scale_colour_manual(values = c("#D95F02", "#1B9E77"), name = "") +
    scale_fill_manual(values = c("#D95F02", "#1B9E77"), name = "") +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = "Age, years", y = "Probability of having been infected", title = "(B)") +
    theme_bw() +
    theme(axis.text = element_text(size = 9),
          plot.title = element_text(face = 'bold', size = 14),
          axis.title = element_text(size = 11),
          legend.position = c(0.8, 0.6))
  
  # combine the datasets above:
  
  allDF <- both_m2 %>% bind_rows(both_m5) %>% 
    mutate(virus_f = factor(Serotype, levels=c("EV-A71", "CVA6")))
  
  # require(paletteer); https://r-charts.com/color-palettes/
  A <- allDF %>% 
    ggplot(aes(x = age, y = middle)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.5) +
    geom_line(aes(colour = model)) +
    scale_colour_manual(values = c("#003C67", "#8F7700"), name = "", 
                        labels=c('Age-constant FOI & seroreversion', 'Age-dependent FOI & no seroreversion')) +
    scale_fill_manual(values = c("#003C67", "#8F7700"), name = "", 
                      labels=c('Age-constant FOI & seroreversion', 'Age-dependent FOI & no seroreversion')) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = "Age, years", y = "Probability of having been infected", title = "(A)") +
    theme_bw() +
    theme(axis.text = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = "bold"),
          strip.text.x = element_text(size = 11),
          strip.background = element_rect(fill = "gray90"),
          legend.position = "bottom") +
    facet_wrap(~ virus_f)
  
  #------------------------------------------------------------------------------------------
  # (B)
  # age foi curves:
  exponential_age_foi <- function(lambda, beta, age_range) {
    tibble(age = age_range, foi = lambda * exp(- beta * age_range))
  }
  
  age_foi_curves_subsample <- function(df, age_range, n_iterations = 1000) {
    for(i in 1:n_iterations) {
      index <- sample(nrow(df), 1)
      df_tmp <- df[index, ]
      foi_tmp <- exponential_age_foi(df_tmp$lambda[1], df_tmp$beta[1], age_range) %>% 
        mutate(iteration = i)
      if(i == 1)
        big_df <- foi_tmp
      else
        big_df <- big_df %>% bind_rows(foi_tmp)
    }
    big_df
  }
  
  summarise_age_curve_uncertainty <- function(filename, age_range, n_iterations = 1000) {
    df <- readRDS(filename) %>% as.data.frame()
    tmp <- age_foi_curves_subsample(df, age_range, n_iterations)
    tmp %>% group_by(age) %>% 
      summarise(middle = median(foi),
                lower = quantile(foi, 0.025), upper = quantile(foi, 0.975))
  }
  
  age_range <- seq(0, 20, 0.1)
  cva6 <- summarise_age_curve_uncertainty("results/CVA6/CVA6_model5_lambda_beta.rds",
                                          age_range, 1000) %>% 
    mutate(virus = "CVA6", model = "M5")
  eva71 <- summarise_age_curve_uncertainty("results/EVA71/EVA71_model5_lambda_beta.rds", 
                                           age_range, 1000) %>% 
    mutate(virus = "EV-A71", model = "M5")
  
  # constant foi
  foi <- function(lambda, age_range) {
    tibble(age = age_range, foi = lambda)
  }
  
  foi_curves_subsample <- function(df, age_range, n_iterations = 1000) {
    for(i in 1:n_iterations) {
      index <- sample(nrow(df), 1)
      df_tmp <- df[index, ]
      foi_tmp <- foi(df_tmp$lambda[1], age_range) %>% mutate(iteration = i)
      if(i == 1)
        big_df <- foi_tmp
      else
        big_df <- big_df %>% bind_rows(foi_tmp)
    }
    big_df
  }
  
  foi_uncertainty <- function(filename, age_range, n_iterations = 1000) {
    df <- readRDS(filename) %>% as.data.frame()
    tmp <- foi_curves_subsample(df, age_range, n_iterations)
    tmp %>% group_by(age) %>% 
      summarise(middle = median(foi), 
                lower = quantile(foi, 0.025), upper = quantile(foi, 0.975))
  }
  
  age_range <- seq(0, 20, 0.1)
  cva6b <- foi_uncertainty("results/CVA6/CVA6_model2_lambda_rho.rds",
                           age_range, 1000) %>% mutate(virus = "CVA6", model = "M2")
  eva71b <- foi_uncertainty("results/EVA71/EVA71_model2_lambda_rho.rds",
                            age_range, 1000) %>% mutate(virus = "EV-A71", model = "M2")
  both <- cva6 %>% bind_rows(eva71, cva6b, eva71b)
  
  g1 <- ggplot() +
    geom_ribbon(data = both[which(both$model == 'M5'), ], 
                aes(x = age, ymin = lower, ymax = upper, fill = virus), alpha = 0.5) +
    geom_line(data = both[which(both$model == 'M5'), ], aes(x = age, y = middle, 
                                                            colour = virus)) +
    geom_ribbon(data = both[which(both$model == 'M2'), ], 
                aes(x = age, ymin = lower, ymax = upper, fill = virus), alpha = 0.5) +
    geom_line(data = both[which(both$model == 'M2'), ], aes(x = age, y = middle, 
                                                            colour = virus)) +
    scale_colour_manual(values = c("#D95F02", "#1B9E77")) +
    scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
    ylim(0, 0.7) +
    #scale_color_brewer("Serotype", palette = "Dark2") + 
    #scale_fill_brewer("Serotype", palette = "Dark2") + 
    labs(x = "Age, years", y = "FOI", title = "(B)") +
    theme_bw() +
    theme(axis.text = element_text(size = 9),
          plot.title = element_text(face = 'bold', size = 14),
          axis.title = element_text(size = 11),
          legend.position = "none")
  
  # probability of detectable antibody curves ----
  prob_detection <- function(rho, time_range) {
    tibble(time = time_range, prob_detection = exp(- rho * time_range))
  }
  
  antibody_detection_curves_subsample <- function(df, time_range, n_iterations = 1000) {
    for(i in 1:n_iterations) {
      index <- sample(nrow(df), 1)
      rho <- df[index, ]
      antibody_tmp <- prob_detection(rho, time_range) %>% mutate(iteration = i)
      if(i == 1)
        big_df <- antibody_tmp
      else
        big_df <- big_df %>% bind_rows(antibody_tmp)
    }
    big_df
  }
  
  summarise_antibody_curve_uncertainty <- function(filename, time_range, n_iterations = 1000) {
    df <- readRDS(filename) %>% as.data.frame()
    tmp <- antibody_detection_curves_subsample(df, time_range, n_iterations)
    tmp %>% group_by(time) %>% 
      summarise(middle = median(prob_detection),
                lower = quantile(prob_detection, 0.025),
                upper = quantile(prob_detection, 0.975))
  }
  
  time_range <- seq(0, 20, 0.1)
  cva6 <- summarise_antibody_curve_uncertainty("results/CVA6/CVA6_model2_seroreversion_rho.rds",
                                               time_range, 1000) %>% mutate(virus = "CVA6")
  eva71 <- summarise_antibody_curve_uncertainty("results/EVA71/EVA71_model2_seroreversion_rho.rds",
                                                time_range, 1000) %>% mutate(virus = "EV-A71")
  both <- cva6 %>% bind_rows(eva71)
  
  g2 <- both %>% 
    ggplot(aes(x = time, y = middle)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = virus), alpha = 0.5) +
    geom_line(aes(colour = virus)) +
    scale_colour_manual(values = c("#D95F02", "#1B9E77"), name = "") +
    scale_fill_manual(values = c("#D95F02", "#1B9E77"), name = "") +
    labs(x = "Time since infection, years", y = "Probability of detectable antibodies", 
         title = "(C)") +
    theme_bw() +
    theme(axis.text = element_text(size = 9),
          plot.title = element_text(face = 'bold', size = 14),
          axis.title = element_text(size = 11),
          legend.position = c(0.8, 0.8))
  
  B <- (g1 | g2)
  
  fig6 <- A / B
  ggsave(filename = 'results/figures/Figure6.pdf', fig6, height = 183, width = 183, units = 'mm', dpi = 300)
  
}

plot_fig6()
