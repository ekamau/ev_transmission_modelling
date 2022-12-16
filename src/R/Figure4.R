plot_fig4 <- function() {
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
  fig4 <- allDF %>% 
    ggplot(aes(x = age, y = middle)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.5) +
    geom_line(aes(colour = model)) +
    scale_colour_manual(values = c("#003C67", "#8F7700"), name = "", 
                        labels=c('Constant FOI & seroreversion', 'Age-dependent FOI')) +
    scale_fill_manual(values = c("#003C67", "#8F7700"), name = "", 
                      labels=c('Constant FOI & seroreversion', 'Age-dependent FOI')) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = "Age, years", y = "Probability (having been infected)") +
    theme_bw() +
    theme(axis.text = element_text(size = 9),
          axis.title = element_text(size = 11),
          strip.text.x = element_text(size = 11),
          strip.background = element_rect(fill = "gray90"),
          legend.position = "bottom") +
    facet_wrap(~ virus_f)
  
  ggsave(filename = 'figures/Figure4.pdf', fig4, height = 90, width = 183, units = 'mm', dpi = 300)
  
}
