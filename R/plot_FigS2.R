lapply(c("rstan", "tidyverse", "lubridate", "reshape2", "patchwork", "cowplot", "viridis", "viridisLite"), require, character.only = TRUE)

plot_figS2 <- function() {
  # For Model 5
  foi_calc <- function(beta, a){
    foi_counter = 0.0
    for(j in 0:(a-1)){
      foi_counter = foi_counter + exp(-beta * j)
    }
    foi_counter
  }
  
  z <- function(lambda, beta, a){
    foi_counter = foi_calc(beta, a)
    (1-exp(-foi_counter * lambda))
  }
  
  zs <- function(lambda, beta, as){
    zvals <- vector(length = length(as))
    for(i in seq_along(as)){
      zvals[i] = z(lambda, beta, as[i])
    }
    zvals
  }
  
  N = 80 # number of age classes
  as = seq(1, N)
  
  prior_predictive_plot <- function(lambda, beta, title) { # lambda and beta are vectors!
    n <- length(lambda)
    for(i in seq(1, n)) {
      z_tmp <- zs(lambda[i], beta[i], as)
      prev_tmp <- tibble(
        age = as,
        z = z_tmp,
        replicate = i
      )
      if(i == 1)
        big_df <- prev_tmp
      else
        big_df <- big_df %>% bind_rows(prev_tmp)
    }
    
    sum_df <- big_df %>% 
      group_by(age) %>% 
      summarise(middle = median(z),
                lower = quantile(z, 0.025),
                upper = quantile(z, 0.975))
    
    sum_df %>% 
      ggplot(aes(x = age, y = middle)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
      geom_line() +
      labs(x = "Age", y = "Seropositivity", title = title) +
      scale_y_continuous(labels = scales::percent) +
      theme_bw() +
      theme(axis.text = element_text(size = 9),
            axis.title = element_text(size = 11),
            plot.title = element_text(size = 14, face = 'bold'))
  }
  
  n = 10000
  
  ## prior predictive plot for model 5 (without seroreversion) ---
  lambda = rexp(n, 1)
  beta <- rexp(n, 20)
  a <- prior_predictive_plot(lambda, beta, title = "(A)")
  
  # sensitivity analysis on the above model ----
  lambda = rexp(n, 10)
  b <- prior_predictive_plot(lambda, beta, title = "(B)")
  
  # For Model 6:
  foi_calc <- function(beta, a){
    foi_counter = 0.0
    for(j in 0:(a-1)){
      foi_counter = foi_counter + exp(-beta * j)
    }
    foi_counter
  }
  
  analytic_integrator <- function(lambda, beta, rho, a, a_primed){
    first = lambda / beta * (exp(-beta * a_primed) - exp(-beta * a))
    second = rho * (a - a_primed)
    (first + second)
  }
  
  analytic_integrator_full <- function(lambda, beta, rho, N){
    foi_counter = 0.0;
    for(i in 0:(N-1)){
      foi_counter = foi_counter + exp(-analytic_integrator(lambda, beta, rho, N, i))
    }
    foi_counter = foi_counter * rho
    (foi_counter)
  }
  
  z <- function(lambda, beta, rho, a){
    lambda_second = analytic_integrator_full(lambda, beta, rho, a)
    lambda_first = lambda / beta * (1 - exp(-beta * a)) + rho * a
    (1-exp(-lambda_first) - lambda_second)
  }
  
  zs <- function(lambda, beta, rho, as){
    zvals <- vector(length = length(as))
    for(j in seq_along(as)){
      zvals[j] = z(lambda, beta, rho, as[j])
    }
    zvals
  }
  
  N = 80 # number of age classes
  as = seq(1, N)
  
  prior_predictive_plot <- function(lambda, beta, rho, title) { # lambda and beta are vectors!
    n <- length(lambda)
    for(i in seq(1, n)) {
      z_tmp <- zs(lambda[i], beta[i], rho[i], as)
      prev_tmp <- tibble(
        age = as,
        z = z_tmp,
        replicate = i
      )
      if(i == 1)
        big_df <- prev_tmp
      else
        big_df <- big_df %>% bind_rows(prev_tmp)
    }
    
    sum_df <- big_df %>% 
      group_by(age) %>% 
      summarise(middle = median(z),
                lower = quantile(z, 0.025),
                upper = quantile(z, 0.975))
    
    sum_df %>% 
      ggplot(aes(x = age, y = middle)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
      geom_line() +
      labs(x = "Age", y = "Seropositivity", title = title) +
      scale_y_continuous(labels = scales::percent) +
      theme_bw() +
      theme(axis.text = element_text(size = 9),
            axis.title = element_text(size = 11),
            plot.title = element_text(size = 14, face = 'bold'))
  }
  
  n = 10000
  
  ### prior predictive plot for model 6 (with seroreversion) ----
  lambda = rexp(n, 1)
  beta = rexp(n, 20)
  rho = rexp(n, 20)
  c <- prior_predictive_plot(lambda, beta, rho, title = "(C)")
  
  # prior that gives more weight to lower seropositivities ---
  lambda = rexp(n, 10)
  d <- prior_predictive_plot(lambda, beta, rho, title = "(D)")
  
  figS2 <- (a | b) / (c | d)
  ggsave('figures/FigureS2.pdf', figS2, height = 150, width = 183, units = 'mm', dpi = 300)
  
}

plot_figS2()
