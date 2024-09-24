lapply(c("rstan", "tidyverse", "lubridate", "reshape2", "patchwork", "cowplot", "viridis", "viridisLite"), require, character.only = TRUE)

plot_figS1 <- function() {
  z <- function(lambda, rho, a){
    (lambda / (lambda + rho)) * (1 - exp(- a * (lambda + rho))) 
  }
  
  zs <- function(lambda, rho, as){
    zvals <- vector(length = length(as))
    for(i in seq_along(as)){
      zvals[i] = z(lambda, rho, as[i])
    }
    zvals
  }
  
  N = 80 # number of age classes
  as = seq(1, N)
  
  prior_predictive_plot <- function(lambda, rho, title) { # lambda and rho are vectors!
    n <- length(lambda)
    for(i in seq(1, n)) {
      z_tmp <- zs(lambda[i], rho[i], as)
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
    
    sum_df <- big_df %>% group_by(age) %>% 
      summarise(middle = median(z),
                lower = quantile(z, 0.025),
                upper = quantile(z, 0.975))
    
    sum_df %>% 
      ggplot(aes(x = age, y = middle)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue",
                  alpha = 0.2) +
      geom_line() +
      labs(x = "Age", y = "Seropositivity", title = title) +
      scale_y_continuous(labels = scales::percent) +
      theme_bw() +
      theme(axis.text = element_text(size = 9),
            axis.title = element_text(size = 11),
            plot.title = element_text(size = 14, face = 'bold'))
  }
  
  # prior predictive plot for model with seroreversion ----
  n = 10000
  lambda = rexp(n, 1)
  rho = rexp(n, 20)
  a <- prior_predictive_plot(lambda, rho, title = "(A)")

  # prior that gives more weight to lower seropositivities ---
  lambda = rexp(n, 10)
  b <- prior_predictive_plot(lambda, rho, title = "(B)")
  
  # prior predictive plot for model without seroreversion ---
  rho <- rep(0, n)
  lambda = rexp(n, 1)
  c <- prior_predictive_plot(lambda, rho, title = "(C)")
  
  # sensitivity analysis on the above model ----
  rho <- rep(0, n)
  lambda = rexp(n, 10)
  d <- prior_predictive_plot(lambda, rho, title = "(D)")
   
  figS1 <- (a | b) / (c | d)
  ggsave('figures/FigureS1.pdf', figS1, height = 150, width = 183, units = 'mm', dpi = 300)
  
}

plot_figS1()
