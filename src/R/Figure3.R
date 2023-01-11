plot_fig3 <- function(){
  #models 1 and 2:
  
  A <- ggplot() +
    geom_point(aes(x = as.factor(0), y = 0.0644), color = "#1B9E77") +
    geom_errorbar(aes(x = as.factor(0), ymin = 0.06, ymax = 0.0691), color = "#1B9E77", 
                  width = 0.25) +
    geom_point(aes(x = as.factor(0), y = 0.26), color = "#1B9E77") +
    geom_errorbar(aes(x = as.factor(0), ymin = 0.21, ymax = 0.32), color = "#1B9E77", 
                  width = 0.25, linetype='dashed') +
    geom_point(aes(x = as.factor(1), y = 0.0822), color = "#D95F02") +
    geom_errorbar(aes(x = as.factor(1), ymin = 0.0763, ymax = 0.0884), color = "#D95F02", 
                  width = 0.25) +
    geom_point(aes(x = as.factor(1), y = 0.46), color = "#D95F02") +
    geom_errorbar(aes(x = as.factor(1), ymin = 0.36, ymax = 0.59), color = "#D95F02", 
                  width = 0.25, linetype='dashed') +
    #annotate(geom='text', x = as.factor(1), y = 0.46, label="II", color='black', hjust = -1.5, size=4) +
    #annotate(geom='text', x = as.factor(1), y = 0.0822, label="I", color='black', hjust = -1.5, vjust = 1.5, size=4) +
    #annotate(geom='text', x = as.factor(0), y = 0.26, label="II", color='black', hjust = -1.5, size=4) +
    #annotate(geom='text', x = as.factor(0), y = 0.0644, label="I", color='black', hjust = -1.5, vjust = 1.5, size=4) +
    labs(x = '', y = "Annual probability of infection", title = '(A)') +
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 0.7, by = 0.1), limits = c(0,0.7)) +
    scale_x_discrete(labels = c('EV-A71', 'CVA6')) +
    theme(axis.title = element_text(size = 11),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          plot.title = element_text(size = 14, face = "bold"),
          panel.grid = element_blank()) +
    coord_fixed(ratio = 5)
  
  A
  
  # model 3:
  # EV-A71:
  
  df <- read.csv('data/E71_fit_summary_model3.csv', header = TRUE, sep = ',', row.names = 1)
  est.lambda <- data.frame(year = 2017 - seq(0,91-1), mean = rep(NA,times=91), low = rep(NA,times=91), up = rep(NA,times=91))
  
  for(i in 1:91){
    est.lambda$mean[i] = 1 - exp(-df$mean[i])
    est.lambda$low[i] = 1 - exp(-df$X2.5.[i])
    est.lambda$up[i] = 1 - exp(-df$X97.5.[i])
  }
  
  # CVA6:
  
  dfB <- read.csv('data/CA6_fit_summary_model3.csv', header = TRUE, sep = ',', row.names = 1)
  est.lambdaB <- data.frame(year = 2017 - seq(0,91-1), mean = rep(NA,times=91), low = rep(NA,times=91), up = rep(NA,times=91))
  
  for(i in 1:91){
    est.lambdaB$mean[i] = 1 - exp(-dfB$mean[i])
    est.lambdaB$low[i] = 1 - exp(-dfB$X2.5.[i])
    est.lambdaB$up[i] = 1 - exp(-dfB$X97.5.[i])
  }
  
  B <- ggplot() +
    geom_line(data = est.lambdaB[1:12, ], aes(x = year, y = mean), linewidth = 1, 
              col = "#D95F02") + # CVA6
    geom_ribbon(data = est.lambdaB[1:12, ], aes(x = year, ymin = low, ymax = up), 
                fill = "#D95F02", alpha=0.3) + # CVA6
    geom_ribbon(data = est.lambda[1:12, ], aes(x = year, ymin = low, ymax = up) , 
                fill = "#1B9E77", alpha=0.4) + # EV-A71
    geom_line(data = est.lambda[1:12, ], aes(x = year, y = mean), col = "#1B9E77", 
              linewidth = 1) + # EV-A71
    scale_y_continuous(breaks = seq(0, 0.7, by = 0.1), limits = c(0,0.7)) +
    labs(x = "Year", title = '(B)', y = '') +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = 14),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 9)) +
    scale_x_continuous(breaks = seq(from = 2017, to = 2006, by = -2)) +
    annotate(geom = 'text', x = 2010, y = 0.2, label = "CVA6", color = "#D95F02", size = 4) +
    annotate(geom = 'text', x = 2010, y = 0.08, label = "EV-A71", color = "#1B9E77", size = 4)
  
  B
  
  fig3 <- A | B
  ggsave(filename = 'figures/Figure3.pdf', fig3, height = 90, width = 183, units = 'mm', dpi = 300)
}

