
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(patchwork)

source("src/R/process_cva6_data.R")
source("src/R/process_eva71_data.R")
source("src/R/process_cva6_fit_summary.R")
source("src/R/process_eva71_fit_summary.R")

plot_fig_S12 <- function() {
  ### CVA6:
  # Serology data:
  datCA6 <- process_cva6_data()
  CA6_06 <- as.data.frame(datCA6[1])
  CA6_11 <- as.data.frame(datCA6[2])
  CA6_17 <- as.data.frame(datCA6[3])
  
  # Model fit data - Model 4:
  fitCA6_3 <- process_cva6_fit_summary("results/CVA6/model3/fitSummary_CA6_model3.csv")
  est_CA6_06_3 <- as.data.frame(fitCA6_3[1])
  est_CA6_11_3 <- as.data.frame(fitCA6_3[2])
  est_CA6_17_3 <- as.data.frame(fitCA6_3[3])
  
  df06_CA6_3 <- cbind(CA6_06[,], est_CA6_06_3[,])
  df06_CA6_3$estProp06 = df06_CA6_3$mean/df06_CA6_3$n # estimated seroprevalence
  df06_CA6_3$estProp06low = df06_CA6_3$low/df06_CA6_3$n
  df06_CA6_3$estProp06up = df06_CA6_3$up/df06_CA6_3$n
  
  df11_CA6_3 <- cbind(CA6_11[,], est_CA6_11_3[,])
  df11_CA6_3$estProp11 = df11_CA6_3$mean/df11_CA6_3$n # estimated seroprevalence
  df11_CA6_3$estProp11low = df11_CA6_3$low/df11_CA6_3$n
  df11_CA6_3$estProp11up = df11_CA6_3$up/df11_CA6_3$n
  
  df17_CA6_3 <- cbind(CA6_17[,], est_CA6_17_3[,])
  df17_CA6_3$estProp17 = df17_CA6_3$mean/df17_CA6_3$n # estimated seroprevalence
  df17_CA6_3$estProp17low = df17_CA6_3$low/df17_CA6_3$n
  df17_CA6_3$estProp17up = df17_CA6_3$up/df17_CA6_3$n
  
  colnames(df06_CA6_3) <- c("Year","age","n","seropositive","propn","mean","low",
                          "up","estProp","estProplow","estPropup")
  colnames(df11_CA6_3) <- c("Year","age","n","seropositive","propn","mean","low",
                          "up","estProp","estProplow","estPropup")
  colnames(df17_CA6_3) <- c("Year","age","n","seropositive","propn","mean","low",
                          "up","estProp","estProplow","estPropup")
  df_fitCA6_3 <- bind_rows(df06_CA6_3, df11_CA6_3, df17_CA6_3)
  
  CA6_figM3 <- df_fitCA6_3 %>% 
    ggplot() +
    geom_ribbon(data = , aes(ymin = estProplow, ymax = estPropup, x = age), 
                fill = "#D95F02", alpha = 0.5) +
    geom_line(data = df_fitCA6_3, aes(x = age, y = estProp), color = "#D95F02", linewidth = 0.8) +
    #geom_pointrange(data = df_fitCA6, aes( ymin = LCB, ymax = UCB),  color = 'black') +
    geom_point(data = df_fitCA6_3, aes(x = age, y = propn), shape = 1, size=0.2, color = 'black') + 
    theme_bw() +
    facet_wrap(~Year) +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 9),
          plot.title = element_text(face = 'bold', size = 14),
          axis.title = element_text(size = 11),
          legend.position = 'none',
          strip.text.x = element_text(size = 11),
          strip.background = element_rect(fill = "gray90")) + 
    scale_x_continuous(breaks = seq(from = 0, to = 80, by = 20)) + 
    scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2), 
                       labels = scales::percent_format(accuracy = 1)) + 
    labs(y = 'Seroprevalence', x = 'Age, years', title = '(B)')
  
  
  #-------------------------------------------------------------------------------------------------------------
  ### EV-A71:
  # Serology data:
  datE71 <- process_eva71_data()
  E71_06 <- as.data.frame(datE71[1])
  E71_11 <- as.data.frame(datE71[2])
  E71_17 <- as.data.frame(datE71[3])
  
  # Model fit data - Model 4:
  fitE71_3 <- process_eva71_fit_summary("results/EVA71/model3/fitSummary_E71_m3.csv")
  est_E71_06_3 <- as.data.frame(fitE71_3[1])
  est_E71_11_3 <- as.data.frame(fitE71_3[2])
  est_E71_17_3 <- as.data.frame(fitE71_3[3])
  
  df06_E71_3 <- cbind(E71_06[,], est_E71_06_3[,])
  df06_E71_3$estProp06 = df06_E71_3$mean/df06_E71_3$n # estimated seroprevalence
  df06_E71_3$estProp06low = df06_E71_3$low/df06_E71_3$n
  df06_E71_3$estProp06up = df06_E71_3$up/df06_E71_3$n
  
  df11_E71_3 <- cbind(E71_11[,], est_E71_11_3[,])
  df11_E71_3$estProp11 = df11_E71_3$mean/df11_E71_3$n # estimated seroprevalence
  df11_E71_3$estProp11low = df11_E71_3$low/df11_E71_3$n
  df11_E71_3$estProp11up = df11_E71_3$up/df11_E71_3$n
  
  df17_E71_3 <- cbind(E71_17[,], est_E71_17_3[,])
  df17_E71_3$estProp17 = df17_E71_3$mean/df17_E71_3$n # estimated seroprevalence
  df17_E71_3$estProp17low = df17_E71_3$low/df17_E71_3$n
  df17_E71_3$estProp17up = df17_E71_3$up/df17_E71_3$n
  
  colnames(df06_E71_3)
  colnames(df06_E71_3) <- c("Year","age","n","seropositive","propn","mean","low",
                          "up","estProp","estProplow","estPropup")
  colnames(df11_E71_3) <- c("Year","age","n","seropositive","propn","mean","low",
                            "up","estProp","estProplow","estPropup")
  colnames(df17_E71_3) <- c("Year","age","n","seropositive","propn","mean","low",
                          "up","estProp","estProplow","estPropup")
  df_fitE71_3 <- bind_rows(df06_E71_3, df11_E71_3, df17_E71_3)
  
  E71_figM3 <- df_fitE71_3 %>%
    ggplot() +
    geom_ribbon(aes(ymin = estProplow, ymax = estPropup, x = age), fill = "#1B9E77", alpha = 0.5) +
    geom_line(aes(x = age, y = estProp), color = "#1B9E77", linewidth = 0.8) +
    geom_point(aes(x = age, y = propn), shape = 1, size = 0.2, color = 'black') + 
    theme_bw() +
    facet_wrap(~Year) +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 9),
          plot.title = element_text(face = 'bold', size = 14),
          axis.title = element_text(size = 11),
          legend.position = 'none',
          strip.text.x = element_text(size = 11),
          strip.background = element_rect(fill = "gray90")) + 
    scale_x_continuous(breaks = seq(from = 0, to = 80, by = 20)) + 
    scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2), 
                       labels = scales::percent_format(accuracy = 1)) + 
    labs(y = 'Seroprevalence', x = '', title = '(A)')
  
  
  fig <- E71_figM3 / CA6_figM3 
  ggsave(filename = 'results/figures/FigS12_model3-fit.png', fig, height = 150, width = 183, units = 'mm', dpi = 300)
  
}

plot_fig_S12()
