process_eva71_fit_summary <- function(filename) {
  fit_E71 <- read.csv(filename, header = TRUE, sep = ',')
  rownames(fit_E71) <- fit_E71[,1]
  fit_E71 <- fit_E71[,-1]
  est_E71_06 <- data.frame(mean = rep(NA, times = 80), low = rep(NA, times = 80), 
                           up = rep(NA, times = 80))
  est_E71_11 <- data.frame(mean = rep(NA, times = 80), low = rep(NA, times = 80), 
                           up = rep(NA, times = 80))
  est_E71_17 <- data.frame(mean = rep(NA, times = 80), low = rep(NA, times = 80), 
                           up = rep(NA, times = 80))
  
  est_E71_06$mean = fit_E71$mean[grep("z_sim_2006",rownames(fit_E71))]
  est_E71_06$low  = fit_E71$X2.5.[grep("z_sim_2006",rownames(fit_E71))]
  est_E71_06$up   = fit_E71$X97.5.[grep("z_sim_2006",rownames(fit_E71))]
  
  est_E71_11$mean = fit_E71$mean[grep("z_sim_2011",rownames(fit_E71))]
  est_E71_11$low  = fit_E71$X2.5.[grep("z_sim_2011",rownames(fit_E71))]
  est_E71_11$up   = fit_E71$X97.5.[grep("z_sim_2011",rownames(fit_E71))]
  
  est_E71_17$mean = fit_E71$mean[grep("z_sim_2017",rownames(fit_E71))]
  est_E71_17$low  = fit_E71$X2.5.[grep("z_sim_2017",rownames(fit_E71))]
  est_E71_17$up   = fit_E71$X97.5.[grep("z_sim_2017",rownames(fit_E71))]
  
  output <- list(est_E71_06, est_E71_11, est_E71_17)
  return(output)
}
