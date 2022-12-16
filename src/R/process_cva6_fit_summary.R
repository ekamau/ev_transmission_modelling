process_cva6_fit_summary <- function(filename) {
  fit_CA6 <- read.csv(filename, header = TRUE, sep = ',')
  rownames(fit_CA6) <- fit_CA6[,1]
  fit_CA6 <- fit_CA6[,-1]
  est_CA6_06 <- data.frame(mean = rep(NA, times = 80), low = rep(NA, times = 80), 
                           up = rep(NA, times = 80))
  est_CA6_11 <- data.frame(mean = rep(NA, times = 80), low = rep(NA, times = 80), 
                           up = rep(NA, times = 80))
  est_CA6_17 <- data.frame(mean = rep(NA, times = 80), low = rep(NA, times = 80), 
                           up = rep(NA, times = 80))
  
  est_CA6_06$mean = fit_CA6$mean[grep("z_sim_2006", rownames(fit_CA6))]
  est_CA6_06$low  = fit_CA6$X2.5.[grep("z_sim_2006", rownames(fit_CA6))]
  est_CA6_06$up   = fit_CA6$X97.5.[grep("z_sim_2006", rownames(fit_CA6))]
  
  est_CA6_11$mean = fit_CA6$mean[grep("z_sim_2011", rownames(fit_CA6))]
  est_CA6_11$low  = fit_CA6$X2.5.[grep("z_sim_2011", rownames(fit_CA6))]
  est_CA6_11$up   = fit_CA6$X97.5.[grep("z_sim_2011", rownames(fit_CA6))]
  
  est_CA6_17$mean = fit_CA6$mean[grep("z_sim_2017", rownames(fit_CA6))]
  est_CA6_17$low  = fit_CA6$X2.5.[grep("z_sim_2017", rownames(fit_CA6))]
  est_CA6_17$up   = fit_CA6$X97.5.[grep("z_sim_2017", rownames(fit_CA6))]
  
  output <- list(est_CA6_06, est_CA6_11, est_CA6_17)
  return(output)
}
