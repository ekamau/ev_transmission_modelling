create_stan_data_constFOI_CVA6 <- function(output){
  df_2006 = as.data.frame(output[1])
  df_2011 = as.data.frame(output[2])
  df_2017 = as.data.frame(output[3])
  CA6_dat <- list(N = 80, n_2006 = df_2006$n, z_2006 = df_2006$seropositive, 
                  n_2011 = df_2011$n, z_2011 = df_2011$seropositive, 
                  n_2017 = df_2017$n, z_2017 = df_2017$seropositive)
  
  CA6_dat
}
