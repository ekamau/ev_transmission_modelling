create_stan_data_timeFOI_EVA71 <- function(output, rho=NULL){
  df_2006 = as.data.frame(output[1])
  df_2011 = as.data.frame(output[2])
  df_2017 = as.data.frame(output[3])
  yr3 = 2017; yr2 = 2011; yr1 = 2006
  if(is.null(rho)){
    E71_dat <- list(N = 80, yr3 = yr3, yr2 = yr2, yr1 = yr1, 
                    n_2006 = df_2006$n, z_2006 = df_2006$seropositive,
                    n_2011 = df_2011$n, z_2011 = df_2011$seropositive, 
                    n_2017 = df_2017$n, z_2017 = df_2017$seropositive)
  }
  else{
    E71_dat <- list(N = 80, yr3 = yr3, yr2 = yr2, yr1 = yr1, 
                    n_2006 = df_2006$n, z_2006 = df_2006$seropositive,
                    n_2011 = df_2011$n, z_2011 = df_2011$seropositive, 
                    n_2017 = df_2017$n, z_2017 = df_2017$seropositive,
                    rho = rho)
  }
  
  E71_dat
}
