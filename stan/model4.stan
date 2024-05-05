// time-varying random-walk FOI and sensitivity analysis of rho!
// http://htmlpreview.github.io/?https://github.com/nathoze/Rsero/blob/master/vignettes/models.html

data{
  int<lower=0> N;
  int<lower=0> yr3; int<lower=0> yr2; int<lower=0> yr1;
  int n_2006[N]; int z_2006[N];
  int n_2011[N]; int z_2011[N];
  int n_2017[N]; int z_2017[N];
  int max_yr_cst; // assume max is 1995, as first cross-sectional is 2006
  
}

// The parameters accepted by the model. 
parameters{
  real<lower=0> lambda[yr3 - max_yr_cst];
  real<lower=0> lambda_cst;
  real<lower=0> rho;
  real<lower=0> sigma;

}

transformed parameters{
  vector[N] prob_pos_2006;
  vector[N] prob_pos_2011;
  vector[N] prob_pos_2017; 
  
  // 2017 optimised
  for(i in 1:N){
    if(i < (yr3 - max_yr_cst + 1)){
      prob_pos_2017[i] = (lambda[i]/(lambda[i] + rho)) * (1 - exp(-(lambda[i] + rho)));
      if(i > 1){
        for(j in 1:(i-1)){
          prob_pos_2017[i] = prob_pos_2017[i] * exp(-(lambda[i-j] + rho)) + (lambda[i-j]/(lambda[i-j] + rho)) * (1 - exp(-(lambda[i-j] + rho))); 
        }
      }  
    }else{
      prob_pos_2017[i] = (1 - exp(-(i - yr3 + max_yr_cst) * (lambda_cst + rho))) * (lambda_cst / (lambda_cst + rho));
      for(j in (i - yr3 + max_yr_cst):(i-1)){
        prob_pos_2017[i] = prob_pos_2017[i] * exp(-(lambda[i-j] + rho)) + (lambda[i-j]/(lambda[i-j] + rho)) * (1 - exp(-(lambda[i-j] + rho))); 
      }
    }
  }
  
  // 2011 optimised
  for(i in 1:N){
    if((i + (yr3 - yr2)) < (yr3 - max_yr_cst + 1)){
      prob_pos_2011[i] = (lambda[i+(yr3-yr2)]/(lambda[i+(yr3-yr2)] + rho)) * (1 - exp(-(lambda[i+(yr3-yr2)] + rho))); 
      if(i > 1){
        for(j in 1:(i-1)){
          prob_pos_2011[i] = prob_pos_2011[i] * exp(-(lambda[i-j+(yr3-yr2)] + rho)) + (lambda[i-j+(yr3-yr2)]/(lambda[i-j+(yr3-yr2)] + rho)) * (1 - exp(-(lambda[i-j+(yr3-yr2)] + rho)));
        }
      } 
    }else{
      prob_pos_2011[i] = (1 - exp(-(i - yr2 + max_yr_cst) * (lambda_cst + rho))) * (lambda_cst / (lambda_cst + rho));
      for(j in (i - yr2 + max_yr_cst):(i-1)){
        prob_pos_2011[i] = prob_pos_2011[i] * exp(-(lambda[i-j+(yr3-yr2)] + rho)) + (lambda[i-j+(yr3-yr2)]/(lambda[i-j+(yr3-yr2)] + rho)) * (1 - exp(-(lambda[i-j+(yr3-yr2)] + rho)));
      }
    }
  }

  // 2006 optimised
  for(i in 1:N){
    if((i + (yr3 - yr1)) < (yr3 - max_yr_cst + 1)){
      prob_pos_2006[i] = (lambda[i+(yr3-yr1)]/(lambda[i+(yr3-yr1)] + rho)) * (1 - exp(-(lambda[i+(yr3-yr1)] + rho)));  
      if(i > 1){
        for(j in 1:(i-1)){
          prob_pos_2006[i] = prob_pos_2006[i] * exp(-(lambda[i-j+(yr3-yr1)] + rho)) + (lambda[i-j+(yr3-yr1)]/(lambda[i-j+(yr3-yr1)] + rho)) * (1 - exp(-(lambda[i-j+(yr3-yr1)] + rho)));
        }
      }
    }else{
      prob_pos_2006[i] = (1 - exp(-(i - yr1 + max_yr_cst) * (lambda_cst + rho))) * (lambda_cst / (lambda_cst + rho));
      for(j in (i - yr1 + max_yr_cst):(i-1)){
        prob_pos_2006[i] = prob_pos_2006[i] * exp(-(lambda[i-j+(yr3-yr1)] + rho)) + (lambda[i-j+(yr3-yr1)]/(lambda[i-j+(yr3-yr1)] + rho)) * (1 - exp(-(lambda[i-j+(yr3-yr1)] + rho)));
      }
    }
  }

}

// The model to be estimated. 
model{
  lambda_cst ~ exponential(1);
  lambda[1] ~ exponential(1);
  rho ~ exponential(1);
  sigma ~ exponential(1);

  for(i in 2:(yr3 - max_yr_cst))
    lambda[i] ~ normal(lambda[i-1], sigma);
  
  for (i in 1:N)
    z_2006[i] ~ binomial(n_2006[i], prob_pos_2006[i]);

  for (i in 1:N)
    z_2011[i] ~ binomial(n_2011[i], prob_pos_2011[i]);

  for (i in 1:N)
    z_2017[i] ~ binomial(n_2017[i], prob_pos_2017[i]);
  
}

generated quantities{
  int z_sim_2006[N];
  int z_sim_2011[N];
  int z_sim_2017[N];
  vector[3*N] log_likelihood;
  
  for(i in 1:N)
    z_sim_2006[i] = binomial_rng(n_2006[i], prob_pos_2006[i]);
    
  for(i in 1:N)
    z_sim_2011[i] = binomial_rng(n_2011[i], prob_pos_2011[i]);
  
  for(i in 1:N)
    z_sim_2017[i] = binomial_rng(n_2017[i], prob_pos_2017[i]);
      
  for(i in 1:N)
    //if(n_2006[i] > 0)
      log_likelihood[i] = binomial_lpmf(z_2006[i] | n_2006[i], prob_pos_2006[i]); 
  
  for(i in 1:N)
    //if(n_2011[i] > 0)
      log_likelihood[N+i] = binomial_lpmf(z_2011[i] | n_2011[i], prob_pos_2011[i]); 
  
  for(i in 1:N)
    //if(n_2017[i] > 0)
      log_likelihood[2*N+i] = binomial_lpmf(z_2017[i] | n_2017[i], prob_pos_2017[i]); 
  
}

