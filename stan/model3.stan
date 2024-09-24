// time-varying random-walk FOI:

data{
  int<lower=0> N;
  int<lower=0> yr3; int<lower=0> yr2; int<lower=0> yr1;
  int n_2006[N]; int z_2006[N];
  int n_2011[N]; int z_2011[N];
  int n_2017[N]; int z_2017[N];
  int max_yr_cst;

}

// The parameters to be estimated by the model. 
parameters {
  real<lower=0> lambda[yr3 - max_yr_cst];
  real<lower=0> lambda_cst;
  real<lower=0> sigma;
  
}

transformed parameters{
  vector[N] lambda_sum_2006;
  vector[N] lambda_sum_2011;
  vector[N] lambda_sum_2017;
  vector[N] lambda_exp_2006;
  vector[N] lambda_exp_2011;
  vector[N] lambda_exp_2017; 
    
  // 2017 optimised
  for(i in 1:N){
    if(i < (yr3 - max_yr_cst + 1)){
      lambda_sum_2017[i] = lambda[i];
      if(i > 1){
        for(j in 1:(i-1)){
          lambda_sum_2017[i] = lambda_sum_2017[i] + lambda[i-j]; 
        }
      }  
    }else{
      lambda_sum_2017[i] = (i - yr3 + max_yr_cst) * lambda_cst;
      for(j in (i - yr3 + max_yr_cst):(i-1)){
        lambda_sum_2017[i] = lambda_sum_2017[i] + lambda[i-j]; 
      }
    }
    lambda_exp_2017[i] = 1 - exp(-lambda_sum_2017[i]);
  }

  // 2011 optimised
  for(i in 1:N){
    if((i + (yr3 - yr2)) < (yr3 - max_yr_cst + 1)){
      lambda_sum_2011[i] = lambda[i+(yr3-yr2)]; 
      if(i > 1){
        for(j in 1:(i-1)){
          lambda_sum_2011[i] = lambda_sum_2011[i] + lambda[i-j+(yr3-yr2)];
        }
      } 
    }else{
      lambda_sum_2011[i] = (i - yr2 + max_yr_cst) * lambda_cst;
      for(j in (i - yr2 + max_yr_cst):(i-1)){
        lambda_sum_2011[i] = lambda_sum_2011[i] + lambda[i-j+(yr3-yr2)];
      }
    }
    lambda_exp_2011[i] = 1 - exp(-lambda_sum_2011[i]);
  }
  
  // 2006 optimised
  for(i in 1:N){
    if((i + (yr3 - yr1)) < (yr3 - max_yr_cst + 1)){
      lambda_sum_2006[i] = lambda[i+(yr3-yr1)];  
      if(i > 1){
        for(j in 1:(i-1)){
          lambda_sum_2006[i] = lambda_sum_2006[i] + lambda[i-j+(yr3-yr1)];
        }
      }
    }else{
      lambda_sum_2006[i] = (i - yr1 + max_yr_cst) * lambda_cst;
      for(j in (i - yr1 + max_yr_cst):(i-1)){
        lambda_sum_2006[i] = lambda_sum_2006[i] + lambda[i-j+(yr3-yr1)];
      }
    }
    lambda_exp_2006[i] = 1 - exp(-lambda_sum_2006[i]);
  }
  
}

// The model to be estimated. 
model{
  sigma ~ exponential(1);
  lambda[1] ~ exponential(1);
  lambda_cst ~ exponential(1);
  
  for(i in 2:(yr3 - max_yr_cst))
    lambda[i] ~ normal(lambda[(i-1)], sigma);
  
  for(i in 1:N)
    z_2006[i] ~ binomial(n_2006[i], lambda_exp_2006[i]);
  
  for(i in 1:N)
    z_2011[i] ~ binomial(n_2011[i], lambda_exp_2011[i]);
  
  for(i in 1:N)
    z_2017[i] ~ binomial(n_2017[i], lambda_exp_2017[i]);
  
}

generated quantities{
  int z_sim_2006[N];
  int z_sim_2011[N];
  int z_sim_2017[N];
  vector[3*N] log_likelihood;
  
  for(i in 1:N)
    z_sim_2006[i] = binomial_rng(n_2006[i], lambda_exp_2006[i]);
    
  for(i in 1:N)
    z_sim_2011[i] = binomial_rng(n_2011[i], lambda_exp_2011[i]);
  
  for(i in 1:N)
    z_sim_2017[i] = binomial_rng(n_2017[i], lambda_exp_2017[i]);
  
  for(i in 1:N)
    log_likelihood[i] = binomial_lpmf(z_2006[i] | n_2006[i], lambda_exp_2006[i]); 
  
  for(i in 1:N)
    log_likelihood[N+i] = binomial_lpmf(z_2011[i] | n_2011[i], lambda_exp_2011[i]); 
  
  for(i in 1:N)
    log_likelihood[2*N+i] = binomial_lpmf(z_2017[i] | n_2017[i], lambda_exp_2017[i]); 
  
}

