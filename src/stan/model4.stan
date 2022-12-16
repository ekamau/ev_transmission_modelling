// time-varying random-walk FOI and sensitivity analysis of rho!
// http://htmlpreview.github.io/?https://github.com/nathoze/Rsero/blob/master/vignettes/models.html

data{
  int<lower=0> N;
  int<lower=0> yr3; int<lower=0> yr2; int<lower=0> yr1;
  int n_2006[N]; int z_2006[N];
  int n_2011[N]; int z_2011[N];
  int n_2017[N]; int z_2017[N];
  real<lower=0> rho;
}

// The parameters accepted by the model. 
parameters{
  real<lower=0> sigma;
  real<lower=0, upper=1> lambda[N+(yr3-yr1)];
  
}

transformed parameters{
  vector[N] lambda_sum_2006;
  vector[N] lambda_sum_2011;
  vector[N] lambda_sum_2017;
  vector[N] lambda_exp_2006;
  vector[N] lambda_exp_2011;
  vector[N] lambda_exp_2017; 
  
  for(i in 1:N){
    lambda_sum_2017[i] = exp(-(lambda[i] + rho)) + (rho/(lambda[i]+rho)) * (1-exp(-(lambda[i]+rho)));
    if(i > 1){
      lambda_sum_2017[i] = lambda_sum_2017[i] * lambda_sum_2017[(i-1)];
    }
    lambda_exp_2017[i] = 1 - lambda_sum_2017[i];
  }
  
  for(i in 1:N){
    lambda_sum_2011[i] = exp(-(lambda[(i+(yr3-yr2))] + rho)) + (rho/(lambda[(i+(yr3-yr2))]+rho)) * (1-exp(-(lambda[(i+(yr3-yr2))]+rho)));
    if(i > 1){
      lambda_sum_2011[i] = lambda_sum_2011[i] * lambda_sum_2011[(i-1)];
    }
    lambda_exp_2011[i] = 1 - lambda_sum_2011[i];
  }
  
  for(i in 1:N){
    lambda_sum_2006[i] = exp(-(lambda[(i+(yr3-yr1))] + rho)) + (rho/(lambda[(i+(yr3-yr1))]+rho)) * (1-exp(-(lambda[(i+(yr3-yr1))]+rho)));
    if(i > 1){
      lambda_sum_2006[i] = lambda_sum_2006[i] * lambda_sum_2006[(i-1)];
    }
    lambda_exp_2006[i] = 1 - lambda_sum_2006[i];
  }
  
}

// The model to be estimated. 
model{
  sigma ~ exponential(1);
  lambda[1] ~ exponential(1);

  for(i in 2:(N+(yr3-yr1)))
    lambda[i] ~ normal(lambda[i-1], sigma);
  
  for (i in 1:N)
    z_2006[i] ~ binomial(n_2006[i], lambda_exp_2006[i]);

  for (i in 1:N)
    z_2011[i] ~ binomial(n_2011[i], lambda_exp_2011[i]);

  for (i in 1:N)
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


