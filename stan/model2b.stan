// constant FOI with sero-reversion, :

data{
  int<lower=0> N;
  int n_2006[N]; int z_2006[N];
  int n_2011[N]; int z_2011[N];
  int n_2017[N]; int z_2017[N];

}

// The parameters to be estimated by the model. 
parameters {
  real<lower=0> lambda;
  real<lower=0> rho;
  
}

transformed parameters{
  vector[N] lambda_exp; 
  
  for(i in 1:N){
    lambda_exp[i] = (1 - exp(-i * (lambda + rho))) * (lambda / (lambda + rho));
  }
  
}

// The model to be estimated. 
model{
  lambda ~ exponential(10);
  rho ~ exponential(20);
  
  for(i in 1:N){
    if(n_2006[i] > 0)
      z_2006[i] ~ binomial(n_2006[i], lambda_exp[i]);
    if(n_2011[i] > 0)
      z_2011[i] ~ binomial(n_2011[i], lambda_exp[i]);
    if(n_2017[i] > 0)
      z_2017[i] ~ binomial(n_2017[i], lambda_exp[i]);
  }  
  
}

generated quantities{
  int z_sim_2006[N];
  int z_sim_2011[N];
  int z_sim_2017[N];
  vector[3*N] log_likelihood;
  
  // posterior predictive checks:
  for(i in 1:N)
      z_sim_2006[i] = binomial_rng(n_2006[i], lambda_exp[i]);
    
  for(i in 1:N)
      z_sim_2011[i] = binomial_rng(n_2011[i], lambda_exp[i]);
  
  for(i in 1:N)
      z_sim_2017[i] = binomial_rng(n_2017[i], lambda_exp[i]);
  
  // pointwise log likelihoods:
  for(i in 1:N)
    log_likelihood[i] = binomial_lpmf(z_2006[i] | n_2006[i], lambda_exp[i]); 
  
  for(i in 1:N)
    log_likelihood[N+i] = binomial_lpmf(z_2011[i] | n_2011[i], lambda_exp[i]); 
  
  for(i in 1:N)
    log_likelihood[2*N+i] = binomial_lpmf(z_2017[i] | n_2017[i], lambda_exp[i]); 
  
}
