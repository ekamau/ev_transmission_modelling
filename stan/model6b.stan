// constant FOI with age-dependency and seroreversion:

functions{
  real foi_calculator(real beta, int N){
    real foi_counter = 0.0;
    for(j in 0:(N-1)){
      foi_counter += exp(-beta * j);
    }
    return(foi_counter);
  }
  
  real foi_calculator2(real beta, real lambda, real rho, int N){
    real foi_counter = 0.0;
    for(i in 0:(N-1)){
      real foi_temp = 0.0;
      for(j in i:(N-1)){
        foi_temp += exp(-beta * j) * lambda;
      }
      foi_temp += rho * (N-1-i);
      foi_counter += exp(-foi_temp);
    }
    foi_counter *= rho;
    return(foi_counter);
  }
  
  real analytic_integrator(real lambda, real beta, real rho, int a, int a_primed){
    real first = lambda/beta * (exp(-beta * a_primed) - exp(-beta * a));
    real second = rho*(a - a_primed);
    return(first + second);
  }
  
  real analytic_integrator_full(real lambda, real beta, real rho, int N){
    real foi_counter = 0.0;
    for(i in 0:(N-1)){
      foi_counter += exp(-analytic_integrator(lambda, beta, rho, N, i));
    }
    foi_counter *= rho;
    return(foi_counter);
  }
}
  
data{
    int<lower=0> N;
    int n_2006[N]; int z_2006[N];
    int n_2011[N]; int z_2011[N];
    int n_2017[N]; int z_2017[N];
    
  }

// The parameters to be estimated by the model. 
parameters {
  real<lower=0> lambda; 
  real<lower=0> beta;
  real<lower=0> rho;
  
}

transformed parameters{
  vector[N] lambda_exp_2006;
  vector[N] lambda_exp_2011;
  vector[N] lambda_exp_2017; 
  real lambda_first;
  
  for(i in 1:N){
    real lambda_second = analytic_integrator_full(lambda, beta, rho, i);
    lambda_first = lambda / beta * (1 - exp(-beta * i)) + rho * i;
    
    lambda_exp_2017[i] = 1 - exp(-lambda_first) - lambda_second;
    lambda_exp_2011[i] = 1 - exp(-lambda_first) - lambda_second;
    lambda_exp_2006[i] = 1 - exp(-lambda_first) - lambda_second;
  }
  
}

// The model to be estimated. 
model{
  lambda ~ exponential(10);
  rho ~ exponential(20);
  beta ~ exponential(20);
  
  for(i in 1:N){
    if(n_2006[i] > 0)
      z_2006[i] ~ binomial(n_2006[i], lambda_exp_2006[i]);
    if(n_2011[i] > 0)
      z_2011[i] ~ binomial(n_2011[i], lambda_exp_2011[i]);
    if(n_2017[i] > 0)
      z_2017[i] ~ binomial(n_2017[i], lambda_exp_2017[i]);
  }  
  
}

generated quantities{
  int z_sim_2006[N];
  int z_sim_2011[N];
  int z_sim_2017[N];
  vector[3*N] log_likelihood;
  
  // posterior predictive checks:
    for(i in 1:N){
      z_sim_2006[i] = binomial_rng(n_2006[i], lambda_exp_2006[i]);
      z_sim_2011[i] = binomial_rng(n_2011[i], lambda_exp_2011[i]);
      z_sim_2017[i] = binomial_rng(n_2017[i], lambda_exp_2017[i]);
    }
  
  // pointwise log likelihoods:
    for(i in 1:N){
      log_likelihood[i] = binomial_lpmf(z_2006[i] | n_2006[i], lambda_exp_2006[i]); 
      log_likelihood[N+i] = binomial_lpmf(z_2011[i] | n_2011[i], lambda_exp_2011[i]); 
      log_likelihood[2*N+i] = binomial_lpmf(z_2017[i] | n_2017[i], lambda_exp_2017[i]); 
    }
}

