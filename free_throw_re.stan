
data {
  int<lower=0> n_train;// The number of data points in training data (2014-15)
  int<lower=0> n_test;// The number of data points in prediction set (2015-16)
  
  int y[n_train] ;// The number of made free throws in training data (2014-15)
  int y_test[n_test];// The number of free throws made in test (2015-16)
  
  int<lower=0> Attempts[n_train]; //The number of attempted free throws in training (2014-15)
  int<lower=0> Attempts_test[n_test];// number of attempted in test (2015-16)
  
  int<lower=0> player_training_int[n_train];
  int<lower =0> player_test_int[n_test];
  
  
  int in_training[n_test];// Binary variable 1 if player is in training, 0 otherwise
  
  int np; // Number of players in training data (2014-15)
  
  real tau_std_prior;// The variance for between variance prior
  real mu_std_prior;// The variance for the group mean prior
}

parameters {
  real mu;// group mean
  vector[np] mu_0i;// demeaned individual player mean, mu_i = mu + mu_0i*tau
  real<lower=0> tau;// Between player mean
}
transformed parameters{
  
 vector[np] theta;
 
 for(i in 1:np){
 theta[i] = 1.0/(1.0+exp(-(mu + mu_0i[i]*tau)));
 }
}

model {
  y ~ binomial(Attempts,theta);// likelihood
  
  mu_0i ~ normal(0.0,1.0);//standardized player level prior
  
  mu ~ normal(0.0,mu_std_prior); // group mean prior
  tau ~ normal(0.0,tau_std_prior);// between player std prior
}
generated quantities{
  
  vector[n_test] predicted_makes;
  vector[n_test] predicted_probability;
  vector[n_test] error;
  vector[n_train] log_lik;
  real avg_prob = 1/(1+exp(-mu));
  real mu_draw;
  
  for(i in 1:n_test){
    if(in_training[i]==1){
      predicted_makes[i] = binomial_rng(Attempts_test[i],theta[player_test_int[i]]);
      predicted_probability[i] = theta[player_test_int[i]];
      
    }else{
      mu_draw = normal_rng(mu,tau);
      predicted_probability[i] = 1.0/(1.0+exp(-mu_draw));
      predicted_makes[i] = binomial_rng(Attempts_test[i],predicted_probability[i]);
    }
    error[i] = y_test[i] - predicted_makes[i];
    
  }
  
  for(j in 1:n_train){
    log_lik[j] = binomial_lpmf(y[j]|Attempts[j],theta[j]);
  }
}

