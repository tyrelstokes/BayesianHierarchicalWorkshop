
data {
  int<lower=0> n_train;// The number of data points in training data (2014-15)
  int<lower=0> n_test;// The number of data points in prediction set (2015-16)
  
  int y[n_train] ;// The number of made free throws in training data (2014-15)
  int y_test[n_test];// The number of free throws made in test (2015-16)
  
  int<lower=0> Attempts[n_train]; //The number of attempted free throws in training (2014-15)
  int<lower=0> Attempts_test[n_test];// number of attempted in test (2015-16)

  
  real<lower=0> pooled_std_prior;// The variance for between variance prior

}

parameters {
  
real mu;// demeaned individual player mean, mu_i = mu + mu_0i*tau
 
}
transformed parameters{
  
 real theta;
 

 theta = 1.0/(1.0+exp(-(mu)));
 
}

model {
  y ~ binomial(Attempts,theta);// likelihood
  
  mu ~ normal(0.0,pooled_std_prior);//standardized player level prior
  
}


