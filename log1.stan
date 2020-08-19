/*Stan file for ride sharing data*/

data {
  int<lower=1> D;
  int<lower=0> N;
  int<lower=1> N_forecast;   // Forecast Index
  int<lower=1> L;
  int<lower=0,upper=1> y[N];
  int<lower=0,upper=1> y_new[N_forecast];
  int<lower=1,upper=L> ll[N];
  int<lower=1,upper=L> ll_new[N_forecast];
  row_vector[D] X[N];
  row_vector[D] X_new[N_forecast];
}


parameters {
  real mu[D];
  real<lower=0> sigma[D];
  vector[D] beta[L];
}

model {
  for (d in 1:D) {
    mu[d] ~ normal(0, 10);
    sigma[d] ~ cauchy(0, 2.5);
    for (l in 1:L)
      beta[l,d] ~ normal(mu[d], sigma[d]);
  }
  for (n in 1:N)
    y[n] ~ bernoulli_logit(X[n] * beta[ll[n]]);
}

generated quantities {
  vector[N_forecast] log_lik;
  for(n in 1:N_forecast) {
    // y_new[n] ~ bernoulli_logit(X_new[n] * beta[ll_new[n]]);
    log_lik[n] = bernoulli_logit_lpmf(y_new[n] | X_new[n] * beta[ll_new[n]]);
  }
}