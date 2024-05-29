data {
  int<lower=0> N;
  vector[N] y;
  vector[N] t;
}
parameters {
  real b0;
  real<upper=0> b1;
  real<lower=0> sigma;
}
model {
  b0 ~ normal(2.7, 0.06);
  b1 ~ normal(0.2, 0.03);
  sigma ~ normal(0, 0.03)
  log(y) ~ normal(b0 + b1 * t, sigma);
}
generated quantities {
  vector[N] mu;
  vector[N] log_y_rep;
  vector[N] log_likelihood;
  
  mu = b0 + b1 * t;

  for (i in 1:N) {
    log_y_rep[i] = normal_rng(mu[i], sigma);
    log_likelihood[i] = normal_lpdf(t[i] | mu[i], sigma);
  }
}

