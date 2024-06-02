data {
  int<lower=0> N;
  real y;
  real t;
}
parameters {
  real b0;
  real<upper=0> b1;
  real<lower=0> sigma;
  real<upper=5.83> x;
}
model {
  b0 ~ normal(2.69, 0.017);
  b1 ~ normal(-0.248, 0.018);
  sigma ~ normal(0, 0.007);
  -(x - 5.883) ~ gamma(5, 0.1);
  y ~ normal(b0 + b1 * (t - x), sigma);
}
// generated quantities {
//   vector[N] mu;
//   vector[N] dif_temp;
//   vector[N] log_likelihood;
//   
//   mu = b0 + b1 * t;
// 
//   for (i in 1:N) {
//     dif_temp[i] = normal_rng(mu[i], sigma);
//     log_likelihood[i] = normal_lpdf(t[i] | mu[i], sigma);
//   }
// }

