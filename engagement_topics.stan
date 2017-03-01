data {
  int<lower=1> nposts;
  int<lower=1> ntopics;
  int<lower=1> npages;
  real engagement[nposts];
  int<lower=1, upper=npages> p_id[nposts];
  int<lower=0, upper=1> t_id[nposts, ntopics];
}

parameters {
  matrix[npages, ntopics] t_coef;
  vector<lower=0>[npages] e_sigma;
  vector<lower=0>[ntopics] t_sigma;
  //vector<lower=0>[ntopics] t_mean;
}

model {
  for (i in 1:ntopics)
    t_coef[, i] ~ normal(0, t_sigma[i]);
  //t_mean ~ normal(0,10);
  t_sigma ~ cauchy(0,5);
  e_sigma ~ cauchy(0,5);
  engagement ~ normal(rows_dot_product(t_coef[p_id], to_matrix(t_id)), e_sigma[p_id]);
}