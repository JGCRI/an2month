data {
  int Nyear;
  simplex[12] monfrac_obs[Nyear];
}
parameters {
  vector<lower=0>[12] alpha;
}
model {
  for (year in 1:Nyear)
    monfrac_obs[year] ~ dirichlet(alpha);
}
generated quantities {
  simplex[12] monfrac;
  monfrac = dirichlet_rng(alpha);
}

