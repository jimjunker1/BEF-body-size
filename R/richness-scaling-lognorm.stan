data {
  int<lower=1> N;               // individuals
  int<lower=1> S;               // species
  int<lower=1,upper=S> species[N]; // species id for each individual
  vector[N] logM;               // individual log-mass
}

parameters {
  real mu_S;                    // mean log-species size
  real<lower=0> sigma_S;        // sd log-species size

  real lambda;                  // abundance scaling
  real<lower=0> alpha_A;        // abundance intercept
  real<lower=0> sigma_M;        // within-species sd

  vector[S] logM_species;       // latent species log-mass
}

model {
  // Priors
  mu_S ~ normal(0,5);
  sigma_S ~ exponential(1);
  lambda ~ normal(-1.75,1);
  alpha_A ~ exponential(1);
  sigma_M ~ exponential(1);

  // Species log-mass
  logM_species ~ normal(mu_S, sigma_S);

  // Abundance-size scaling
  {
    int A[S];
    for (k in 1:S) A[k] = 0;
    for (i in 1:N) A[species[i]] += 1;
    for (k in 1:S)
      A[k] ~ poisson(alpha_A * exp(lambda * logM_species[k]));
  }

  // Individual-level body masses
  for (i in 1:N)
    logM[i] ~ normal(logM_species[species[i]], sigma_M);
}