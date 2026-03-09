data {

  int<lower=1> N;              // individuals
  int<lower=1> S;              // species

  int<lower=1,upper=S> species[N];
  vector[N] logM;              // log body mass

  real M_min;
  real M_max;
}

parameters {

  real beta;                   // richness scaling
  real lambda;                 // abundance scaling

  real<lower=0> alpha_A;

  vector[S] logM_species;      // latent species sizes
  real<lower=0> sigma_M;
}

model {

  beta ~ normal(0,2);
  lambda ~ normal(-1.75,1);
  alpha_A ~ exponential(1);
  sigma_M ~ exponential(1);

  // species size distribution
  for (k in 1:S)
    target += log(beta+1)
              + beta*logM_species[k]
              - log(pow(M_max,beta+1)-pow(M_min,beta+1));

  // abundance-size relationship
  {
    int A[S];
    for (k in 1:S) A[k] = 0;
    for (i in 1:N) A[species[i]] += 1;

    for (k in 1:S)
      A[k] ~ poisson(alpha_A * exp(lambda * logM_species[k]));
  }

  // individual masses
  for (i in 1:N)
    logM[i] ~ normal(logM_species[species[i]], sigma_M);
}

