// gp_model.stan
// GP model for log(bacteria) with Student-t likelihood.
//
// Model:
//   Y[i] ~ Student-t(nu, mu[i], sigma)
//   mu[i] = Intercept + GP(t_s[i]) + b_rain * rain_effect(tau)[i]
//
// GP: Hilbert Space approximation (Riutort-Mayol et al. 2022)
//   K = 10 basis functions, boundary factor c = 5/4.
//   Squared-exponential kernel, parameters: sdgp (marginal SD), lscale (length scale).
//
// Rain effect: exponential decay kernel with ESTIMATED decay half-life tau
//   rain_effect[i] = Σ_{k=0}^{W-1} rain_lag[i, k+1] * exp(-k / tau)
//                    ─────────────────────────────────────────────────
//                          Σ_{k=0}^{W-1} exp(-k / tau)
//
// Predictions for every grid row (observed + unobserved) are generated
// in the generated quantities block so that posterior_predict is not needed.

data {
  int<lower=1> N;               // total rows: swimming-season days (obs + unobs)
  int<lower=1> N_obs;           // number of rows with bacteria measurements
  int          obs_idx[N_obs];  // 1-based row indices of observed rows
  vector[N_obs] Y;              // log(bacteria) for observed rows
  vector[N]    t_s;             // z-scored time index, all rows
  matrix[N, 21] rain_lag;       // rain (mm) for lags 0..20 days, all rows
                                //   column k  =  lag k-1  (k=1 → same day)
}

transformed data {
  int K  = 10;     // number of HSGP basis functions
  real c = 1.25;   // boundary factor (c = 5/4)
  int  W = 21;     // rain lookback window (days)

  // Hilbert-space boundary
  real L = c * max(fabs(t_s));

  // Basis matrix PHI[n, j] = phi_j(t_s[n])
  matrix[N, K] PHI;
  for (n in 1:N)
    for (j in 1:K)
      PHI[n, j] = sin(j * pi() * (t_s[n] + L) / (2.0 * L)) / sqrt(L);

  // Laplacian eigenvalues lambda[j] = (j*pi / (2L))^2
  vector[K] lambda;
  for (j in 1:K)
    lambda[j] = square(j * pi() / (2.0 * L));
}

parameters {
  real                Intercept;
  vector[K]           beta_gp;          // HSGP basis coefficients ~ N(0,1)
  real<lower=0>       lscale;           // GP length scale
  real<lower=0>       sdgp;             // GP marginal SD
  real<lower=0>       tau;              // rain decay half-life (days)
  real                b_rain;           // rain effect coefficient
  real<lower=0>       sigma;            // residual SD (on log scale)
  real<lower=2>       nu;               // Student-t degrees of freedom
  real<lower=0>	bandwidth;
}

transformed parameters {
  // Spectral density weights for the SE kernel:
  //   w_j = sqrt( S(sqrt(lambda_j)) )
  //   S(w; sdgp, lscale) = sdgp^2 * lscale * sqrt(2*pi) * exp(-lscale^2*w^2/2)
  vector[K] spd;
  for (j in 1:K)
    spd[j] = sdgp * sqrt(lscale * sqrt(2.0 * pi()))
             * exp(-0.25 * square(lscale) * lambda[j]);

  // Linear predictor for all N rows
  vector[N] mu;
  {
    // GP contribution
    vector[N] gp_eff = PHI * (spd .* beta_gp);

    // Rain effect with estimated tau
    for (n in 1:N) {
      real w_sum = 0.0;
      real re    = 0.0;
      for (k in 1:W) {
        real w  = exp(-(k - bandwidth) / tau);
        re     += rain_lag[n, k] * w;
        w_sum  += w;
      }
      mu[n] = Intercept + gp_eff[n] + b_rain * (re / w_sum);
    }
  }
}

model {
  // Priors
  Intercept ~ normal(0, 2);
  beta_gp   ~ normal(0, 1);
  lscale    ~ normal(0, 1);
  sdgp      ~ normal(0, 1);
  tau       ~ gamma(1, 1);    // mean = 10 days, allows range ~1–30
  b_rain    ~ normal(0, 1);
  sigma     ~ exponential(0.5);
  nu        ~ gamma(2, 0.1);
  bandwidth ~ gamma(1,1;
  // Likelihood (observed rows only)
  Y ~ student_t(nu, mu[obs_idx], sigma);
}

generated quantities {
  // Posterior predictive samples for every grid row (on log scale)
  vector[N] y_pred;
  for (n in 1:N)
    y_pred[n] = student_t_rng(nu, mu[n], sigma);
}
