

set.seed(123)

library(tidyverse)

#### 1. Simulate data from a Poisson Gamma model ####

# true parameters
lambda_true <- 4      # expected number of events per period
alpha_true  <- 2      # gamma shape
beta_true   <- 500    # gamma scale (mean size = alpha * beta)

T_periods <- 100

# simulate counts
N_t <- rpois(T_periods, lambda_true)

# simulate sizes for all events
sizes <- map2(
  N_t,
  seq_along(N_t),
  \(n, t) if (n == 0) NULL else tibble(
    period = t,
    size   = rgamma(n, shape = alpha_true, scale = beta_true)
  )
) |> list_rbind()

# summary of data
mean(N_t)             # should be near lambda_true
mean(sizes$size)      # should be near alpha_true * beta_true

#### 2. Log posterior function ####

# independent priors
# lambda ~ Gamma(2, 1) on rate scale
# alpha  ~ Gamma(2, 1)
# beta   ~ Gamma(2, 1)   (for simplicity)

log_post <- function(par, counts, sizes) {
  # par is a vector of paramerters on the log scale
  log_lambda <- par[1]; log_alpha  <- par[2]; log_beta   <- par[3]

  # convert back to original scale
  lambda <- exp(log_lambda); alpha  <- exp(log_alpha); beta   <- exp(log_beta)

  # compute the likelihood for counts: Poisson
  ll_counts <- sum(dpois(counts, lambda, log = TRUE))

  # compute the likelihood for sizes: Gamma(shape = alpha, scale = beta)
  ll_sizes <- sum(dgamma(sizes, shape = alpha, scale = beta, log = TRUE))

  # log priors (Gamma with shape = 2, rate = 1)
  # density is proportional to x^(shape-1) exp(-rate x)
  # work on original scale then add log Jacobian from log transform
  lp_lambda <- dgamma(lambda, shape = 2, rate = 1, log = TRUE) + log_lambda
  lp_alpha  <- dgamma(alpha,  shape = 2, rate = 1, log = TRUE) + log_alpha
  lp_beta   <- dgamma(beta,   shape = 2, rate = 1, log = TRUE) + log_beta

  # return log likelihood plus log priors
  ll_counts + ll_sizes + lp_lambda + lp_alpha + lp_beta
}

#### 3. sampler ####

mh_sampler <- function(
    counts,
    sizes,
    n_iter   = 20000,
    start    = c(log(3), log(1.5), log(400)),
    proposal_sd = c(0.05, 0.05, 0.05)
) {
  n_par <- length(start)
  chain <- matrix(NA_real_, nrow = n_iter, ncol = n_par)
  colnames(chain) <- c("log_lambda", "log_alpha", "log_beta")

  log_theta_curr <- start
  log_post_curr  <- log_post(log_theta_curr, counts, sizes)

  accept <- 0L

  for (iter in 1:n_iter) {
    # random walk proposal on log scale
    log_theta_prop <- rnorm(n_par, mean = log_theta_curr, sd = proposal_sd)

    log_post_prop <- log_post(log_theta_prop, counts, sizes)

    log_acc_ratio <- log_post_prop - log_post_curr
    if (log(runif(1)) < log_acc_ratio) {
      # accept
      log_theta_curr <- log_theta_prop
      log_post_curr  <- log_post_prop
      accept <- accept + 1L
    }

    chain[iter, ] <- log_theta_curr
  }

  list(
    chain_log = chain,
    accept_rate = accept / n_iter,
    chain = cbind(
      lambda = exp(chain[, 1]),
      alpha  = exp(chain[, 2]),
      beta   = exp(chain[, 3])
    )
  )
}

#### 4. Run the sampler ####

mh_out <- mh_sampler(
  counts = N_t,
  sizes  = sizes$size,
  n_iter = 300000
)

mh_out$accept_rate

# drop burn in
burn  <- 5000
draws <- as_tibble(mh_out$chain[-seq_len(burn), ])

summ <- draws |>
  summarise(
    lambda_mean = mean(lambda),
    alpha_mean  = mean(alpha),
    beta_mean   = mean(beta),
    lambda_q    = quantile(lambda, c(0.05, 0.95)),
    alpha_q     = quantile(alpha,  c(0.05, 0.95)),
    beta_q      = quantile(beta,   c(0.05, 0.95))
  )

summ

#### 5. Quick trace plots ####

par(mfrow = c(3, 1))
plot(draws$lambda, type = "l", main = "lambda", ylab = "", xlab = "iter")
abline(h = lambda_true, col = "red")
plot(draws$alpha,  type = "l", main = "alpha",  ylab = "", xlab = "iter")
abline(h = alpha_true, col = "red")
plot(draws$beta,   type = "l", main = "beta",   ylab = "", xlab = "iter")
abline(h = beta_true, col = "red")
par(mfrow = c(1, 1))