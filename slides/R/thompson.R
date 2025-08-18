# --------------------------------------------
# Thompson Sampling for Campaign Budgeting
# --------------------------------------------
set.seed(8740)

# True (unknown) CTRs for 3 campaigns
true_p <- c(A = 0.020, B = 0.050, C = 0.035); K <- length(true_p)

# Prior Beta(α, β) per campaign (uninformative)
alpha <- rep(1, K)
beta  <- rep(1, K)

# Simulation length: number of ad impressions to allocate sequentially
T <- 5000

# Storage
history <- data.frame(
  t = integer(T),
  chosen = character(T),
  click = integer(T),
  stringsAsFactors = FALSE
)

# Thompson sampling loop
for (t in 1:T) {
  # sample one theta from each campaign's posterior Beta
  thetas <- rbeta(K, shape1 = alpha, shape2 = beta)
  arm <- which.max(thetas)       # choose the arm with max sampled theta
  arm_name <- names(true_p)[arm]

  # simulate the outcome (click/no-click) from the true CTR
  y <- rbinom(1, size = 1, prob = true_p[arm])

  # update posterior for the chosen arm
  alpha[arm] <- alpha[arm] + y
  beta[arm]  <- beta[arm]  + (1 - y)

  # record
  history$t[t] <- t
  history$chosen[t] <- arm_name
  history$click[t] <- y
}

# Summaries
library(dplyr)

post_summary <- tibble::tibble(
  campaign = names(true_p),
  alpha = alpha,
  beta  = beta,
  posterior_mean = alpha / (alpha + beta),
  posterior_sd   = sqrt((alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))),
  true_ctr = as.numeric(true_p)
) %>%
  dplyr::arrange(desc(posterior_mean))

alloc_summary <- history %>%
  dplyr::count(chosen, name = "allocated_impressions") %>%
  dplyr::rename(campaign = chosen) %>%
  dplyr::right_join(tibble(campaign = names(true_p)), by = "campaign") %>%
  dplyr::mutate(allocated_impressions = tidyr::replace_na(allocated_impressions, 0L)) %>%
  dplyr::arrange(desc(allocated_impressions))

performance <- history %>%
  dplyr::summarize(total_clicks = sum(click),
            ctr_overall = mean(click),
            .groups = "drop")

per_campaign_perf <- history %>%
  dplyr::group_by(chosen) %>%
  dplyr::summarize(clicks = sum(click),
            impressions = n(),
            empirical_ctr = ifelse(impressions > 0, clicks / impressions, NA_real_),
            .groups = "drop") %>%
  dplyr::rename(campaign = chosen) %>%
  dplyr::arrange(desc(empirical_ctr))

list(
  allocation = alloc_summary,
  posterior  = post_summary,
  overall    = performance,
  per_campaign_empirical = per_campaign_perf
)