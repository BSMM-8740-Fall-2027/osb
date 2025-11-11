

n <- 1500
ids <- tibble::tibble(id = 1:n,
              age = rnorm(n, 40, 10),
              inc = rlnorm(n, log(50), 0.4),
              site = sample(letters[1:5], n, TRUE))

# treatment assigned by covariates
ps_lin <- with(ids, -1 + 0.03*age + 0.6*(site %in% c("a","b")) + 0.015*(inc))
p_treat <- stats::plogis(ps_lin)
ids$treat <- rbinom(n, 1, p_treat)

# build long panel
df <- ids |>
  tidyr::crossing(time = 0:1) |>
  dplyr::mutate(
    post = as.integer(time == 1),
    # baseline potential outcomes
    mu = 5 + 0.06*age + 0.08*inc + 0.5*(site %in% c("a","b")) + rnorm(n()*2, 0, 1.5),
    # treatment effect appears only post
    tau = 2 + 0.03*age,
    y = mu + treat*post*tau
)

# ============================
library(did)
library(tidyverse)
set.seed(42)

N <- 3000
Tmax <- 6

# unit covariates
ids <- tibble(
  id  = 1:N,
  age = rnorm(N, 40, 9),
  inc = rlnorm(N, log(50), 0.5)
)

# assign adoption cohort per unit
# Inf means never treated
ids <- ids |>
  mutate(G = sample(c(2,3,4,5,Inf), N, replace = TRUE,
                    prob = c(.2, .25, .25, .2, .1)))

# build panel
panel <- ids |>
  crossing(t = 1:Tmax) |>
  mutate(
    treat = as.integer(t >= G),
    post  = treat,
    mu = 3 + 0.05*age + 0.07*inc + 0.2*t + 0.1*(t*(age - 40)/10),
    te = pmax(0, t - ifelse(is.finite(G), G, Inf) + 1),
    y  = mu + 1.5*treat + 0.5*te + rnorm(n(), 0, 1.2)
  )

# format for did

panel_did <- panel %>%
  dplyr::mutate(
    idname = as.integer(id),
    tname  = as.integer(t),            # time index
    # first replace Inf with 0, *then* convert to integer
    gname  = ifelse(is.infinite(G), 0, G),
    gname  = as.integer(gname)
  )

dplyr::count(panel_did, gname)
summary(panel_did$gname)
any(is.na(panel_did$gname))

# baseline propensity for ever treated vs never
base <- panel_did |> filter(tname == 1)
ps_fit <- glm(I(gname > 0) ~ age + inc, data = base, family = binomial())
panel_did$ps <- predict(ps_fit, newdata = panel_did, type = "response")

# IPW estimator
att <- att_gt(
  yname   = "y",
  tname   = "tname",
  idname  = "idname",
  gname   = "gname",
  xformla = ~ age + inc,
  data    = panel_did,
  panel   = TRUE,
  est_method = "ipw",
  pl = TRUE
)

# overall effect
agg <- aggte(att, type = "overall")
summary(agg)

# dynamic effects
dyn <- aggte(att, type = "dynamic")
library(ggplot2)
ggplot(tibble(eg = dyn$egt, att = dyn$att.egt, se = dyn$se.egt),
       aes(x = eg, y = att)) +
  geom_point() +
  geom_errorbar(aes(ymin = att - 1.96*se, ymax = att + 1.96*se), width = 0) +
  labs(x = "event time", y = "ATT")


# third try ----

set.seed(123)

# Setup
N  <- 1500          # units
Tt <- 6             # periods
g0 <- 4             # treatment starts at period 4 for treated group

# Units and baseline covariates
units <- tibble::tibble(
  id   = 1:N,
  treat = rbinom(N, 1, 0.5),                     # treated vs untreated
  age  = rnorm(N, 40, 10),
  inc  = rlnorm(N, log(50), 0.5),
  site = factor(sample(letters[1:4], N, TRUE))
)

# Build panel
panel <- units |>
  tidyr::crossing(t = 1:Tt) |>
  dplyr::mutate(
    post = as.integer(t >= g0),
    # treatment active only for treated units in post
    A = treat * post,

    # outcome process
    # baseline level and trend vary with covariates
    mu  = 3 + 0.05*age + 0.08*inc + 0.2*t + 0.1*(t*(age - 40)/10),
    # dynamic treatment effect grows after start
    te  = A * (1.2 + 0.4*pmax(0, t - g0)),
    y   = mu + te + rnorm(n(), 0, 1.2)
  )

# Propensity score from baseline only
base <- panel |> filter(t == 1)
ps_fit <- glm(treat ~ age + inc + site, data = base, family = binomial())
ps_by_id <- base |>
  dplyr::mutate(ps = predict(ps_fit, type = "response")) |>
  dplyr::select(id, treat, ps)

# Stabilized weights
pbar <- mean(ps_by_id$treat == 1)
w_tbl <- ps_by_id |>
  mutate(w = if_else(treat == 1, pbar/ps, (1 - pbar)/(1 - ps))) |>
  select(id, w)

# Attach weights to the panel
panel_w <- panel |>
  dplyr::left_join(w_tbl, by = "id")

# Check balance at baseline (try love package)
bal_before <- cobalt::bal.tab(treat ~ age + inc + site, data = base)
bal_after  <- cobalt::bal.tab(treat ~ age + inc + site, data = base, weights = w_tbl$w)
bal_before$Balance$Diff.Adj[1:5]
bal_after$Balance$Diff.Adj[1:5]


# Plain DiD with FE
did_plain <- fixest::feols(y ~ treat * post | id + t, data = panel, cluster = ~ id)

# IPW DiD with FE
did_ipw <- fixest::feols(y ~ treat * post | id + t, data = panel_w, weights = ~ w, cluster = ~ id)

fixest::etable(did_plain, did_ipw, se = "cluster", cluster = ~ id)

# By hand DiD on weighted means for a quick check
means <- panel_w |>
  dplyr::group_by(treat, t) |>
  dplyr::summarise(ybar = weighted.mean(y, w), .groups = "drop")

did_hand <- with(means,
                 ybar[treat == 1 & t == g0] - ybar[treat == 1 & t == g0 - 1] -
                   (ybar[treat == 0 & t == g0] - ybar[treat == 0 & t == g0 - 1])
)
did_hand

# Simple event study with weights
es <- fixest::feols(y ~ i(t, treat, ref = g0 - 1) | id, data = panel_w, weights = ~ w, cluster = ~ id)
fixest::iplot(es)


# final ----
set.seed(123)
# Setup
N  <- 3000 #1500          # units
Tt <- 6             # periods
g0 <- 4             # treatment starts at period 4 for treated group

# Units and baseline covariates
units <- tibble::tibble(
  id   = 1:N,
  treat = rbinom(N, 1, 0.5),                     # treated vs untreated
  age  = rnorm(N, 40, 10),
  inc  = rlnorm(N, log(50), 0.5),
  site = factor(sample(c("Windsor", "London", "Toronto", "Kingston"), N, TRUE))
)

# Build panel
panel <- units |>
  tidyr::crossing(t = 1:Tt) |>
  dplyr::mutate(
    post = as.integer(t >= g0),
    # treatment active only for treated units in post
    A = treat * post,
    # outcome process
    # baseline level and trend vary with covariates
    mu  = 3 + 0.05*age + 0.08*inc + 0.2*t + 0.1*(t*(age - 40)/10),
    # dynamic treatment effect grows after start
    te  = A * (1.2 + 0.4*pmax(0, t - g0)),
    y   = mu + te + rnorm(n(), 0, 1.2)
  )

panel <- panel |> dplyr::select(id, y, treat, age, inc, site, t, post)
panel |> readr::write_csv("slides/data/coupon.csv")

panel |> dplyr::mutate(date = lubridate::as_datetime("2025-10-01") + lubridate::dweeks(x=t)) |>
  timetk::plot_time_series(
    .date_var = date
    , .value = y
    , .color_var = as.factor(post)
  )

# Propensity score from baseline only
base <- panel |> filter(t == 1)
ps_fit <- glm(treat ~ age + inc + site, data = base, family = binomial())
ps_by_id <- base |>
  dplyr::mutate(ps = predict(ps_fit, type = "response")) |>
  dplyr::select(id, treat, ps)

# Stabilized weights
pbar <- mean(ps_by_id$treat == 1)
w_tbl <- ps_by_id |>
  mutate(w = if_else(treat == 1, pbar/ps, (1 - pbar)/(1 - ps))) |>
  select(id, w)

# Attach weights to the panel
panel_w <- panel |>
  dplyr::left_join(w_tbl, by = "id")

# Check balance at baseline (try love package)
bal_before <- cobalt::bal.tab(treat ~ age + inc + site, data = base)
bal_after  <- cobalt::bal.tab(treat ~ age + inc + site, data = base, weights = w_tbl$w)
bal_before$Balance$Diff.Adj[1:5]
bal_after$Balance$Diff.Adj[1:5]

# Plain DiD with FE
did_plain <- fixest::feols(y ~ treat * post | id + t, data = panel, cluster = ~ id)

# IPW DiD with FE
did_ipw <- fixest::feols(y ~ treat * post | id + t, data = panel_w, weights = ~ w, cluster = ~ id)

fixest::etable(did_plain, did_ipw, se = "cluster", cluster = ~ id)

# By hand DiD on weighted means for a quick check
means <- panel_w |>
  dplyr::group_by(treat, t) |>
  dplyr::summarise(ybar = weighted.mean(y, w), .groups = "drop")

did_hand <- with(means,
                 ybar[treat == 1 & t == g0] - ybar[treat == 1 & t == g0 - 1] -
                   (ybar[treat == 0 & t == g0] - ybar[treat == 0 & t == g0 - 1])
)
did_hand

# Simple event study with weights
es <- fixest::feols(y ~ i(t, treat, ref = g0 - 1) | id, data = panel_w, weights = ~ w, cluster = ~ id)
fixest::iplot(es)

# correct event study with unit and time FE
es <- fixest::feols(
  y ~ i(t, treat, ref = g0 - 1) | id + t,
  data = panel_w,
  weights = ~ w,
  cluster = ~ id
)

es$coeftable |> as.data.frame() |>
  dplyr::slice_tail(n=3) |>
  dplyr::summarize(ATT = mean(Estimate))

fixest::iplot(es)

es_xt <- fixest::feols(
  y ~ i(t, treat, ref = g0 - 1) + age:t | id + t,
  data = panel_w,
  weights = ~ w,
  cluster = ~ id
)
fixest::iplot(es_xt)

b <- broom::tidy(es_xt)
subset(b, grepl("t::treat", term))[, c("term","estimate")]

es_xt$coeftable

# Inputs
# panel has id, t, y, treat
# w_tbl has id, w     # stabilized weights from your ps model
# g0 is the first treated period for the treated group
# post periods are g0, g0+1, g0+2 in your example

# make sure each id carries its group flag
id_treat <- panel %>%
  distinct(id) %>%
  mutate(treat = if_else(id <= N/2, 1L, 0L))   # or however you assigned groups

panel <- panel %>%
  select(-treat) %>%            # drop any half-broken version
  left_join(id_treat, by = "id")

g0 <- 4
post_times <- c(g0, g0 + 1, g0 + 2)
pre_time   <- g0 - 1

# Treated share
pbar <- mean(panel$treat == 1)

# Build ATT weights from stabilized w
# Treated weight = 1
# Control weight ∝ ps/(1-ps) = [w - (1 - pbar)] / (1 - pbar)
w_att_tbl <- panel %>%
  distinct(id, treat) %>%
  left_join(w_tbl, by = "id") %>%
  mutate(
    w_att = ifelse(
      treat == 1, 1,
      pmax(0, (w - (1 - pbar)) / (1 - pbar))  # floor at 0 to avoid tiny negatives from noise
    )
  ) %>%
  select(id, w_att, treat)

# Helper to compute ATT at one event time e
att_event <- function(t_post) {
  dat <- panel %>%
    filter(t %in% c(pre_time, t_post)) %>%
    left_join(w_att_tbl, by = "id") %>%
    arrange(id, t) %>%
    group_by(id, treat) %>%
    summarise(
      dy = y[t == t_post] - y[t == pre_time],
      w  = first(w_att),
      .groups = "drop"
    )

  # Treated mean change with weight 1
  d_t  <- with(dat, mean(dy[treat == 1]))
  # Control weighted mean change
  d_c  <- with(dat, {
    num <- sum(dy[treat == 0] * w[treat == 0])
    den <- sum(w[treat == 0])
    num / den
  })

  d_t - d_c
}

# Event study ATTs at e = 0, 1, 2
atts <- tibble(
  t_post = post_times,
  event  = post_times - g0,
  att    = vapply(post_times, att_event, numeric(1))
)
atts

#@@@@@@@
# inputs
# panel has id, t, y, treat
# w_tbl has id, w   stabilized weights
g0 <- 4
pre_time  <- g0 - 1
post_times <- c(g0, g0 + 1, g0 + 2)

# rebuild treat from the unit table if needed
# panel <- panel %>% select(-treat) %>% left_join(units %>% select(id, treat), by = "id")

# recover ATT weights from stabilized w
pbar <- mean(panel$treat == 1)
w_att_tbl <- panel %>%
  dplyr::distinct(id, treat) %>%
  dplyr::left_join(w_tbl, by = "id") %>%
  dplyr::mutate(
    w_att = ifelse(treat == 1, 1,
                   pmax(0, (w - (1 - pbar)) / (1 - pbar)))
  ) %>%
  dplyr::select(id, treat, w_att)

# helper for one post time
att_event <- function(t_post) {
  delta <- panel %>%
    dplyr::filter(t %in% c(pre_time, t_post)) %>%
    dplyr::select(id, treat, t, y) %>%
    dplyr::mutate(tp = ifelse(t == pre_time, "pre", "post")) %>%
    tidyr::pivot_wider(names_from = tp, values_from = y) %>%
    dplyr::mutate(dy = post - pre) %>%
    dplyr::left_join(w_att_tbl, by = c("id", "treat"))

  d_t <- delta %>% filter(treat == 1) %>% summarise(m = mean(dy)) %>% pull(m)
  d_c <- delta %>%
    dplyr::filter(treat == 0) %>%
    dplyr::summarise(m = sum(dy * w_att) / sum(w_att)) %>%
    dplyr::pull(m)

  d_t - d_c
}

atts <- tibble(
  t_post = post_times,
  event  = post_times - g0,
  att    = vapply(post_times, att_event, numeric(1))
)

atts

#^^^^^^^^^^^^^^^^^^^^^^^^
inputs you already have
# panel has id, t, y, treat
# w_tbl has id, w
g0 <- 4
pre_time   <- g0 - 1
post_times <- c(g0, g0 + 1, g0 + 2)

# 1) make sure every id has a single treat flag
units_grp <- panel %>% distinct(id, treat)
stopifnot(all(!is.na(units_grp$treat)))
stopifnot(nrow(units_grp) == length(unique(panel$id)))

# 2) build ATT weights from stabilized weights
pbar <- mean(units_grp$treat == 1)

w_att_tbl <- units_grp %>%
  left_join(w_tbl, by = "id") %>%
  mutate(
    # treated weight = 1
    w_att = ifelse(
      treat == 1, 1,
      # control ATT weight proportional to ps/(1-ps)
      # from control stabilized weight w = (1 - pbar)/(1 - ps)
      # so ps/(1-ps) = (w - (1 - pbar)) / (1 - pbar)
      (w - (1 - pbar)) / (1 - pbar)
    ),
    # clamp tiny or negative to a small positive value
    w_att = ifelse(treat == 0 & (is.na(w_att) | w_att <= 0), 1e-6, w_att)
  ) %>%
  select(id, treat, w_att)

# 3) helper to compute one event-time ATT
att_event <- function(t_post) {
  # keep only ids with both pre and this post
  keep_ids <- panel %>%
    filter(t %in% c(pre_time, t_post)) %>%
    count(id) %>%
    filter(n == 2) %>%
    pull(id)

  delta <- panel %>%
    filter(id %in% keep_ids, t %in% c(pre_time, t_post)) %>%
    select(id, treat, t, y) %>%
    mutate(tp = ifelse(t == pre_time, "pre", "post")) %>%
    pivot_wider(names_from = tp, values_from = y) %>%
    mutate(dy = post - pre) %>%
    left_join(w_att_tbl, by = c("id", "treat"))

  # drop any remaining NAs
  delta <- delta %>% filter(!is.na(dy), !is.na(treat), !is.na(w_att))

  # treated mean change, weight 1
  d_t <- delta %>% filter(treat == 1) %>% summarise(m = mean(dy)) %>% pull(m)

  # control weighted mean change
  den_c <- delta %>% filter(treat == 0) %>% summarise(den = sum(w_att)) %>% pull(den)
  if (length(den_c) == 0 || den_c <= 0) return(NA_real_)

  d_c <- delta %>%
    filter(treat == 0) %>%
    summarise(m = sum(dy * w_att) / den_c) %>%
    pull(m)

  d_t - d_c
}

# 4) compute event-time ATTs
atts <- tibble(
  t_post = post_times,
  event  = post_times - g0,
  att    = vapply(post_times, att_event, numeric(1))
)

atts
