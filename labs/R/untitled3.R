

set.seed(123)

library(tidyverse)

# number of firms
n_firms <- 200

# number of years
years <- 2016:2020

# firm traits
firms <- tibble::tibble(
  firm_id = 1:n_firms,
  size = rnorm(n_firms, 50, 12),
  sector = sample(c("A", "B", "C"), n_firms, TRUE)
)

# build panel
panel <- firms %>%
  tidyr::crossing(year = years) %>%
  dplyr::mutate(
    # base profit driven by size
    base = 10 + 0.8 * size,
    # time effect
    year_effect = case_when(
      year == 2016 ~ 0,
      year == 2017 ~ 1,
      year == 2018 ~ 2,
      year == 2019 ~ 3,
      year == 2020 ~ 5
    ),
    # treatment. some firms adopt a new system in 2019
    treated = if_else(firm_id <= 80, 1, 0),
    post = if_else(year >= 2019, 1, 0),
    A = treated * post,
    # outcome with random noise
    profit = base + year_effect + 4 * A + rnorm(n(), 0, 5)
  )

# quick structure check
sales_panel %>%
  arrange(firm_id, year) %>%
  head()


did_fe <- fixest::feols(
  sales ~ treated * post + size + sector | firm_id + year,
  data    = panel,
  cluster = ~ firm_id          # cluster by unit
)

summary(did_fe)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Packages
library(tidyverse)
library(fixest)

set.seed(123)

# 1. Simulate panel data for firms
n_firms <- 200
n_years <- 6
years   <- 2015:2020

firms <- tibble::tibble(
  firm_id = 1:n_firms,
  size    = runif(n_firms, 50, 500),                   # firm size
  sector  = factor(sample(c("A", "B", "C"), n_firms, replace = TRUE)),
  treated = rbinom(n_firms, 1, 0.5)                    # 1 = treated firm
)

panel <- firms %>%
  tidyr::crossing(year = years) %>%
  dplyr::mutate(
    post = if_else(year >= 2018, 1L, 0L),              # policy starts in 2018
    # baseline sales process
    base_sales = 1000 +
      3 * size +
      if_else(sector == "B", 500, 0) +
      if_else(sector == "C", -300, 0) +
      200 * (year - 2015),
    # true treatment effect after policy
    true_te = if_else(treated == 1 & post == 1, 1500, 0),
    # observed outcome
    sales = base_sales + true_te + rnorm(n(), 0, 1000)
  )

panel <- panel |> dplyr::select(-c(base_sales, true_te)) |>
panel |> readr::write_csv("data/panel.csv")

panel <- readr::read_csv("data/panel.csv", show_col_types = FALSE)

# Check that 'sales' is really in the data
names(panel)
# You should see "sales" in the printed list

# 2. Fixed effects DiD with covariates
did_fe <- fixest::feols(
  sales ~ treated * post + size + sector | firm_id + year,
  data   = panel,
  cluster = ~ firm_id
)

summary(did_fe)

lm(sales ~ treated * post + size + sector, data   = panel
   )

m_lm <- fixest::feols(sales ~ treated * post + size + sector + firm_id + year, data = panel)

m_lm <- fixest::feols(sales ~ treated * post + size + factor(sector) + factor(firm_id) + year, data = panel)

fixest::etable(m_lm, did_fe)

policy_year <- 2018

es_fe <- fixest::feols(
  sales ~ fixest::i(year, treated, ref = year - 1) + size + sector
  | firm_id + year,
  data    = panel,
  cluster = ~ firm_id
)

fixest::iplot(es_fe)

fixest::feols(
  sales ~ treated * post | firm_id + year,
  data   = panel,
  cluster = ~ firm_id
)

panel <- readr::read_csv("data/panel.csv", show_col_types = FALSE) |>
  dplyr::mutate( firm_id = paste0("firm_", firm_id) )

# (1) baseline with covariates
m_lm <- fixest::feols(sales ~ treated * post + size + sector + year, data = panel)


# (1) baseline with covariates
m_lm <- fixest::feols(sales ~ treated * post + size + sector + firm_id + year, data = panel)

# (2) sector + firm_id fixed effects DiD without covariates
did_fe <- fixest::feols(
  sales ~ treated * post | firm_id + year,
  data   = panel,
  #cluster = ~ firm_id
)

  # 2. sector + firm_id fixed effects DiD with covariates
did_fe_cov <- fixest::feols(
  sales ~ treated * post + size + sector | firm_id + year,
  data   = panel,
  cluster = ~ firm_id
)

fixest::etable(m_lm, did_fe, did_fe_cov)
