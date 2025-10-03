# created September 2025

# see https://pub.towardsai.net/measuring-uplift-without-randomised-control-a-quick-and-practical-guide-8a9425da9d96

df_sales <-
  readr::read_csv("slides/data/rossmann-store-sales/train.csv", show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::mutate(date = lubridate::as_datetime(date))

df_stores <- readr::read_csv("slides/data/rossmann-store-sales/store.csv", show_col_types = FALSE) |>
  janitor::clean_names()

# store with competitors opening in April 2013
df_stores_treat <- df_stores |>
  dplyr::filter(
    competition_open_since_year == 2013 &
      competition_open_since_month == 4 &
      competition_distance < 1000
  )

set.seed(40)
df_stores_control <- df_stores |>
  dplyr::filter(is.na(competition_open_since_year)) |>
  dplyr::filter(store %in% c(554,491,769,216,331))
  # dplyr::slice_sample(n = dim(df_stores_treat)[1])




df_sales_treat <- df_sales |>
  dplyr::filter(store %in% df_stores_treat$store) |>
  dplyr::mutate(group = 'treat')

df_sales_control <- df_sales |>
  dplyr::filter(store %in% df_stores_control$store) |>
  dplyr::mutate(group = 'control')

# Aggregate sales by week
df_sales_combined <- df_sales_treat |>
  dplyr::bind_rows(df_sales_control) |>
  dplyr::mutate(
    year = lubridate::year(date)
    , week = lubridate::week(date)
  )

df_weekly_sales <- df_sales_combined |>
  dplyr::group_by(group, year, week) |>
  dplyr::mutate(
    sales = sum(sales)
    , date = min(date)
  )

df_weekly_sales <- df_sales_combined |>
  dplyr::group_by(group, year, week) |>
  dplyr::summarize(
    sales = sum(sales)
    , date = min(date)
    , .groups = "drop"
  )

# Identify when competition started
comp_start <- df_weekly_sales |>
  dplyr::filter(
    group == 'treat' &
      year == 2013 &
      lubridate::month(date) ==4
  ) |> dplyr::arrange(date) |>
  dplyr::slice(1) |>
  dplyr::pull(date)

# Cut data to 3months on each side
start_date <- comp_start - lubridate::dmonths(x=3)
end_date   <- comp_start + lubridate::dmonths(x=3)

df_weekly_sales <- df_weekly_sales |>
  dplyr::filter(dplyr::between(date, start_date, end_date))

df_weekly_sales |>
    timetk::plot_time_series(
      .date_var = date
      , .value = sales
      , .color_var = group
      , .smooth = FALSE
      , .interactive = FALSE
    ) +
  ggplot2::geom_vline(
    xintercept = comp_start, color = "blue", linetype = 3, linewidth = 1
  )


# Create indicators for treatment vs. control, and pre- vs. post-
df <- df_sales_combined %>%
  dplyr::mutate(
    post = dplyr::if_else(date >= comp_start, 1L, 0L),
    treat = dplyr::if_else(group == "treat", 1L, 0L),
    year_week = paste0(year, week)
  )

df_did <- df %>%
  dplyr::group_by(group, post) %>%
  dplyr::summarise(sales = mean(sales), .groups = "drop")


# Extract individual values
treat_post <- df_did %>% dplyr::filter(group == "treat" & post == 1) %>% dplyr::pull(sales)
treat_pre <- df_did %>% dplyr::filter(group == "treat" & post == 0) %>% dplyr::pull(sales)
control_post <- df_did %>% dplyr::filter(group == "control" & post == 1) %>% dplyr::pull(sales)
control_pre <- df_did %>% dplyr::filter(group == "control" & post == 0) %>% dplyr::pull(sales)

# Calculate DID estimator
did <- (treat_post - treat_pre) - (control_post - control_pre) # -1177.15 | -1456.413

# Run DiD
model <- lm(sales ~ treat * post, data = df |> dplyr::mutate(group = ifelse(group == treat, 1, 0)))

# Display coefficient table
broom::tidy(model, conf.int = TRUE)

# Or use summary for traditional R output
summary(model)

stage1 <- fixest::feols(sales ~ treat | post+store , data = df |> dplyr::mutate(group = ifelse(group == treat, 1, 0)))
stage1 <- fixest::feols(sales ~ treat , data = df |> dplyr::mutate(group = ifelse(group == treat, 1, 0)))


model <- fixest::feols(sales ~ treat * post
                       , data = df |> dplyr::mutate(group = ifelse(group == treat, 1, 0))
                       , cluster = ~store)
summary(model)

# =========

# Aggregate sales by week

df_sales_combined <- dplyr::bind_rows(df_stores_treat, df_sales_control)
dplyr::full_join(df_stores_treat, df_sales_control, by = "store")

# Aggregate sales by week
df_sales_combined <- dplyr::bind_rows(df_sales_treat, df_sales_control) |>
  dplyr::mutate(
    # year = isoyear(date)
    year = lubridate::year(date)
    , week = lubridate::week(date)
  )

df_weekly_sales <- df_sales_combined |>
  dplyr::group_by(group, year, week) |>
  dplyr::summarise(
    sales = sum(sales),
    date = first(date),
    .groups = "drop"
  )

# Identify when competition started
comp_start <- df_weekly_sales |>
  filter(group == "treat" & year == 2013 & lubridate::month(date) == 4) |>
  pull(date) |>
  min()

# Cut data to 3 months on each side
start_date <- comp_start - lubridate::dmonths(3)
end_date <- comp_start + lubridate::dmonths(3)

df_weekly_sales <- df_weekly_sales %>%
  dplyr::filter(date >= start_date & date <= end_date)

# Plot sales
ggplot2::ggplot(
  data = df_weekly_sales,
  ggplot2::aes(x = date, y = sales, color = group)
) +
  ggplot2::geom_line(linewidth = 1.5) +
  ggplot2::geom_vline(
    xintercept = as.numeric(comp_start),
    color = "red",
    linetype = "dashed"
  ) +
  ggplot2::scale_color_manual(
    values = c("Control" = "#55A868", "Treat" = "#4C72B0")
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::comma_format(accuracy = 1)
  ) +
  ggplot2::labs(
    title = "Weekly Sales: Control vs Treatment",
    x = "Week",
    y = "Weekly Sales",
    color = ""
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(color = "gray90"),
    panel.grid.minor = ggplot2::element_line(color = "gray95"),
    text = ggplot2::element_text(size = 11),
    legend.position = "top"
  )

df_did <- df %>%
  group_by(Group, Post) %>%
  summarise(Sales = mean(Sales), .groups = "drop")

# Extract individual values
treat_post <- df_did %>% filter(Group == "Treatment" & Post == 1) %>% pull(Sales)
treat_pre <- df_did %>% filter(Group == "Treatment" & Post == 0) %>% pull(Sales)
control_post <- df_did %>% filter(Group == "Control" & Post == 1) %>% pull(Sales)
control_pre <- df_did %>% filter(Group == "Control" & Post == 0) %>% pull(Sales)

# Calculate DID estimator
did <- (treat_post - treat_pre) - (control_post - control_pre)
