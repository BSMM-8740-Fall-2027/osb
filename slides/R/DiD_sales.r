# created September 2025

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
  dplyr::slice_sample(n = dim(df_stores_treat)[1])


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


# =========

# Aggregate sales by week
df_sales_combined = pd.concat([df_sales_treat, df_sales_control])
df_sales_combined["Year"] = df_sales_combined["Date"].dt.isocalendar().year
df_sales_combined["Week"] = df_sales_combined["Date"].dt.isocalendar().week
df_weekly_sales = df_sales_combined.groupby(["Group", "Year", "Week"]).agg({'Sales':'sum', 'Date':'first'}).reset_index()

# Identify when competition started
comp_start = (
  df_weekly_sales
  .query("Group == 'Treat' and Year==2013 and Date.dt.month==4")
  .Date
  .min()
)
