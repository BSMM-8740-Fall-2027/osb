# created November 14, 2025


# rubric default ----
tibble::tibble(
  exercise = "1-10"
  , points = "3 points each"
) |>
  gt::gt() |>
  gt::cols_align(
    align = "left",
    columns = exercise
  ) |>
  gt::cols_width(exercise ~ gt::px(250), points ~ gt::px(150)) |>
  gt::tab_footnote(
    footnote = gt::md("code (if any) does not run to completion: **-1 point**")
    , locations = gt::cells_column_labels(points)
  ) |>
  gt::tab_footnote(
    footnote = gt::md("explicit question (if any) not answered, even if code runs to completion: **-1 point**")
    , locations = gt::cells_column_labels(points)
  ) |>
  gtExtras::gt_theme_espn()



# rubric 8 ----
tibble::tibble(
  exercise = c("1-2, 5-8", "3-4")
  , points = c("3 points each", "6 points each")
) |>
  gt::gt() |>
  gt::cols_align(
    align = "left",
    columns = exercise
  ) |>
  gt::cols_width(exercise ~ gt::px(250), points ~ gt::px(150)) |>
  gt::tab_footnote(
    footnote = gt::md("code (if any) does not run to completion: **-1 point**")
    , locations = gt::cells_column_labels(points)
  ) |>
  gt::tab_footnote(
    footnote = gt::md("explicit question (if any) not answered, even if code runs to completion: **-1 point**")
    , locations = gt::cells_column_labels(points)
  ) |>
  gtExtras::gt_theme_espn() |>
  gt::gtsave("labs/images/rubric_lab_8.png")

# rubric 9 ----
tibble::tibble(
  exercise = "1-6"
  , points = "5 points each"
) |>
  gt::gt() |>
  gt::cols_align(
    align = "left",
    columns = exercise
  ) |>
  gt::cols_width(exercise ~ gt::px(250), points ~ gt::px(150)) |>
  gt::tab_footnote(
    footnote = gt::md("code (if any) does not run to completion: **-1 point**")
    , locations = gt::cells_column_labels(points)
  ) |>
  gt::tab_footnote(
    footnote = gt::md("explicit question (if any) not answered, even if code runs to completion: **-1 point**")
    , locations = gt::cells_column_labels(points)
  ) |>
  gtExtras::gt_theme_espn() |>
  gt::gtsave("labs/images/rubric_lab_9.png")


# rubric midterm ----
tibble::tibble(
  question = c("1-10", "11", "12", "13", "14")
  , points = c("1 point each", "4 points", "5 points", "3 points", "3 points")
) |>
  gt::gt() |>
  gt::cols_align(
    align = "left",
    columns = question
  ) |>
  gt::cols_width(question ~ gt::px(250), points ~ gt::px(150)) |>
  gt::tab_footnote(
    footnote = gt::md("code (if any) does not run to completion: **-1 point**")
    , locations = gt::cells_column_labels(points)
  ) |>
  gt::tab_footnote(
    footnote = gt::md("explicit question (if any) not answered, even if code runs to completion: **-1 point**")
    , locations = gt::cells_column_labels(points)
  ) |>
  gtExtras::gt_theme_espn() |>
  gt::gtsave("labs/images/rubric_midterm.png")
