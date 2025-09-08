# created September 07, 2025

# Load the sample assignment
assignment_path = system.file("examples/hw03-full.qmd", package = "parsermd")
cat(readLines(assignment_path), sep = "\n")
rmd = parsermd::parse_rmd(assignment_path)

s0 <- "/Users/louisodette/Documents/R_projects/OSB_2024_labs/2024_class/"
s1 <- "quiz-1/2024-quiz-1_zaidi32/2024-quiz-1.qmd"

rmd = parsermd::parse_rmd(paste0(s0,s1))

# *************

rmd = parsermd::parse_rmd("docs/2024-quiz-X.qmd")


rmd_tbl <- rmd |> tibble::as_tibble() |> print(n=25)

print(rmd_tbl, n=100)

rmd_mod <- rmd |> parsermd::rmd_select(
  # parsermd::has_heading("YOUR ANSWER:")
  # , parsermd::has_type("rmd_heading")
  parsermd::by_fenced_div(class = ".callout-note")
  , keep_yaml=TRUE
)

rmd_mod |> parsermd::rmd_select(
  parsermd::by_fenced_div(class = ".callout-note")
  ,parsermd::has_heading("YOUR ANSWER (10):")
)

rmd_mod |>
  parsermd::rmd_select(parsermd::by_section("YOUR ANSWER (1):"), keep_yaml=FALSE) |>
  parsermd::rmd_select(parsermd::has_type("rmd_markdown")) |>
  parsermd::as_document() |> toString() |>

  cat(sep = "\n")

rmd_mod |>
  parsermd::as_document() |>
  cat(sep = "\n")


