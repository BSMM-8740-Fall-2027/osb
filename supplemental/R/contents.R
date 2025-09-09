# created September 9, 2025

dat <- readr::read_csv("supplemental/data/contents.csv", show_col_types = FALSE)

f <- "supplemental/data/contents.txt"

dat |> tibble::add_row(
  link = "https://blog.djnavarro.net/posts/2025-09-06_p-splines/"
  , title = "Splines"
  ) |>
  dplyr::arrange(title) |>
  dplyr::mutate(
    res =
      purrr::map2(
        link
        , title
        , (\(l,t){
          cat( paste0('          - href: ',l),"\n", file = f, append = T)
          cat( paste0('            text: ',t),"\n", file = f, append = T )
          return("-1")
        })
      )
  )


