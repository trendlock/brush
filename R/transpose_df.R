
#' @export

transpose_df <- function(df) {
  df %>%
    t() %>%
    as_tibble()
}




