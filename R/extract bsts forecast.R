
#' @export

extract_bstb_forecast <- function(pred_obj) {

  df_mean <- pred_obj$mean %>%
    enframe() %>%
    mutate(key = "mean",
           x.id = row_number())


  df_med <- pred_obj$median %>%
    enframe() %>%
    mutate(key = "median",
           x.id = row_number())


  df_int <- pred_obj$interval %>%
    as.tibble() %>%
    transpose_df() %>%
    set_names(rownames(pred_obj$interval)) %>%
    mutate(x.id = row_number()) %>%
    gather(key, value, -x.id)

 bind_rows(df_int, df_mean, df_med) %>%
   select(-name)

}
