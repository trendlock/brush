
#' @export

extract_bstb_forecast <- function(pred_obj, next_periods) {

  df_mean <- pred_obj$mean %>%
    enframe() %>%
    mutate(key = "Forecast (Mean)",
           x.id = row_number())


  df_med <- pred_obj$median %>%
    enframe() %>%
    mutate(key = "Forecast (Median)",
           x.id = row_number())

  df_int <- pred_obj$interval %>%
    as.tibble() %>%
    transpose_df() %>%
    set_names(rownames(pred_obj$interval)) %>%
    mutate(x.id = row_number()) %>%
    gather(key, value, -x.id)

  fc_df <- bind_rows(df_int, df_mean, df_med) %>%
   dplyr::select(-name)

 fc_df <-  fc_df %>%
   mutate(bin_id = rep(pull(next_periods, bin_id), 4))

 next_periods <- next_periods %>%
   dplyr::select(-Product.Cat)

 fc_df <- fc_df %>%
   inner_join(next_periods, by = "bin_id")

 fc_df <- fc_df %>%
   dplyr::select(bin_id, key, value, Total, AIR_TEMP, PRCP) %>%
   spread(key, value) %>%
   gather(key, value, -bin_id)


 fc_plot <- ggplot(fc_df, aes(bin_id, value, col = key, size = key, alpha = key)) +
   geom_line() +
   scale_colour_manual(values = c("green", "green","yellow","orange","red","blue","black")) +
   scale_size_manual(values = c(1, 1, 1, 1.2, 1.2, 1, 1.5)) +
   scale_alpha_manual(values = c(0.5, 0.5, 1, 1, 1, 1, 1))


 list(data = fc_df, plot = fc_plot)

}
