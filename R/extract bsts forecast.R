
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
   dplyr::select(bin_id, key, value, Total) %>%
   spread(key, value)

 # root mean square error (for mean and median forecast)

 err_vals <- fc_df %>%
   ungroup() %>%
   mutate(PE.med = abs(`Forecast (Median)` - Total) / Total * 100,
          PE.mean = abs(`Forecast (Mean)` - Total) / Total * 100,
          Error.med = abs(`Forecast (Median)` - Total),
          Error.mean = abs(`Forecast (Mean)` - Total)) %>%
   summarise(RMSE.med = mean(Error.med),
             RMSE.mean = mean(Error.mean),
             MAPE.med = mean(PE.med),
             MAPE.mean = mean(PE.mean))

 fc_df <- fc_df %>%
   gather(key, value, -bin_id)

 fc_plot <- ggplot(fc_df, aes(as_datetime(bin_id), value, col = key)) +
   geom_line() +
   scale_x_datetime(labels=scales::date_format("%A"), date_breaks = "1 day", name = NULL)

 list(data = fc_df, plot = fc_plot, err_vals = err_vals)

}
