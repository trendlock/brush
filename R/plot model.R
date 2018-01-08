
#' @export

plot_model <- function(fc_ls, comp_ls, prod_cat, replication_id, rmse_ls) {

  print(is_empty(rmse_ls[["rmse_med"]]))
  print(is_empty(rmse_ls[["rmse_mean"]]))

  print(is_empty(fc_ls[["plot"]]))
  print(is_empty(comp_ls[["seasonal.24.1"]][["plot"]]))
  print(is_empty(comp_ls[["seasonal.168.1"]][["plot"]]))
  print(is_empty(comp_ls[["seasonal.168.1"]][["plot"]]))
  print(is_empty(comp_ls[["regression"]][["plot"]]))


  print(paste0("Model ", replication_id, " ", prod_cat, "  ---- RMSE med:", round(rmse_ls[["rmse_med"]]), " mean:", round(rmse_ls[["rmse_mean"]])))

  obj <- grid.arrange(
    grobs = list(fc_ls[["plot"]] + ggtitle(paste0("Model ", replication_id, " ", prod_cat, " RMSE med:", round(rmse_ls[["rmse_med"]]), " mean:", round(rmse_ls[["rmse_mean"]]))),
                 comp_ls[["trend"]][["plot"]],
                 comp_ls[["seasonal.24.1"]][["plot"]],
                 comp_ls[["seasonal.168.1"]][["plot"]],
                 comp_ls[["regression"]][["plot"]]),
    widths = c(1, 1),
    layout_matrix = rbind(c(1, 1),
                          c(1, 1),
                          c(2, 3),
                          c(4, 5)))


  plot_out <<- obj

  file_nm <- paste0("extdata/plots/model_", replication_id,"_", prod_cat,".png")

  ggsave(filename = file_nm, plot = obj,
         units = "in", width = 15, height = 10)

  file_nm
}
