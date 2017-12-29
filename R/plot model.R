
#' @export

plot_model <- function(fc_ls, comp_ls, prod_cat, replication_id) {

  obj <- grid.arrange(
    grobs = list(fc_ls[["plot"]] + ggtitle(paste0("Model ", replication_id, " ", prod_cat)),
                 comp_ls[["trend"]][["plot"]],
                 comp_ls[["seasonal.24.1"]][["plot"]],
                 comp_ls[["seasonal.168.1"]][["plot"]],
                 comp_ls[["regression"]][["plot"]]),
    widths = c(1, 1),
    layout_matrix = rbind(c(1, 1),
                          c(1, 1),
                          c(2, 3),
                          c(4, 5)))

  ggsave(filename = paste0("plots/saved models/model_", replication_id,"_", prod_cat,".png"), plot = obj,
         units = "in", width = 15, height = 10)

}
