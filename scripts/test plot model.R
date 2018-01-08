
library(brush)

model <- read_rds(find::this("/internal data storage/bat country/bsts models/model_x1.rds"))

pred <- read_rds(find::this("/internal data storage/bat country/bsts models/pred_x1.rds"))

next_p <- read_rds(find::this("/internal data storage/bat country/bsts models/next_x1.rds"))

fc_ls <- extract_bstb_forecast(pred, next_p)
comp_ls <- extract_bsts_components(model)


library(ggplot2)
library(gtable)
library(gridExtra)

file_nm <- plot_model(fc_ls, comp_ls, "beer", 1, rmse_ls = fc_ls[["rmse_ls"]])
