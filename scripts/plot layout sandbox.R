
library(brush)


model <- read_rds(find::this("/internal data storage/bat country/bsts models/model_x1.rds"))

pred <- read_rds(find::this("/internal data storage/bat country/bsts models/pred_x1.rds"))

next_p <- read_rds(find::this("/internal data storage/bat country/bsts models/next_x1.rds"))

fc_ls <- extract_bstb_forecast(pred, next_p)
comp_ls <- extract_bsts_components(model)

library(ggplot2)
library(gtable)
library(gridExtra)

# check plots...

comp_ls[["trend"]][["plot"]] %>% is_empty()



title <- lubridate::now() %>% as.numeric() %>% round()
cat <-  "Beer"
x <- grid.arrange(
  grobs = list(fc_ls[["plot"]] + ggtitle(paste0("Model ", title, " ", cat)),
               comp_ls[["trend"]][["plot"]],
               comp_ls[["seasonal.24.1"]][["plot"]],
               comp_ls[["seasonal.168.1"]][["plot"]],
               comp_ls[["regression"]][["plot"]]),
  widths = c(1, 1),
  layout_matrix = rbind(c(1, 1),
                        c(1, 1),
                        c(2, 3),
                        c(4, 5)))


x
ggsave(filename="ab.png", plot = x, units = "in", width = 15, height = 10)
