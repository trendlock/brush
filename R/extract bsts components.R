
#' @export


extract_bsts_components <- function(model_obj) {


  df <- model_obj$state.contributions %>%
    as.tibble() %>%
    mutate(id = row_number()) %>%
    gather(key, distrobution, -id)


  diff_last_comp_each <- function(x) {
    x_last <- last(x)
    x <- x[-length(x)] %>%
      str_c(collapse = ".")
    str_c(c(x, x_last), collapse = "_")
  }

  diff_last_comp <- function(var) {
    var %>%
      str_split("[[:punct:]]") %>%
      map_chr( ~ diff_last_comp_each(.x))
  }

  key_df <- df %>%
    pull(key) %>%
    unique() %>%
    enframe() %>%
    rename(key = value) %>%
    mutate(new.key = diff_last_comp(key))


  df <- df %>%
    inner_join(key_df, by = "key") %>%
    dplyr::select(-key, -name) %>%
    rename(key = new.key)

  df <- df %>%
    separate(key, c("key", "key.id"), sep = "_") %>%
    mutate(key.id = as.numeric(key.id))

  opts <- df %>%
    pull(key) %>%
    unique()

  #  data
  df_ls <- opts %>%
    map( ~ df %>%
           filter(key == .x) %>%
           group_by(key.id) %>%
           summarise(distrobution = mean(distrobution)) %>%
           mutate(Time = row_number()))

  #  plots
  plot_lims <- list(max = max(df$distrobution), min = min(df$distrobution))

  plot_ls <- opts %>%
    map2(df_ls, ~ .y %>%
           ggplot(aes(Time, distrobution)) +
           geom_line() +
           ggtitle(.x) +
           ylim(plot_lims[["min"]], plot_lims[["max"]]))

  map2(df_ls, plot_ls, ~ list(data = .x, plot = .y)) %>%
    set_names(opts)

}

