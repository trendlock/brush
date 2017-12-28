

df <- tibble(
  mean
)



df_mean <- pred$mean %>%
  enframe() %>%
  mutate(key = "mean",
         bin.id = row_number())


df_med <- pred$median %>%
  enframe() %>%
  mutate(key = "median",
         bin.id = row_number())


df_int <- pred$interval %>%
  as.tibble() %>%
  transpose_df() %>%
  set_names(rownames(pred$interval)) %>%
  mutate(bin.id = row_number()) %>%
  gather(key, value, -bin.id)

df <- bind_rows(df_int, df_mean, df_med)

ggplot(df, aes(x = bin.id, y = value, col = key)) +
  geom_line()

df <- df %>%
  mutate(value_zerod = ifelse(value < 0, 0, value))


ggplot(df, aes(x = bin.id, y = value_zerod, col = key)) +
  geom_line()

df <- pred$interval %>%
  as.tibble() %>%
  transpose_df() %>%
  set_names(rownames(pred$interval)) %>%
  mutate(bin.id = row_number()) %>%
  gather(key, val, -bin.id)



ggplot(df, aes(x = bin.id, y = val, col = key)) +
  geom_line()


df <- pred$distribution %>%
  as.tibble() %>%
  transpose_df() %>%
  mutate(bin.id = row_number()) %>%
  gather(key, val, -bin.id)

ggplot(df, aes(x = bin.id, y = val, col = key)) +
  geom_line(alpha = 0.1, show.legend = F)


df <- pred$distribution %>%
  as.tibble() %>%
  transpose_df()


df <- pred$distribution %>%
  as.tibble() %>%
  transpose_df() %>%
  mutate(bin.id = row_number()) %>%
  gather(key, val, -bin.id)

rand_vals <- df %>%
  pull(key) %>%
  unique()

ggplot(df, aes(x = bin.id, y = val, grp = key)) +
  geom_line(alpha = 0.5, show.legend = F)


