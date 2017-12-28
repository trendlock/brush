

# coefficients ====
x <- model$coefficients %>%
  as.tibble() %>%
  mutate(id = row_number()) %>%
  gather(key, val, -id, -`(Intercept)`)

ggplot(x, aes(id, val, col = key)) +
  geom_line()


sigma_df <- tibble(
  sigma.obs = model$sigma.obs,
  sigma.level = model$sigma.level,
  sigma.seasonal.24 = model$sigma.seasonal.24
  ) %>%
  mutate(id = row_number()) %>%
  gather(key, val, -id)

ggplot(sigma_df, aes(id, val, col = key)) +
  geom_line()

ggplot(sigma_df %>% filter(key == "sigma.seasonal.24"), aes(id, val, col = key)) +
  geom_line()






df <- model$final.state %>%
  as_tibble() %>%
  mutate(id = row_number()) %>%
  gather(key, val, -id)


ggplot(df, aes(id, val, col = key)) +
  geom_line()



# Regression ====
x <- model$predictors  %>%
  as.tibble() %>%
  mutate(id = row_number()) %>%
  gather(key, val, -id, -`(Intercept)`)

ggplot(x, aes(id, val, col = key)) +
  geom_line()


# season.21.1  ===
df <- model$state.contributions %>%
  as.tibble() %>%
  mutate(id = row_number()) %>%
  gather(key, val, -id)

opts <- df %>%
  pull(key) %>%
  unique()

df_x <- df %>%
  filter(key == "trend.1")

opts <- df_x %>%
  pull(key) %>%
  unique()


df <- model$state.contributions %>%
  as.tibble() %>%
  mutate(id = row_number()) %>%
  gather(key, val, -id)

df_x <- df %>%
  filter(stringr::str_detect(key, "seasonal.24")) %>%
  mutate(idx = row_number())

ggplot(df_x, aes(idx, val, grp = id)) +
  geom_point(alpha = 0.1, size = 0.1)


df_x %>% arrange(idx) %>% head(100)

df_sum <- df_x %>%
  group_by(key) %>%
  summarise(val.mean = mean(val),
            val.med = median(val)) %>%
  mutate(idx = row_number())



ggplot(df_sum, aes(idx, val.mean)) +
  geom_line(alpha = 0.3)
ggplot(df_sum, aes(idx, val.med)) +
  geom_line(alpha = 0.3)




# components  ===
df <- model$state.contributions %>%
  as.tibble() %>%
  mutate(id = row_number()) %>%
  gather(key, val, -id)


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
  select(-key, -name) %>%
  rename(key = new.key)

df <- df %>%
  separate(key, c("key", "key.id"), sep = "_")

opts <- df %>%
  pull(key) %>%
  unique()

df %>% head()
opts %>%
  map( ~ df %>%
         filter(key == .x) %>%
         mutate(idx = row_number()) %>%
         ggplot(aes(idx, val, col = key, grp = key.id)) +
           geom_line(alpha = 0.3) +
           facet_grid(key ~ .))


df_x <- df %>%
  filter(key == "seasonal.24.1") %>%
  mutate(idx = row_number())

ggplot(df_x, aes(idx, val, grp = id)) +
  geom_line(alpha = 0.5, size = 0.1)


