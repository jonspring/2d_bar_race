##### setup ######
library(tidyverse)
library(gganimate)

##### Some fake data for chart concept ########
set.seed(42)
mo = 24
some_data <- tibble(
  entity = rep(LETTERS[1:10], each = mo),
  month  = rep(seq.Date(as.Date("2018-01-01"), 
                        by = "month", length.out = mo), times = 10),
  change = rnorm(360, mean = 10, sd = 3)
) %>%
  group_by(entity) %>%
  mutate(cuml = cumsum(change))


# Plain race without ranking shifts#######
ggplot(some_data, aes(entity, cuml)) +
  geom_col() +
  coord_flip() +
  transition_time(month)

# data with monthly rank #####

some_data_rank_mo <- some_data %>%
  group_by(month) %>%
  mutate(rank = min_rank(cuml)) %>%
  ungroup()

some_data_rank_mo %>% View()

a <- ggplot(some_data_rank_mo, aes(x = rank)) +
  geom_tile(aes(y = cuml/2, width = 1, height = cuml)) +
  geom_text(aes(label = entity, y = 0), hjust = 1) +
  coord_flip(clip = "off") +
  # facet_wrap(~month)
  transition_time(month)
# a
animate(a, nframes = 36)
