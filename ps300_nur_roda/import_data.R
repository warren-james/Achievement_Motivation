library(tidyverse)
library(readxl)

read_excel("raw_data_need for achievement.xlsx") %>%
  gather( 'key', 'value', -ps_code, -group, -gender, -age, -handedness) %>%
  glimpse() -> d

unique(d$key)

# First extract measurement task data

d %>% filter(str_detect(key, 'MT')) %>%
  mutate(
    distance = as.numeric(str_extract(key, '[0-9]+')),
    accuracy = value / 12) %>%
  glimpse() -> d_mt


ggplot(d_mt, aes(x = distance, y = accuracy) ) + 
  geom_point() + 
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'), se = FALSE) + 
  facet_wrap(~ ps_code) +
  theme_bw()

ggsave("nur_mt.png", width = 8, height = 8)