


source("functions_nir.R")

library(tidyverse)

read.csv("RALLF_nirData_20Feb2023.csv") -> dat5

# fixing st. paul naming issue
dat5 %>% 
  mutate(site = fct_recode(site,
                           "st paul" = "st. paul")) -> dat5

# fixing multiple code observations

dat5 %>% 
  group_by(code) %>% 
  # glimpse()
  summarise(
    across(
      .cols = 10:33,
      .fns = mean
    )) -> dat7

dat7 %>%
  left_join(key3) %>%
  left_join(key2) %>%
  left_join(key1) -> dat5


dat6 <- dat5

