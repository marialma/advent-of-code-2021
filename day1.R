#advent of code day 1
library(tidyverse)

advent_code <- as.data.frame(advent_code_list)
advent_code %>% 
  mutate(change = advent_code_list - lag(advent_code_list)) %>% 
  mutate(inc = case_when(change > 0 ~ "increased") ) %>% 
  filter(inc == "increased") %>% nrow()


advent_code %>% 
  mutate(lag1 = lag(advent_code_list),
         lag2 = lag(advent_code_list, 2)) %>% 
  mutate(sliding = advent_code_list + lag1 + lag2) %>% 
  filter(!is.na(sliding)) %>% 
  mutate(change = sliding - lag(sliding)) %>% 
  mutate(inc = case_when(change > 0 ~ "increased") ) %>% 
  filter(inc == "increased") %>% nrow()
# there's definitely a more efficient way to do this lol

