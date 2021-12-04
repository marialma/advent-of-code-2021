# advent of code day 2

library(tidyverse)

input <- read.delim("data/day2_input", sep = " ", header = FALSE) 

input %>% rename(dir = V1, 
                 num = V2) -> input
horizontal <- input %>% filter(dir == "forward")
down <- depth %>% filter(dir == "down")
up <- depth %>% filter(dir == "up")

(sum(down$num) - sum(up$num)) * (sum(horizontal$num))
# this isn't actually a way to solve this problem since supposedly your depth can never be negative - so 
# if the up number takes you beyond 0, then those numbers wouldn't be 'valid'?

input %>% 
  mutate(rn = row_number()) %>% 
  select(rn,dir,num)-> input

input %>% 
  mutate(aim_change = case_when(dir == "down" ~ as.numeric(num), 
                                dir == "up" ~ as.numeric(num * -1), 
                                dir  == "forward" ~ as.numeric(0))) %>% 
  mutate(aim = cumsum(aim_change)) %>% 
  mutate(horiz_change = case_when(dir == "forward" ~ as.numeric(num),
                                  TRUE ~ 0)) %>% 
  mutate(horiz_sum = cumsum(horiz_change)) %>% 
  select(-horiz_change) %>% 
  mutate(vert_change = case_when(dir == "forward" ~ aim*num, 
                                 TRUE ~ 0)) %>% 
  mutate(vert_sum = cumsum(vert_change)) %>% 
  tail(1) %>% 
  mutate(answer= vert_sum * horiz_sum)

