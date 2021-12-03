#advent of code day 1
library(tidyverse)
advent_code <- read.table("day1_input")


advent_code %>% 
  rename(nums = V1) %>%
  mutate(change = nums - lag(nums)) %>% 
  mutate(inc = case_when(change > 0 ~ "increased") ) %>% 
  filter(inc == "increased") %>% nrow()

# cleaner solution:
advent_code %>% 
  rename(nums = V1) %>%
  filter(nums > lag(nums)) %>% nrow()


advent_code %>% 
  rename(nums = V1) %>%
  mutate(lag1 = lag(nums),
         lag2 = lag(nums, 2)) %>% 
  mutate(sliding = nums + lag1 + lag2) %>% 
  filter(!is.na(sliding)) %>% 
  mutate(change = sliding - lag(sliding)) %>% 
  mutate(inc = case_when(change > 0 ~ "increased") ) %>% 
  filter(inc == "increased") %>% nrow()
# there's definitely a more efficient way to do this lol

# more efficient way:
advent_code %>%
  rename(nums = V1) %>%
  mutate(sliding = nums + lag(nums) + lag(nums,2)) %>% 
  filter(!is.na(sliding)) %>% 
  filter(sliding > lag(sliding)) %>% nrow()




# base R solution
advent_code <- read.table("day1_input")
lag <- c(NA, head(advent_code$V1, -1))
advent_code <- cbind(advent_code, lag)


advent_code$change = advent_code$V1 - advent_code$lag
advent_code$increase = ifelse(advent_code$change > 0, "increase", "decrease")
increase <- advent_code[advent_code$increase == "increase",]
increase <- increase[!is.na(increase$increase),]
nrow(increase)


# hmm why doesn't this one work
nrow(advent_code[advent_code$V1 > advent_code$lag,]) 