library(tidyverse)

advent <- scan("data/day6_input", what = "", sep = ",")
advent %>% as.data.frame() %>% 
  rename(init_fish = 1) %>% 
  group_by(init_fish) %>% tally() -> init_fish

init_fish %>% 
  mutate(init_fish = paste0("t", init_fish)) -> init_fish

# model this as 8 separate compartments 
# initialize all compartments as 0
t0 <- 0
t1 <- 0
t2 <- 0
t3 <- 0
t4 <- 0
t5 <- 0
t6 <- 0
t7 <- 0
t8 <- 0 

# add in counts
for(row in 1:nrow(init_fish)){
  assign(paste(init_fish$init_fish[row]), init_fish$n[row])
}


time_steps <- 256
for (step in 1:time_steps) {
  
  curr_t0 <- t1
  curr_t1 <- t2
  curr_t2 <- t3
  curr_t3 <- t4
  curr_t4 <- t5 
  curr_t5 <- t6 
  curr_t6 <- t0 + t7
  curr_t7 <- t8
  curr_t8 <- t0 
  
  curr_t0 -> t0
  curr_t1 -> t1
  curr_t2 -> t2
  curr_t3 -> t3
  curr_t4 -> t4
  curr_t5 -> t5
  curr_t6 -> t6
  curr_t7 -> t7
  curr_t8 -> t8
  
}

paste(sum(t0,t1,t2,t3,t4,t5,t6,t7,t8))
