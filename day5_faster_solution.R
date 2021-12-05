#faster day 5 solution 

library(tidyverse)
file <- readLines("data/day5_input")
advent <- as.data.frame(file)

advent <- advent %>% 
  separate(file, c("start", "end"), 
           sep = " -> ") %>% 
  separate(start, c("x1", "y1"), 
           sep = ",") %>% 
  separate(end, c("x2", "y2"), 
           sep = ",") %>% 
  mutate_all(as.numeric)

part_1 <- advent %>% 
  filter(x1 == x2 | y1 == y2)

advent_5 <- function(input_df){
  points_df <- data.frame()
  
  for(point in 1:nrow(input_df)){
    input_df[point, 1] -> startx
    input_df[point, 2] -> starty
    input_df[point, 3] -> endx
    input_df[point, 4] -> endy
    
    list_of_pointsx <- c(startx:endx)
    list_of_pointsy <- c(starty:endy)
    
    points_temp <- data.frame(list_of_pointsx, list_of_pointsy)
    points_df <- bind_rows(points_df, points_temp)
  }
  points_df %>%
    group_by(list_of_pointsx, list_of_pointsy) %>% 
    tally() %>% 
    filter(n>1) %>% 
    nrow()
}


advent_5(part_1)
advent_5(advent)

