library(tidyverse)
input <- readLines("day3_input")

ac <- as.data.frame(input)
bit_length <- nchar(ac[1,1]) 

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
opposite_mode <- function(x) {
  ux <- unique(x)
  ux[which.min(tabulate(match(x, ux)))]
}

advent_3 <- function(ac, func) {
  str <- ac %>%
  separate(input, c(paste0(1:bit_length)),
           sep = c(1:bit_length)) %>%
  mutate_all(as.numeric) %>% 
  apply(2, func) %>%
  as.data.frame %>%
  rename(bits = 1) %>%
  mutate(power = bit_length:1, 
         power = power-1,
         binary_calc = (2^power) * bits) %>%
  summarise(sum(binary_calc))
str[1,1] 
}

gamma <- advent_3(ac, mode)
epsilon <- advent_3(ac, opposite_mode)

gamma*epsilon



# part 2

advent_3_pt2 <- function(ac, most_or_least){
  
  col_number = 1
  bits <- ac %>%
    separate(input, c(paste0(1:bit_length)),
             sep = c(1:bit_length)) %>%
    mutate_all(as.numeric) 

  for(co in 1:ncol(bits)){
    determine_mode <- bits[[co]]
    udm <- unique(determine_mode)
    tab <- tabulate(match(determine_mode, udm))
    print(tab)
    if(tab[1] == tab[2]){
      if(most_or_least == "most") {
        mc <- 1} 
      else if(most_or_least == "least"){
        mc <- 0}
    } else {
      if(most_or_least == "most") {
        mc <- udm[tab == max(tab)] }
      else if(most_or_least == "least"){ 
        mc <- udm[tab == min(tab)] }
    }
    
    bits_temp <- bits[bits[[col_number]] == mc,]
    if(nrow(bits_temp)==1) {
      break
    }
    bits <- bits_temp
    col_number = col_number + 1
  }
  
  final <- bits_temp %>%
    gather() %>% 
    mutate(power = bit_length:1, 
           power = power-1,
           binary_calc = (2^power) * value) %>%
    summarise(sum(binary_calc))
  final[1,1] 
}

oxy <- advent_3_pt2(ac, "most")
co2 <- advent_3_pt2(ac, "least")

oxy*co2
