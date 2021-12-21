library(tidyverse)

advent <- read.delim("data/day8_input", sep = "|", header = FALSE) %>% 
  mutate_all(str_trim) %>% 
  rename(input = V1, 
         output = V2)

output_chars <- strsplit(advent$output, " ")
output_char_list <- lapply(output_chars, nchar) %>% unlist

# count 1, 4, 7, 8 = 2, 3, 4, 7

sum(
  sum(output_char_list == "2"),
  sum(output_char_list == "3"),
  sum(output_char_list == "4"),
  sum(output_char_list == "7"))

input_chars <- strsplit(advent$input, " ")
input_char_list <- lapply(input_chars, nchar)


total = 0


for (index in 1:length(input_chars)) {
  inp_c <- input_chars[[index]]
  inp <- strsplit(inp_c, "")
  
  len <- sapply(inp, length)
  inp <- inp[order(len)]
  
  one <- inp[lapply(inp, length) == 2] %>% unlist
  four <- inp[lapply(inp, length) == 4]%>% unlist
  seven <- inp[lapply(inp, length) == 3]%>% unlist
  eight <- inp[lapply(inp, length) == 7]%>% unlist
  
  top_left_and_mid <- setdiff(four, one)
  
  # find the two lists that don't have the same chars that are in the 1 (length 2 list)
  
  # five segments =  2, 3, 5
  # six segments = 0, 6, 9 
  
  five_segments <- inp[lapply(inp, length) == 5]
  
  for(segment in five_segments){
    temp <- (top_left_and_mid %in% segment)
    if(sum(temp, na.rm = TRUE) == 2) {
      five <- segment
    } else {
      temp2 <- (one %in% segment)
      if(sum(temp2, na.rm = TRUE) == 2){
        three <- segment} else {
          two <- segment}
    }
  }
  
  six_segments <- inp[lapply(inp, length) == 6]
  
  for(segment in six_segments){
    temp <- (one %in% segment)
    if(sum(temp, na.rm = TRUE) == 1) {
      six <- segment
    } 
    else {
      if(length(setdiff(segment, three)) == 2){
        zero <- segment
      } else{
        nine <- segment
      }
      
    }
  }
  
  one <- str_flatten(sort(one), "")
  two <- str_flatten(sort(two), "")
  three <- str_flatten(sort(three), "")
  four <- str_flatten(sort(four), "")
  five <- str_flatten(sort(five), "")
  six <- str_flatten(sort(six), "")
  seven <- str_flatten(sort(seven), "")
  eight <- str_flatten(sort(eight), "")
  nine <- str_flatten(sort(nine), "")
  zero <- str_flatten(sort(zero), "")
  
  dict_list <- c(one,two,three,four,five,six,seven,eight,nine,zero)
  key_list <- c(1,2,3,4,5,6,7,8,9,0)

  dictionary <- data.frame(key_list, dict_list)
  
  
  out_list <- output_chars[[index]] %>%  strsplit("")  
  for(num in 1:length(out_list)) {
    out_list[[num]] <- str_flatten(sort(out_list[[num]]))
  }
  
  output_list <- data.frame(output = unlist(out_list))
  output_list %>% left_join(dictionary, by = c("output" = "dict_list")) -> decoded
  
  decoded$key_list %>% str_flatten() %>% as.numeric -> decoded_sum
  
  total = total + decoded_sum

  
  }

