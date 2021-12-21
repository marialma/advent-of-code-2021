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



for (index in 1:length(input_chars)) {
index <- 1  
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
      print(five)
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
      print("six")
    } 
    else {
      if(length(setdiff(segment, three)) == 2){
        zero <- segment
      } else{
        nine <- segment
      }
      
    }
  }
  
  one <- str_flatten(one, "")
  two <- str_flatten(two, "")
  three <- str_flatten(three, "")
  four <- str_flatten(four, "")
  five <- str_flatten(five, "")
  six <- str_flatten(six, "")
  seven <- str_flatten(seven, "")
  eight <- str_flatten(eight, "")
  nine <- str_flatten(nine, "")
  zero <- str_flatten(zero, "")
  
  dict_list <- c(one,two,three,four,five,six,seven,eight,nine,zero)
  key_list <- c(1,2,3,4,5,6,7,8,9,0)

  dictionary <- data.frame(key_list, dict_list)
  
  
  out_list <- output_chars[[index]] %>%  strsplit("") 
  out_list <- out_list[order(sapply(out_list,'[', 1))]
  
  
  output_list <- data.frame(output = output_chars[[index]])
  output_list %>% left_join(dictionary, by = c("output" = "dict_list")) %>% View

  
  }




}

}
}