library(tidyverse)
advent <- readLines(con = "data/day4_input")

# Text parsing time

line_number = 1
list_for_matrix = c()
bingo_list = list()
matrix_number = 1

for(line in advent){
  current_line <- advent[line_number]
  if(line_number == 1) {
    bingo <- strsplit(advent[line_number], ",")
    bingo <- bingo[[1]]
    line_number <- line_number + 2
    
  } else {
    if (is.na(current_line)) {break} 
    
    else if(nchar(current_line) > 0 ){
      current_line <- trimws(current_line, which = c("both"), whitespace = "[ \t\r\n]")
      curr_line <- strsplit(current_line, "\\s+")
      current_line <- curr_line[1][[1]]
      
      list_for_matrix <- append(list_for_matrix, current_line)
    } 
    else if(nchar(current_line) == 0){
      bingo_board <- matrix(as.numeric(list_for_matrix), byrow = TRUE, nrow = 5, ncol = 5)
      list_for_matrix <- c()
      bingo_list[[matrix_number]] <- bingo_board
      matrix_number = matrix_number +1
    } 
    
    line_number <- line_number + 1
  }
}

master_bingo <- bingo_list
# create empty matrix

test_matrices <- lapply(1:length(bingo_list), matrix, data= 0, nrow=5, ncol=5) 

board_number <- 1


# Part 1 solution

for (number in bingo){
  for (board in bingo_list){
    index_to_change <- which(board == number, arr.ind = T) 
    
    if(length(index_to_change) > 0) {
      test_matrices[[board_number]][index_to_change] <- 1
    } 
      board_number = board_number + 1
  }
  board_number = 1
  
  matrix_number = 1
  for (matrix in test_matrices) {
    c <- colSums(matrix)
    r <- rowSums(matrix)
    
    if(is.na(match(5,c)) & is.na(match(5,r))) {
      matrix_number = matrix_number + 1
      next
    } else {
      print(matrix_number)
      first_winner_index <- test_matrices[[matrix_number]]
      first_winner <- bingo_list[[matrix_number]]
      break
    }
  }
  
  if(exists("first_winner")) {
    final_number_1 <- number
    break
  }
}


first_winner[which(first_winner_index == 1, arr.ind = T)] <- 0
sum(first_winner) * as.numeric(final_number_1)





# Part 2 solution

bingo_list <- master_bingo # turning it into a function was a headache so whatever

test_matrices <- lapply(1:length(bingo_list), matrix, data= 0, nrow=5, ncol=5) 

board_number <- 1
matrix_index_all <- c(1:length(bingo_list))

for (number in bingo){
  if(length(matrix_index_all) > 1) {
    for (board in bingo_list){
      index_to_change <- which(board == number, arr.ind = T) 
      
      if(length(index_to_change) > 0) {
        test_matrices[[board_number]][index_to_change] <- 1
        board_number = board_number + 1
      } else {
        board_number = board_number + 1
      }
    }
    board_number = 1
    
    matrix_number = 1
    for (matrix in test_matrices) {
      c <- colSums(matrix)
      r <- rowSums(matrix)
      
      if(!is.na(match(5,c)) | !is.na(match(5,r))){
        matrix_index_all <- matrix_index_all[matrix_index_all != matrix_number]
      } 
      matrix_number = matrix_number + 1
    }
  } else {
    
    board <- bingo_list[[matrix_index_all[1]]]
    indices <- test_matrices[[matrix_index_all[1]]]
    
    index_to_change <- which(board == number, arr.ind = T) 
    
    if(length(index_to_change) > 0) {
      final_number <- number
      
      indices[index_to_change] <- 1
      board[which(indices == 1, arr.ind = T)] <- 0
      
      print(sum(board) * as.numeric(final_number))
      break
      
    } else {next}
    
  }
}

