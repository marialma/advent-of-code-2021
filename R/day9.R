library(tidyverse)

advent <- scan("data/day9_input", what = "", sep = "\n")

chars <- strsplit(advent, "")
vent_map <- sapply(chars, function(x) `length<-`(unlist(x),  max(lengths(chars))))
vent_map_pad = rbind(9, cbind(9, vent_map, 9), 9)

indexing_matrix <- matrix(NA, 102,102)

for (row in 2:(nrow(vent_map_pad)-1)){
  for(col in 2:(ncol(vent_map_pad)-1)){
    current_char <- vent_map_pad[row,col]
    east <- vent_map_pad[row, col + 1]
    west <- vent_map_pad[row, col - 1]
    north <- vent_map_pad[row -1, col]
    south <- vent_map_pad[row + 1, col]
    
    if(current_char < north & current_char < south & current_char < west & current_char < east){
      indexing_matrix[row, col] <- as.numeric(current_char) + 1
    } else {
      indexing_matrix[row,col] <- NA
    }
  }
}

sum(indexing_matrix, na.rm = TRUE)

low_points <- which(!is.na(indexing_matrix), arr.ind = TRUE)


vmp <- apply(vent_map_pad, 1:2, as.numeric)
basins <- matrix(NA, 102,102)
vmp[vmp == 9] <- NA

vent_map_pad <- vmp


for (points_row in 1:nrow(low_points)){
  index_row <- low_points[points_row,1]
  index_col <- low_points[points_row,2]
  
  low_point <- vent_map_pad[index_row, index_col]
  curr_value <- low_point
  
  points_to_check <- data.frame(row = index_row, col = index_col, dir = "cent")

  
  east <- vent_map_pad[index_row, eastcol]
  west <- vent_map_pad[index_row, westcol]
  north <- vent_map_pad[northrow, index_col]
  south <- vent_map_pad[southrow, index_col]
  
# if a neighbor doesn't = 9, then add it to the list to iterate over.
  
  ptc_row = 1
  while(nrow(points_to_check) > 0 ) {
    row <- points_to_check[1,1]
    col <- points_to_check[1,2]
    
    eastcol <- col + 1
    westcol <- col - 1
    northrow <- row -1
    southrow <- row +1
    
    east <- vent_map_pad[row, eastcol]
    west <- vent_map_pad[row, westcol]
    north <- vent_map_pad[northrow, col]
    south <- vent_map_pad[southrow, col] 
    
    if(!is.na(east)) {
      eastdf <- data.frame(row = row, col = eastcol, dir = "east")
      points_to_check <- rbind(points_to_check, eastdf)
      eastcol <- eastcol + 1
      east <- vent_map_pad[row, eastcol]
    } 
    if(!is.na(west)) {
      westdf <- data.frame(row = row, col = westcol, dir = "west")
      points_to_check <- rbind(points_to_check, westdf)
      westcol <- westcol - 1
      west <- vent_map_pad[row, westcol]
    } 
    if(!is.na(south)) {
      southdf <- data.frame(row = southrow, col = col, dir = "south")
      points_to_check <- rbind(points_to_check, southdf)
      southrow <- southrow + 1
      south <- vent_map_pad[southrow, col]
    } 
    if(!is.na(north)) {
      northdf <- data.frame(row = northrow, col = col, dir = "north")
      points_to_check <- rbind(points_to_check, northdf)
      northrow <- northrow - 1
      north <- vent_map_pad[northrow, col]
    } 
    vent_map_pad[row, col] <- NA
    points_to_check[-1,] -> points_to_check
    ptc_row <- ptc_row + 1
    
  }
  
  {
    
    
    
  }  
  
  
}

  