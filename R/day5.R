file <- readLines("data/day5_input")

advent <- as.data.frame(file)

advent <- advent %>% 
  separate(file, c("start", "end"), 
           sep = " -> ") %>% 
  separate(start, c("x1", "y1"), 
           sep = ",") %>% 
  separate(end, c("x2", "y2"), 
           sep = ",") %>% 
  mutate_all(as.numeric) %>% 
  rownames_to_column() 

part_1 <- advent %>% 
  filter(x1 == x2 | y1 == y2) 
part_1$rowname -> not_diag_lines
part_1 <- part_1 %>% select(-rowname)


p1_matrix <- matrix(data = c(0), nrow=max(c(advent$y1, advent$y2)), ncol=max(c(advent$x1, advent$x2)))

temp_matrix_master <- matrix(data = c(0), nrow=max(c(advent$y1, advent$y2)), ncol=max(c(advent$x1, advent$x2)))

temp_matrix <- temp_matrix_master

rn <- 1
for(line in 1:nrow(part_1)){
  
  part_1[rn,1] -> startx
  part_1[rn,2] -> starty
  part_1[rn,3] -> endx
  part_1[rn,4] -> endy
  
  endy - starty -> y_dist
  endx - startx -> x_dist
  
  x_list <- c(startx, endx)
  y_list <- c(starty, endy)
  
  coords <- data.frame(x_list, y_list)
  
  if(x_dist == 0) {
    dir = "horizontal"
    print(dir)
    coords %>% complete(y_list = seq(min(y_list), max(y_list)), 
                        fill = list(x_list = startx)) %>% 
      select(x_list, y_list) -> coords
    
  } else if (y_dist == 0) {
    dir = "vertical"
    print(dir)
    coords %>% complete(x_list = seq(min(x_list), max(x_list)), 
                        fill = list(y_list = starty)) %>% 
      select(x_list, y_list) -> coords
    
  } else {print("damn you fucked up") 
    break}
  
  rn_fill_matrix <- 1
  
  for (row in 1:nrow(coords)) {
    y_coord <- coords[[rn_fill_matrix,1]]  
    x_coord <- coords[[rn_fill_matrix,2]]
    
    temp_matrix[x_coord, y_coord] <- 1
    
    rn_fill_matrix <- rn_fill_matrix + 1
  }
  p1_matrix <- p1_matrix + temp_matrix
  temp_matrix <- temp_matrix_master
  rn <- rn + 1
}

answer_p1 <- which(p1_matrix > 1, arr.ind=TRUE)

part_2 <- advent %>% filter(!rowname %in% not_diag_lines) %>% 
  mutate_all(as.numeric) %>% 
  select(-rowname)

p2_matrix <- matrix(data = c(0), nrow=max(c(advent$y1, advent$y2)), ncol=max(c(advent$x1, advent$x2)))
temp_matrix2 <- temp_matrix_master
rn <- 1
for(line in 1:nrow(part_2)){
  
  part_2[rn,1] -> startx
  part_2[rn,2] -> starty
  part_2[rn,3] -> endx
  part_2[rn,4] -> endy
  
  x_list <- c(startx, endx)
  y_list <- c(starty, endy)
  y_list = seq(y_list[1], y_list[2])
  x_list = seq(x_list[1], x_list[2])
  
  coords = data.frame(x_list, y_list)
  
  rn_fill_matrix <- 1
  
  for (row in 1:nrow(coords)) {
    y_coord <- coords[[rn_fill_matrix,1]]  
    x_coord <- coords[[rn_fill_matrix,2]]
    
    temp_matrix2[x_coord, y_coord] <- 1
    
    rn_fill_matrix <- rn_fill_matrix + 1
  }
  p2_matrix <- p2_matrix + temp_matrix2
  temp_matrix2 <- temp_matrix_master
  rn <- rn + 1
}
ans <- p1_matrix + p2_matrix
final_answer <- which(ans > 1, arr.ind=TRUE)

writeLines(paste("part 1 answer:", nrow(answer_p1), "\npart 2 answer:", nrow(final_answer)))
