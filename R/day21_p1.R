# rolling the die

player_1 <- 9
player_2 <- 3
score_1 <- 0
score_2 <- 0

n <- 1

dice_roll_count <- 0

while(score_1 < 1000 & score_2 < 1000) {
  
increase <- n:(n+2)
increase[increase>100] <- increase[increase>100] %% 100

inc1 <- sum(increase)

new_position <- player_1 + inc1 
new_position <- new_position %% 10
if(new_position == 0) {new_position <- 10}
score_1 <- score_1 + new_position
player_1 <- new_position

dice_roll_count <- dice_roll_count + 3
if(score_1 >= 1000) {
  break}

player_2_roll = n+3
increase <- player_2_roll:(player_2_roll+2)
increase[increase>100] <- increase[increase>100] %% 100

inc2 <- sum(increase)
new_pos_2 <- player_2 + inc2
new_pos_2 <- new_pos_2 %% 10 
if(new_pos_2 == 0) {new_pos_2 <- 10}

score_2 <- score_2 + new_pos_2
player_2 <- new_pos_2

n <- (player_2_roll+3) %% 100 
dice_roll_count <- dice_roll_count + 3
}

