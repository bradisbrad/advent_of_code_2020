# Q1: How many trees encountered for a given slope?
# Q2: Product of trees encountered for several slopes?
library(tidyverse)

data <- read_tsv(here('data/2020/day3_input.txt'), col_names = F)

# 31 wide
# str_length(data$X1)

trees_encountered <- function(.data, right, down){
  
  args <- list(right = right,
               down = down)
  pos <- c(1, 1)
  hit <- 0
  while(pos[2] < nrow(data)){
    pos <- c(pos[1]+args$right, pos[2]+args$down)
    adj_pos <- c(ifelse(pos[1] %% 31 == 0, 31, pos[1] %% 31), pos[2])
    hit <- ifelse(str_sub(data[adj_pos[2],1], adj_pos[1], adj_pos[1]) == '#', 
                  hit+1,
                  hit)
  }
  final_pos <- pos
  hit_count <- hit
  
  list(`Final Position` = final_pos,
       `Trees Encountered` = hit_count)
}

a1 <- trees_encountered(data, 3, 1)

a2 <- pmap(list(right  = c(1, 3, 5, 7, 1), 
                down = c(1, 1, 1, 1, 2)),
           trees_encountered, 
           .data = data) %>% 
  map('Trees Encountered') %>% 
  unlist() %>% 
  prod()
