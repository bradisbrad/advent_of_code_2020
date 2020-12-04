# Q1: What floor do the instructions take Santa?
# Q2: Which character causes Santa to enter the basement?

library(tidyverse)
library(here)

data <- read_lines(here('data/2015/day1_input.txt'))

a1 <- str_count(data, '\\(') - str_count(data, '\\)')

a2 <- 
  tibble(inst = unlist(str_split(data, ''))) %>% 
  mutate(pos = row_number(),
         move = ifelse(inst == '(', 1, -1),
         floor = cumsum(move)) %>% 
  filter(floor == -1) %>% 
  pull(pos) %>% 
  min()
