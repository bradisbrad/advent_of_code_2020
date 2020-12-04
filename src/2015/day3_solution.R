#Q1: How many houses get 1+ presents?
#Q2: How many houses get 1+ present with robo-santa?

library(tidyverse)
library(here)

data <- read_lines(here('data/2015/day3_input.txt'))

dir <- tibble(dir = c(0, unlist(str_split(data, ''))))

a1 <- dir %>% 
  mutate(x = case_when(dir == '>' ~ 1,
                       dir == '<' ~ -1,
                       T ~ 0),
         y = case_when(dir == '^' ~ 1,
                       dir == 'v' ~ -1,
                       T ~ 0),
         x = cumsum(x),
         y = cumsum(y)) %>% 
  distinct(x, y) %>% 
  nrow()

robosanta <- dir %>% 
  mutate(row = row_number(),
         x1 = case_when(dir == '>' & row %% 2 == 0 ~ 1,
                        dir == '<' & row %% 2 == 0  ~ -1,
                        T ~ 0),
         y1 = case_when(dir == '^' & row %% 2 == 0  ~ 1,
                        dir == 'v' & row %% 2 == 0  ~ -1,
                        T ~ 0),
         x1 = cumsum(x1),
         y1 = cumsum(y1),
         x2 = case_when(dir == '>' & row %% 2 != 0 ~ 1,
                       dir == '<' & row %% 2 != 0  ~ -1,
                       T ~ 0),
         y2 = case_when(dir == '^' & row %% 2 != 0  ~ 1,
                       dir == 'v' & row %% 2 != 0  ~ -1,
                       T ~ 0),
         x2 = cumsum(x2),
         y2 = cumsum(y2))

a2 <- bind_rows(robosanta %>% 
            select(x = x1, y = y1),
          robosanta %>% 
            select(x = x2, y = y2)) %>% 
  distinct() %>% 
  nrow()
