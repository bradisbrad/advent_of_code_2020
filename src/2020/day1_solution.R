# Q1: Find the two entries that sum to 2020 and return the product
# Q2: Find the three entries that sum to 2020 and return the product

library(tidyverse)
library(here)

data <- read_tsv(here('data/2020/day1_input.txt'), col_names = F)

a1 <- 
  expand_grid(data, 
              rename(data, X2 = X1)) %>% 
  mutate(sm = X1 + X2,
         pd = X1 * X2) %>% 
  filter(sm == 2020) %>% 
  distinct(pd) %>% 
  pull()

a2 <- 
  expand_grid(data, 
              rename(data, X2 = X1),
              rename(data, X3 = X1)) %>% 
  mutate(sm = X1 + X2 + X3,
         pd = X1 * X2 * X3) %>% 
  filter(sm == 2020) %>% 
  distinct(pd) %>% 
  pull()
