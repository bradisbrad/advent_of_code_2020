#Q1: Total square feet of wrapping paper to order?
#Q2: How much ribbon is needed?

library(tidyverse)
library(here)

data <- read_delim(here('data/2015/day2_input.txt'),
                   delim = 'x', 
                   col_names = c('l', 'w', 'h'))

a1 <- data %>% 
  mutate(sa = 2*l*w + 2*w*h + 2*h*l) %>% 
  rowwise() %>% 
  mutate(slack = min(l*w, w*h, h*l),
         paper = sa+slack) %>% 
  ungroup() %>% 
  summarize(total = sum(paper))

a2 <- data %>% 
  mutate(bow = l*w*h) %>% 
  rowwise() %>% 
  mutate(wrap = sort(c(l, w, h))[1] * 2 + sort(c(l, w, h))[2] * 2,
         ribbon = bow + wrap) %>% 
  ungroup() %>% 
  summarize(total = sum(ribbon))
