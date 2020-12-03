# Q1: Find count of valid passwords given policy and password
# Q2: Policy is now position, one and only 1 of the two positions must contain
#     the given letter. Find count of valid passwords given new policy.

a1 <- 
  read_tsv(here('data/day2_input.txt'), col_names = F) %>% 
    separate(X1, c('policy', 'pass'), sep = ": ") %>% 
    mutate(min = as.numeric(str_extract(policy, '[0-9]+')),
           max = as.numeric(str_remove(str_extract(policy, '-[0-9]+'), '-')),
           let = str_extract(policy, '[a-z]'),
           pass_cnt = str_count(pass, let),
           valid = pass_cnt <= max & pass_cnt >= min) %>% 
    summarize(cnt = sum(valid))
  
a2 <- 
  read_tsv(here('data/day2_input.txt'), col_names = F) %>% 
    separate(X1, c('policy', 'pass'), sep = ": ") %>% 
    mutate(pos1 = as.numeric(str_extract(policy, '[0-9]+')),
           pos2 = as.numeric(str_remove(str_extract(policy, '-[0-9]+'), '-')),
           let = str_extract(policy, '[a-z]'),
           pass_pos1 = str_sub(pass, pos1, pos1),
           pass_pos2 = str_sub(pass, pos2, pos2),
           valid = (pass_pos1 == let | pass_pos2 == let) & 
                    !(pass_pos1 == let & pass_pos2 == let)) %>% 
    summarize(cnt = sum(valid))
  