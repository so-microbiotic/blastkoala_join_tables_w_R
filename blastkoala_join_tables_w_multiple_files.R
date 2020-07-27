ko_full_join <- ko_full_join %>% mutate(count_sum = rowSums(ko_full_join[,3:14], na.rm = TRUE))

ko_full_join <- ko_full_join %>% 
  mutate(count_sum = rowSums(ko_full_join[,3:14], na.rm = TRUE)) %>% 
  arrange(desc(count_sum)) %>%
  filter(count_sum > 9) %>%
  ggplot(., aes(KO, fill = ))

tko_full_join = t(ko_full_join)