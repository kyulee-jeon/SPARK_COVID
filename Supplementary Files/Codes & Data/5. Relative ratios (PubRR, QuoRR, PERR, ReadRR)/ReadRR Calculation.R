library(tidyverse)

Twitter_count_preprint_with_all_user_information <- readRDS("Twitter_count_preprint_with_all_user_information.R")
Twitter_count_prp_with_all_user_information <- readRDS("Twitter_count_prp_with_all_user_information.R")

readers_all <- Twitter_count_preprint_with_all_user_information %>% rename(preprint_readers = Count)

readers_all_both <- merge(readers_all,
                          Twitter_count_prp_with_all_user_information %>% select(Country2, Count) %>% rename(prp_readers = Count),
                          by = "Country2",
                          all.x = TRUE)

readers_barplot <- readers_all_both %>% filter(!is.na(gdp))

readers_barplot <- readers_barplot %>% 
  mutate(preprint_ratio = preprint_readers / sum(preprint_readers),
         prp_ratio = prp_readers / sum(prp_readers),
         ratio = preprint_ratio / prp_ratio)