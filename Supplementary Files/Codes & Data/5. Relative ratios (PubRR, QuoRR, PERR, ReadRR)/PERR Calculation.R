library(tidyverse)

mem_prp <- readRDS("Twitter_count_prp_with_all_paper_information (Demographic).R")
mem_preprint <- readRDS("Twitter_count_preprint_with_all_paper_information (Demographic).R")

both_read <- intersect(mem_prp$Country.Name, mem_preprint$Country.Name)

mem_preprint <- mem_preprint %>% filter(Country.Name %in% both_read)
mem_prp <- mem_prp %>% filter(Country.Name %in% both_read)

mem_all <- merge(mem_preprint, mem_prp, by = "Country.Name")
mem_all$ratio <- mem_all$mem_percent.x / mem_all$mem_percent.y