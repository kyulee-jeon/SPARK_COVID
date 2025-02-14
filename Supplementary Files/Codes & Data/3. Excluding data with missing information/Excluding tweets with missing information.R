library(tidyverse)

twitter_count_preprint <- readRDS("Twitter_count_preprint.R")
twitter_count_prp <- readRDS("Twitter_count_prp.R")
GDP_pop_region_income_2021 <- readRDS("GDP, population, income information of world bank data countries.R")

preprint_final <- readRDS("preprint_final.R")
prp_final <- readRDS("prp_final.R")


twitter_count_preprint_with_country <- merge(twitter_count_preprint,
                                             preprint_final,
                                             by = "doi") 


twitter_count_prp_with_country <- merge(twitter_count_prp,
                                        prp_final,
                                        by = "doi")


twitter_count_preprint_with_country$count <- as.integer(twitter_count_preprint_with_country$count)
twitter_count_prp_with_country$count <- as.integer(twitter_count_prp_with_country$count)

saveRDS(twitter_count_preprint_with_country, "Twitter_count_preprint_with_all_paper_information.R")
saveRDS(twitter_count_prp_with_country, "Twitter_count_prp_with_all_paper_information.R")