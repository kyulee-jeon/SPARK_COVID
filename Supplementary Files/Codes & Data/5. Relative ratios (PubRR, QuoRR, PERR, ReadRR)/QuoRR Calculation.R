library(tidyverse)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggtext)
library(reshape2)

twitter_count_preprint <- readRDS("Twitter_count_preprint.R")
twitter_count_prp <- readRDS("Twitter_count_prp.R")
twitter_count_preprint_with_country <- readRDS("Twitter_count_preprint_with_all_paper_information.R")
twitter_count_prp_with_country <- readRDS("Twitter_count_prp_with_all_paper_information.R")


twitter_count_preprint_with_country$count <- as.integer(twitter_count_preprint_with_country$count)
twitter_count_preprint <- twitter_count_preprint_with_country %>%
  group_by(Country.Name) %>%
  summarise(preprint_tweet = sum(count),
            gdp = first(`2021_gdp`),
            population = first(`2021_population`),
            IncomeGroup = first(IncomeGroup), 
            subregion = first(subregion)) %>%
  as.data.frame()

twitter_count_prp_with_country$count <- as.integer(twitter_count_prp_with_country$count)
twitter_count_prp <- twitter_count_prp_with_country %>%
  group_by(Country.Name) %>%
  summarise(prp_tweet = sum(count),
            gdp = first(`2021_gdp`),
            population = first(`2021_population`),
            IncomeGroup = first(IncomeGroup), 
            subregion = first(subregion)) %>%
  as.data.frame()

twitter_count_all <- merge(twitter_count_preprint, 
                           twitter_count_prp %>% select("Country.Name", "prp_tweet"), 
                           by = "Country.Name",
                           all.x = FALSE,
                           all.y = FALSE)


twitter_count_all$subregion <- factor(twitter_count_all$subregion ,  levels =rev(c("East Asia & Pacific",
                                                                                   "South Asia",
                                                                                   "Central Asia", 
                                                                                   "Europe",
                                                                                   "Latin America & Caribbean",
                                                                                   "North America",
                                                                                   "Middle East & North Africa", 
                                                                                   "Sub-Saharan Africa")), ordered = TRUE)

twitter_count_all$preprint_ratio <- twitter_count_all$preprint_tweet/sum(twitter_count_preprint$preprint_tweet) 
twitter_count_all$prp_ratio <- twitter_count_all$prp_tweet/sum(twitter_count_prp$prp_tweet)
twitter_count_all$ratio<- twitter_count_all$preprint_ratio/ twitter_count_all$prp_ratio

twitter_count_all <- twitter_count_all[!is.na(twitter_count_all$gdp), ]
twitter_count_all <- twitter_count_all[!is.na(twitter_count_all$subregion), ]
