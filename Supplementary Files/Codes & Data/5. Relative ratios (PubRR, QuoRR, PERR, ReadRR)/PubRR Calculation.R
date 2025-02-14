library(tidyverse)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggtext)
library(reshape2)

preprint_final <- readRDS("preprint_final.R")
prp_final <- readRDS("prp_final.R")
GDP_pop_region_income_2021 <- readRDS("GDP, population, income information of world bank data countries.R")

papers_all <- preprint_final %>%
  group_by(Country.Name) %>%
  summarise(subregion = first(subregion), preprint_count = n()) %>%
  as.data.frame()

papers_all <- merge(
  papers_all,
  prp_final %>%
    group_by(Country.Name) %>%
    summarise(subregion = first(subregion), prp_count = n()) %>%
    as.data.frame(),
  by = "Country.Name"
)

papers_all <- papers_all %>%
  select(-subregion.y) %>%
  rename(subregion = subregion.x)

papers_all <- merge(
  papers_all,
  GDP_pop_region_income_2021 %>% select(-Region),
  by = "Country.Name",
  all.x = TRUE
)

colnames(papers_all)[5:6] <- c("population", "gdp")

subregion_levels <- rev(c(
  "East Asia & Pacific", "South Asia", "Central Asia", "Europe", 
  "Latin America & Caribbean", "North America", 
  "Middle East & North Africa", "Sub-Saharan Africa"
))

papers_all$subregion <- factor(
  papers_all$subregion, levels = subregion_levels, ordered = TRUE
)

papers_all$IncomeGroup <- factor(
  papers_all$IncomeGroup, 
  levels = rev(c("High income", "Upper middle income", "Lower middle income", "Low income"))
)

papers_all$preprint_ratio <- 
  papers_all$preprint_count / nrow(preprint_final)

papers_all$prp_ratio <- 
  papers_all$prp_count / nrow(prp_final)

papers_all$ratio <- 
  papers_all$preprint_ratio / papers_all$prp_ratio

papers_all <- papers_all %>%
  filter(!is.na(gdp), !is.na(IncomeGroup))
