library(tidyverse)

preprint_2nd <- readRDS("preprint_2nd.R")
prp_2nd <- readRDS("prp_2nd.R")
GDP_pop_region_income_2021 <- readRDS("GDP, population, income information of world bank data countries.R")

preprint_final <- merge(
  preprint_2nd %>% rename(Country.Name = country2),
  GDP_pop_region_income_2021 %>% select(-Region),
  by = "Country.Name",
  all.x = TRUE
)

prp_final <- merge(
  prp_2nd %>% rename(Country.Name = country2),
  GDP_pop_region_income_2021 %>% select(-Region),
  by = "Country.Name",
  all.x = TRUE
)

preprint_final <- preprint_final %>% filter(IncomeGroup != "" & !is.na(IncomeGroup))
prp_final <- prp_final %>% filter(IncomeGroup != "" & !is.na(IncomeGroup))

preprint_final$IncomeGroup <- factor(
  preprint_final$IncomeGroup, 
  levels = c("Low income", "Lower middle income", "Upper middle income", "High income"),
  ordered = TRUE
)

prp_final$IncomeGroup <- factor(
  prp_final$IncomeGroup, 
  levels = c("Low income", "Lower middle income", "Upper middle income", "High income"),
  ordered = TRUE
)

subregion_levels <- rev(c(
  "East Asia & Pacific", "South Asia", "Central Asia", "Europe", 
  "Latin America & Caribbean", "North America", 
  "Middle East & North Africa", "Sub-Saharan Africa"
))

preprint_final$subregion <- factor(preprint_final$subregion, levels = subregion_levels, ordered = TRUE)
prp_final$subregion <- factor(prp_final$subregion, levels = subregion_levels, ordered = TRUE)

saveRDS(preprint_final, "preprint_final.R")
saveRDS(prp_final, "prp_final.R")