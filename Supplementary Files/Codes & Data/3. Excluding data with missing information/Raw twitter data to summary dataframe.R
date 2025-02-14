library(dplyr)
library(stringi)

twitter_reaction_medrxiv <- readRDS("Twitter_count_preprint.R")

twitter_countries_df_preprint <- data.frame(matrix(ncol = 2, nrow = 0))
for(i in names(twitter_reaction_medrxiv)) {
  if(!is.na(twitter_reaction_medrxiv[[i]]$twitter_geo)) {
    twitter_countries_df_preprint <- aggregate(
      Count ~ Country,
      data = rbind(
        twitter_countries_df_preprint,
        twitter_reaction_medrxiv[[i]]$twitter_geo %>% select("Country", "Count")
      ),
      FUN = sum
    )
  }
}

Encoding(twitter_countries_df_preprint[, "Country"]) <- "UTF-8"
twitter_countries_df_preprint$Country <- stringi::stri_trans_general(str = twitter_countries_df_preprint$Country, id = "Latin-ASCII")
saveRDS(twitter_countries_df_preprint, "Twitter_count_preprint.R")

twitter_reaction_peer_all <- readRDS("Twitter_count_prp.R")

twitter_countries_df_prp <- data.frame(matrix(ncol = 2, nrow = 0))
for(i in names(twitter_reaction_peer_all)[!is.na(names(twitter_reaction_peer_all))]) {
  if(!is.na(twitter_reaction_peer_all[[i]]$twitter_geo)) {
    twitter_countries_df_prp <- aggregate(
      Count ~ Country,
      data = rbind(
        twitter_countries_df_prp,
        twitter_reaction_peer_all[[i]]$twitter_geo %>% select("Country", "Count")
      ),
      FUN = sum
    )
  }
}

Encoding(twitter_countries_df_prp[, "Country"]) <- "UTF-8"
twitter_countries_df_prp$Country <- stringi::stri_trans_general(str = twitter_countries_df_prp$Country, id = "Latin-ASCII")
saveRDS(twitter_countries_df_prp, "Twitter_count_prp.R")