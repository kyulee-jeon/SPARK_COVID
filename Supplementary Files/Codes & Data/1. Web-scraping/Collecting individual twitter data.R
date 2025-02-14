library(tidyverse)
library(httr)
library(urltools)
library(RSelenium)
library(rvest)
library(jsonlite)
library(stringr)
library(dplyr)
library(writexl)
library(rcrossref)
library(usethis)
library(listviewer)
library(rAltmetric)
library(js)
library(bibliometrix)
library(rjson)
library(pracma)
library(devtools)

altmetrics_new <- function(doi = NULL, apikey = NULL, ...) {
  base_url <- "https://api.altmetric.com/v1/"
  request <- httr::GET(paste0(base_url, "doi/", doi))
  
  if (httr::status_code(request) == 404) {
    return(NA)
  } else {
    httr::warn_for_status(request)
    results <- jsonlite::fromJSON(httr::content(request, as = "text"), flatten = TRUE)
    results <- rlist::list.flatten(results)
    class(results) <- "altmetric"
    results
  }
}

Altmetric_ID_preprints <- list()
medrxiv_list_DI <- c()

for (i in seq_along(medrxiv_list)) {
  medrxiv_list_DI <- append(medrxiv_list_DI, medrxiv_list[[i]]$medrxiv_doi)
}

for (i in medrxiv_list_DI) {
  acuna <- altmetrics_new(doi = i)
  
  if (!all(is.na(acuna))) {
    Altmetric_ID_preprints[[i]] <- list(
      altmetric_id = acuna$altmetric_id,
      full_acuna = altmetric_data(acuna)
    )
  }
}

Twitter_text_preprints <- list()

for (nth_preprint in 1:length(Altmetric_ID_preprints)) {
  ID <- Altmetric_ID_preprints[[nth_preprint]]$altmetric_id
  c <- list()
  
  first_page <- GET(url = paste0("https://medrxiv.altmetric.com/details/", ID, "/twitter/page:1")) %>% 
    read_html()
  
  for (i in seq_along(first_page %>% html_nodes(css = "#main-content > div > section > article"))) {
    author_handle <- first_page %>%
      html_node(css = paste0("#main-content > div > section > article:nth-child(", i, ") > a > div > div.author > div.handle")) %>%
      html_text(trim = TRUE)
    
    tweet_text <- first_page %>%
      html_node(css = paste0("#main-content > div > section > article:nth-child(", i, ")")) %>%
      html_text(trim = TRUE)
    
    tweet_count <- sum(gsub(pattern = "^[[:digit:]]+", replacement = "", x = names(c)) == author_handle)
    
    if (author_handle %in% gsub(pattern = "^[[:digit:]]+", replacement = "", x = names(c))) {
      c[[paste0(tweet_count, author_handle)]] <- tweet_text
    } else {
      c[[author_handle]] <- tweet_text
    }
  }
  
  total_tweets <- first_page %>%
    html_node(css = "#main-content > div > div.section-summary > div.text > strong:nth-child(1)") %>%
    html_text(trim = TRUE) %>%
    strtoi()
  
  if (total_tweets <= 100) {
    Twitter_text_preprints[[Altmetric_ID_preprints[[nth_preprint]]$full_acuna$doi]] <- c
    next
  }
  
  total_pages <- first_page %>%
    html_node(css = "#main-content > div > div.post_pagination.top > div.pagination_page_links > a:nth-last-child(2)") %>%
    html_text(trim = TRUE) %>%
    strtoi()
  
  for (j in 2:total_pages) {
    nth_page <- GET(url = paste0("https://medrxiv.altmetric.com/details/", ID, "/twitter/page:", j)) %>% 
      read_html()
    
    for (k in seq_along(nth_page %>% html_nodes(css = "#main-content > div > section > article"))) {
      author_handle <- nth_page %>%
        html_node(css = paste0("#main-content > div > section > article:nth-child(", k, ") > a > div > div.author > div.handle")) %>%
        html_text(trim = TRUE)
      
      tweet_text <- nth_page %>%
        html_node(css = paste0("#main-content > div > section > article:nth-child(", k, ")")) %>%
        html_text(trim = TRUE)
      
      tweet_count <- sum(gsub(pattern = "^[[:digit:]]+", replacement = "", x = names(c)) == author_handle)
      
      if (author_handle %in% gsub(pattern = "^[[:digit:]]+", replacement = "", x = names(c))) {
        c[[paste0(tweet_count, author_handle)]] <- tweet_text
      } else {
        c[[author_handle]] <- tweet_text
      }
    }
  }
  
  Twitter_text_preprints[[Altmetric_ID_preprints[[nth_preprint]]$full_acuna$doi]] <- c
}

saveRDS(Twitter_text_preprints, "Twitter_text_preprints.R")
