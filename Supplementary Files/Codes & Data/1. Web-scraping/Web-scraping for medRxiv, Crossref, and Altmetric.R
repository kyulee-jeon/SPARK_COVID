library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(jsonlite)
library(altmetric)
library(rcrossref)
library(rlist)

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


medrxiv_list <- list()

for(paper_link in medrxiv_links) {
  res_html <- GET(url = paper_link) %>% read_html()
  
  peer_review <- res_html %>%
    html_node(css = "div.highwire-cite-metadata>div>i") %>%
    html_text(trim = TRUE)
  
  peer_review_doi <- if(is.na(peer_review)) { NA } else {
    res_html %>%
      html_node(css = "div.highwire-cite-metadata>div>a") %>%
      html_text(trim = TRUE)
  }
  
  res_html_info <- GET(url = paste0(paper_link, ".article-info")) %>% read_html()
  
  medrxiv_doi <- res_html_info %>%
    html_node(css = "div.field-items > div > a") %>%
    html_text(trim = TRUE) %>%
    str_extract(pattern = "[[:digit:]].+")
  
  date <- res_html_info %>%
    html_node(css = "div>div>ul>li.published") %>%
    html_text(trim = TRUE)
  
  author <- res_html_info %>%
    html_node(css = "ol.contributor-list > li > span") %>%
    html_text(trim = TRUE)
  
  affiliation <- res_html_info %>%
    html_node(css = "address") %>%
    html_text(trim = TRUE)
  
  citation_number <- tryCatch({
    res <- cr_citation_count(doi = medrxiv_doi)
    if (!is.null(res$count)) res$count else NA
  }, error = function(e) NA)
  
  
  subject_area <- res_html_info %>%
    html_node(css="div.panel-pane.pane-highwire-article-collections > div > div > div > ul > li > span > a") %>%
    html_text(trim=TRUE)
  
  title <- res_html_info %>%
    html_node(css="#page-title") %>%
    html_text(trim=TRUE)
  
  metas_with_name <- res_html_info %>%
    html_nodes(xpath = '//meta[@name]') %>%
    html_attr('name')
  
  auths <- res_html_info %>%
    html_nodes(xpath = '//meta[@name="citation_author"]') %>%
    html_attr('content')
  
  affs <- res_html_info %>%
    html_nodes(xpath = '//meta[@name="citation_author_institution"]') %>%
    html_attr('content')
  
  auth_indexes <- which(metas_with_name[metas_with_name %in% c("citation_author","citation_author_institution")] == "citation_author")
  aff_indexes <- which(metas_with_name[metas_with_name %in% c("citation_author","citation_author_institution")] == "citation_author_institution")
  
  aff_count_for_each_auth <- c()
  if(length(auths) ==1){
    aff_count_for_each_auth <- length(affs)
  }else{
    for(nth_author in 1:length(auths)){
      if(nth_author == length(auths)){
        last_num <- length(affs)-sum(aff_count_for_each_auth)
        aff_count_for_each_auth <- append(aff_count_for_each_auth,last_num)
        
      }else{aff_count_for_each_auth <- append(aff_count_for_each_auth,
                                              sum((aff_indexes >=auth_indexes[nth_author]) & (aff_indexes<= auth_indexes[nth_author+1])))
      }
    }
  }
  
  auth_aff_list <- list()
  left_affs <- affs
  for(i in 1:length(auths)){
    auth_aff_list[[auths[i]]] <- left_affs[1:(aff_count_for_each_auth[i])]
    
    if(i!=length(affs)){
      left_affs <- left_affs[(aff_count_for_each_auth[i]+1):length(left_affs)]
    }
  }
  
  
  medrxiv_list[[medrxiv_doi]] <- list(
    peer_review = peer_review,
    peer_review_doi = peer_review_doi,
    medrxiv_doi = medrxiv_doi,
    date = date,
    author = author,
    affiliation = affiliation, 
    citation_number = citation_number,
    twitter_geo = NA,
    twitter_demo = NA,
    mendeley_geo = NA,
    mendeley_demo = NA,
    attention_score = NA,
    subject_area = subject_area,
    title = title,
    auth_aff_list = auth_aff_list
  )
  
  if(all(which(medrxiv_links == paper_link) %% 10 == 0)) Sys.sleep(0.1)
}


for(i in seq_along(medrxiv_list)) {
  acuna <- altmetrics_new(doi = medrxiv_list[[i]]$medrxiv_doi)
  
  if(is.na(acuna)) {
    medrxiv_list[[medrxiv_list[[i]]$medrxiv_doi]]$twitter_geo <- NA
    medrxiv_list[[medrxiv_list[[i]]$medrxiv_doi]]$twitter_demo <- NA
    medrxiv_list[[medrxiv_list[[i]]$medrxiv_doi]]$attention_score <- NA
    next
  }
  
  altmetric_acuna <- altmetric_data(acuna)
  
  altmetric_html <- GET(url = paste0("https://www.altmetric.com/details/", 
                                     altmetric_acuna$altmetric_id)) %>% read_html()
  
  medrxiv_list[[medrxiv_list[[i]]$medrxiv_doi]]$twitter_geo <- altmetric_html %>%
    html_node(css = "div.table-wrapper.geo") %>%
    html_table(fill = TRUE) %>% as.data.frame()
  
  medrxiv_list[[medrxiv_list[[i]]$medrxiv_doi]]$twitter_demo <- altmetric_html %>%
    html_node(css = "div.table-wrapper.users") %>%
    html_table(fill = TRUE) %>% as.data.frame()
  
  medrxiv_list[[medrxiv_list[[i]]$medrxiv_doi]]$attention_score <- as.numeric(altmetric_acuna$score)
  
  if(length(altmetric_html %>% html_nodes(css = "div.table-wrapper.geo")) > 1) {
    medrxiv_list[[medrxiv_list[[i]]$medrxiv_doi]]$mendeley_geo <- altmetric_html %>% 
      html_nodes(css = "div.table-wrapper.geo") %>%
      '[['(2) %>%
      html_table(fill = TRUE) %>% as.data.frame()
    
    medrxiv_list[[medrxiv_list[[i]]$medrxiv_doi]]$mendeley_demo <- altmetric_html %>% 
      html_nodes(css = "div.table-wrapper.users") %>%
      '[['(2) %>%
      html_table(fill = TRUE) %>% as.data.frame()
  }
}

saveRDS(medrxiv_list, "preprint_1st.R")
