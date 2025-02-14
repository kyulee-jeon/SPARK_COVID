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
library(rjson)
library(pracma)
library(devtools)
library(bibliometrix)
library(bib2df)

medrxiv_list <- readRDS("preprint_final.R")
affiliation_to_country_preprints_final <- readRDS("affiliation_to_country_preprints_final.R")

co_node_edge <- data.frame(matrix(ncol = 3))
colnames(co_node_edge) <- c("from", "to", "weight")

current <- 1
start_time <- Sys.time()
for(name in names(medrxiv_list)) {
  if (current %% 100 == 0) {
    time_taken <- Sys.time() - start_time
    cat(current, "- Time taken:", format(time_taken), "\n")
  }
  current <- current + 1
  
  combined_affs <- unique(unlist(lapply(names(medrxiv_list[[name]]$auth_aff_list), 
                                        function(auth) medrxiv_list[[name]]$auth_aff_list[[auth]][1])))
  
  extracted <- intersect(combined_affs, names(affiliation_to_country_preprints_final))
  extracted2 <- as.vector(sapply(extracted, function(idx) affiliation_to_country_preprints_final[[idx]]))
  
  if (length(extracted2) >= 2) {
    for (i in 1:(length(extracted2) - 1)) {
      for (j in (i + 1):length(extracted2)) {
        existing_row <- co_node_edge[(co_node_edge$from == extracted2[i] & co_node_edge$to == extracted2[j]) |
                                       (co_node_edge$from == extracted2[j] & co_node_edge$to == extracted2[i]), ]
        
        if (nrow(existing_row) > 0) {
          co_node_edge[co_node_edge$from == existing_row$from & co_node_edge$to == existing_row$to, "weight"] <- 
            as.integer(existing_row$weight) + 1
        } else {
          co_node_edge <- rbind(co_node_edge, data.frame(from = extracted2[i], to = extracted2[j], weight = 1))
        }
      }
    }
  }
}

co_node_edge <- co_node_edge[co_node_edge$from != "" & co_node_edge$to != "", ]
co_node_edge <- co_node_edge[-1, ]

Encoding(co_node_edge$from) <- "UTF-8"
co_node_edge$from <- stringi::stri_trans_general(str = co_node_edge$from, id = "Latin-ASCII")
Encoding(co_node_edge$to) <- "UTF-8"
co_node_edge$to <- stringi::stri_trans_general(str = co_node_edge$to, id = "Latin-ASCII")

saveRDS(co_node_edge, "Co_node_edge.R")