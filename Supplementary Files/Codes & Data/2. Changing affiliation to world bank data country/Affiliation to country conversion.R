library(stringr)
library(stringi)
library(qdap)
library(dplyr)

load("countries_list.rda")
load("states_list.rda")
load("universities_list.rda")

cities_list <- read.csv("https://pkgstore.datahub.io/core/world-cities/world-cities_csv/data/6cc66692f0e82b18216a48443b6b95da/world-cities_csv.csv")

countries <- countries_list
Encoding(countries[, "name"]) <- "UTF-8"
countries$country_name <- countries$name
countries$name <- tolower(countries$name)
countries$name <- stringi::stri_trans_general(str = countries$name, id = "Latin-ASCII")
countries$name <- paste0('\\b' ,countries$name, '\\b')

universities <- universities_list
Encoding(universities[, "university_name"]) <- "UTF-8"
universities$university_name <- tolower(universities$university_name)
universities$university_name <- stringi::stri_trans_general(str = universities$university_name, id = "Latin-ASCII")

states <- states_list
Encoding(states[, "name"]) <- "UTF-8"
states$name <- tolower(states$name)
states$name <- stringi::stri_trans_general(str = states$name, id = "Latin-ASCII")
states$name <- paste0('\\b' ,states$name, '\\b')

cities <- cities_list
Encoding(cities[, "name"]) <- "UTF-8"
Encoding(cities[, "country"]) <- "UTF-8"
Encoding(cities[, "subcountry"]) <- "UTF-8"

cities$name <- tolower(cities$name)
cities$name <- stringi::stri_trans_general(str = cities$name, id = "Latin-ASCII")
cities$name <- paste0('\\b' ,cities$name, '\\b')

cities$subcountry <- tolower(cities$subcountry)
cities$subcountry <- stringi::stri_trans_general(str = cities$subcountry, id = "Latin-ASCII")
cities$subcountry <- paste0('\\b' ,cities$subcountry, '\\b')

cities$country <- stringi::stri_trans_general(str = cities$country, id = "Latin-ASCII")

detecting_country_country <- function(x) {
  text <- tolower(gsub('[[:digit:]]+|[[:punct:] ]+', ' ', stringi::stri_trans_general(str = x, id = "Latin-ASCII")))
  text <- qdap::rm_stopwords(text, separate = FALSE)
  text <- gsub('centre|university', '', text)
  return(toString(unique(countries[stringr::str_detect(text, countries$name),]$country_name)))
}

detecting_country_university <- function(x) {
  text <- tolower(gsub('[[:digit:]]+|[[:punct:] ]+', ' ', stringi::stri_trans_general(str = x, id = "Latin-ASCII")))
  return(toString(unique(universities[stringr::str_detect(text, universities$university_name),]$country_name)))
}

detecting_country_state <- function(x) {
  text <- tolower(gsub('[[:digit:]]+|[[:punct:] ]+', ' ', stringi::stri_trans_general(str = x, id = "Latin-ASCII")))
  text <- qdap::rm_stopwords(text, separate = FALSE)
  text <- gsub('centre|university', '', text)
  return(toString(unique(states[stringr::str_detect(text, states$name),]$country_name)))
}

detecting_country_city <- function(x) {
  text <- tolower(gsub('[[:digit:]]+|[[:punct:] ]+', ' ', stringi::stri_trans_general(str = x, id = "Latin-ASCII")))
  text <- qdap::rm_stopwords(text, separate = FALSE)
  text <- gsub('centre|university', '', text)
  return(toString(unique(cities[stringr::str_detect(text, cities$name),]$country)))
}

detecting_country_subcountry <- function(x) {
  text <- tolower(gsub('[[:digit:]]+|[[:punct:] ]+', ' ', stringi::stri_trans_general(str = x, id = "Latin-ASCII")))
  text <- qdap::rm_stopwords(text, separate = FALSE)
  text <- gsub('centre|university', '', text)
  return(toString(setdiff(unique(cities[stringr::str_detect(text, cities$subcountry),]$country), c("Monaco", "Thua Thien-Hue,1580240", "Al Bayda',79836"))))
}


medrxiv_list <- readRDS("preprint_1st.R")
Affiliations_preprints <- unique(unlist(lapply(medrxiv_list, function(entry) {
    if (!is.null(entry$auth_aff_list)) {
      unlist(entry$auth_aff_list)
    } else {
      NULL
    }
  })))




Affiliation_to_country_preprints <- list()
for(i in Affiliations_preprints){
  detect1 <- detecting_country_country(i)
  detect2 <- detecting_country_university(i)
  detect3 <- detecting_country_state(i)
  detect4 <- detecting_country_city(i)
  detect5 <- detecting_country_subcountry(i)
  
  if((detect1 !="")){
    Affiliation_to_country_preprints[[i]] <- detect1
  }else{
    if((detect2 !="")){
      Affiliation_to_country_preprints[[i]] <- detect2
    }else{
      if((detect3 !="")){
        Affiliation_to_country_preprints[[i]] <- detect3
      }else{
        if((detect4 !="")){
          Affiliation_to_country_preprints[[i]] <- detect4
        }else{
          if((detect5 !="")){
            Affiliation_to_country_preprints[[i]] <- detect5
          }else{
            next
          }
      }
    }
    }  
  }
}

saveRDS(Affiliation_to_country_preprints, "affiliation_to_country_preprints_final.R")
