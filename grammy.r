# Load the necessary packages
library(httr)
library(jsonlite)
library(dplyr)
library(spotifyr)
library(purrr)
library(tidyr)
library(tidyverse)

get_grammy <- function(edition) {
  
  suffix <- 'th'
  
  if (edition > 13 | edition < 4) {
    if (substr(edition, nchar(edition), nchar(edition)) == 1) {
      suffix <- 'st'
    } else if (substr(edition, nchar(edition), nchar(edition)) == 2) {
      suffix <- 'nd'
    } else if(substr(edition, nchar(edition), nchar(edition)) == 3) {
      suffix <- 'rd'
    }
  }
  
  year <- 1957 + edition
  year1 <- paste0('-', year)
  
  if (year < 2017) {
    year1 <- ''
  }
  
  # Construct the API request URL
  url <- paste0("https://www.grammy.com/_next/data/wamWA3jCekTtdI96fdeIU/awards/", edition, suffix, "-annual-grammy-awards", year1, ".json")
  cat(url)
  params <- list(
    slug = paste0(edition, suffix, "-annual-grammy-awards")
  )
  
  # Send the API request
  response <- httr::GET(url, add_headers(Authorization = token), query = params)
  
  if (http_status(response)$category == "Success") {
    
    # Parse the JSON response
    grammy_info <- fromJSON(httr::content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    
    # Extract relevant information from the response
    categories <- grammy_info$pageProps$pageContent$getAwardsYears$hits[[1]]$categoryDetails
    #print(typeof(categories))
    awards <- data.frame(matrix(ncol = 5, nrow = 0))
    x <- c("Year", "GrammyAward", "Song", "Artist", "IsWinner")
    colnames(awards) <- x
    
    for (category in categories) {
      #print(category$title[1])
      for (nomination in category$nominations) {
        cat(c(year, category$title[[1]]$name, nomination$title[[1]], nomination$creditedArtists[[1]]$title, nomination$isWinner))
        if (grepl('Song', category$title[[1]]$name, fixed = TRUE) | grepl('Record ', category$title[[1]]$name, fixed = TRUE)) {
          awards[nrow(awards) + 1,] <- c(year, category$title[[1]]$name, nomination$title[[1]], nomination$creditedArtists[[1]]$title, nomination$isWinner)
        }
      }
    }
    return(awards)
    
  } else {
    cat("Error in retrieving audio features:", http_status(response)$message, "\n")
    return(NULL)
  }
}

