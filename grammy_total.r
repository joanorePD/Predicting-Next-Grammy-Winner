# Load the necessary packages
library(httr)
library(jsonlite)
library(dplyr)
library(spotifyr)
library(purrr)
library(tidyr)
library(tidyverse)

get_grammy_total <- function() {
  
  awards <- data.frame(matrix(ncol = 5, nrow = 0))
  
  for (edition in 35:65) {
    
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
    
    # Construct the API request URL (sometimes the endpoint code needs to be updated, just go to the website https://www.grammy.com/awards/62nd-annual-grammy-awards-2019 and get the new one using the network on developer tools)
    url <- paste0("https://www.grammy.com/_next/data/4_vzGRQnqGNjjCD161852/awards/", edition, suffix, "-annual-grammy-awards", year1, ".json")
    params <- list(
      slug = paste0(edition, suffix, "-annual-grammy-awards")
    )
    # Send the API request
    response <- httr::GET(url, query = params)
    
    if (http_status(response)$category == "Success") {
      
      # Parse the JSON response
      grammy_info <- fromJSON(httr::content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
      
      # Extract relevant information from the response
      categories <- grammy_info$pageProps$pageContent$getAwardsYears$hits[[1]]$categoryDetails
      #print(typeof(categories))
      x <- c("Year", "GrammyAward", "Song", "Artist", "IsWinner")
      colnames(awards) <- x
      
      for (category in categories) {
        #print(category$title[1])
        for (nomination in category$nominations) {
          if ((category$title[[1]]$name == 'Best Rock Song')) {
            if (nomination$displayLine2[[1]] != '') {
              if (substr(nomination$displayLine2[[1]][[1]], 1, 1) == '<') {
                rartist <- str_match(nomination$displayLine2[[1]], "<a[^>]*>([^<]*)</a>")
                rartist <- rartist[1, 2]
              } else {
                rartist <- nomination$displayLine2[[1]]
              }
            } else {
              rartist <- str_match(nomination$displayLine3[[1]], "\\(([^\\)]*)\\)")
              rartist <- rartist[1, 2]
            }
            winner <- ifelse(nomination$isWinner == TRUE, 'Winner', 'Nominee')
            #print(rartist[1, 2])
            awards[nrow(awards) + 1,] <- c(year, category$title[[1]]$name, nomination$title[[1]], rartist, winner)
          }
        }
      }
    } else {
      cat("Error in retrieving data:", http_status(response)$message, "\n")
    }
  }
  return(awards)
}

grammyspoti <- get_grammy_total()
#grammyspoti <- grammyspoti[!duplicated(grammyspoti[c(4,5)]),]
write.csv(grammyspoti, "grammy.csv", row.names=TRUE)
