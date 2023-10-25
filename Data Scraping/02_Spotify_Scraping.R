# Load the necessary packages
library(httr)
library(jsonlite)
library(dplyr)
library(spotifyr)
library(purrr)
library(tidyr)
library(tidyverse)
library(stringr)
library(stringdist)

setwd() #<- set your current working directory
# Set up your API credentials for Spotify and YouTube
spotify_client_id <- #insert your Spotify API client ID
spotify_client_secret <- #insert your Spotify API client secret

Sys.setenv(SPOTIFY_CLIENT_ID = spotify_client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = spotify_client_secret)

get_track_info <- function(Song, Artist) {
  success <- TRUE
  access_token <- get_spotify_access_token()
  tol <- 0.3
  
  #search_for = paste0(str_replace_all(Song, "\\([^\\)]*\\).*", ""), ' ', Artist)
  search_for = paste0(gsub("\\(.*", "", Song), ' ', gsub("\\(.*", "", Artist))
  
  #print(search_for)
  # Construct the API request URL
  url <- "https://api.spotify.com/v1/search"

  # Set up the API authorization header
  token <- paste0("Bearer ", access_token)
  
  # Set up the API request parameters
  params <- list(
    q = search_for,
    type = 'track',
    limit = 1
  )
  #print(search_for)
  # Send the API request
  response <- httr::GET(url, add_headers(Authorization = token), query = params)
  #print(response)
  if (http_status(response)$category == "Success") {
    # Parse the JSON response
    track_info <- jsonlite::fromJSON(httr::content(response, "text"))
    
    if (length(track_info$tracks$item) != 0) {

      # Extract relevant information from the response
      artist <- track_info$tracks$items$artists[[1]]$name
      artist_id <- track_info$tracks$items$artists[[1]]$id[[1]]
      song <- track_info$tracks$items$name
      #year <- substr(track_info$tracks$items$album$release_date, 1, 4)
      track_id <- track_info$tracks$items$id
      #href <- track_info$tracks$href
      #list_of_top_tracks <- get_artist_top_tracks(artist_id)
      song_bool <- stringdist::stringdist(gsub("\\(.*", "", Song), gsub("\\(.*", "", song), method = "jw") <= tol
      artist_bool <- stringdist::stringdist(gsub("\\(.*", "", Artist), gsub("\\(.*", "", artist), method = "jw") <= tol
      requires_check <- ifelse(song_bool == TRUE & any(artist_bool) == TRUE, FALSE, TRUE)
      
    } else {
      cat("Error in track info Spotify: Song not found", "\n")
      success <- FALSE
    }

  } else {
    cat("Error in track info Spotify:", http_status(response)$message, "\n")
    success <- FALSE
  }
  
  cont <<- cont + 1
  if (success == TRUE) {
    cat("\014")  
    cat("Progress: ", 100*cont/415, " %")
    return(list(artist = artist, song = song, track_id = track_id, requires_check = requires_check))
    #return(c(list(artist = artist, name = name, year = year, track_id = track_id, href = href), get_artist_info(artist_id), get_audio_features(track_id)))
  } else {
    return(NULL)
  }
}

grammyspoti <- read.csv("grammy.csv")

#grammyspoti$rartist <- gsub("(?i)(featuring|&|,|\\.|'|;|\\(|\\?).*", "", grammyspoti$Artist, perl = TRUE)
#grammyspoti$unique <- paste(substr(grammyspoti$Artist, 1, 20), " ", grammyspoti$Song)

cont <- 0

# Apply the get_track_info() function and create a new column 'track_info'
grammyspoti <- grammyspoti %>%
  rowwise() %>%
  mutate(track_info = map2(Song, Artist, get_track_info))

# Unnest the 'track_info' column into separate columns
grammyspoti <- grammyspoti %>%
  unnest_wider(track_info)

grammyspoti <- grammyspoti %>%
  mutate_all(as.character)

write.csv(grammyspoti, "grammy_trackid.csv", row.names=TRUE)
#perform filtering (see pdf for more info) and save the file as final_grammy.csv
