# Load thenecessary packages
library(httr)
library(jsonlite)
library(dplyr)
library(spotifyr)
library(purrr)
library(tidyr)
library(tidyverse)
library(stringr)
library(stringdist)

setwd("/Users/joanorellanarios/Library/CloudStorage/GoogleDrive-joanorellanarios@gmail.com/La meva unitat/Master/Second Semester/Statistical Learning Mod B/SL_Project")

# Set up your API credentials for Spotify and YouTube
spotify_client_id <- '74a2687466954d4e92633e8f0f370cce'
spotify_client_secret <- '953a979b453d49c582a46ce3034f818b'

Sys.setenv(SPOTIFY_CLIENT_ID = spotify_client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = spotify_client_secret)

get_audio_features <- function(track_id) {

  access_token <- get_spotify_access_token()
  
  # Construct the API request URL
  url <- paste0("https://api.spotify.com/v1/audio-features/", track_id)
  
  # Set up the API authorization header
  token <- paste0("Bearer ", access_token)
  
  # Send the API request
  response <- httr::GET(url, add_headers(Authorization = token))
  
  if (http_status(response)$category == "Success") {
    
    # Parse the JSON response
    track_info <- jsonlite::fromJSON(httr::content(response, "text"))
    
    # Extract relevant information from the response
    acousticness <- track_info$acousticness
    danceability <- track_info$danceability
    duration_ms <- track_info$duration_ms
    energy <- track_info$energy
    instrumentalness <- track_info$instrumentalness
    key <- track_info$key
    liveness <- track_info$liveness
    loudness <- track_info$loudness
    mode <- track_info$mode
    tempo <- track_info$tempo
    time_signature <- track_info$time_signature
    valence <- track_info$valence
    
    return(list(acousticness = acousticness, danceability = danceability, duration_ms = duration_ms, 
                energy = energy, instrumentalness = instrumentalness, key = key, liveness = liveness, 
                loudness = loudness, mode = mode, tempo = tempo, time_signature = time_signature, valence = valence))
  } else {
    cat("Error in retrieving audio features:", http_status(response)$message, "\n")
    return(NULL)
  }
}

get_artist_info<- function(artist_id) {

  access_token <- get_spotify_access_token()
  
  # Construct the API request URL
  url <- paste0('https://api.spotify.com/v1/artists/', artist_id)
  
  # Set up the API authorization header
  token <- paste0("Bearer ", access_token)
  
  # Send the API request
  response <- httr::GET(url, add_headers(Authorization = token))
  
  if (http_status(response)$category == "Success") {
    
    # Parse the JSON response
    track_info <- jsonlite::fromJSON(httr::content(response, "text"))
    
    # Extract relevant information from the response
    genres <- track_info$genres
    followers <- track_info$followers$total
    
    return(list(genres = genres, followers = followers))
  } else {
    cat("Error in retrieving artist info:", http_status(response)$message, "\n")
    return(NULL)
  }
}

get_artist_top_tracks<- function(artist_id) {

  access_token <- get_spotify_access_token()
  tracks <- list()

  # Construct the API request URL
  url <- paste0('https://api.spotify.com/v1/artists/', artist_id, '/top-tracks?market=US')

  # Set up the API authorization header
  token <- paste0("Bearer ", access_token)

  # Send the API request
  response <- httr::GET(url, add_headers(Authorization = token))
  
  if (http_status(response)$category == "Success") {
    print('hey')
    
    # Parse the JSON response
    track_info <- jsonlite::fromJSON(httr::content(response, "text"))
    # Extract relevant information from the response
    for (track in track_info$tracks$id) {
      tracks <- append(tracks, track)
    }
    return(tracks)

  } else {
    cat("Error in retrieving artist top tracks:", http_status(response)$message, "\n")
    print('hey')
    
    return(NULL)
  }
}

get_track_info <- function(Song, Artist) {
  print('hey')
  success <- TRUE
  access_token <- get_spotify_access_token()
  tol <- 0.3
  
  search_for = paste0(str_replace_all(Song, "\\([^\\)]*\\)", ""), '+', Artist)
  
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
  print(search_for)
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
      #track_id <- track_info$tracks$items$id
      #href <- track_info$tracks$href
      #list_of_top_tracks <- get_artist_top_tracks(artist_id)
      song_bool <- stringdist::stringdist(Song, song, method = "jw") <= tol
      artist_bool <- stringdist::stringdist(Artist, artist, method = "jw") <= tol
      requires_check <- ifelse(song_bool == TRUE & artist_bool == TRUE, FALSE, TRUE)
      
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
    return(list(artist = artist, song = song, requires_check = requires_check))
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
  mutate(track_info = map(Song, Artist, get_track_info))

# Unnest the 'track_info' column into separate columns
grammyspoti <- grammyspoti %>%
  unnest_wider(track_info)

#compare <- grammyspoti[c('Year', 'Song', 'name', 'Artist', 'artist', 'unique', 'href', 'search_for', 'GrammyAward')]

matches_song <- logical(length(grammyspoti$Song))
matches_artist <- logical(length(grammyspoti$Artist))

# Compare the titles using fuzzy string matching

for (i in 1:length(grammyspoti$Song)) {
  matches_song[i] <- stringdist::stringdist(grammyspoti$Song[i], grammyspoti$name[i], method = "jw") <= 0.4
}

for (i in 1:length(grammyspoti$Artist)) {
  matches_artist[i] <- stringdist::stringdist(grammyspoti$Artist[i], grammyspoti$artist[i], method = "jw") <= 0.4
}

grammyspoti$bool_song <- matches_song
grammyspoti$bool_artist <- matches_artist

#compare$bool <- grepl(compare$Song, compare$name, ignore.case = TRUE)

grammyspoti$bool_total <- ifelse(grammyspoti$bool_song == TRUE & grammyspoti$bool_artist == TRUE, TRUE, FALSE)
#compare <- filter(compare, compare$bool_song == TRUE & compare$bool_artist == TRUE)
#compare <- filter(compare, compare$bool_song == FALSE | compare$bool_artist == FALSE)

