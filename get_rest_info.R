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

cont <- 0

get_artist_id <- function(track_id) {
  
  access_token <- get_spotify_access_token()
  
  # Construct the API request URL
  url <- paste0("https://api.spotify.com/v1/tracks/", track_id)
  
  # Set up the API authorization header
  token <- paste0("Bearer ", access_token)
  
  # Send the API request
  response <- httr::GET(url, add_headers(Authorization = token))
  
  if (http_status(response)$category == "Success") {
    
    # Parse the JSON response
    track_info <- jsonlite::fromJSON(httr::content(response, "text"))
    
    # Extract relevant information from the response
    artist_id <- track_info$artists$id
    
    return(list(artist_id = artist_id))
    
  } else {
    cat("Error in retrieving audio features:", http_status(response)$message, "\n")
    return(NULL)
  }
}

get_track_name <- function(track_id) {
  
  access_token <- get_spotify_access_token()

  # Construct the API request URL
  url <- paste0("https://api.spotify.com/v1/tracks/", track_id)
  
  # Set up the API authorization header
  token <- paste0("Bearer ", access_token)
  
  # Send the API request
  response <- httr::GET(url, add_headers(Authorization = token))
  
  if (http_status(response)$category == "Success") {
    
    # Parse the JSON response
    track_info <- jsonlite::fromJSON(httr::content(response, "text"))
    
    # Extract relevant information from the response
    track_name <- track_info$name
    year <- as.numeric(substr(track_info$album$release_date, 1, 4))
    return(list(track_name = track_name, year = year))
           
  } else {
    cat("Error in retrieving audio features:", http_status(response)$message, "\n")
    return(NULL)
  }
}

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
    artist_name <- track_info$name
    genres <- track_info$genres
    followers <- track_info$followers$total
    
    return(list(artist_name = artist_name, genres = genres, followers = followers))
  } else {
    cat("Error in retrieving artist info:", http_status(response)$message, "\n")
    return(NULL)
  }
}

get_artist_top_tracks<- function(artist_id) {
  
  access_token <- get_spotify_access_token()
  track_df <- data.frame(matrix(ncol = 3, nrow = 0))
  
  tracks <- list()
  
  # Construct the API request URL
  url <- paste0('https://api.spotify.com/v1/artists/', artist_id, '/top-tracks?market=US')
  
  # Set up the API authorization header
  token <- paste0("Bearer ", access_token)
  
  # Send the API request
  response <- httr::GET(url, add_headers(Authorization = token))
  
  x <- c("IsWinner", "track_id", "artist_id")
  colnames(track_df) <- x
  
  if (http_status(response)$category == "Success") {

    # Parse the JSON response
    track_info <- jsonlite::fromJSON(httr::content(response, "text"))
    # Extract relevant information from the response
    for (track in track_info$tracks$id) {
      track_df[nrow(track_df) + 1,] <- c('Nothing', track, artist_id)
    }
    
    return(track_df)
    
  } else {
    cat("Error in retrieving artist top tracks:", http_status(response)$message, "\n")

    return(NULL)
  }
}

get_total_info <- function(track_id, artist_id) {
  cont <<- cont + 1
  cat("\014")  
  cat("Progress: ", 100*cont/5533, " %")
  if (cont == 4900) {
    Sys.sleep(1000)
  }
  return(c(get_track_name(track_id), get_artist_info(artist_id), get_audio_features(track_id)))
}

list_tracks <- read.csv("final_grammy.csv", sep = ';')

# Apply the get_track() function and create a new column 'track_info'
list_tracks <- list_tracks %>%
  rowwise() %>%
  mutate(artist_id = map(track_id, get_artist_id))

list_tracks$artist_id <- lapply(list_tracks$artist_id, as.list)

list_tracks <- unnest(list_tracks, artist_id)

list_top_tracks <- data.frame()
for (i in 1:nrow(list_tracks)) {
  list_top_tracks <- rbind(list_top_tracks, get_artist_top_tracks(list_tracks$artist_id[[i]]))
}

list_total_tracks <- rbind(list_tracks[c('IsWinner', 'track_id', 'artist_id')], list_top_tracks[c('IsWinner', 'track_id', 'artist_id')])

# Apply the get_track_name
final_df <- list_total_tracks %>%
  rowwise() %>%
  mutate(general = map2(track_id, artist_id, get_total_info))

final_df_n <- final_df
  
for (i in 1:4985) {
  # Access values of each row using the row index 'i'
  names(final_df_n[[4]][[i]])[3] <- 'artist_name'
}

# Unnest the 'track_info' column into separate columns
final_df_n <- final_df_n %>%
  unnest_wider(general, names_sep=NULL)

for (i in 1:nrow(final_df_n)) {
  # Access values of each row using the row index 'i'
  if (is.vector(final_df_n$genres[[i]])) {
    final_df_n$genres[[i]] <- final_df_n$genres[[i]][1]
  }
}

# Find columns with list elements
list_columns <- sapply(final_df_n, is.list)

# Print the column names with list elements
print(names(list_columns)[list_columns])

save(final_df_n, file = "final_df_n.RData")

final_df_n$genres <- sapply(final_df_n$genres, toString)
final_df_n$artist_name <- sapply(final_df_n$artist_name, toString)


# Save dataframe as a CSV file
write.csv(final_df_n, "final_df_n.csv")

# Compute genre frequencies
genre_counts <- table(final_df_n$genres)

# Sort genre frequencies in descending order and take top 10
top_10_genres <- head(sort(genre_counts, decreasing = TRUE), 10)

# Plot bar chart
barplot(top_10_genres, main = "Top 10 Genres", xlab = "Genres", ylab = "Count", las = 2)

