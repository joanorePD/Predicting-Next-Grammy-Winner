# Load the necessary packages
library(httr)
library(jsonlite)
library(dplyr)
library(spotifyr)
library(purrr)
library(tidyr)
library(tidyverse)

setwd("/Users/joanorellanarios/Library/CloudStorage/GoogleDrive-joanorellanarios@gmail.com/La meva unitat/Master/Second Semester/Statistical Learning Mod B/SL_Project")

# Set up your API credentials for Spotify and YouTube
spotify_client_id <- '74a2687466954d4e92633e8f0f370cce'
spotify_client_secret <- '953a979b453d49c582a46ce3034f818b'
youtube_api_keys <- c("AIzaSyBE6GiIFNCVEgOXYVrl-WLJroSHQzh3sa8", "AIzaSyAmYFIdYbcfLB1fmNduajuxplPqfRf_iDQ",
                      "AIzaSyDerB4q0q572Wryg598WmkcIrpud1YRa40", "AIzaSyBrDUPDZAEkHQv5ObuTFWoYyLk04yPgqZ0",
                      "AIzaSyBNmZbwwSJR_ZiHC_dxJXlMvdZWrUSu2nc", "AIzaSyAG705ADexz9ZRddZ24zct8q36HaVT08kM", 
                      "AIzaSyC4CG-OvO_j0yPyYKgaVSSh6a5mqCt1MzM", "AIzaSyBWjyKfYEeqxhy4paIrLErzPxXtOpShD4c")

spotify_access_token <- get_spotify_access_token()

key_idx <- 1

Sys.setenv(SPOTIFY_CLIENT_ID = spotify_client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = spotify_client_secret)

get_audio_features <- function(track_id) {
  
  access_token <- get_spotify_access_token()
  
  # Construct the API request URL
  url <- paste0("https://api.spotify.com/v1/audio-features/", track_id)
  
  # Set up the API authorization header
  token <- paste0("Bearer ", spotify_access_token)
  
  # Send the API request
  response <- httr::GET(url, add_headers(Authorization = token), query = params)

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

get_video_metrics <- function(video_id) {
  
  api_url <- "https://www.googleapis.com/youtube/v3/videos"
  youtube_api_key <- youtube_api_keys[key_idx]
  
  query <- list(
    key = youtube_api_key,
    part = "statistics",
    id = video_id
  )
  response <- httr::GET(api_url, query = query)
  
  if (http_status(response)$category == "Success") {
    
    metrics <- jsonlite::fromJSON(httr::content(response, "text"))
    
    view_count <- metrics$items$statistics$viewCount
    like_count <- metrics$items$statistics$likeCount
    comment_count <- metrics$items$statistics$commentCount
    
    return(list(view_count = view_count, like_count = like_count, comment_count = comment_count))
    
  } else if (http_status(response)$category == "Forbidden"){
    cat("Error in retrieving video metrics:", http_status(response)$message, "\n")
    cat(key_idx)
    key_idx <<- key_idx + 1
    return(NULL)
  } else {
    cat("Error in retrieving video metrics:", http_status(response)$message, "\n")
    return(NULL)
  }
}

get_track_info <- function(search_for) {
  
  success <- TRUE
  
  # Construct the API request URL
  url <- "https://api.spotify.com/v1/search"
  
  # Set up the API authorization header
  token <- paste0("Bearer ", spotify_access_token)
  
  # Set up the API request parameters
  params <- list(
    q = search_for,
    type = 'track',
    limit = 1
  )
  
  # Send the API request
  response <- httr::GET(url, add_headers(Authorization = token), query = params)

  if (http_status(response)$category == "Success") {
    
    # Parse the JSON response
    track_info <- jsonlite::fromJSON(httr::content(response, "text"))

    # Extract relevant information from the response
    artist <- track_info$tracks$items$artists[[1]]$name
    name <- track_info$tracks$items$name
    year <- substr(track_info$tracks$items$album$release_date, 1, 4)
    track_id <- track_info$tracks$items$id
    
  } else {
    cat("Error in track info Spotify:", http_status(response)$message, "\n")
    success <- FALSE
  }
  # Get YouTube video information
  
  # Construct the API request URL
  url <- "https://www.googleapis.com/youtube/v3/search"
  youtube_api_key <- youtube_api_keys[key_idx]
  
  # Set up the API request parameters
  params <- list(
    key = youtube_api_key,
    q = search_for,
    part = "snippet",
    type = "video",
    maxResults = 1
  )
  
  # Send the API request
  response <- httr::GET(url, query = params)
  
  if (http_status(response)$category == "Success") {
    
    # Parse the JSON response
    video_info <- jsonlite::fromJSON(httr::content(response, "text"))
    
    # Extract relevant information from the response
    title <- video_info$items$snippet$title
    videoId <- video_info$items$id$videoId
    channel <- video_info$items$snippet$channelTitle
    
    #views <- video_info$items[[1]]$statistics$viewCount
  } else {
    cat("Error in track info Youtube:", http_status(response)$message, "\n")
    cat(key_idx)
    success <- FALSE
    key_idx <<- key_idx + 1
  }
  if (success == TRUE) {
    return(c(list(artist = artist, name = name, year = year, track_id = track_id, title = title, videoId = videoId, channel = channel), get_audio_features(track_id), get_video_metrics(videoId)))
  } else if (http_status(response)$category == "Forbidden"){
    cat("Error in retrieving video metrics:", http_status(response)$message, "\n")
    cat(key_idx)
    key_idx <<- key_idx + 1
    return(NULL)
  } else {
    cat("Error in retrieving video metrics:", http_status(response)$message, "\n")
    return(NULL)
  }
}

grammy <- read.csv("grammySongs.csv", sep=',', encoding = 'UTF-8')
grammy$unique <- paste(grammy$Artist, " ", grammy$Name)

grammy <- grammy[!duplicated(grammy$unique), ]

grammy$col[1] <- map(grammy$unique[1], get_track_info)
grammy <- unnest_wider(grammy, col)

# Apply the get_track_info() function to the 'unique' field and create a new column
grammy <- grammy %>%
  mutate(track_info = map(unique, get_track_info))

# Unnest the list column into separate columns
grammy <- grammy %>%
  unnest_wider(track_info)

grammy$view_count = as.numeric(grammy$view_count)
grammy <- grammy[!duplicated(grammy$title), ]

# View the updated dataframe
View(grammy)
