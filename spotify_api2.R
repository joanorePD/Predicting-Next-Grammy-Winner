# Load the necessary packages
library(httr)
library(jsonlite)
library(dplyr)
library(spotifyr)
library(purrr)

# Set up your API credentials for Spotify and YouTube
spotify_client_id <- '74a2687466954d4e92633e8f0f370cce'
spotify_client_secret <- '953a979b453d49c582a46ce3034f818b'
youtube_api_key <- "AIzaSyBNmZbwwSJR_ZiHC_dxJXlMvdZWrUSu2nc"

Sys.setenv(SPOTIFY_CLIENT_ID = spotify_client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = spotify_client_secret)

access_token <- get_spotify_access_token()

# Function to get Spotify track information
get_spotify_track_info <- function(track_id) {
  # Construct the API request URL
  url <- paste0("https://api.spotify.com/v1/tracks/", track_id)
  
  # Set up the API authorization header
  token <- paste0("Bearer ", get_spotify_access_token())
  
  # Send the API request
  response <- httr::GET(url, add_headers(Authorization = token))
  
  # Parse the JSON response
  track_info <- jsonlite::fromJSON(httr::content(response, "text"))
  
  # Extract relevant information from the response
  artist <- track_info$artists$name
  name <- track_info$name
  year <- substr(track_info$album$release_date, 1, 4)
  audio_features <- track_info$audio_features
  
  # Return a list of track information
  return(list(artist = artist, name = name, year = year, audio_features = audio_features))
}

# Function to get YouTube video information
get_youtube_video_info <- function(query) {
  # Construct the API request URL
  url <- "https://www.googleapis.com/youtube/v3/search"
  
  # Set up the API request parameters
  params <- list(
    key = youtube_api_key,
    q = query,
    part = "snippet",
    type = "video",
    maxResults = 1
  )
  
  # Send the API request
  response <- httr::GET(url, query = params)
  
  # Parse the JSON response
  video_info <- jsonlite::fromJSON(httr::content(response, "text"))
  print(video_info)
    # Extract relevant information from the response
  title <- video_info$items$snippet$title
  #views <- video_info$items[[1]]$statistics$viewCount
  print(title)
  # Return a list of video information
  return(list(title = title))
}
  
# Example usage: Get Spotify and YouTube information for a list of songs
song_list <- data.frame(
  track_id = c("11dFghVXANMlKmJXsNCbNl", "11dFghVXANMlKmJXsNCbNl"),
  query = c("Cut To The Feeling", "Cut To The Feeling")
)

song_list <- song_list %>%
  rowwise() %>%
  mutate(spotify_info = list(get_spotify_track_info(track_id)),
         youtube_info = list(get_youtube_video_info(query)))%>%
  ungroup()


# Merge Spotify and YouTube information together
# Unnest spotify_info and youtube_info columns
merged_data <- song_list %>%
  tidyr::unnest_wider(spotify_info, names_sep = "_", names_repair = "universal") %>%
  tidyr::unnest_wider(youtube_info, names_sep = "_", names_repair = "universal")

# Print the merged data
print(merged_data)

