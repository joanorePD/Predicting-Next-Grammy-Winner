setwd("/Users/joanorellanarios/Library/CloudStorage/GoogleDrive-joanorellanarios@gmail.com/La meva unitat/Master/Second Semester/Statistical Learning Mod B/SL_Project")
sp_y <- read.csv("Spotify_Youtube.csv", sep=',')

attach(sp_y)

hist(Views)

install.packages(c("httr", "jsonlite", "dplyr", "spotifyr", "plotly", "ggplot2"))

library(httr)
library(jsonlite)

library(dplyr)
library(spotifyr)
library(plotly)
library(ggplot2)

res = GET("https://api.open-notify.org/astros.json")

id <- '74a2687466954d4e92633e8f0f370cce'
secret <- '953a979b453d49c582a46ce3034f818b'

Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)

access_token <- get_spotify_access_token()
















