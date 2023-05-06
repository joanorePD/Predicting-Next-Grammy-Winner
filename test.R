setwd("/Users/joanorellanarios/Library/CloudStorage/GoogleDrive-joanorellanarios@gmail.com/La meva unitat/Master/Second Semester/Statistical Learning Mod B/SL_Project")

sp_y <- read.csv("Spotify_Youtube.csv", sep=',')
sp_y$unique <- paste(sp_y$Artist, "_", sp_y$Track)
grammy <- read.csv("grammySongs.csv", sep=',')
grammy$unique <- paste(grammy$Artist, "_", grammy$Name)

library(dplyr)

df <- left_join(sp_y, grammy, by = "unique")

summary(df)

colnames(df)

df = df[c('X.x', 'Artist.x', 'Track', 'Album', 'Album_type', 'Danceability', "Energy", "Key", "Loudness", "Speechiness", "Acousticness", "Instrumentalness", "Liveness", "Valence", "Tempo", "Duration_ms", "Views", "Likes", "Comments", "GrammyAward", "GrammyYear", "Genre")]

df$winner <- ifelse(!is.na(df$GrammyAward),TRUE, FALSE)

audio_f <- df[c('Danceability', "Energy", "Key", "Loudness", "Speechiness", "Acousticness", "Instrumentalness", "Liveness", "Valence", "Tempo")]

cor(audio_f)

