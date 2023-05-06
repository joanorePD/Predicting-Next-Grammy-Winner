setwd("/Users/joanorellanarios/Library/CloudStorage/GoogleDrive-joanorellanarios@gmail.com/La meva unitat/Master/Second Semester/Statistical Learning Mod B/SL_Project")

install.packages("rjson")
library("rjson")

sp_y <- read.csv("Spotify_Youtube.csv", sep=',')
grammy <- fromJSON(file = "grammy.json", flatten=TRUE)

json_data_frame <- as.data.frame(grammy)

attach(sp_y)

hist(Views)

summary(sp_y)










