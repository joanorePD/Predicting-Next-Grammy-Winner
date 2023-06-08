### Data Preprocessing

#Selecting the relevant variables

data = final_df_n_str


data = data[,c("track_name", "artist_name", "IsWinner", "Year","year", "followers", "acousticness", "danceability", "duration_ms",
               "energy", "instrumentalness", "key", "liveness", "loudness", "mode",
               "tempo", "time_signature", "valence")]

#Merge the two year variable

data$Year[data$Year == "Undefined"] <- data$year[data$Year == "Undefined"]

data = data[,c("track_name","artist_name", "IsWinner", "Year", "followers", "acousticness", "danceability", "duration_ms",
               "energy", "instrumentalness", "key", "liveness", "loudness", "mode",
               "tempo", "time_signature", "valence")]

#Eliminating duplicates

data$track_name == "Closing Time"
data$track_name == "Smells Like Teen Spirit"
data$track_name == "Don't Wanna Fight"

data[914, ]
data[789,]
data[669,]

data = data[-c(669, 789, 914),]

#Creating row names

names = paste0(data$track_name, " - ", data$artist_name)

#Eliminating unusable variables

data = data[,c("IsWinner", "Year", "followers", "acousticness", "danceability", "duration_ms",
               "energy", "instrumentalness", "key", "liveness", "loudness", "mode",
               "tempo", "time_signature", "valence")]

data = cbind(names = names, data)


#Casting variables

data$IsWinner[data$IsWinner == "Winner"] = 1
data$IsWinner[data$IsWinner == "Nominee"] = 1
data$IsWinner[data$IsWinner == "Nothing"] = 0

data$IsWinner = as.integer(data$IsWinner)

data$Year = as.integer(data$Year)

data$mode = as.factor(data$mode)

data$key = as.factor(data$key)

data$IsWinner = as.factor(data$IsWinner)

#Giving row names

summary(data)

summary(data$IsWinner)

# Splitting training and test set

training_size = floor(0.8 * nrow(data))

set.seed(42)

train_ind = sample(seq_len(nrow(data)), size = training_size)

training_set = data[train_ind,]

test_set = data[-train_ind,]

summary(training_set)

#Checking if the ratio is preserved

sum(data$IsWinner == 1)/ sum(data$IsWinner == 0)
sum(training_set$IsWinner == 1)/ sum(training_set$IsWinner == 0)

###############################################################################

# Exploratory Data Analysis




