### Data Preprocessing

install.packages("correlation")

library(ggplot2)
library(correlation)

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

sum(data$Year < 1992)

nrow(data)

data = data[!data$Year < 1992,]



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

data$time_signature = as.factor(data$time_signature)

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

# Relationship between independent variables 

#Correlations 

cor_matrix = cor(training_set[,c(-1, -2, -10, -13, -15)])
corrplot(cor_matrix)
pairs(training_set[,c(-1, -2, -10, -13, -15)], lower.panel = panel.smooth)

summary(data)

install.packages("confintr")
library(confintr)

as.numeric(key)

cramersv(matrix(c(as.numeric(key), as.numeric(mode)), ncol = 2))
cramersv(matrix(c(as.numeric(key), as.numeric(time_signature)), ncol = 2))
cramersv(matrix(c(as.numeric(mode), as.numeric(time_signature)), ncol = 2))


# Partial correlations
correlation(training_set[,c(-1, -2, -10, -13, -15)], partial = TRUE)

ggplot(data = training_set, aes(danceability, valence)) + geom_jitter(color = "blue")
ggplot(data = training_set, aes(loudness, energy)) + geom_jitter(color = "blue")
ggplot(data = training_set, aes(acousticness, energy)) + geom_jitter(color = "blue")


#Weird song veeeeeeeeeeeeeeeeeeeeeeeery long 
which.max(data$duration_ms)
data[504, ]

##############

attach(training_set)

#Checking distributions

par(mfrow= c(2, 5))

hist(followers)
hist(acousticness)
hist(danceability)
hist(duration_ms)
hist(energy)
hist(instrumentalness)
hist(liveness)
hist(loudness)
hist(tempo)
hist(valence)


#Relationship between dependent and independent variables

boxplot(danceability ~ IsWinner)
boxplot(followers ~ IsWinner)
boxplot(acousticness ~ IsWinner)
boxplot(duration_ms ~ IsWinner)
boxplot(energy ~ IsWinner)
boxplot(instrumentalness ~ IsWinner)
boxplot(liveness ~ IsWinner)
boxplot(loudness ~ IsWinner)
boxplot(tempo ~ IsWinner)
boxplot(valence ~ IsWinner)

par(mfrow = c(1, 1))



###############################################################################




