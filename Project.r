### Data Preprocessing

load("final_df_n_str.RData")

install.packages("correlation")
install.packages("confintr")

library(confintr)
library(ggplot2)
library(correlation)
library(corrplot)

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

attach(training_set)

# Correlations between continuous variables
cor_matrix = cor(training_set[,c(-1, -2, -10, -13, -15)])
corrplot(cor_matrix)
pairs(training_set[,c(-1, -2, -10, -13, -15)], lower.panel = panel.smooth)

# Association measure for categorical variables (Cramer's V is a normalized 
# version of the chi-square statistics)
cramersv(matrix(c(as.numeric(key), as.numeric(mode)), ncol = 2))
cramersv(matrix(c(as.numeric(key), as.numeric(time_signature)), ncol = 2))
cramersv(matrix(c(as.numeric(mode), as.numeric(time_signature)), ncol = 2))

# Association between continuous and categorical variables

# Key

fol_key.aov <- aov(followers ~ key)
summary(fol_key.aov) # SIGNIFICANT

aco_key.aov <- aov(acousticness ~ key)
summary(aco_key.aov)

dan_key.aov <- aov(danceability ~ key)
summary(dan_key.aov) # SIGNIFICANT

dur_key.aov <- aov(duration_ms ~ key)
summary(dur_key.aov)

ene_key.aov <- aov(energy ~ key)
summary(ene_key.aov) # SIGNIFICANT

ins_key.aov <- aov(instrumentalness ~ key)
summary(ins_key.aov)

liv_key.aov <- aov(liveness ~ key)
summary(liv_key.aov)

loud_key.aov <- aov(loudness ~ key)
summary(loud_key.aov)

tem_key.aov <- aov(tempo ~ key)
summary(tem_key.aov)

val_key.aov <- aov(valence ~ key)
summary(val_key.aov)

# Mode

fol_mode.aov <- aov(followers ~ mode)
summary(fol_mode.aov) # SIGNIFICANT

aco_mode.aov <- aov(acousticness ~ mode)
summary(aco_mode.aov) # SIGNIFICANT

dan_mode.aov <- aov(danceability ~ mode)
summary(dan_mode.aov)

dur_mode.aov <- aov(duration_ms ~ mode)
summary(dur_mode.aov)

ene_mode.aov <- aov(energy ~ mode)
summary(ene_mode.aov) # SIGNIFICANT

ins_mode.aov <- aov(instrumentalness ~ mode)
summary(ins_mode.aov)

liv_mode.aov <- aov(liveness ~ mode)
summary(liv_mode.aov)

loud_mode.aov <- aov(loudness ~ mode)
summary(loud_mode.aov) # SIGNIFICANT

tem_mode.aov <- aov(tempo ~ mode)
summary(tem_mode.aov)

val_mode.aov <- aov(valence ~ mode)
summary(val_mode.aov)

# Time signature

fol_time.aov <- aov(followers ~ time_signature)
summary(fol_time.aov)

aco_time.aov <- aov(acousticness ~ time_signature)
summary(aco_time.aov) # SIGNIFICANT

dan_time.aov <- aov(danceability ~ time_signature)
summary(dan_time.aov) # SIGNIFICANT

dur_time.aov <- aov(duration_ms ~ time_signature)
summary(dur_time.aov) # SIGNIFICANT

ene_time.aov <- aov(energy ~ time_signature)
summary(ene_time.aov) # SIGNIFICANT

ins_time.aov <- aov(instrumentalness ~ time_signature)
summary(ins_time.aov) # SIGNIFICANT

liv_time.aov <- aov(liveness ~ time_signature)
summary(liv_time.aov)

loud_time.aov <- aov(loudness ~ time_signature)
summary(loud_time.aov) # SIGNIFICANT

tem_time.aov <- aov(tempo ~ time_signature)
summary(tem_time.aov) # SIGNIFICANT

val_time.aov <- aov(valence ~ time_signature)
summary(val_time.aov) # SIGNIFICANT


# Partial correlations
correlation(training_set[,c(-1, -2, -10, -13, -15)], partial = TRUE)

# Plots of variables with the largest partial correlation
ggplot(data = training_set, aes(danceability, valence)) + geom_jitter(color = "blue")
ggplot(data = training_set, aes(loudness, energy)) + geom_jitter(color = "blue")
ggplot(data = training_set, aes(acousticness, energy)) + geom_jitter(color = "blue")


#Weird song veeeeeeeeeeeeeeeeeeeeeeeery long 
which.max(data$duration_ms)
data[504, ]

##############


# Checking distributions

par(mfrow= c(2, 5))
 
# Continuous variables

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


# Categorical variables

par(mfrow = c(1, 3))

barplot(table(key), main = "Key distribution")
barplot(table(mode), main = "Mode")
barplot(table(time_signature), main = "Time signature")

#Relationship between dependent and independent variables

par(mfrow= c(2, 5))

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

### Model fitting 




