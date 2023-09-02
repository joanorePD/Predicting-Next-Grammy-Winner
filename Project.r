### Data Preprocessing

# Set the working directory to this file's folder
library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))
load("final_df_n_str.RData")

Sys.setenv(LANG = "en") 

#install.packages("correlation")
#install.packages("confintr")
#install.packages("ROSE")
#install.packages("caret")
#install.packages("glmnet")


library(pROC)
library(MASS)
library(ROSE)
library(confintr)
library(ggplot2)
library(correlation)
library(corrplot)
library(class)
library(caret)
library(glmnet)

# Selecting the relevant variables

data = final_df_n_str


data = data[,c("track_name", "artist_name", "IsWinner", "Year","year", "followers", "acousticness", "danceability", "duration_ms",
               "energy", "instrumentalness", "key", "liveness", "loudness", "mode",
               "tempo", "time_signature", "valence")]

# Merge the two year variable

data$Year[data$Year == "Undefined"] <- data$year[data$Year == "Undefined"]

data = data[,c("track_name","artist_name", "IsWinner", "Year", "followers", "acousticness", "danceability", "duration_ms",
               "energy", "instrumentalness", "key", "liveness", "loudness", "mode",
               "tempo", "time_signature", "valence")]

# Eliminating duplicates

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



# Creating row names

names = paste0(data$track_name, " - ", data$artist_name)

# Eliminating unusable variables

data = data[,c("IsWinner", "Year", "acousticness", "danceability", "duration_ms",
               "energy", "instrumentalness", "key", "loudness", "mode",
               "tempo", "time_signature", "valence")]

data = cbind(names = names, data)


# Casting variables

data$IsWinner[data$IsWinner == "Winner"] = 1
data$IsWinner[data$IsWinner == "Nominee"] = 1
data$IsWinner[data$IsWinner == "Nothing"] = 0

data$IsWinner = as.integer(data$IsWinner)

data$Year = as.integer(data$Year)

data$mode = as.factor(data$mode)

data$key = as.factor(data$key)


data$time_signature = as.factor(data$time_signature)

summary(data)

# Checking balance between classes

length(data$IsWinner[data$IsWinner == 0]) / (length(data$IsWinner[data$IsWinner == 0]) + length(data$IsWinner[data$IsWinner == 1]))
length(data$IsWinner[data$IsWinner == 1]) / (length(data$IsWinner[data$IsWinner == 0]) + length(data$IsWinner[data$IsWinner == 1]))

# Splitting training and test set

training_size = floor(0.8 * nrow(data))

set.seed(42)

train_ind = sample(seq_len(nrow(data)), size = training_size)

training_set = data[train_ind,]

test_set = data[-train_ind,]

summary(training_set)

# Checking if the ratio is preserved

length(training_set$IsWinner[data$IsWinner == 0]) / (length(training_set$IsWinner[data$IsWinner == 0]) + length(training_set$IsWinner[data$IsWinner == 1]))
length(training_set$IsWinner[data$IsWinner == 1]) / (length(training_set$IsWinner[data$IsWinner == 0]) + length(training_set$IsWinner[data$IsWinner == 1]))

length(test_set$IsWinner[data$IsWinner == 0]) / (length(test_set$IsWinner[data$IsWinner == 0]) + length(test_set$IsWinner[data$IsWinner == 1]))
length(test_set$IsWinner[data$IsWinner == 1]) / (length(test_set$IsWinner[data$IsWinner == 0]) + length(test_set$IsWinner[data$IsWinner == 1]))

###############################################################################

# Exploratory Data Analysis

# Relationship between independent variables 

attach(training_set)

# Correlations between continuous variables

cor_matrix = cor(training_set[,c(-1, -2, -9, -11, -13)])
# corrplot(cor_matrix, method='number')
# dev.new(width=10, height=5, unit="in")
png(file="corplot_indep_1.png",
    width=1200, height=1000, pointsize = 26)
corrplot.mixed(cor_matrix, tl.pos='lt')
dev.off()
#pairs(training_set[,c(-1, -2, -9, -11, -13)], lower.panel = panel.smooth)

# Send pairs() to png to resize and visualize better
png(file = "corplot_indep_2.png", width = 1200, height = 1000, pointsize=20)
pairs(training_set[,c(-1, -2, -9, -11, -13)], lower.panel = panel.smooth)
dev.off()  # important!


# Association measure for categorical variables (Cramer's V is a normalized 
# version of the chi-square statistics)
cramersv(matrix(c(as.numeric(key), as.numeric(mode)), ncol = 2))
cramersv(matrix(c(as.numeric(key), as.numeric(time_signature)), ncol = 2))
cramersv(matrix(c(as.numeric(mode), as.numeric(time_signature)), ncol = 2))

# Association between continuous and categorical variables

# Key

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

loud_key.aov <- aov(loudness ~ key)
summary(loud_key.aov)

tem_key.aov <- aov(tempo ~ key)
summary(tem_key.aov)

val_key.aov <- aov(valence ~ key)
summary(val_key.aov)

# Mode

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

loud_mode.aov <- aov(loudness ~ mode)
summary(loud_mode.aov) # SIGNIFICANT

tem_mode.aov <- aov(tempo ~ mode)
summary(tem_mode.aov)

val_mode.aov <- aov(valence ~ mode)
summary(val_mode.aov)

# Time signature

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

loud_time.aov <- aov(loudness ~ time_signature)
summary(loud_time.aov) # SIGNIFICANT

tem_time.aov <- aov(tempo ~ time_signature)
summary(tem_time.aov) # SIGNIFICANT

val_time.aov <- aov(valence ~ time_signature)
summary(val_time.aov) # SIGNIFICANT


# Partial correlations
correlation(training_set[,c(-1, -2, -9, -11, -13)], partial = TRUE)

# Plots of variables with the largest partial correlation
ggplot(data = training_set, aes(danceability, valence)) + geom_jitter(color = "blue")
ggplot(data = training_set, aes(loudness, energy)) + geom_jitter(color = "blue")
ggplot(data = training_set, aes(acousticness, energy)) + geom_jitter(color = "blue")


#Weird song veeeeeeeeeeeeeeeeeeeeeeeery long 
which.max(data$duration_ms)
data[504, ]

##############


# Checking distributions

par(mfrow= c(2, 4))
 
# Continuous variables

hist(acousticness, main='Acousticness')
hist(danceability, main='Danceability')
hist(duration_ms, main='Duration')
hist(energy, main='Energy')
hist(instrumentalness, main='Instrumentalness')
hist(loudness, main='Loudness')
hist(tempo, main='Tempo')
hist(valence, main='Valence')

# Comparison IsWinner 0 vs 1 - Valence

par(mfrow = c(1, 2))

x <- data[data$IsWinner == 0,]$valence

hist(x)

abline(v = mean(x),                       # Add line for mean
       col = "red",
       lwd = 3)
text(x = mean(x) * 0.5,                   # Add text for mean
     y = 120,
     paste("Mean =", round(mean(x), digits=2)),
     col = "red",
     cex = 1)

x <- data[data$IsWinner == 1,]$valence

hist(x)

abline(v = mean(x),                       # Add line for mean
       col = "red",
       lwd = 3)
text(x = mean(x) * 0.5,                   # Add text for mean
     y = 30,
     paste("Mean =", round(mean(x), digits=2)),
     col = "red",
     cex = 1)

# Categorical variables

par(mfrow = c(1, 3))

barplot(table(key), main = "Key distribution")
barplot(table(mode), main = "Mode")
barplot(table(time_signature), main = "Time signature")

# Comparison IsWinner 0 vs 1 - Key

par(mfrow = c(1, 2))

x <- data[data$IsWinner == 0,]$key

barplot(table(x), main = "Key: Non-Nominated")

x <- data[data$IsWinner == 1,]$key

barplot(table(x), main = "Key: Nominated/Winner")

# Relationships between dependent and independent variables

par(mfrow= c(2, 4))

boxplot(danceability ~ training_set$IsWinner, xlab='Nominee Boolean')
boxplot(acousticness ~ training_set$IsWinner, xlab='Nominee Boolean')
boxplot(duration_ms ~ training_set$IsWinner, xlab='Nominee Boolean')
boxplot(energy ~ training_set$IsWinner, xlab='Nominee Boolean')
boxplot(instrumentalness ~ training_set$IsWinner, xlab='Nominee Boolean')
boxplot(loudness ~ training_set$IsWinner, xlab='Nominee Boolean')
boxplot(tempo ~ training_set$IsWinner, xlab='Nominee Boolean')
boxplot(valence ~ training_set$IsWinner, xlab='Nominee Boolean')

par(mfrow = c(1, 1))

chisq.test(key, training_set$IsWinner)
chisq.test(mode, training_set$IsWinner)
chisq.test(time_signature, training_set$IsWinner)

cramersv(matrix(c(as.numeric(key), as.numeric(training_set$IsWinner)), ncol = 2))
cramersv(matrix(c(as.numeric(mode), as.numeric(training_set$IsWinner)), ncol = 2))
cramersv(matrix(c(as.numeric(time_signature), as.numeric(training_set$IsWinner)), ncol = 2))

###############################################################################

### Model fitting 

## Oversampling

oversampled_train_data = ovun.sample(IsWinner ~., data = training_set[,-1], method = "over", p = 0.5, seed = 42)$data


# Checking oversampled training set balance

sum(oversampled_train_data$IsWinner == 0)
sum(oversampled_train_data$IsWinner == 1) 

## Simple logistic model

logistic = glm(IsWinner ~ ., data = training_set[,c(-1,-2)], family = "binomial")
summary(logistic)

# Computing predictions

logistic_predictions_full = predict(logistic, newdata = test_set[,c(-1, -2)], type = "response")

# Threshold = 0.2

logistic_predictions_full_02 = ifelse(logistic_predictions_full > 0.2, 1, 0)
logistic_accuracy_full_02 = sum(logistic_predictions_full_02 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_predictions_full_02)

false_positive_logistic_full_02 = table(test_set$IsWinner, logistic_predictions_full_02)[3]
negative_logistic_full_02 = table(test_set$IsWinner, logistic_predictions_full_02)[1] + table(test_set$IsWinner, logistic_predictions_full_02)[3]
typeIerror_logistic_full_02 = false_positive_logistic_full_02 / negative_logistic_full_02

true_positive_logistic_full_02 = table(test_set$IsWinner, logistic_predictions_full_02)[4]
positive_logistic_full_02 = table(test_set$IsWinner, logistic_predictions_full_02)[2] + table(test_set$IsWinner, logistic_predictions_full_02)[4]
sensitivity_logistic_full_02 = true_positive_logistic_full_02 / positive_logistic_full_02

# Threshold = 0.3

logistic_predictions_full_03 = ifelse(logistic_predictions_full > 0.3, 1, 0)
logistic_accuracy_full_03 = sum(logistic_predictions_full_03 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_predictions_full_03)

false_positive_logistic_full_03 = table(test_set$IsWinner, logistic_predictions_full_03)[3]
negative_logistic_full_03 = table(test_set$IsWinner, logistic_predictions_full_03)[1] + table(test_set$IsWinner, logistic_predictions_full_03)[3]
typeIerror_logistic_full_03 = false_positive_logistic_full_03 / negative_logistic_full_03

true_positive_logistic_full_03 = table(test_set$IsWinner, logistic_predictions_full_03)[4]
positive_logistic_full_03 = table(test_set$IsWinner, logistic_predictions_full_03)[2] + table(test_set$IsWinner, logistic_predictions_full_03)[4]
sensitivity_logistic_full_03 = true_positive_logistic_full_03 / positive_logistic_full_03

# Threshold = 0.4

logistic_predictions_full_04 = ifelse(logistic_predictions_full > 0.4, 1, 0)
logistic_accuracy_full_04 = sum(logistic_predictions_full_04 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_predictions_full_04)

false_positive_logistic_full_04 = table(test_set$IsWinner, logistic_predictions_full_04)[3]
negative_logistic_full_04 = table(test_set$IsWinner, logistic_predictions_full_04)[1] + table(test_set$IsWinner, logistic_predictions_full_04)[3]
typeIerror_logistic_full_04 = false_positive_logistic_full_04 / negative_logistic_full_04

true_positive_logistic_full_04 = table(test_set$IsWinner, logistic_predictions_full_04)[4]
positive_logistic_full_04 = table(test_set$IsWinner, logistic_predictions_full_04)[2] + table(test_set$IsWinner, logistic_predictions_full_04)[4]
sensitivity_logistic_full_04 = true_positive_logistic_full_04 / positive_logistic_full_04


# ROC curve

roc.out <- roc(test_set$IsWinner, logistic_predictions_full)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

# Stepwise variable selection

log_both =  stepAIC(logistic, direction = "both")

# Fitting the reduced model

logistic_reduced = glm(IsWinner ~ Year + acousticness + duration_ms + valence, data = training_set,  family = "binomial")

summary(logistic_reduced)

# Computing predictions

logistic_predictions = predict(logistic_reduced, newdata = test_set[,c(-1, -2)], type = "response")

# Threshold = 0.2

logistic_predictions_02 = ifelse(logistic_predictions > 0.2, 1, 0)
logistic_accuracy_02 = sum(logistic_predictions_02 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_predictions_02)

false_positive_logistic_02 = table(test_set$IsWinner, logistic_predictions_02)[3]
negative_logistic_02 = table(test_set$IsWinner, logistic_predictions_02)[1] + table(test_set$IsWinner, logistic_predictions_02)[3]
typeIerror_logistic_02 = false_positive_logistic_02 / negative_logistic_02

true_positive_logistic_02 = table(test_set$IsWinner, logistic_predictions_02)[4]
positive_logistic_02 = table(test_set$IsWinner, logistic_predictions_02)[2] + table(test_set$IsWinner, logistic_predictions_02)[4]
sensitivity_logistic_02 = true_positive_logistic_02 / positive_logistic_02

# Threshold = 0.3

logistic_predictions_03 = ifelse(logistic_predictions > 0.3, 1, 0)
logistic_accuracy_03 = sum(logistic_predictions_03 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_predictions_03)

false_positive_logistic_03 = table(test_set$IsWinner, logistic_predictions_03)[3]
negative_logistic_03 = table(test_set$IsWinner, logistic_predictions_03)[1] + table(test_set$IsWinner, logistic_predictions_03)[3]
typeIerror_logistic_03 = false_positive_logistic_03 / negative_logistic_03

true_positive_logistic_03 = table(test_set$IsWinner, logistic_predictions_03)[4]
positive_logistic_03 = table(test_set$IsWinner, logistic_predictions_03)[2] + table(test_set$IsWinner, logistic_predictions_03)[4]
sensitivity_logistic_03 = true_positive_logistic_03 / positive_logistic_03

# Threshold = 0.4

logistic_predictions_04 = ifelse(logistic_predictions > 0.4, 1, 0)
logistic_accuracy_04 = sum(logistic_predictions_04 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_predictions_04)

false_positive_logistic_04 = table(test_set$IsWinner, logistic_predictions_04)[3]
negative_logistic_04 = table(test_set$IsWinner, logistic_predictions_04)[1] + table(test_set$IsWinner, logistic_predictions_04)[3]
typeIerror_logistic_04 = false_positive_logistic_04 / negative_logistic_04

true_positive_logistic_04 = table(test_set$IsWinner, logistic_predictions_04)[4]
positive_logistic_04 = table(test_set$IsWinner, logistic_predictions_04)[2] + table(test_set$IsWinner, logistic_predictions_04)[4]
sensitivity_logistic_04 = true_positive_logistic_04 / positive_logistic_04


# ROC curve

roc.out <- roc(test_set$IsWinner, logistic_predictions)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

# Fitting logistic oversampled

logistic_over = glm(as.numeric(unlist(oversampled_train_data[1])) ~ ., data = oversampled_train_data[-1], family = "binomial")
summary(logistic_over)

# Computing predictions

logistic_over_predictions_full = predict(logistic_over, newdata = test_set[,c(-1, -2)], type = "response")

# Threshold = 0.2

logistic_over_predictions_full_02 = ifelse(logistic_over_predictions_full > 0.2, 1, 0)
logistic_over_accuracy_full_02 = sum(logistic_over_predictions_full_02 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_over_predictions_full_02)

false_positive_logistic_over_full_02 = table(test_set$IsWinner, logistic_over_predictions_full_02)[3]
negative_logistic_over_full_02 = table(test_set$IsWinner, logistic_over_predictions_full_02)[1] + table(test_set$IsWinner, logistic_over_predictions_full_02)[3]
typeIerror_logistic_over_full_02 = false_positive_logistic_over_full_02 / negative_logistic_over_full_02

typeIerror_logistic_over_full_02

true_positive_logistic_over_full_02 = table(test_set$IsWinner, logistic_over_predictions_full_02)[4]
positive_logistic_over_full_02 = table(test_set$IsWinner, logistic_over_predictions_full_02)[2] + table(test_set$IsWinner, logistic_over_predictions_full_02)[4]
sensitivity_logistic_over_full_02 = true_positive_logistic_over_full_02 / positive_logistic_over_full_02

sensitivity_logistic_over_full_02

# Threshold = 0.3

logistic_over_predictions_full_03 = ifelse(logistic_over_predictions_full > 0.3, 1, 0)
logistic_over_accuracy_full_03 = sum(logistic_over_predictions_full_03 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_over_predictions_full_03)

false_positive_logistic_over_full_03 = table(test_set$IsWinner, logistic_over_predictions_full_03)[3]
negative_logistic_over_full_03 = table(test_set$IsWinner, logistic_over_predictions_full_03)[1] + table(test_set$IsWinner, logistic_over_predictions_full_03)[3]
typeIerror_logistic_over_full_03 = false_positive_logistic_over_full_03 / negative_logistic_over_full_03

typeIerror_logistic_over_full_03

true_positive_logistic_over_full_03 = table(test_set$IsWinner, logistic_over_predictions_full_03)[4]
positive_logistic_over_full_03 = table(test_set$IsWinner, logistic_over_predictions_full_03)[2] + table(test_set$IsWinner, logistic_over_predictions_full_03)[4]
sensitivity_logistic_over_full_03 = true_positive_logistic_over_full_03 / positive_logistic_over_full_03

sensitivity_logistic_over_full_03

# Threshold = 0.4

logistic_over_predictions_full_04 = ifelse(logistic_over_predictions_full > 0.4, 1, 0)
logistic_over_accuracy_full_04 = sum(logistic_over_predictions_full_04 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_over_predictions_full_04)

false_positive_logistic_over_full_04 = table(test_set$IsWinner, logistic_over_predictions_full_04)[3]
negative_logistic_over_full_04 = table(test_set$IsWinner, logistic_over_predictions_full_04)[1] + table(test_set$IsWinner, logistic_over_predictions_full_04)[3]
typeIerror_logistic_over_full_04 = false_positive_logistic_over_full_04 / negative_logistic_over_full_04

typeIerror_logistic_over_full_04

true_positive_logistic_over_full_04 = table(test_set$IsWinner, logistic_over_predictions_full_04)[4]
positive_logistic_over_full_04 = table(test_set$IsWinner, logistic_over_predictions_full_04)[2] + table(test_set$IsWinner, logistic_over_predictions_full_04)[4]
sensitivity_logistic_over_full_04 = true_positive_logistic_over_full_04 / positive_logistic_over_full_04

sensitivity_logistic_over_full_04

# ROC curve

roc.out <- roc(test_set$IsWinner, logistic_over_predictions_full)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

# Stepwise variable selection of oversampled data

log_over_both =  stepAIC(logistic_over, direction = "both")

# fitting the reduced logistic model on oversampled data

response_variable_over = as.numeric(unlist(oversampled_train_data[1]))

reduced_variables_over = as.matrix(oversampled_train_data[,c(2, 3, 5, 7, 8, 9, 12, 13)], ncol = 8)

reduced_train_data_over = matrix(c(
  response_variable_over,
  as.numeric(reduced_variables_over[,1]),
  as.numeric(reduced_variables_over[,2]),
  as.numeric(reduced_variables_over[,3]),
  as.numeric(reduced_variables_over[,4]),
  as.factor(reduced_variables_over[,5]),
  as.numeric(reduced_variables_over[,6]),
  as.factor(reduced_variables_over[,7]),
  as.numeric(reduced_variables_over[,8])
), ncol = 9)

head(reduced_variables_over)


colnames(reduced_train_data_over) = c("IsWinner", "Year", "acousticness", "duration_ms",
                                     "instrumentalness", "key", "loudness", "time_signature", "valence" )


logistic_reduced_over = glm(response_variable_over ~ Year + acousticness 
                            + duration_ms + instrumentalness + key + loudness + 
                            + time_signature + valence, data = oversampled_train_data,
                            family = "binomial")

logistic_reduced_over_predictions = predict(logistic_reduced_over, newdata = test_set[,c(-1, -2)], type = "response")

# Threshold = 0.2

logistic_reduced_over_predictions_02 = ifelse(logistic_reduced_over_predictions > 0.2, 1, 0)
logistic_reduced_over_accuracy_02 = sum(logistic_reduced_over_predictions_02 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_reduced_over_predictions_02)

false_positive_logistic_reduced_over_02 = table(test_set$IsWinner, logistic_reduced_over_predictions_02)[3]
negative_logistic_reduced_over_02 = table(test_set$IsWinner, logistic_reduced_over_predictions_02)[1] + table(test_set$IsWinner, logistic_reduced_over_predictions_02)[3]
typeIerror_logistic_reduced_over_02 = false_positive_logistic_reduced_over_02 / negative_logistic_reduced_over_02

typeIerror_logistic_reduced_over_02

true_positive_logistic_reduced_over_02 = table(test_set$IsWinner, logistic_reduced_over_predictions_02)[4]
positive_logistic_reduced_over_02 = table(test_set$IsWinner, logistic_reduced_over_predictions_02)[2] + table(test_set$IsWinner, logistic_reduced_over_predictions_02)[4]
sensitivity_logistic_reduced_over_02 = true_positive_logistic_reduced_over_02 / positive_logistic_reduced_over_02

sensitivity_logistic_reduced_over_02

# Threshold = 0.3

logistic_reduced_over_predictions_03 = ifelse(logistic_reduced_over_predictions > 0.3, 1, 0)
logistic_reduced_over_accuracy_03 = sum(logistic_reduced_over_predictions_03 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_reduced_over_predictions_03)

false_positive_logistic_reduced_over_03 = table(test_set$IsWinner, logistic_reduced_over_predictions_03)[3]
negative_logistic_reduced_over_03 = table(test_set$IsWinner, logistic_reduced_over_predictions_03)[1] + table(test_set$IsWinner, logistic_reduced_over_predictions_03)[3]
typeIerror_logistic_reduced_over_03 = false_positive_logistic_reduced_over_03 / negative_logistic_reduced_over_03

typeIerror_logistic_reduced_over_03

true_positive_logistic_reduced_over_03 = table(test_set$IsWinner, logistic_reduced_over_predictions_03)[4]
positive_logistic_reduced_over_03 = table(test_set$IsWinner, logistic_reduced_over_predictions_03)[2] + table(test_set$IsWinner, logistic_reduced_over_predictions_03)[4]
sensitivity_logistic_reduced_over_03 = true_positive_logistic_reduced_over_03 / positive_logistic_reduced_over_03

sensitivity_logistic_reduced_over_03

# Threshold = 0.4

logistic_reduced_over_predictions_04 = ifelse(logistic_reduced_over_predictions > 0.4, 1, 0)
logistic_reduced_over_accuracy_04 = sum(logistic_reduced_over_predictions_04 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_reduced_over_predictions_04)

false_positive_logistic_reduced_over_04 = table(test_set$IsWinner, logistic_reduced_over_predictions_04)[3]
negative_logistic_reduced_over_04 = table(test_set$IsWinner, logistic_reduced_over_predictions_04)[1] + table(test_set$IsWinner, logistic_reduced_over_predictions_04)[3]
typeIerror_logistic_reduced_over_04 = false_positive_logistic_reduced_over_04 / negative_logistic_reduced_over_04

typeIerror_logistic_reduced_over_04

true_positive_logistic_reduced_over_04 = table(test_set$IsWinner, logistic_reduced_over_predictions_04)[4]
positive_logistic_reduced_over_04 = table(test_set$IsWinner, logistic_reduced_over_predictions_04)[2] + table(test_set$IsWinner, logistic_reduced_over_predictions_04)[4]
sensitivity_logistic_reduced_over_04 = true_positive_logistic_reduced_over_04 / positive_logistic_reduced_over_04

sensitivity_logistic_reduced_over_04

# ROC curve

roc.out <- roc(test_set$IsWinner, logistic_reduced_over_predictions)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(roc.out)


# Linear discriminant analysis

# Checking normality of the predictors conditioned to the classes of the variable IsWinner  

shapiro.test(danceability[IsWinner == 0]) # Yes
shapiro.test(danceability[IsWinner == 1]) # Yes

shapiro.test(acousticness[IsWinner == 0]) # No
shapiro.test(acousticness[IsWinner == 1]) # No

shapiro.test(duration_ms[IsWinner == 0]) # No
shapiro.test(duration_ms[IsWinner == 1]) # No

shapiro.test(energy[IsWinner == 0]) # No
shapiro.test(energy[IsWinner == 1]) # No

shapiro.test(instrumentalness[IsWinner == 0]) # No
shapiro.test(instrumentalness[IsWinner == 1]) # No

shapiro.test(loudness[IsWinner == 0]) # No
shapiro.test(loudness[IsWinner == 1]) # No

shapiro.test(tempo[IsWinner == 0]) # No
shapiro.test(tempo[IsWinner == 1]) # No

shapiro.test(valence[IsWinner == 0]) # No
shapiro.test(valence[IsWinner == 1]) # No

# danceability looking normal
par(mfrow = c(1, 2))

qqnorm(danceability[IsWinner == 0])
grid()               
qqline(danceability[IsWinner == 0],lwd = 2, col = "red")

qqnorm(danceability[IsWinner == 1])
grid()               
qqline(danceability[IsWinner == 1],lwd = 2, col = "red")

# acousticness S shaped
par(mfrow = c(1, 2))

qqnorm(acousticness[IsWinner == 0])
grid()               
qqline(acousticness[IsWinner == 0],lwd = 2, col = "red")

qqnorm(acousticness[IsWinner == 1])
grid()               
qqline(acousticness[IsWinner == 1],lwd = 2, col = "red")

# duration_ms  big right tail
par(mfrow = c(1, 2))

qqnorm(duration_ms[IsWinner == 0])
grid()               
qqline(duration_ms[IsWinner == 0],lwd = 2, col = "red")

qqnorm(duration_ms[IsWinner == 1])
grid()               
qqline(duration_ms[IsWinner == 1],lwd = 2, col = "red")

# energy tails not normal
par(mfrow = c(1, 2))

qqnorm(energy[IsWinner == 0])
grid()               
qqline(energy[IsWinner == 0],lwd = 2, col = "red")

qqnorm(energy[IsWinner == 1])
grid()               
qqline(energy[IsWinner == 1],lwd = 2, col = "red")

# instrumentalness huge right tail
par(mfrow = c(1, 2))

qqnorm(instrumentalness[IsWinner == 0])
grid()               
qqline(instrumentalness[IsWinner == 0],lwd = 2, col = "red")

qqnorm(instrumentalness[IsWinner == 1])
grid()               
qqline(instrumentalness[IsWinner == 1],lwd = 2, col = "red")

# loudness tails not normal
par(mfrow = c(1, 2))

qqnorm(loudness[IsWinner == 0])
grid()               
qqline(loudness[IsWinner == 0],lwd = 2, col = "red")

qqnorm(loudness[IsWinner == 1])
grid()               
qqline(loudness[IsWinner == 1],lwd = 2, col = "red")

# tempo tails slightly not normal
par(mfrow = c(1, 2))

qqnorm(tempo[IsWinner == 0])
grid()               
qqline(tempo[IsWinner == 0],lwd = 2, col = "red")

qqnorm(tempo[IsWinner == 1])
grid()               
qqline(tempo[IsWinner == 1],lwd = 2, col = "red")

# valence tails slightly not normal
par(mfrow = c(1, 2))

qqnorm(valence[IsWinner == 0])
grid()               
qqline(valence[IsWinner == 0],lwd = 2, col = "red")

qqnorm(valence[IsWinner == 1])
grid()               
qqline(valence[IsWinner == 1],lwd = 2, col = "red")

# Applying transformations to the predictors in the attempt of making them normal


# Acousticness plots improved, test not passed

par(mfrow = c(1, 1))

b_acousticness <- boxcox(lm(acousticness ~ 1))
lambda <- b_acousticness$x[which.max(b_acousticness$y)]
acousticness_tran <- (acousticness ^ lambda - 1) / lambda

par(mfrow = c(1, 2))

qqnorm(acousticness_tran[IsWinner == 0])
grid()               
qqline(acousticness_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(acousticness_tran[IsWinner == 1])
grid()               
qqline(acousticness_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(acousticness_tran[IsWinner == 0]) # No 
shapiro.test(acousticness_tran[IsWinner == 1]) # No

# duration_ms plots improved, test not passed

par(mfrow = c(1, 1))

b_duration_ms <- boxcox(lm(duration_ms ~ 1))
lambda <- b_duration_ms$x[which.max(b_duration_ms$y)]
duration_ms_tran <- (duration_ms ^ lambda - 1) / lambda

par(mfrow = c(1, 2))

qqnorm(duration_ms_tran[IsWinner == 0])
grid()               
qqline(duration_ms_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(duration_ms_tran[IsWinner == 1])
grid()               
qqline(duration_ms_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(duration_ms_tran[IsWinner == 0]) # No 
shapiro.test(duration_ms_tran[IsWinner == 1]) # No

# energy plots are pretty much the same test not passed

par(mfrow = c(1, 1))

b_energy <- boxcox(lm(energy ~ 1))
lambda <- b_energy$x[which.max(b_energy$y)]
energy_tran <- (energy ^ lambda - 1) / lambda

par(mfrow = c(1, 2))

qqnorm(energy_tran[IsWinner == 0])
grid()               
qqline(energy_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(energy_tran[IsWinner == 1])
grid()               
qqline(energy_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(energy_tran[IsWinner == 0]) # No 
shapiro.test(energy_tran[IsWinner == 1]) # No

# instrumentalness can't apply the boxcox transformation because there are some 0's
# Tried to apply a linear transformation before but the plot is weird

par(mfrow = c(1, 1))

# Added a tiny value to overcome the issue of 0's
new_instrumentalness = instrumentalness + 1e-05

b_instrumentalness <- boxcox(lm(new_instrumentalness ~ 1))
lambda <- b_instrumentalness$x[which.max(b_instrumentalness$y)]
instrumentalness_tran <- (new_instrumentalness ^ lambda - 1) / lambda

par(mfrow = c(1, 2))

qqnorm(instrumentalness_tran[IsWinner == 0])
grid()               
qqline(instrumentalness_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(instrumentalness_tran[IsWinner == 1])
grid()               
qqline(instrumentalness_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(instrumentalness_tran[IsWinner == 0]) # No 
shapiro.test(instrumentalness_tran[IsWinner == 1]) # No


# loudness boxcox transformation not directly applicable because the variable
# is always negative, I multiplied the values by -1 and then applied it, 
# it gave very good results, but we need to pay attention to the interpretation

par(mfrow = c(1, 1))

new_loudness = loudness * (-1)

b_loudness <- boxcox(lm(new_loudness ~ 1))
lambda <- b_loudness$x[which.max(b_loudness$y)]
loudness_tran <- (new_loudness ^ lambda - 1) / lambda

par(mfrow = c(1, 2))

qqnorm(loudness_tran[IsWinner == 0])
grid()               
qqline(loudness_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(loudness_tran[IsWinner == 1])
grid()               
qqline(loudness_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(loudness_tran[IsWinner == 0]) # Yes
shapiro.test(loudness_tran[IsWinner == 1]) # Yes

# tempo, slight improvement in the plots

par(mfrow = c(1, 1))

b_tempo <- boxcox(lm(tempo ~ 1))
lambda <- b_tempo$x[which.max(b_tempo$y)]
tempo_tran <- (tempo ^ lambda - 1) / lambda

par(mfrow = c(1, 2))

qqnorm(tempo_tran[IsWinner == 0])
grid()               
qqline(tempo_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(tempo_tran[IsWinner == 1])
grid()               
qqline(tempo_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(tempo_tran[IsWinner == 0]) # No
shapiro.test(tempo_tran[IsWinner == 1]) # No

# valence, pretty much the same

par(mfrow = c(1, 1))

b_valence <- boxcox(lm(valence ~ 1))
lambda <- b_valence$x[which.max(b_valence$y)]
valence_tran <- (valence ^ lambda - 1) / lambda

par(mfrow = c(1, 2))

qqnorm(valence_tran[IsWinner == 0])
grid()               
qqline(valence_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(valence_tran[IsWinner == 1])
grid()               
qqline(valence_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(valence_tran[IsWinner == 0]) # No
shapiro.test(valence_tran[IsWinner == 1]) # No

##########################################

par(mfrow = c(1, 1))

b_data_acousticness <- boxcox(lm(data$acousticness ~ 1))
lambda <- b_data_acousticness$x[which.max(b_data_acousticness$y)]
data_acousticness_tran <- (data$acousticness ^ lambda - 1) / lambda

b_data_duration_ms <- boxcox(lm(data$duration_ms ~ 1))
lambda <- b_data_duration_ms$x[which.max(b_data_duration_ms$y)]
data_duration_ms_tran <- (data$duration_ms ^ lambda - 1) / lambda

neg_loudness = data$loudness * (-1)
b_data_loudness <- boxcox(lm(neg_loudness ~ 1))
lambda <- b_data_loudness$x[which.max(b_data_loudness$y)]
data_loudness_tran <- (neg_loudness ^ lambda - 1) / lambda

b_data_tempo <- boxcox(lm(data$tempo ~ 1))
lambda <- b_data_tempo$x[which.max(b_data_tempo$y)]
data_tempo_tran <- (data$tempo ^ lambda - 1) / lambda


tran_data = matrix(c(data$IsWinner, data_acousticness_tran, data_duration_ms_tran, 
            data$energy, data$instrumentalness, data_loudness_tran, 
            data_tempo_tran, data$valence), ncol = 8)

training_tran_data = tran_data[train_ind,]

test_tran_data = tran_data[-train_ind,]

colnames_tran_data = c("IsWinner", "acousticness_tran", "duration_ms_tran", "energy",
                       "instrumentalness", "loudness_tran", "tempo_tran", "valence")

colnames(training_tran_data) = colnames_tran_data
colnames(test_tran_data) = colnames_tran_data
colnames(tran_data) = colnames_tran_data


training_tran_data = as.data.frame(training_tran_data)
test_tran_data = as.data.frame(test_tran_data)
tran_data = as.data.frame(tran_data)

## LDA

simple_lda = lda(IsWinner ~ acousticness_tran + duration_ms_tran + 
                energy + instrumentalness + loudness_tran + tempo_tran +
                valence, data = training_tran_data, family = "binomial")


pred_simple_lda = predict(simple_lda, newdata = test_tran_data, type = "Response")

# ROC curve

roc.out <- roc(test_set$IsWinner, as.numeric(pred_simple_lda$class) - 1)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

# Oversampled LDA

oversampled_train_tran_data = ovun.sample(IsWinner ~., data = training_tran_data, method = "over", p = 0.5, seed = 42)$data


simple_over_lda = lda(IsWinner ~ acousticness_tran + duration_ms_tran + 
                   energy + instrumentalness + loudness_tran + tempo_tran +
                   valence, data = oversampled_train_tran_data, family = "binomial")


pred_simple_over_lda = predict(simple_over_lda, newdata = test_tran_data, type = "Response")

# ROC curve

roc.out <- roc(test_set$IsWinner, as.numeric(pred_simple_over_lda$class) - 1)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

# Predictions

# Threshold 0.4

lda_over_predictions_04 = ifelse(pred_simple_over_lda$posterior[,1] > 0.4, 1, 0)
lda_over_accuracy_04 = sum(lda_over_predictions_04 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, lda_over_predictions_04)

false_positive_lda_over_04 = table(test_set$IsWinner, lda_over_predictions_04)[3]
negative_lda_over_04 = table(test_set$IsWinner, lda_over_predictions_04)[1] + table(test_set$IsWinner, lda_over_predictions_04)[3]
typeIerror_lda_over_04 = false_positive_lda_over_04 / negative_lda_over_04

true_positive_lda_over_04 = table(test_set$IsWinner, lda_over_predictions_04)[4]
positive_lda_over_04 = table(test_set$IsWinner, lda_over_predictions_04)[2] + table(test_set$IsWinner, lda_over_predictions_04)[4]
sensitivity_lda_over_04 = true_positive_lda_over_04 / positive_lda_over_04


# Threshold 0.5

lda_over_predictions_05 = list(pred_simple_over_lda$class)
lda_over_accuracy_05 = sum(lda_over_predictions_05 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, lda_over_predictions_05[[1]])

false_positive_lda_over_05 = table(test_set$IsWinner, lda_over_predictions_05[[1]])[3]
negative_lda_over_05 = table(test_set$IsWinner, lda_over_predictions_05[[1]])[1] + table(test_set$IsWinner, lda_over_predictions_05[[1]])[3]
typeIerror_lda_over_05 = false_positive_lda_over_05 / negative_lda_over_05

true_positive_lda_over_05 = table(test_set$IsWinner, lda_over_predictions_05[[1]])[4]
positive_lda_over_05 = table(test_set$IsWinner, lda_over_predictions_05[[1]])[2] + table(test_set$IsWinner, lda_over_predictions_05[[1]])[4]
sensitivity_lda_over_05 = true_positive_lda_over_05 / positive_lda_over_05


## QDA

qda = qda(IsWinner ~ acousticness_tran + duration_ms_tran + 
            energy + instrumentalness + loudness_tran + tempo_tran +
            valence, data = training_tran_data, family = "binomial")


pred_qda = predict(qda, newdata = test_tran_data, type = "Response")

# ROC

roc.out <- roc(test_set$IsWinner, as.numeric(pred_qda$class) - 1)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

# QDA oversampled

qda_over = qda(IsWinner ~ acousticness_tran + duration_ms_tran + 
            energy + instrumentalness + loudness_tran + tempo_tran +
            valence, data = oversampled_train_tran_data, family = "binomial")


pred_qda_over = predict(qda_over, newdata = test_tran_data, type = "Response")

# ROC

roc.out <- roc(test_set$IsWinner, as.numeric(pred_qda_over$class) - 1)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

# Predictions

# Threshold 0.4

qda_over_predictions_04 = ifelse(pred_qda_over$posterior[,1] > 0.4, 1, 0)
qda_over_accuracy_04 = sum(qda_over_predictions_04 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, qda_over_predictions_04)

false_positive_qda_over_04 = table(test_set$IsWinner, qda_over_predictions_04)[3]
negative_qda_over_04 = table(test_set$IsWinner, qda_over_predictions_04)[1] + table(test_set$IsWinner, qda_over_predictions_04)[3]
typeIerror_qda_over_04 = false_positive_qda_over_04 / negative_qda_over_04
typeIerror_qda_over_04

true_positive_qda_over_04 = table(test_set$IsWinner, qda_over_predictions_04)[4]
positive_qda_over_04 = table(test_set$IsWinner, qda_over_predictions_04)[2] + table(test_set$IsWinner, qda_over_predictions_04)[4]
sensitivity_qda_over_04 = true_positive_qda_over_04 / positive_qda_over_04
sensitivity_qda_over_04

# Threshold 0.5

qda_over_predictions_05 = list(pred_qda_over$class)
qda_over_accuracy_05 = sum(qda_over_predictions_05 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, qda_over_predictions_05[[1]])

false_positive_qda_over_05 = table(test_set$IsWinner, qda_over_predictions_05[[1]])[3]
negative_qda_over_05 = table(test_set$IsWinner, qda_over_predictions_05[[1]])[1] + table(test_set$IsWinner, qda_over_predictions_05[[1]])[3]
typeIerror_qda_over_05 = false_positive_qda_over_05 / negative_qda_over_05
typeIerror_qda_over_05

true_positive_qda_over_05 = table(test_set$IsWinner, qda_over_predictions_05[[1]])[4]
positive_qda_over_05 = table(test_set$IsWinner, qda_over_predictions_05[[1]])[2] + table(test_set$IsWinner, qda_over_predictions_05[[1]])[4]
sensitivity_qda_over_05 = true_positive_qda_over_05 / positive_qda_over_05
sensitivity_qda_over_05


#########################################C 

# Regularized regression

## Ridge regression

 
ridge_cv = cv.glmnet(data.matrix(oversampled_train_data[,-1]), oversampled_train_data$IsWinner,
                      alpha = 0, family = "binomial", type.measure = "class")

plot(ridge_cv)

lambda_opt_ridge <- ridge_cv$lambda.min

pred_ridge = predict(ridge_cv, data.matrix(test_set[, c(-1, -2)]), type = "class", s = lambda_opt_ridge)

table(test_set$IsWinner, pred_ridge)

## Lasso regression

lasso_cv = cv.glmnet(data.matrix(oversampled_train_data[,-1]), oversampled_train_data$IsWinner,
                     alpha = 1, family = "binomial", type.measure = "class")

plot(lasso_cv)

lambda_opt_lasso <- lasso_cv$lambda.min

pred_lasso = predict(lasso_cv, data.matrix(test_set[, c(-1, -2)]), type = "class", s = lambda_opt_ridge)

table(test_set$IsWinner, pred_lasso)


# K-NN

min_max_norm = function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalized_data = as.data.frame(lapply(data[,c(-1, -2, -9, -11, -13)], min_max_norm))

IsWinner_norm = data$IsWinner

normalized_data = cbind(IsWinner_norm, normalized_data)

training_norm_data = normalized_data[train_ind,]

test_norm_data = normalized_data[-train_ind,]

# Selecting k

kmax = 100

test_error = numeric(kmax)

for (k in 1:kmax) {
  knn_pred = as.factor(knn(training_norm_data[,-1], test_norm_data[,-1],
                            cl = training_norm_data$IsWinner_norm, k = k))
  cm = confusionMatrix(data = knn_pred, reference = as.factor(test_norm_data$IsWinner_norm))
  test_error[k] = 1 - cm$overall[1]
}


k_min = which.min(test_error)
k_min

knn = knn(training_norm_data[,-1], test_norm_data[,-1],
          cl = training_norm_data$IsWinner_norm, k = k_min)

knn_pred_min = as.factor(knn)

table(test_norm_data$IsWinner_norm, knn)

# oversampled

test_over_error = numeric(kmax)

oversampled_train_data

normalized_over_data = as.data.frame(lapply(oversampled_train_data[,c(-8, -10, -12)], min_max_norm))

training_norm_data_over = normalized_over_data[train_ind,]

for (k in 1:kmax) {
  knn_over_pred = as.factor(knn(training_norm_data_over[,-1], test_norm_data[,-1],
                           cl = training_norm_data$IsWinner_norm, k = k))
  cm_over = confusionMatrix(data = knn_over_pred, reference = as.factor(test_norm_data$IsWinner_norm))
  test_over_error[k] = 1 - cm_over$overall[1]
}


k_min_over = which.min(test_over_error)
k_min_over

knn_over = knn(training_norm_data_over[,-1], test_norm_data[,-1],
          cl = training_norm_data$IsWinner_norm, k = k_min_over)

knn_pred_min_over = as.factor(knn_over)

table(test_norm_data$IsWinner_norm, knn_over)

