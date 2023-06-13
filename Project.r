### Data Preprocessing

load("final_df_n_str.RData")

#install.packages("correlation")
#install.packages("confintr")
#install.packages("ROSE")

library(pROC)
library(MASS)
library(ROSE)
library(confintr)
library(ggplot2)
library(correlation)
library(corrplot)

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

data = data[,c("IsWinner", "Year", "followers", "acousticness", "danceability", "duration_ms",
               "energy", "instrumentalness", "key", "liveness", "loudness", "mode",
               "tempo", "time_signature", "valence")]

data = cbind(names = names, data)


# Casting variables

data$IsWinner[data$IsWinner == "Winner"] = 1
data$IsWinner[data$IsWinner == "Nominee"] = 1
data$IsWinner[data$IsWinner == "Nothing"] = 0

#data$IsWinner <- ifelse((data$IsWinner == 'Winner' | data$IsWinner == 'Nominee'), 1, 0)

data$IsWinner = as.integer(data$IsWinner)

data$Year = as.integer(data$Year)

data$mode = as.factor(data$mode)

data$key = as.factor(data$key)


data$time_signature = as.factor(data$time_signature)

# Giving row names

summary(data)

summary(data$IsWinner)

# Splitting training and test set

training_size = floor(0.8 * nrow(data))

set.seed(42)

train_ind = sample(seq_len(nrow(data)), size = training_size)

training_set = data[train_ind,]

test_set = data[-train_ind,]

summary(training_set)

# Checking if the ratio is preserved

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

# Relationships between dependent and independent variables

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

chisq.test(key, IsWinner)
chisq.test(mode, IsWinner)
chisq.test(time_signature, IsWinner)

cramersv(matrix(c(as.numeric(key), as.numeric(IsWinner)), ncol = 2))
cramersv(matrix(c(as.numeric(mode), as.numeric(IsWinner)), ncol = 2))
cramersv(matrix(c(as.numeric(time_signature), as.numeric(IsWinner)), ncol = 2))

table(mode, IsWinner)
table(time_signature, IsWinner)

###############################################################################

### Model fitting 

## Oversampling

oversampled_train_data = ovun.sample(IsWinner ~., data = training_set[-1], method = "over", p = 0.5, seed = 42)$data

# Checking oversampled training set balance

sum(oversampled_train_data$IsWinner == 0)
sum(oversampled_train_data$IsWinner == 1) 

## Simple logistic model

logistic = glm(IsWinner ~ ., data = training_set[,c(-1,-2)], family = "binomial")
summary(logistic)

# Stepwise variable selection

log_back = stepAIC(logistic, direction = "backward")

log_for = stepAIC(logistic, direction = "forward")

log_both =  stepAIC(logistic, direction = "both")

# Fitting the reduced model

logistic_reduced = glm(IsWinner ~ danceability + loudness + followers + valence + duration_ms + acousticness, data = training_set,  family = "binomial")

summary(logistic_reduced)

# Compiuting predictions

logistic_predictions = predict(logistic_reduced, newdata = test_set[,c(-1, -2)], type = "response")

# Threshold = 0.2

logistic_predictions_02 = ifelse(logistic_predictions > 0.2, 1, 0)
logistic_accuracy_02 = sum(logistic_predictions_02 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_predictions_02)

false_positive_logistic_02 = table(test_set$IsWinner, logistic_predictions_02)[3]
negative_logistic_02 = table(test_set$IsWinner, logistic_predictions_02)[1] + table(test_set$IsWinner, logistic_predictions_02)[2]
typeIerror_logistic_02 = false_positive_logistic_02 / negative_logistic_02

true_positive_logistic_02 = table(test_set$IsWinner, logistic_predictions_02)[4]
positive_logistic_02 = table(test_set$IsWinner, logistic_predictions_02)[2] + table(test_set$IsWinner, logistic_predictions_02)[4]
sensitivity_logistic_02 = true_positive_logistic_02 / positive_logistic_02

# Threshold = 0.3

logistic_predictions_03 = ifelse(logistic_predictions > 0.3, 1, 0)
logistic_accuracy_03 = sum(logistic_predictions_03 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_predictions_03)

false_positive_logistic_03 = table(test_set$IsWinner, logistic_predictions_03)[3]
negative_logistic_03 = table(test_set$IsWinner, logistic_predictions_03)[1] + table(test_set$IsWinner, logistic_predictions_03)[2]
typeIerror_logistic_03 = false_positive_logistic_03 / negative_logistic_03

true_positive_logistic_03 = table(test_set$IsWinner, logistic_predictions_03)[4]
positive_logistic_03 = table(test_set$IsWinner, logistic_predictions_03)[2] + table(test_set$IsWinner, logistic_predictions_03)[4]
sensitivity_logistic_03 = true_positive_logistic_03 / positive_logistic_03

# Threshold = 0.4

logistic_predictions_04 = ifelse(logistic_predictions > 0.4, 1, 0)
logistic_accuracy_04 = sum(logistic_predictions_04 == test_set[2]) / dim(test_set[2])[1]

table(test_set$IsWinner, logistic_predictions_04)

false_positive_logistic_04 = table(test_set$IsWinner, logistic_predictions_04)[3]
negative_logistic_04 = table(test_set$IsWinner, logistic_predictions_04)[1] + table(test_set$IsWinner, logistic_predictions_04)[2]
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


log_over_back = stepAIC(logistic_over, direction = "backward")

log_over_for = stepAIC(logistic_over, direction = "forward")

log_over_both =  stepAIC(logistic_over, direction = "both")

response_variable_over = as.numeric(unlist(oversampled_train_data[1]))

reduced_variables_over = as.matrix(oversampled_train_data[,c(2, 3, 4, 6, 8, 9, 11, 14, 15)], ncol = 9)

reduced_variables_over = matrix(c(
  as.numeric(reduced_variables_over[,1]),
  as.numeric(reduced_variables_over[,2]),
  as.numeric(reduced_variables_over[,3]),
  as.numeric(reduced_variables_over[,4]),
  as.numeric(reduced_variables_over[,5]),
  as.factor(reduced_variables_over[,6]),
  as.numeric(reduced_variables_over[,7]),
  as.factor(reduced_variables_over[,8]),
  as.numeric(reduced_variables_over[,9])
), ncol = 9)


colnames(reduced_variables_over) = c("Year", "followers", "acousticness", "duration_ms",
                                     "instrumentalness", "key", "loudness", "time_signature", "valence" )

head(reduced_variables_over)


logistic_reduced_over = glm(response_variable_over ~ reduced_variables_over, data = oversampled_train_data,  family = "binomial")

predictions_oversample = predict(logistic_reduced_over, newdata = test_set[,c(-1, -2)], type = "response")

# Linear discriminant analysis

# Checking normality of the predictors conditioned to the classes of the variable IsWinner  

shapiro.test(danceability[IsWinner == 0]) # Yes
shapiro.test(danceability[IsWinner == 1]) # Yes

shapiro.test(followers[IsWinner == 0]) # No
shapiro.test(followers[IsWinner == 1]) # No

shapiro.test(acousticness[IsWinner == 0]) # No
shapiro.test(acousticness[IsWinner == 1]) # No

shapiro.test(duration_ms[IsWinner == 0]) # No
shapiro.test(duration_ms[IsWinner == 1]) # No

shapiro.test(energy[IsWinner == 0]) # No
shapiro.test(energy[IsWinner == 1]) # No

shapiro.test(instrumentalness[IsWinner == 0]) # No
shapiro.test(instrumentalness[IsWinner == 1]) # No

shapiro.test(liveness[IsWinner == 0]) # No
shapiro.test(liveness[IsWinner == 1]) # No

shapiro.test(loudness[IsWinner == 0]) # No
shapiro.test(loudness[IsWinner == 1]) # No

shapiro.test(tempo[IsWinner == 0]) # No
shapiro.test(tempo[IsWinner == 1]) # No

shapiro.test(valence[IsWinner == 0]) # No
shapiro.test(valence[IsWinner == 1]) # No

par(mfrow = c(2, 5))

# danceability looking normal

qqnorm(danceability[IsWinner == 0])
grid()               
qqline(danceability[IsWinner == 0],lwd = 2, col = "red")

qqnorm(danceability[IsWinner == 1])
grid()               
qqline(danceability[IsWinner == 1],lwd = 2, col = "red")

# followers huge right tail 

qqnorm(followers[IsWinner == 0])
grid()               
qqline(followers[IsWinner == 0],lwd = 2, col = "red")

qqnorm(followers[IsWinner == 1])
grid()               
qqline(followers[IsWinner == 1],lwd = 2, col = "red")

# acousticness S shaped

qqnorm(acousticness[IsWinner == 0])
grid()               
qqline(acousticness[IsWinner == 0],lwd = 2, col = "red")

qqnorm(acousticness[IsWinner == 1])
grid()               
qqline(acousticness[IsWinner == 1],lwd = 2, col = "red")

# duration_ms  big right tail

qqnorm(duration_ms[IsWinner == 0])
grid()               
qqline(duration_ms[IsWinner == 0],lwd = 2, col = "red")

qqnorm(duration_ms[IsWinner == 1])
grid()               
qqline(duration_ms[IsWinner == 1],lwd = 2, col = "red")

# energy tails not normal

qqnorm(energy[IsWinner == 0])
grid()               
qqline(energy[IsWinner == 0],lwd = 2, col = "red")

qqnorm(energy[IsWinner == 1])
grid()               
qqline(energy[IsWinner == 1],lwd = 2, col = "red")

# instrumentalness huge right tail

qqnorm(instrumentalness[IsWinner == 0])
grid()               
qqline(instrumentalness[IsWinner == 0],lwd = 2, col = "red")

qqnorm(instrumentalness[IsWinner == 1])
grid()               
qqline(instrumentalness[IsWinner == 1],lwd = 2, col = "red")

# liveness S shaped

qqnorm(liveness[IsWinner == 0])
grid()               
qqline(liveness[IsWinner == 0],lwd = 2, col = "red")

qqnorm(liveness[IsWinner == 1])
grid()               
qqline(liveness[IsWinner == 1],lwd = 2, col = "red")

# loudness tails not normal

qqnorm(loudness[IsWinner == 0])
grid()               
qqline(loudness[IsWinner == 0],lwd = 2, col = "red")

qqnorm(loudness[IsWinner == 1])
grid()               
qqline(loudness[IsWinner == 1],lwd = 2, col = "red")

# tempo tails slightly not normal

qqnorm(tempo[IsWinner == 0])
grid()               
qqline(tempo[IsWinner == 0],lwd = 2, col = "red")

qqnorm(tempo[IsWinner == 1])
grid()               
qqline(tempo[IsWinner == 1],lwd = 2, col = "red")

# valence tails slightly not normal

qqnorm(valence[IsWinner == 0])
grid()               
qqline(valence[IsWinner == 0],lwd = 2, col = "red")

qqnorm(valence[IsWinner == 1])
grid()               
qqline(valence[IsWinner == 1],lwd = 2, col = "red")

# Applying transformations to the predictors in the attempt of making them normal

# followers plots improved, one passes the test

b_followers <- boxcox(lm(followers ~ 1))
lambda <- b_followers$x[which.max(b_followers$y)]
followers_tran <- (followers ^ lambda - 1) / lambda

qqnorm(followers_tran[IsWinner == 0])
grid()               
qqline(followers_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(followers_tran[IsWinner == 1])
grid()               
qqline(followers_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(followers_tran[IsWinner == 0]) # No 
shapiro.test(followers_tran[IsWinner == 1]) # Yes

# Acousticness plots improved, test not passed

b_acousticness <- boxcox(lm(acousticness ~ 1))
lambda <- b_acousticness$x[which.max(b_acousticness$y)]
acousticness_tran <- (acousticness ^ lambda - 1) / lambda

qqnorm(acousticness_tran[IsWinner == 0])
grid()               
qqline(acousticness_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(acousticness_tran[IsWinner == 1])
grid()               
qqline(acousticness_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(acousticness_tran[IsWinner == 0]) # No 
shapiro.test(acousticness_tran[IsWinner == 1]) # No

# duration_ms plots improved, test not passed

b_duration_ms <- boxcox(lm(duration_ms ~ 1))
lambda <- b_duration_ms$x[which.max(b_duration_ms$y)]
duration_ms_tran <- (duration_ms ^ lambda - 1) / lambda

qqnorm(duration_ms_tran[IsWinner == 0])
grid()               
qqline(duration_ms_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(duration_ms_tran[IsWinner == 1])
grid()               
qqline(duration_ms_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(duration_ms_tran[IsWinner == 0]) # No 
shapiro.test(duration_ms_tran[IsWinner == 1]) # No

# energy plots are pretty much the same test not passed

b_energy <- boxcox(lm(energy ~ 1))
lambda <- b_energy$x[which.max(b_energy$y)]
energy_tran <- (energy ^ lambda - 1) / lambda

qqnorm(energy_tran[IsWinner == 0])
grid()               
qqline(energy_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(energy_tran[IsWinner == 1])
grid()               
qqline(energy_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(energy_tran[IsWinner == 0]) # No 
shapiro.test(energy_tran[IsWinner == 1]) # No

# instrumentalness can't apply the boxcox transformation because there are some 0's
# I tried to apply a linear transformation before but the plot is weird

new_instrumentalness = instrumentalness + 1e-05

b_instrumentalness <- boxcox(lm(new_instrumentalness ~ 1))
lambda <- b_instrumentalness$x[which.max(b_instrumentalness$y)]
instrumentalness_tran <- (new_instrumentalness ^ lambda - 1) / lambda

qqnorm(instrumentalness_tran[IsWinner == 0])
grid()               
qqline(instrumentalness_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(instrumentalness_tran[IsWinner == 1])
grid()               
qqline(instrumentalness_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(instrumentalness_tran[IsWinner == 0]) # No 
shapiro.test(instrumentalness_tran[IsWinner == 1]) # No

# liveness plots improved a lot, test not passed because of a few points in the tails

b_liveness <- boxcox(lm(liveness ~ 1))
lambda <- b_liveness$x[which.max(b_liveness$y)]
liveness_tran <- (liveness ^ lambda - 1) / lambda

qqnorm(liveness_tran[IsWinner == 0])
grid()               
qqline(liveness_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(liveness_tran[IsWinner == 1])
grid()               
qqline(liveness_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(liveness_tran[IsWinner == 0]) # No 
shapiro.test(liveness_tran[IsWinner == 1]) # No

# loudness boxcox transformation not directly applicable because the variable
# is always negative, I multiplied the values by -1 and then applied it, 
# it gave very good results, but we need to pay attention to the interpretation

new_loudness = loudness * (-1)

b_loudness <- boxcox(lm(new_loudness ~ 1))
lambda <- b_loudness$x[which.max(b_loudness$y)]
loudness_tran <- (new_loudness ^ lambda - 1) / lambda

qqnorm(loudness_tran[IsWinner == 0])
grid()               
qqline(loudness_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(loudness_tran[IsWinner == 1])
grid()               
qqline(loudness_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(loudness_tran[IsWinner == 0]) # Yes
shapiro.test(loudness_tran[IsWinner == 1]) # Yes

# tempo, slight improvement in the plots

b_tempo <- boxcox(lm(tempo ~ 1))
lambda <- b_tempo$x[which.max(b_tempo$y)]
tempo_tran <- (tempo ^ lambda - 1) / lambda

qqnorm(tempo_tran[IsWinner == 0])
grid()               
qqline(tempo_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(tempo_tran[IsWinner == 1])
grid()               
qqline(tempo_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(tempo_tran[IsWinner == 0]) # No
shapiro.test(tempo_tran[IsWinner == 1]) # No

# valence, pretty much the same

b_valence <- boxcox(lm(valence ~ 1))
lambda <- b_valence$x[which.max(b_valence$y)]
valence_tran <- (valence ^ lambda - 1) / lambda

qqnorm(valence_tran[IsWinner == 0])
grid()               
qqline(valence_tran[IsWinner == 0],lwd = 2, col = "red")

qqnorm(valence_tran[IsWinner == 1])
grid()               
qqline(valence_tran[IsWinner == 1],lwd = 2, col = "red")

shapiro.test(valence_tran[IsWinner == 0]) # No
shapiro.test(valence_tran[IsWinner == 1]) # No


