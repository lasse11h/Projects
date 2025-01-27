library(readr)
Spotify <- read_csv("Spotify_Youtube.csv")
View(Spotify)

#All required packages
library(ggplot2)
library(car)
library(ggplot2)
library(gridExtra)
library(caret)
library(lmtest)

#Removing all NA variables
any(is.na(Spotify))
which(!complete.cases(Spotify))
Spotify <- na.omit(Spotify)
which(!complete.cases(Spotify))

#Selecting only relevant variables. 12:14 could be dependent
Data <- Spotify[, c(8:18, 22, 23, 24, 28)]
Data <- as.data.frame(Data)
head(Data)
nrow(Data)

#Descriptives table
summary_table <- data.frame(
  Variable = names(Data),
  Minimum = sapply(Data, function(x) format(min(x), scientific = FALSE)),
  Maximum = sapply(Data, function(x) format(max(x), scientific = FALSE))
);print(summary_table)

#Initial model
model <- lm(
  Stream ~ Danceability + Energy + Key + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo + Duration_ms,
  data = Data
)

summary(model)


"blue"##LOOCV SPLIT WHEN TRAINING AND TESTING DATA
fitted_value <- NULL

for(i in 1:nrow(Data)) {
  traindataL = Data[-i ,]
  testdataL = Data[i ,]
  modelL <-
    lm(
      Stream ~ Danceability + Energy + Key + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo + Duration_ms,
      data = traindataL
    )
  fitted_value[i] <- predict(modelL, testdataL)
}

rsqL <- cor(fitted_value, Data$Stream)^2
RSS_L <- sum((fitted_value - Data$Stream)^2)


#FINAL MODEL
#Based on summary(model), we removed non-significant variables
Model_Final <- lm(Stream ~ Danceability + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence, 
                  data = Data)

fitted_value_final <- NULL

for(i in 1:nrow(Data)) {
  traindatafinal = Data[-i ,]
  testdatafinal = Data[i ,]
  Model_Final <-
    lm(Stream ~ Danceability + Energy + Loudness + Speechiness + Acousticness + 
         Instrumentalness + Liveness + Valence, 
       data = traindatafinal)
  fitted_value_final[i] <- predict(Model_Final, testdatafinal)
}

summary(Model_Final)


# Model Quality for LOOCV
Residuals <- (Data$Stream - fitted_value_final)^2
PRESS <- sum(Residuals)
TSS <- sum((Data$Stream - mean(Data$Stream))^2)
R2 <- 1 - (PRESS / TSS)
print(R2)

rmse_final <- sqrt(mean((Data$Stream - fitted_value_final)^2))
print(rmse_final)

# Assumption 1: Linearity
# Plotting observed vs. predicted values
LinD <- ggplot(Data, aes(Stream, Danceability)) + geom_point()
LinE <- ggplot(Data, aes(Stream, Energy)) + geom_point()
LinL <- ggplot(Data, aes(Stream, Loudness)) + geom_point()
LinS <- ggplot(Data, aes(Stream, Speechiness)) + geom_point()
LinA <- ggplot(Data, aes(Stream, Acousticness)) + geom_point()
LinI <- ggplot(Data, aes(Stream, Instrumentalness)) + geom_point()
LinLi <- ggplot(Data, aes(Stream, Liveness)) + geom_point()
LinV <- ggplot(Data, aes(Stream, Valence)) + geom_point()

Linearity <- grid.arrange(LinD, LinE, LinL, LinS, LinA, LinI, LinLi, LinV, ncol = 4, nrow = 2)  
print(Linearity)

plot(Data$Stream, Data$Tempo)
plot(Data$Stream, Data$Duration_ms)
plot(Data$Stream, Data$Key)

# Assumption 2: Independence
# Residual vs. Fitted plot
plot(Model_Final, which = 1)

# Assumption 3: Homoscedasticity
# Scale-Location plot
plot(Model_Final, which = 3)

# Assumption 4: Normality of residuals
# Q-Q plot
plot(Model_Final, which = 2)

# Assumption 5: No multicollinearity
# Variance Inflation Factor (VIF)
vif(Model_Final)

# Assumption 6: No endogeneity
# Durbin-Watson test
dwtest(Model_Final)