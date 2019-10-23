#ARIMAX 
#Author: Jackson Perry

########################
#Import the data
########################
df <- read_csv("Desktop/Fall 2/TS/TS_proj_agg.csv")

########################
#Create time series objects
########################
Y_t <- ts(df$PM25[1:54],frequency=12)
X_ts <- as.matrix(df[1:54,4:14])
Y_ttest <- ts(df$PM25[55:60])
 
########################
#Plot and look at ACF, PACF
########################
plot(Y_t)
acf(Y_t, lag.max=24)
pacf(Y_t, lag.max=24)

########################
#Basic ARIMA
########################
model.1 <- auto.arima(Y_t)
summary(model.1)
acf(model.1$residuals, lag.max=24)
pacf(model.1$residuals, lag.max=24)
Box.test(model.1$residuals, lag = 24, type = c("Ljung-Box"))

########################
#Find best predictors
########################
# Fit the full model 
full.model <- lm(df$PM25 ~ df$AWND + df$SNOW + df$SNWD + df$TAVG + df$TMIN + df$TMAX
                 + df$WSF2 + df$WSF5 + df$CO + df$SO + df$NO, data = df)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
#looks like CO, SO and TAVG are the best

X_ts <- as.matrix(df[1:54,c(7,12,13)])


########################
#Add in the regressor variables
########################
model.2 <- auto.arima(Y_t, xreg=X_ts)
summary(model.2)
acf(model.2$residuals, lag.max=24)
pacf(model.2$residuals, lag.max=24)
Box.test(model.2$residuals, lag = 24, type = c("Ljung-Box"))
White.LB <- rep(NA, 10)
for(i in 1:24){
  White.LB[i] <- Box.test(model.2$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}
White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

########################
#Forecast
########################
X_ts <- as.matrix(df[55:60,c(7,12,13)])
forecast <- forecast(model.2, xreg=X_ts, h=6)
plot(forecast)

########################
#Add in the fourier variables
########################
regressors - cbind(X_ts, fourier(Y_t,K=4))
model.3 <- auto.arima(Y_t, xreg=regressors)
summary(model.3)
acf(model.3$residuals, lag.max=24)
pacf(model.3$residuals, lag.max=24)
Box.test(model.3$residuals, lag = 24, type = c("Ljung-Box"))
White.LB <- rep(NA, 10)
for(i in 1:24){
  White.LB[i] <- Box.test(model.2$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}
White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

########################
#Forecast again
########################
X_ts <- as.matrix(df[55:60,c(7,12,13)])

newregressors <- cbind(X_ts,fourier(Y_ttest, K=4))
#X_ts <- as.matrix(df[55:60,c(7,12,13)])
forecast <- forecast(model.2, xreg=newregressors, h=6)
plot(forecast)

########################
#Calculate error
########################
error = abs(forecast$mean-df$PM25[55:60])
MAE = sum(error)/6
MAE
MAPE = sum(error/df$PM25[55:60])/6
MAPE