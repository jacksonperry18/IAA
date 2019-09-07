#Time Series Homework 2
#Author: Jackson Perry

######################
#Part 1: capture, agg, split data
######################
pm <- read_csv("Desktop/Fall1/PM_2_5_Raleigh2.csv")
pm$mon <- month(as.POSIXlt(pm$Date, format="%m/%d/%Y"))
pm$yr <- year(as.POSIXlt(pm$Date, format="%m/%d/%Y"))
agg <- aggregate(pm$`Daily Mean PM2.5 Concentration` ~ pm$mon + pm$yr, FUN = mean)
names(agg) <- c("month", "yr", "meanprecip")
series_t <- ts(agg$meanprecip[1:54], frequency = 12)
series_v <- ts(agg$meanprecip[55:60], frequency = 12)

#######################
#Part 2: ESM
#######################
plot(series_t)
#looks additive to me
#seasonal component may change over time, so we use STL
decomp_stl <- stl(series_t, s.window = 7)
plot(decomp_stl)

#Simple ESM
SES.pm <- ses(series_t, initial = "optimal", h = 12)
summary(SES.pm)

plot(SES.pm, main = "US Steel Shipments with Simple ESM Forecast", xlab = "Date", ylab = "Shipments (Thousands of Net Tons)")
abline(v = 2014, col = "red", lty = "dashed")
round(accuracy(SES.pm),2)

autoplot(SES.pm)+
  autolayer(fitted(SES.pm),series="Fitted")+ylab("US Steel Shipments with Simple ESM Forecast")
lines(series_f,col="black",lwd=2)

#######################
#Part 3: Plots
#######################
#trend/cycle
plot(series_t, col = "grey", main = "PM Monthly - Trend/Cycle", xlab = "", ylab = "Particulate Matter 2.5", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)

#seasonally adjusted
seas_pm=series_t-decomp_stl$time.series[,1]
plot(series_t, col = "grey", main = "PM Monthly - Seasonally Adjusted", xlab = "", ylab = "Particulate Matter 2.5", lwd = 2)
lines(seas_pm, col = "red", lwd = 2)

#######################
#calculation of MAPE
#######################
percerr = 0
for(i in 1:6){
  percerr = percerr + ((series_t[i] - 8.862303)/series_t[i])
}
percerr = percerr/6
print(percerr)
#.2462567





