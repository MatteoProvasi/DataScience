# !diagnostics off

# Libraries
library(dplyr)     # handling datasets
library(dygraphs)  # graphs
library(forecast)  # predictions 
library(ggplot2)   # plots
library(KFAS)      # UCM
library(lmtest)    # coefficients' test
library(lubridate) # date
library(MLmetrics) # stats
library(tseries)   # time series and tests
library(tsfknn)    # machine learning
library(xts)       # time series

# Data read
Sys.setenv(TZ = 'GMT')
dir = "C:\\Users\\prowm\\OneDrive\\Desktop\\Data Science\\Serie Storiche\\Progetto\\energydata_complete.csv"
energy = read.csv(file = dir, sep = ",", header = TRUE)



# Pre-processing
energy = energy[, c(1:2)]
str(energy)
energy$date = as.POSIXct(energy$date, format = "%Y-%m-%d %H:%M:%S")

print(nrow(energy)/6)
for (i in 1:(nrow(energy)-1)){
  diff = as.numeric(energy$date[i] - energy$date[i+1])
  if (diff == -10) {}
  else {
    print(i)
  }
}

energy = energy[-nrow(energy), ]

## Aggregate
head(energy); tail(energy)
energy$time = floor_date(energy$date, "60 mins")
energy = energy %>% 
  group_by(time) %>% 
  summarize(Appliances = sum(Appliances))



# Stationarity
energy_ts = xts(energy$Appliances, order.by = as.POSIXct(energy$time, format = "%Y-%m-%d %H:%M:%S"))
names(energy_ts) = "Appliances"
dygraph(energy_ts, main = "Hourly energy used (in Wh)") %>%
  dyRangeSelector()

medie = tapply(energy_ts, as.Date(index(energy_ts)), mean)
stdev = tapply(energy_ts, as.Date(index(energy_ts)), sd)
plot(medie, stdev)

## Log transformation
print(min(energy_ts$Appliances))
energy_log = log(energy_ts$Appliances)

dygraph(energy_log, main = "Hourly log(energy) used (in Wh)") %>%
  dyRangeSelector()

packageVersion("tseries")
kpss.test(energy_log, null = "Level", lshort = TRUE); kpss.test(energy_log, null = "Trend", lshort = TRUE)



# Test-train split
dim(energy_log)[1] == nrow(energy)
min(energy$time)
cutoff_id = which(energy$time == "2016-04-11 17:00:00")
l_train = ts(energy_log[1:(cutoff_id-1)], frequency = 24)
l_test = ts(energy_log[cutoff_id:dim(energy_log)[1]], frequency = 24)



# ARIMA
acfpacf = function(x, max.lag, na.action = na.pass, main.acf = ''){
  par(mfrow = c(2,1))
  Acf(x, max.lag, main = main.acf)
  Pacf(x, max.lag, main = '')
  par(mfrow = c(1,1))
}

acfpacf(l_train, max.lag = 96)

mod1 = Arima(l_train, c(3,0,0), c(0,0,0))
acfpacf(mod1$residuals, max.lag = 96, main.acf = "ARIMA (3,0,0)(0,0,0)")

mod2 = Arima(l_train, c(3,0,0), seasonal = list(order = c(0,1,0), period = 24))
acfpacf(mod2$residuals, max.lag = 96, main.acf = "ARIMA (3,0,0)(0,1,0)")

mod3 = Arima(l_train, c(3,0,0), seasonal = list(order = c(0,1,1), period = 24))
acfpacf(mod3$residuals, max.lag = 96, main.acf = "ARIMA (3,0,0)(0,1,1)")

mod4 = Arima(l_train, c(3,0,0), seasonal = list(order = c(1,1,1), period = 24))
acfpacf(mod4$residuals, max.lag = 96, main.acf = "ARIMA (3,0,0)(1,1,1)")

checkresiduals(mod3,lag = 300, plot = TRUE)
checkresiduals(mod4,lag = 300, plot = TRUE)

## Auto-ARIMA
ar = auto.arima(l_train, D = 1, stepwise = TRUE, stationary = TRUE, trace = TRUE, approximation = TRUE)  
mod_ar = Arima(l_train, c(3,0,1), seasonal = list(order = c(2,0,0), period = 24))
acfpacf(mod_ar$residuals, max.lag = 96, main.acf = "ARIMA (3,0,1)(2,0,0)")

checkresiduals(mod_ar,lag = 300, plot = TRUE)

summary_mod3 = capture.output(mod3)[c(2,9,10)]
summary_mod4 = capture.output(mod4)[c(2,9,10)]
summary_mod_ar = capture.output(mod_ar)[c(2,9,10)]


## Model comparison
models = as.data.frame(rbind(summary_mod3, summary_mod4, summary_mod_ar))
models

## Predictions
forecast_arima = forecast(l_train, h = dim(l_test)[1], model = mod4)
plot_train = energy_log[1:(cutoff_id-1)]
plot_test = energy_log[cutoff_id:nrow(energy_log)]
names(plot_train) = "Train"
names(plot_test) = "Test"
forecast = xts(forecast_arima$mean, order.by = index(plot_test))

lines = cbind(plot_train, plot_test, forecast)
dygraph(lines, main = "Hourly log(energy) used (in Wh) - ARIMA predictions") %>%
  dySeries("Train", color = "green") %>%
  dySeries("Test", color = "blue") %>%
  dySeries("forecast", color = "red") %>%
  dyRangeSelector()

lines2 = cbind(plot_test, forecast)
dygraph(lines2, main = "Hourly log(energy) used (in Wh) - ARIMA predictions (test)") %>%
  dySeries("Test", color = "blue") %>%
  dySeries("forecast", color = "red") %>%
  dyRangeSelector()

accuracy(forecast_arima)
MAE(forecast_arima$fitted, as.vector(l_train)); MAE(forecast_arima$mean, as.vector(l_test))
MAE(exp(forecast_arima$fitted), as.vector(exp(l_train))); MAE(exp(forecast_arima$mean), as.vector(exp(l_test)))



# UCM
## 1 Seasonal
mod_ucm1 = SSModel(l_train ~ 0 +
                     SSMtrend(1, 0) +
                     SSMseasonal(24, NA, sea.type = "dummy"),
                   H = NA)

init = c(logvar_eps = log(100), logvar_om = log(20))

updt = function(pars, model) {
  model$H[1, 1, 1] = exp(pars[1])
  model$Q[2, 2, 1] = exp(pars[2])
  model
}

fit = fitSSM(mod_ucm1, init, updt)
fit$optim.out$convergence

smo1 = KFS(fit$model)

par(mfrow = c(2,2))

for (i in 1:4){
  plot(smo1$alphahat[, paste0("sea_dummy", as.character(i))], type = "l", 
       ylab = paste0("Componente dummy #", as.character(i)))
}

par(mfrow = c(1,1))
pred_ucm = predict(fit$model, n.ahead = dim(l_test)[1])
forecast_ucm = xts(pred_ucm, order.by = index(plot_test))

lines3 = cbind(plot_train, plot_test, forecast_ucm)
dygraph(lines3, main = "Hourly log(energy) used (in Wh) - UCM predictions (daily seasonal)") %>%
  dySeries("Train", color = "green") %>%
  dySeries("Test", color = "blue") %>%
  dySeries("fit", color = "red") %>%
  dyRangeSelector()

dygraph(energy_log, main = "Hourly log(energy) used (in Wh)") %>%
  dySeries(label = "log(energy)", color = "black") %>%
  dyShading(from = "2016-1-24", to = "2016-1-25", color = "#FFE6E6") %>%
  dyShading(from = "2016-1-30", to = "2016-1-31", color = "#FFE6E6") %>%
  dyRangeSelector()


## 2 Seasonals
mod_ucm2 = SSModel(l_train ~ 0 +
                     SSMtrend(1, 0) +
                     SSMseasonal(24, NA, sea.type = "dummy") +
                     SSMseasonal(168, NA, sea.type = "trig", harmonics = 1:16),
                   H = NA)

init2 = c(logvar_eps = log(100), logvar_om = log(20), logvar_om2 = log(10))

updt2 = function(pars, model) {
  dq = dim(model$Q)[1]
  model$H[1, 1, 1] = exp(pars[1]) #disturbo dummies
  model$Q[2, 2, 1] = exp(pars[2])
  diag(model$Q[3:dq, 3:dq, 1]) = exp(pars[3])
  model
}

fit2 = fitSSM(mod_ucm2, init2, updt2) 
fit2$optim.out$convergence

mean_t = mean(l_train)
var_t = var(l_train)


## 2 Seasonals v2
mod_ucm2 = SSModel(l_train ~ 0 +
                     SSMtrend(1, 0, a1 = mean_t, P1 = var_t) +
                     SSMseasonal(24, NA, sea.type = "dummy", P1 = var_t) +
                     SSMseasonal(168, NA, sea.type = "trig", harmonics = 1:8, P1 = var_t),
                   H = NA)

init2 = c(logvar_eps = log(100), logvar_om = log(20), logvar_om2 = log(50))

updt2 = function(pars, model) {
  dq = dim(model$Q)[1]
  model$H[1, 1, 1] = exp(pars[1])
  model$Q[2, 2, 1] = exp(pars[2])
  diag(model$Q[3:dq, 3:dq, 1]) = exp(pars[3])
  model
}

fit2 = fitSSM(mod_ucm2, init2, updt2)
fit2$optim.out$convergence 

smo2 = KFS(fit2$model, smoothing = 'state')

plot(xts(rowSums(smo2$alphahat[, paste0("sea_trig", 1:8)]),
         order.by = as.POSIXct(energy$time[1:(cutoff_id)-1], format = "%Y-%m-%d %H:%M:%S")),
     type = "l", main = 'Weekly seasonality') 

## Predictions
pred_ucm2 = predict(fit2$model, n.ahead = dim(l_test)[1])
forecast_ucm2 = xts(pred_ucm2, order.by = index(plot_test))

lines4 = cbind(plot_train, plot_test, forecast_ucm2)
dygraph(lines, main = "Hourly log(energy) used (in Wh) - UCM (daily + weekly seasonals)") %>%
  dySeries("Train", color = "green") %>%
  dySeries("Test", color = "blue") %>%
  dySeries("forecast", color = "red") %>%
  dyRangeSelector()

MAE(pred_ucm, as.vector(l_test)); MAE(exp(pred_ucm), as.vector(exp(l_test)))
MAE(pred_ucm2, as.vector(l_test)); MAE(exp(pred_ucm2), as.vector(exp(l_test)))



# Machine Learning (kNN)
knn = knn_forecasting(ts(l_train), h = dim(l_test)[1], lags = 1:24, k = floor(sqrt(dim(l_train)[1])), msas = "MIMO")
autoplot(knn, highlight = "neighbors", faceting = FALSE)

knn3 = knn_forecasting(ts(l_train), h = dim(l_test)[1], lags = 1:24, k = 3, msas = "MIMO")
plot3 = autoplot(knn3, highlight = "neighbors", faceting = FALSE)

knn5 = knn_forecasting(ts(l_train), h = dim(l_test)[1], lags = 1:24, k = 5, msas = "MIMO")
plot5 = autoplot(knn5, highlight = "neighbors", faceting = FALSE)

grid.arrange(arrangeGrob(plot3, top='kNN model with k=3'), 
             arrangeGrob(plot5, top='kNN model with k=5'), ncol = 1)

MAE(exp(knn$prediction), as.vector(exp(l_test)))
MAE(exp(knn3$prediction), as.vector(exp(l_test)))
MAE(exp(knn5$prediction), as.vector(exp(l_test)))

## MAE test
mae_vec = c()
for (i in 1:floor(sqrt(dim(l_train)[1]))){
  knn = knn_forecasting(ts(l_train), h = dim(l_test)[1], lags = 1:24, k = i, msas = "MIMO")
  mae_vec = append(mae_vec, MAE(exp(knn$prediction), as.vector(exp(l_test))))
}
plot(mae_vec, type = "l", ylab= "MAE", xlab = "k", main = "MAE on test set at different k values")
abline(h = min(mae_vec), col = "red", lwd = 0.7)

knn41 = knn_forecasting(ts(l_train), h = dim(l_test)[1], lags = 1:24, k = 41, msas = "MIMO")

## Predictions
forecast_knn41 = xts(knn41$prediction, order.by = index(plot_test))
names(forecast_knn41) = "knn41"

lines6 = cbind(plot_train, plot_test, forecast_knn41)
dygraph(lines6, main = "Hourly log(energy) used (in Wh) - kNN (k=41)") %>%
  dySeries("Train", color = "green") %>%
  dySeries("Test", color = "blue") %>%
  dySeries("knn41", color = "red") %>%
  dyRangeSelector()

MAE(knn41$prediction, as.vector(l_test))
MAE(exp(knn41$prediction), as.vector(exp(l_test)))

