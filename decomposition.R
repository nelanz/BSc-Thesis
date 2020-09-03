# --- time series decomposition 

#---------- step by step decomposition

daily_ag_Wroclaw_feb <- daily_ag_Wroclaw %>%
  filter(DD == 29 & MM == 2)

daily_ag_Wroclaw_no_feb <- daily_ag_Wroclaw %>%
  filter(!YYYYMMDD %in% daily_ag_Wroclaw_feb$YYYYMMDD) %>%
  select(YYYYMMDD, T2M)

Wroclaw_ts_1 <- ts(daily_ag_Wroclaw_no_feb$T2M, start = 1985, frequency = 365)

frequency(Wroclaw_ts_1)


#------ Melbourne
daily_ag_Melbourne_feb <- daily_ag_Melbourne %>%
  filter(DD == 29 & MM == 2)

daily_ag_Melbourne_no_feb <- daily_ag_Melbourne %>%
  filter(!YYYYMMDD %in% daily_ag_Melbourne_feb$YYYYMMDD) %>%
  select(YYYYMMDD, T2M)

Melbourne_ts_1 <- ts(daily_ag_Melbourne_no_feb$T2M, start = 1985, frequency = 365)



# # trend
# create_MA <- function(City_ts, MA_order) {
#   MA <- stats::filter(City_ts, sides = 2, filter = rep(1/MA_order, MA_order))
#   
#   return(MA)
# }
# 
# Ma_Wroclaw_1_year_ts <- create_MA(Wroclaw_ts_1, 360)
# ts.plot(Wroclaw_ts_1, col= "grey")
# lines(Ma_Wroclaw_1_year_ts, col="green")
# 
# Ma_Melbourne_1_ts <- create_MA(Melbourne_ts_1, 360)
# ts.plot(Melbourne_ts_1, col= "grey")
# lines(Ma_Melbourne_1_ts, col="green")
# 
# # detrend time series
# 
# detrend_Wroclaw_ts <- Wroclaw_ts_1 - Ma_Wroclaw_1_year_ts
# ts.plot(detrend_Wroclaw_ts)
# 
# detrend_Melbourne_ts <- Melbourne_ts_1 - Ma_Melbourne_1_ts
# ts.plot(detrend_Melbourne_ts)



# seasonal factor

matrix_Wroclaw_ts <- t(matrix(data= Wroclaw_xts, nrow = 365))
seasonal_Wroclaw <- colMeans(matrix_Wroclaw_ts, na.rm = T)
ts.plot(rep(seasonal_Wroclaw, 34))

autoplot(as.ts(rep(seasonal_Wroclaw, 35), frequency = 365), colour = "purple", size = 1)

matrix_Melbourne_ts <- t(matrix(data= Melbourne_ts_1, nrow = 365))
seasonal_Melbourne <- colMeans(matrix_Melbourne_ts, na.rm = T)
# autoplot(as.ts(seasonal_Melbourne))
ts.plot(rep(seasonal_Melbourne, 34))


# random noise
random_Wroclaw <- Wroclaw_ts_1 - seasonal_Wroclaw
ts.plot(random_Wroclaw)

autoplot(random_Wroclaw, colour = "#6E016B") +
  labs(title = "Random noise component", subtitle = "Wroclaw, Poland", x = "Date", y = "Temperature")
  
random_Wroclaw_ts <- as.ts(random_Wroclaw, frequency = 365)

par(mfrow = c(1, 2))
Acf(random_Wroclaw_ts, plot = T, na.action = na.pass, lag.max = 365, main = "ACF for random noise component, lag.max = 365")
Pacf(random_Wroclaw_ts, lag.max = 15, main = "PACF for random noise component, lag.max = 15", font = 1)
mtext("Random noise component in Wroclaw, Poland", outer = T, cex = 1, line = -1, font=2)

par(mfrow = c(1,1))

random_Melbourne <- Melbourne_ts_1 - seasonal_Melbourne
random_Melbourne_ts <- as.ts(random_Melbourne, frequency = 365)
Acf(random_Melbourne_ts, lag.max = 365)
Pacf(random_Melbourne_ts, lag.max = 30)

ar(random_Melbourne_ts)

#### GGPLOT FOR ACF AN PACF

ACF_Wroclaw_gg <- ggAcf(random_Wroclaw_ts, lag.max = 365) + 
  labs(title = "ACF for random noise component, lag.max = 365", subtitle = "Wroclaw, Poland")
  
PACF_Wroclaw_gg <- ggPacf(random_Wroclaw_ts, lag.max = 15, size = 1.2) +
  labs(title = "PACF for random noise component, lag.max = 15", subtitle = "Wroclaw, Poland")

grid.arrange(ACF_Wroclaw_gg, PACF_Wroclaw_gg, ncol = 2)




# #----------one function for decomposition
# decompose_Wroclaw <-  decompose(Wroclaw_ts_1, "additive")
# 
# plot(decompose_Wroclaw)
# 
# autoplot(decompose_Wroclaw$seasonal, colour = "purple", size = 1) +
#   labs(title = "Seasonal component", subtitle = "Wroclaw, Poland", x = "Date", y = "Temperature") 
# 
# autoplot(decompose_Wroclaw$random, colour = "#6E016B") +
#   labs(title = "Random noise component", subtitle = "Wroclaw, Poland", x = "Date", y = "Temperature") 
# 
# par(mfrow = c(1,2))
# Acf(ts(decompose_Wroclaw$random, frequency = 1), na.action = na.pass, lag.max = 30,
#     main = "30 days")
# Acf(ts(decompose_Wroclaw$random), na.action = na.pass, lag.max = 365, main = "1 year", font = 1)
# 
# mtext("ACF of random fluctuations in Wroclaw, Poland", outer = T, cex = 1, line = -1, font=2)
# 
# decompose_Melbourne <- decompose(Melbourne_ts_1, "additive")
# autoplot(decompose_Melbourne$seasonal)

#-------------AR model
Wroclaw_ar <- ar(random_Wroclaw_ts, aic = T)
Wroclaw_ar


Wroclaw_fit <- Wroclaw_ar$resid
ts.plot(random_Wroclaw)
points(Wroclaw_fit, type = 'l', col = 2, lty = 2)

checkresiduals(Wroclaw_ar)

autoplot(forecast(Wroclaw_fit))

Wroclaw_diff <- diff(Wroclaw_ts_1, 365)
tsdisplay(Wroclaw_diff)

Wroclaw_ar_1 <- ar(Wroclaw_diff)
checkresiduals(Wroclaw_ar_1)

Acf(Wroclaw_ar$resid)

#---------- auto arima check
Wroclaw_auto_arima <- auto.arima(random_Wroclaw, ic = "aic")

Acf(Wroclaw_auto_arima$residuals)
plot(Wroclaw_auto_arima$residuals)


Wroclaw_forecast_auto_arima <- forecast(Wroclaw_auto_arima, h = 30)
Wroclaw_forecast_auto_arima$mean
plot(Wroclaw_forecast_auto_arima)

#--------- forecast plots

autoplot(Wroclaw_forecast_ar) +
  coord_cartesian(xlim = c(2019, 2020))

autoplot(Wroclaw_forecast_auto_arima)+
  coord_cartesian(xlim = c(2019, 2020))

Wroclaw_forecast_ar <- forecast(Wroclaw_ar, h = 1)

plot(Wroclaw_forecast_ar)

Wroclaw_forecast_ar
