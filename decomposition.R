# --- time series decomposition and forecasting

#------ step by step decomposition

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

# trend
create_MA <- function(City_ts, MA_order) {
  MA <- stats::filter(City_ts, sides = 2, filter = rep(1/MA_order, MA_order))
  
  return(MA)
}

Ma_Wroclaw_1_year_ts <- create_MA(Wroclaw_ts_1, 360)
ts.plot(Wroclaw_ts_1, col= "grey")
lines(Ma_Wroclaw_1_year_ts, col="green")

Ma_Melbourne_1_ts <- create_MA(Melbourne_ts_1, 360)
ts.plot(Melbourne_ts_1, col= "grey")
lines(Ma_Melbourne_1_ts, col="green")

# detrend time series

detrend_Wroclaw_ts <- Wroclaw_ts_1 - Ma_Wroclaw_1_year_ts
ts.plot(detrend_Wroclaw_ts)

detrend_Melbourne_ts <- Melbourne_ts_1 - Ma_Melbourne_1_ts
ts.plot(detrend_Melbourne_ts)

# average the seasonality

matrix_Wroclaw_ts <- t(matrix(data= detrend_Wroclaw_ts, nrow = 365))
seasonal_Wroclaw <- colMeans(matrix_Wroclaw_ts, na.rm = T)
ts.plot(rep(seasonal_Wroclaw, 34))

matrix_Melbourne_ts <- t(matrix(data= detrend_Melbourne_ts, nrow = 365))
seasonal_Melbourne <- colMeans(matrix_Melbourne_ts, na.rm = T)
# autoplot(as.ts(seasonal_Melbourne))
ts.plot(rep(seasonal_Melbourne, 35))

# random noise
random_Wroclaw <- Wroclaw_ts_1 - Ma_Wroclaw_1_year_ts - seasonal_Wroclaw
ts.plot(random_Wroclaw)
acf(as.ts(random_Wroclaw), plot = T, na.action = na.pass, lag.max = 365)

random_Melbourne <- Melbourne_ts_1 - Ma_Melbourne_1_ts - seasonal_Melbourne
ts.plot(random_Melbourne)
acf(as.ts(random_Melbourne), plot = T, na.action = na.pass, lag.max = 300)

# reconstruct
recomposed_Wroclaw <- Wroclaw_ts_1 + Ma_Wroclaw_1_year_ts + seasonal_Wroclaw
ts.plot(recomposed_Wroclaw)

# ------one function for decomposition
decompose_Wroclaw <-  decompose(Wroclaw_ts_1, "additive")

plot(decompose_Wroclaw)

autoplot(decompose_Wroclaw$seasonal, colour = "purple", size = 1) +
  labs(title = "Seasonal component", subtitle = "Wroclaw, Poland", x = "Date", y = "Temperature") 

autoplot(decompose_Wroclaw$random, colour = "#6E016B") +
  labs(title = "Random fluctuations", subtitle = "Wroclaw, Poland", x = "Date", y = "Temperature") 

acf(decompose_Wroclaw$random, na.action = na.pass)
acf(random_Wroclaw, na.action = na.pass)

stl_Wroclaw <- stl(Wroclaw_ts_1, "periodic")
plot(stl_Wroclaw)

decompose_Melbourne <- decompose(Melbourne_ts_1, "additive")
autoplot(decompose_Melbourne$seasonal)

# autocorrelation
acf(as.ts(stl_Wroclaw$time.series[,3]), plot = T, na.action = na.pass, lag.max = 365)

