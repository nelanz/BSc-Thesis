############################### automatic decomposition

######### function for the City ts
City_ts_1 <- function(City_ag) {
  daily_ag_feb <- City_ag %>%
    filter(DD == 29 & MM == 2)
  
  daily_ag_City_no_feb <- City_ag %>%
    filter(!YYYYMMDD %in% daily_ag_feb$YYYYMMDD) %>%
    select(YYYYMMDD, T2M)
  
  City_ts_1 <- ts(daily_ag_City_no_feb$T2M, start = 1985, frequency = 365)
  return(City_ts_1)
}

######### Function for the seasonal extraction
get_seasonal_component <- function(City_ts_1) {
  matrix_ts <- t(matrix(data= City_ts_1, nrow = 365))
  seasonal_component <- colMeans(matrix_ts, na.rm = T)
  
  return(seasonal_component)
}

####### random noise 
get_random_noise <- function(city_ts_1, seasonal_component) {
  random <- city_ts_1 - seasonal_component
  return(as.ts(random, frequency = 365))
}

####### ACF and PACF
get_ACF_PACF <- function(random_noise, City_name, pacf_lag) {
  ACF <- ggAcf(random_noise, lag.max = 365) +
    labs(title = "ACF for random noise component, lag.max = 365", subtitle = City_name)
  
  PACF <- ggPacf(random_noise, lag.max = pacf_lag, size = 1.2) +
    labs(title = paste("PACF for random noise component, lag.max = ", pacf_lag), subtitle = City_name)
  
  grid.arrange(ACF, PACF, ncol = 2)
}

###### realisation
Wroclaw_ts_1 <- City_ts_1(daily_ag_Wroclaw)
Wroclaw_seasonal <- get_seasonal_component(Wroclaw_ts_1)
Wroclaw_random <- get_random_noise(Wroclaw_ts_1, Wroclaw_seasonal)
get_ACF_PACF(Wroclaw_random, "Wroclaw, Poland", 20)

Melbourne_ts_1 <- City_ts_1(daily_ag_Melbourne)
Melbourne_seasonal <- get_seasonal_component(Melbourne_ts_1)
Melbourne_random <- get_random_noise(Melbourne_ts_1, Melbourne_seasonal)
get_ACF_PACF(Melbourne_random, "Melbourne, Australia", 20)

Reykjavik_ts_1 <- City_ts_1(daily_ag_Rykjavik)
Reykjavik_seasonal <- get_seasonal_component(Reykjavik_ts_1)
Reykjavik_random <- get_random_noise(Reykjavik_ts_1, Reykjavik_seasonal)
get_ACF_PACF(Reykjavik_random, "Reykjavik, Iceland", 20)

Rio_ts_1 <- City_ts_1(daily_ag_Rio)
Rio_seasonal <- get_seasonal_component(Rio_ts_1)
Rio_random <- get_random_noise(Rio_ts_1, Rio_seasonal)
get_ACF_PACF(Rio_random, "Rio de Janeiro, Brasil", 20)

NYC_ts_1 <- City_ts_1(daily_ag_NYC)
NYC_seasonal <- get_seasonal_component(NYC_ts_1)
NYC_random <- get_random_noise(NYC_ts_1, NYC_seasonal)
get_ACF_PACF(NYC_random, "New York City, USA", 20)

Delhi_ts_1 <- City_ts_1(daily_ag_Delhi)
Delhi_seasonal <- get_seasonal_component(Delhi_ts_1)
Delhi_random <- get_random_noise(Delhi_ts_1, Delhi_seasonal)
get_ACF_PACF(Delhi_random, "New Delhi, India", 20)


######### AR MODELS

par(mfrow = c(1,1))
ar_Wroclaw <- stats::ar(Wroclaw_random, ic = "aic")

autoplot(ar_Wroclaw, data=random_Wroclaw)

Wroclaw_fit <- ar_Wroclaw$resid
ts.plot(Wroclaw_random, col = "gray60", tile = "AR model ")
points(Wroclaw_fit, type = 'l', col = "red3", lty = 2)


ar_Melbourne <- ar(Melbourne_random, ic = "aic")
ar_Melbourne
ar_Reykjavik <- ar(Reykjavik_random, ic = "aic")
ar_Reykjavik
ar_Rio <- ar(Rio_random, ic = "aic")
ar_Rio
ar_NYC <- ar(NYC_random, ic = "aic")
ar_NYC
ar_Delhi <- ar(Delhi_random, ic = "aic")
ar_Delhi


########### FORECASTING
Wroclaw_forecast <- forecast(ar_Wroclaw, h =1)
autoplot(Wroclaw_forecast)  +
  coord_cartesian(xlim = c(2019, 2020))

ts.plot(Wroclaw_ts_1, Wroclaw_forecast)
points(Wroclaw_forecast, col = 2, type = "l")

