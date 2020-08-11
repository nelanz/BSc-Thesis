library(forecast)
library(tsbox)

# --- time series decomposition and forecasting
create_ts <- function(City_ag) {
  City_ts <- City_ag %>%
    select(YYYYMMDD, T2M) 
  
  return(City_ts)
}

Wroclaw_ts <- create_ts(daily_ag_Wroclaw)


Wroclaw_ts <- ts_ts(Wroclaw_xts)

Wroclaw_ts <- as.ts(Wroclaw_ts, frequency = 365)

model1 <- tslm(Wroclaw_ts ~ trend + season)

summary(model1)

to.monthly(Wroclaw_xts, indexAt = 'yearmon')

Wroclaw_D1 <- diff(Wroclaw_xts, differences = 1)

autoplot(Wroclaw_D1)
autoplot(Wroclaw_xts)

Wroclaw_decomposed <- diff(Wroclaw_xts, "periodic")
