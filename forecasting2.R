##### Get training set

# Wroclaw, Poland
training_Wroclaw <- get_power(
  community = 'AG',
  lonlat = c(17.0385, 51.1079),
  dates = c("01.01.2020", "31.03.2020"),
  pars = c('T2M'),
  temporal_average = "DAILY")

#Reykjavik, Iceland
training_Rykjavik <- get_power(
  community = 'AG',
  lonlat = c(-21.827774, 64.128288),
  dates = c("01.01.2020", "31.03.2020"),
  pars = c('T2M'),
  temporal_average = "DAILY")


training_Rykjavik$YYYYMMDD[training_Rykjavik$T2M == min(training_Rykjavik$T2M)]

# Melbourne, Australia
training_Melbourne <- get_power(
  community = 'AG',
  lonlat = c(144.963058, -37.813629),
  dates = c("01.01.2020", "31.03.2020"),
  pars = c('T2M'),
  temporal_average = "DAILY")

training_Melbourne$YYYYMMDD[training_Melbourne$T2M == max(training_Melbourne$T2M)]

# Rio de Janeiro, Brazil
training_Rio <- get_power(
  community = 'AG',
  lonlat = c(-43.172897, -22.906847),
  dates = c("01.01.2020", "31.03.2020"),
  pars = c('T2M'),
  temporal_average = "DAILY")

# New York City, USA
training_NYC <- get_power(
  community = 'AG',
  lonlat = c(-74.005974, 40.712776),
  dates = c("01.01.2020", "31.03.2020"),
  pars = c('T2M'),
  temporal_average = "DAILY")

# New Delhi, India
training_Delhi <- get_power(
  community = 'AG',
  lonlat = c(77.216721, 28.6448),
  dates = c("01.01.2020", "31.03.2020"),
  pars = c('T2M'),
  temporal_average = "DAILY")


##### Get training xts

get_training_xts <- function(training_ag) {
  comparison_df <- training_ag%>%
    select(YYYYMMDD, T2M)
  
  comparison_xts <- xts(comparison_df$T2M, order.by = comparison_df$YYYYMMDD)
  
  return(comparison_xts)
}

training_Wroclaw_xts <- get_training_xts(training_Wroclaw)
training_Rykjavik_xts <- get_training_xts(training_Rykjavik)
training_Melbourne_xts <- get_training_xts(training_Melbourne)
training_Rio_xts <- get_training_xts(training_Rio)
training_NYC_xts <- get_training_xts(training_NYC)
training_Delhi_xts <- get_training_xts(training_Delhi)

order_Wr <- ar_Wroclaw$order
fit_Wroclaw <- Arima(Wroclaw_xts, c(order_Wr, 1, 0))

order_Ry <- ar_Reykjavik$order
fit_Ry <- Arima(Reykjavik_xts, c(order_Ry, 1, 0))

order_Ml <- ar_Melbourne$order
fit_Ml <- Arima(Melbourne_xts, c(order_Ml, 1, 0))

order_Rio <- ar_Rio$order
fit_Rio <- Arima(Rio_xts, c(order_Rio, 1 , 0))

order_NYC <- ar_NYC$order
fit_NYC <- Arima(NYC_xts, c(order_NYC, 1, 0))

order_Delhi <- ar_Delhi$order
fit_Delhi <- Arima(Delhi_xts, c(order_Delhi, 1, 0))

get_forecasting_plots <- function(City_xts, Training_xts, City_fit, City_name) {
  fit <- Arima(Training_xts, model = City_fit)
  
  df1 <- data.frame(date = index(Training_xts), T2M = coredata(Training_xts))
  df2 <- data.frame(date = index(Training_xts), T2M = fit$fitted)
  
  colors <- c("Actual" = "#66C2A5", "Predicted" = "#FC8D62")
  
  ggplot() +
    geom_line(data= df1, aes(x = date, y = T2M, color = "Actual"), size = 1, linetype = "dashed") +
    geom_line(data = df2, aes(x = date, y = T2M, color = "Predicted"), size = 1) +
    scale_color_manual(values = colors) +
    scale_x_date(date_labels = "%b %y") +
    labs(x = "Date", y= "Temperature", title = "Forecasting", subtitle = City_name) +
    theme(legend.position = "top", legend.title = element_blank())
}


display.brewer.pal("Set2", n = 8)
brewer.pal("Set2", n = 8)

plot_Wroclaw <- get_forecasting_plots(Wroclaw_xts, training_Wroclaw_xts, fit_Wroclaw, "Wroclaw, Poland")
plot_Reykjavik <- get_forecasting_plots(Reykjavik_xts, training_Rykjavik_xts, fit_Ry, "Reykjavik, Iceland")
plot_Melbourne <- get_forecasting_plots(Melbourne_xts, training_Melbourne_xts, fit_Ml, "Melbourne, Australia")
plot_Rio <- get_forecasting_plots(Rio_xts, training_Rio_xts, fit_Rio, "Rio de Janeiro, Brasil")
plot_NYC <- get_forecasting_plots(NYC_xts, training_NYC_xts, fit_NYC, "New York City, USA")
plot_Delhi <- get_forecasting_plots(Delhi_xts, training_Delhi_xts, fit_Delhi, "New Delhi, India")

### Get residuals plots
get_residuals_plots <- function(City_xts, Training_xts, City_fit, City_name) {
  
  fit <- Arima(Training_xts, model = City_fit)
  
  df_res_tmp <- ts_data.frame(fit$residuals)
  df_residuals <- data.frame(date = index(Training_xts), res = df_res_tmp$value)
  
  ggplot(data = df_residuals, aes(x = date, y = res)) +
    geom_line(color = "#E78AC3", size = 1) +
    scale_x_date(date_labels = "%b %y") +
    labs(title = "Forecast errors", subtitle = City_name, x = "Date", y = "Temperature") +
    geom_hline(yintercept = sqrt(fit$sigma2), color = "blue", linetype = "dashed") +
    geom_hline(yintercept = -sqrt(fit$sigma2), color = "blue", linetype = "dashed") +
    geom_hline(yintercept = 3*sqrt(fit$sigma2), color = "cornflowerblue", linetype = "dashed") +
    geom_hline(yintercept = -3*sqrt(fit$sigma2), color = "cornflowerblue", linetype = "dashed")
}

residuals_Wroclaw <- get_residuals_plots(Wroclaw_xts, training_Wroclaw_xts, fit_Wroclaw, "Wroclaw, Poland")
residuals_Reykjavik <- get_residuals_plots(Reykjavik_xts, training_Rykjavik_xts, fit_Ry, "Reykjavik, Iceland")
residuals_Melbourne <- get_residuals_plots(Melbourne_xts, training_Melbourne_xts, fit_Ml, "Melbourne, Australia")
residuals_Rio <- get_residuals_plots(Rio_xts, training_Rio_xts, fit_Rio, "Rio de Janeiro, Brasil")
residuals_NYC <- get_residuals_plots(NYC_xts, training_NYC_xts, fit_NYC, "New York City, USA")
residuals_Delhi <- get_residuals_plots(Delhi_xts, training_Delhi_xts, fit_Delhi, "New Delhi, India")


#### comparison
Wroclaw <- grid.arrange(ncol = 2, plot_Wroclaw, residuals_Wroclaw)
Reykjavik <- grid.arrange(ncol = 2, plot_Reykjavik, residuals_Reykjavik)
Melbourne <- grid.arrange(ncol = 2, plot_Melbourne, residuals_Melbourne)
grid.arrange(ncol = 2, plot_Rio, residuals_Rio)
NYC <- grid.arrange(ncol = 2, plot_NYC, residuals_NYC)
Delhi <- grid.arrange(ncol = 2, plot_Delhi, residuals_Delhi)


fit <- Arima(training_Delhi_xts, model = fit_Ml)

df_res_tmp <- ts_data.frame(fit$residuals)
df_residuals <- data.frame(date = index(training_Delhi_xts), res = df_res_tmp$value)
plot(x = df_residuals$date, y = df_residuals$res, type = 'l')
lines(sqrt(fit$sigma2))
