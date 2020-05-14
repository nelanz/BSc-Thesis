library(nasapower)
library(xts)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggfortify)

#-------- several cities from around the world and their daily temperature in the last 35 years
# Wroclaw, Poland
daily_ag_Wroclaw <- get_power(
                      community = 'AG',
                      lonlat = c(17.0385, 51.1079),
                      dates = c("01.01.1985", "01.01.2020"),
                      pars = c('T2M'),
                      temporal_average = "DAILY")
#Reykjavik, Iceland
daily_ag_Rykjavik <- get_power(
                      community = 'AG',
                      lonlat = c(-21.827774, 64.128288),
                      dates = c("01.01.1985", "01.01.2020"),
                      pars = c('T2M'),
                      temporal_average = "DAILY")
# Melbourne, Australia
daily_ag_Melbourne <- get_power(
                        community = 'AG',
                        lonlat = c(144.963058, -37.813629),
                        dates = c("01.01.1985", "01.01.2020"),
                        pars = c('T2M'),
                        temporal_average = "DAILY")
# Rio de Janeiro, Brazil
daily_ag_Rio <- get_power(
                  community = 'AG',
                  lonlat = c(-43.172897, -22.906847),
                  dates = c("01.01.1985", "01.01.2020"),
                  pars = c('T2M'),
                  temporal_average = "DAILY")

# New York City, USA
daily_ag_NYC <- get_power(
                  community = 'AG',
                  lonlat = c(-74.005974, 40.712776),
                  dates = c("01.01.1985", "01.01.2020"),
                  pars = c('T2M'),
                  temporal_average = "DAILY")

# New Delhi, India
daily_ag_Delhi <- get_power(
                    community = 'AG',
                    lonlat = c(77.216721, 28.6448),
                    dates = c("01.01.1985", "01.01.2020"),
                    pars = c('T2M'),
                    temporal_average = "DAILY")



#---------create time series fot daily temperature in each city

create_xts <- function(data) {
  return(xts(x = data$T2M, order.by = data$YYYYMMDD))
}

Wroclaw_xts <- create_xts(daily_ag_Wroclaw)
Reykjavik_xts <- create_xts(daily_ag_Rykjavik)
Melbourne_xts <- create_xts(daily_ag_Melbourne)
Rio_xts <- create_xts(daily_ag_Rio)
NYC_xts <- create_xts(daily_ag_NYC)
Delhi_xts <- create_xts(daily_ag_Delhi)

#---------- data exploration

plot_Wroclaw <- autoplot(Wroclaw_xts, ts.col = 'purple') + 
  labs(x = 'Time', y = 'Temperature', title = 'Wroclaw, Poland')

plot_Reykjavik <- autoplot(Reykjavik_xts, ts.col = 'purple')  + 
  labs(x = 'Time', y = 'Temperature', title = 'Reykjavik, Iceland')

plot_Melbourne <- autoplot(Melbourne_xts, ts.col = 'purple') + 
  labs(x = 'Time', y = 'Temperature', title = ' Melbourne, Australia')

plot_Rio <- autoplot(Rio_xts, ts.col = 'purple') + 
  labs(x = 'Time', y = 'Temperature', title = 'Rio de Janeiro, Brasil')

plot_NYC <- autoplot(NYC_xts, ts.col = 'purple') + 
  labs(x = 'Time', y = 'Temperature', title = 'New York City, USA')

plot_Delhi <- autoplot(Delhi_xts, ts.col = 'purple') + 
  labs(x = 'Time', y = 'Temperature', title = 'New Delhi, India')

grid.arrange(plot_Wroclaw, plot_Reykjavik, plot_Melbourne, plot_Rio, plot_NYC, plot_Delhi, 
             ncol=2, top = textGrob("Daily temperature, 01.01.1985 - 01.01.2020", gp=gpar(fontsize=15, fontface='bold')))

start(wroclaw_xts)
end(wroclaw_xts)
time(wroclaw_xts)
deltat(wroclaw_xts)
frequency(wroclaw_xts)
cycle(wroclaw_xts)

#----------- autocorrelation
#--- Wroclaw, Poland

create_acf <- function(City_xts, City_name) {
  title <- paste("Autocorrelation of temperature in ", City_name)
  
  par(mfrow=c(3,2))
  
  acf_30_days <- acf(City_xts, lag.max = 30, plot = T, main = "30 days")
  acf_6_months <- acf(City_xts, lag.max = 30 * 6, plot = T, main = "6 months")
  acf_1_year <- acf(City_xts, lag.max = 365, plot = T, main = "1 year")
  acf_5_years <- acf(City_xts, lag.max = 365 * 5, plot = T, main = "5 years")
  acf_10_years <- acf(City_xts, lag.max = 365 * 10, plot = T, main = "10 years")
  acf_15_years <- acf(City_xts, lag.max = 365 * 20, plot = T, main = "20 years")
  
  mtext(title, outer = T, cex = 1, line = -1.5, font=2)
}

create_acf(Wroclaw_xts, 'Wroclaw, Poland')
create_acf(Reykjavik_xts, "Reykjavik, Iceland")
create_acf(Melbourne_xts, "Melbourne, Australia")
create_acf(Rio_xts, "Rio de Janeiro, Brasil")
create_acf(NYC_xts, "New York City, USA")
create_acf(Delhi_xts, "New Delhi, India")

#--------------DECOMPOSING

#---- Moving Avarage (MA) -- identification of trend
library(stats)
library(zoo)
library(tidyr)
library(RColorBrewer)
library(scales)

# creating MA model 
create_MA <- function(City_xts, MA_order) {
  MA <- stats::filter(City_xts, sides = 2, filter = rep(1/MA_order, MA_order))

  return(MA)
}

# basic plots
Ma_Wroclaw_1_month <- create_MA(Wroclaw_xts, 30)
Ma_Wroclaw_6_months <- create_MA(Wroclaw_xts, 180)
Ma_Wroclaw_1_year <- create_MA(Wroclaw_xts, 360)
Ma_Wroclaw_2_years <- create_MA(Wroclaw_xts, 720)
Ma_Wroclaw_3_years <- create_MA(Wroclaw_xts, 1080)

ts.plot(Wroclaw_xts, col= "grey")
lines(Ma_Wroclaw_1_month, col="red")
lines(Ma_Wroclaw_6_months, col = "blue")
lines(Ma_Wroclaw_1_year, col="green")
lines(Ma_Wroclaw_2_years, col="magenta")
lines(Ma_Wroclaw_3_years, col="darkorange")

# function for creating MA model in ggplot2
create_MA_plot <- function(City_ag, City_name) {
  City_ts <- City_ag %>%
    select(YYYYMMDD, T2M) %>%
    mutate(temp_MA_1 = rollmean(T2M, k=30, fill = NA),
           temp_MA_2 = rollmean(T2M, k=180, fill = NA),
           temp_MA_3 = rollmean(T2M, k = 360, fill = NA),
           temp_MA_4 = rollmean(T2M, k=720, fill=NA),
           temp_MA_5 = rollmean(T2M, k=1080, fill=NA))
  
  MA_plot <- City_ts %>%
    gather(k, value, T2M:temp_MA_5) %>%
    ggplot(aes(YYYYMMDD, value, color=k)) +
    geom_line(size=1.05) +
    labs(x="Date", title="Simple Moving Avarage Model", subtitle = City_name) +
    scale_color_brewer(name="2q + 1",
                       breaks = c('T2M', 'temp_MA_1', 'temp_MA_2', 'temp_MA_3', 'temp_MA_4', 'temp_MA_5'),
                       labels=c("No MA", "1 month", "6 months", "1 year", "2 years", "3 years"),
                       palette = 'Set2', 
                       direction = -1)
  return(MA_plot)
}

# zoom of ggplot2 to identify over- and undersmoothing
zoom_MA_plot <- function(City_ag, City_name, start_date = "2015-01-01", end_date = "2020-01-01") {
  ylim_min <- min(City_ag$T2M)
  ylim_max <- max(City_ag$T2M)
  MA_plot <- create_MA_plot(City_ag, City_name)
  Ma_plot_zoom <- MA_plot +  coord_cartesian(xlim = c(as.Date(start_date), as.Date(end_date)), ylim = c(ylim_min, ylim_max))
  
  return(Ma_plot_zoom)
}

create_MA_plot(daily_ag_Wroclaw, "Wroclaw, Poland")
zoom_MA_plot(daily_ag_Wroclaw, "Wroclaw, Poland")
zoom_MA_plot(daily_ag_Wroclaw, "Wroclaw, Poland", start_date = "2000-01-01", end_date = "2020-01-01")

create_MA_plot(daily_ag_Rykjavik, "Reykjavik, Iceland")
zoom_MA_plot(daily_ag_Rykjavik, "Reykjavik, Iceland")

#--- more general decomposition
create_ts <- function(City_ag) {
  City_ts <- City_ag %>%
    select(YYYYMMDD, T2M) 
  
  return(City_ts)
}

Wroclaw_ts <- create_ts(daily_ag_Wroclaw)

# stats::decompose(Wroclaw_xts, type = "additive")


