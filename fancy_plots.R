xyplot(Melbourne_xts, strip = T, cut = list(number = 3, overlap = 0.5))


ggseasonplot(Wroclaw_ts_1, year.labels = F, season.labels = T, s = 365, labelgap = 0.5)


#---- seasonal plot
seasonal_plots <- function(City_xts, City_name) {
  City_df <-  ts_data.frame(City_xts) 
  
  City_df <- City_df %>%
    mutate(date = ymd(time))
  
  p <- City_df %>% 
    mutate(
      year = factor(year(date)),     # use year to define separate curves
      date = update(date, year = 1)  # use a constant year for the x-axis
    ) %>% 
    ggplot(aes(date, value, color = year)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")
  
  p + geom_smooth(se=F) + scale_color_discrete(guide = F) + labs(title = City_name, x = "Month", y = "Temperature") +
    ylim(c(-15, 40))
}

Wroclaw_seasonal <- seasonal_plots(Wroclaw_xts, "Wroclaw, Poland")
Reykjavik_seasonal <- seasonal_plots(Reykjavik_xts, "Reykjavik, Iceland")
Melbourne_seasonal <- seasonal_plots(Melbourne_xts, "Melbourne, Australia")
Rio_seasonal <- seasonal_plots(Rio_xts, "Rio de Janeiro, Brasil")
NYC_seasonal <- seasonal_plots(NYC_xts, "New York City, USA")
Delhi_seasonal <- seasonal_plots(Delhi_xts, "New Delhi, India")

grid.arrange(Wroclaw_seasonal, Reykjavik_seasonal, Melbourne_seasonal, Rio_seasonal, NYC_seasonal, Delhi_seasonal, 
             ncol=2)



