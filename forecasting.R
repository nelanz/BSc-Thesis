Wroclaw_coeff <- ar_Wroclaw$ar
last_7_Wroclaw <- tail(Wroclaw_xts, 7)
X_1 <- sum(Wroclaw_coeff * last_7_Wroclaw)
X_2 <- Wroclaw_coeff[1] * X_1 + sum(Wroclaw_coeff[2:7] * last_7_Wroclaw[1:6])
X_3 <- Wroclaw_coeff[1] * X_2 + Wroclaw_coeff[2] * X_1 + sum(Wroclaw_coeff[3:7] * last_7_Wroclaw[1:5])
X_4 <- Wroclaw_coeff[1] * X_3 + Wroclaw_coeff[2] * X_2 + Wroclaw_coeff[3] * X_1 + sum(Wroclaw_coeff[4:7] * last_7_Wroclaw[1:4])
X_5 <- Wroclaw_coeff[1] * X_4 + Wroclaw_coeff[2] * X_3 + Wroclaw_coeff[3] * X_2 + Wroclaw_coeff[4] * X_1 + sum(Wroclaw_coeff[5:7] * last_7_Wroclaw[1:3])

pred1 <- c(X_1, X_2, X_3, X_4, X_5)


comparison_temp_Wroclaw <- get_power(
  community = 'AG',
  lonlat = c(17.0385, 51.1079),
  dates = c("01.01.2020", "31.01.2020"),
  pars = c('T2M'),
  temporal_average = "DAILY")


Arima_wroclaw <- Arima(Wroclaw_xts, c(7, 1, 0))

Arima_coef_Wroclaw <- Arima_wroclaw$coef

X_1_1 <- sum(Arima_coef_Wroclaw * last_7_Wroclaw)
X_2_1 <- Arima_coef_Wroclaw[1] * X_1_1 + sum(Arima_coef_Wroclaw[2:7] * last_7_Wroclaw[1:6])
X_3_1 <- Arima_coef_Wroclaw[1] * X_2_1 + Arima_coef_Wroclaw[2] * X_1_1 + sum(Arima_coef_Wroclaw[3:7] * last_7_Wroclaw[1:5])
X_4_1 <- Arima_coef_Wroclaw[1] * X_3_1 + Arima_coef_Wroclaw[2] * X_2_1 + Arima_coef_Wroclaw[3] * X_1_1 + sum(Arima_coef_Wroclaw[4:7] * last_7_Wroclaw[1:4])
X_5_1 <- Arima_coef_Wroclaw[1] * X_4_1 + Arima_coef_Wroclaw[2] * X_3_1 + Arima_coef_Wroclaw[3] * X_2_1 + Arima_coef_Wroclaw[4] * X_1_1 + sum(Arima_coef_Wroclaw[5:7] * last_7_Wroclaw[1:3])

pred2 <- c(X_1_1, X_2_1, X_3_1, X_4_1, X_5_1)

Wroclaw_1 <- seasadj(decompose(Wroclaw_ts_1, "additive"))
ar(Wroclaw_1)
Wroclaw_ar

order_dates <- comparison_temp_Wroclaw$YYYYMMDD[1:5]

xts1 <- xts(x = pred1, order.by = order_dates)

xts2 <- xts(pred2, order.by = order_dates)

xts1_binded <- rbind(Wroclaw_xts, xts1)


autoplot(rbind(Wroclaw_xts, xts1), facets = F) +
  coord_cartesian(c(as.Date("2019-07-01"), as.Date("2020-01-05")))

ts1 <- ts(pred1, frequency = 365, start = c(2020, 1))
ts2 <- ts(pred2, frequency = 365, start = c(2020, 1))


autoplot(ts.union(Wroclaw_ts_1, ts1), facets = F)+
  coord_cartesian(c(2019, 2020))

plot(Wroclaw_ts_1, xlim = c(2019.5, 2020))
lines(ts1, col = 2, type = "l", lwd = 2)
lines(ts2, col = 3, type = "l", lwd = 2)
?points

#### data frames 

df_Wroc <- data.frame(date = index(Wroclaw_xts), T2M = coredata(Wroclaw_xts), ifOrg = "original")
df2 <- data.frame(date=index(xts1), T2M = coredata(xts1), ifOrg = "predicted")
df_comp <- data.frame(date = comparison_temp_Wroclaw$YYYYMMDD[1:5], T2M = comparison_temp_Wroclaw$T2M[1:5], ifOrg = "comparison")

df_comp <- df_comp %>%
  mutate(paired = as.numeric( df2$T2M))

df_Wroc_pred <- rbind(df_Wroc, df2)

#prediction plot - how to colour the data???????????

ggplot(data = df_Wroc_pred, aes(x = date, y = T2M, color = factor(ifOrg))) +
  geom_line(size = 1.2, aes(group = 1))+
  geom_point(size = 2, data = df_comp, aes(x = date, y = T2M, group = factor(ifOrg)), color = "gray") +
  geom_point(size = 2, aes(group = 1)) +
  # geom_path(data = df_comp, aes(group = paired)) +
  coord_cartesian(c(as.Date("2019-12-01"), as.Date("2020-01-05"))) +
  labs(title = "Forecasting", subtitle = "Wroclaw, Poland") +
  scale_color_brewer(palette="Set2") +
  theme(legend.title=element_blank())


#MSE
ar_sigma <- ar_Wroclaw$var.pred

chi0 <- 1
chi1 <- chi0 * Wroclaw_coeff[1]
chi2 <- chi1 * Wroclaw_coeff[1] + chi0 * Wroclaw_coeff[2]
chi3 <- chi2 * Wroclaw_coeff[1] + chi1 * Wroclaw_coeff[2] + chi0 * Wroclaw_coeff[3]
chi4 <- chi3 * Wroclaw_coeff[1] + chi2 * Wroclaw_coeff[2] + chi1 * Wroclaw_coeff[3] + chi0 * Wroclaw_coeff[4]
chi5 <- chi4 * Wroclaw_coeff[1] + chi3 * Wroclaw_coeff[2] + chi2 * Wroclaw_coeff[3] + chi1 * Wroclaw_coeff[4] + chi0 * Wroclaw_coeff[5]

mse0 <- ar_sigma * chi0
mse1 <- ar_sigma * chi1
mse2 <- ar_sigma * chi2
mse3 <- ar_sigma * chi3
mse4 <- ar_sigma * chi4
mse5 <- ar_sigma * chi5

Ar_fit <- arima(Wroclaw_ts_1, order = c(1, 0, 0))
predict_ar <- predict(Ar_fit)
predict(Ar_fit, n.ahead = 10)

######## errors
sigma_2 <- ar_Wroclaw$var.pred
