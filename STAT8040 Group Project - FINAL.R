# load libraries
library(fpp3)
library(tidyverse)
library(tsibble)
library(knitr)
library(caret)
library(MASS)
library(dplyr)
library(fable.prophet)
library(readxl)

# load dataset
all_production <- read_excel("CANPRINTO01MLM_production.xls")

# 1. data cleaning
all_production <- all_production %>% mutate(Production = Production/1e9)
all_production <- all_production %>% mutate(Date = yearmonth(Date)) %>% as_tsibble(index = Date) %>% dplyr::select(Date, Production)


# 2. data visualization
# Time plot of all data
all_production %>% autoplot(Production) + 
  labs(y = "Billions Canadian Dollar",  x="Month",
       title = "Production: Total industry excluding construction (Canada)",
       subtitle = "1961-2022")

# Time plot from 1983 to 2003
all_production %>% filter(year(Date) <= 2003 & year(Date) >= 1983) %>% 
  autoplot(Production, col = 'purple') +
  labs(y = "Billions Canadian Dollar",  x="Month",
       title = "Production: Total industry excluding construction (Canada)",
       subtitle = "1983-2003")

# Time plot from 2003 to 2022
all_production |> filter(year(Date) >= 2003) |>  
  autoplot(Production, col = 'blue') +
  labs(y = "Billions Canadian Dollar",  x="Month",
       title = "Production: Total industry excluding construction (Canada)",
       subtitle = "2003-2022")

# Time plot from 1983 to 2022
all_production %>% filter(year(Date) >= 1983) %>%
  autoplot(Production, col = 'darkblue') +
  labs(y = "Billions Canadian Dollar",  x="Month",
       title = "Production: Total industry excluding construction (Canada)",
       subtitle = "1983-2022")

# Seasonal plot from 1983 to 2008
all_production |>
  filter(year(Date) <= 2008 & year(Date) >= 1983) |>
  gg_season(Production, labels = "both") +
  labs(y = "Billions Canadian Dollar", x="Month",
       title = "Seasonal plot - Production: Total industry excluding construction (Canada)",
       subtitle = "1983-2008")

# Seasonal plot from 2008 to 2022
all_production |>
  filter(year(Date) >= 2008) |>
  gg_season(Production, labels = "both") +
  labs(y = "Billions Canadian Dollar", x="Month",
       title = "Seasonal plot - Production: Total industry excluding construction (Canada)",
       subtitle = "2008-2022")

# Seasonal subseries from 1983 to 2003
all_production |> 
  filter(year(Date) <= 2003 & year(Date) >= 1983) |> 
  gg_subseries(Production) +
  labs(
    y = "Billions Canadian Dollar",
    title = "Seasonal subseries - Production: Total industry excluding construction (Canada)",
    subtitle = "1983-2003")

# Seasonal subseries from 2003 to 2022
all_production |>
  filter(year(Date) >= 2003) |>
  gg_subseries(Production) +
  labs(
    y = "Billions Canadian Dollar",
    title = "Seasonal subseries - Production: Total industry excluding construction (Canada)",
    subtitle = "2003-2022")

# ACF plot from 1983 to 2003
all_production %>% 
  filter(year(Date) <= 2003 & year(Date) >= 1983) %>%  
  ACF(Production, lag_max = 120) %>% autoplot() +
  labs(title = "Autocorrelation function of monthly Production: Total industry excluding construction (Canada)",
       subtitle = "1983-2003")

# ACF plot from 2003 to 2022
all_production %>%
  filter(year(Date) >= 2003) |>
  ACF(Production, lag_max = 120) %>%
  autoplot() +
  labs(title = "Autocorrelation function of monthly Production: Total industry excluding construction (Canada)",
       subtitle = "2003-2022")


# 3. data transformation
# Do we need Data transformation? Let's check how the observations' variance change over time
production = all_production %>%
  filter(year(Date) >= 1983)

production %>% autoplot(Production, col = 'steelblue') + ylim(0, 40)
# From the timeplot, ignoring the dip in 2009 and 2020 (outlier years), we can see that the variance doesn't change much over time.
# So there is no need for a transformation here.

# Confirming this with a linear regression model and analyzing residual plots
lmodel1 <- lm(Production ~ Date, data = production)

# Histogram of the residuals
hist(lmodel1$residuals)
# QQplot for the residuals
qqnorm(lmodel1$residuals)
qqline(lmodel1$residuals, col = "steelblue", lwd = 2)
# Fitted vs residual plot
plot(lmodel1)

# From the above plots, the histogram of the residuals seem to be normally distributed. It is slightly left skewed, but not much
# Coming to the QQ plot, the residuals almost form a straight line, only a few data points deviate from the line
# With the above data, we can confirm that it is not necessary for a transformation.

bc <- boxcox(lmodel1, plotit = TRUE)
lambda <- bc$x[which.max(bc$y)]
# lambda = 1.11 which means no transformation is needed.


# Decomposition
# classical decomposition
production |>
  model(
    classical_decomposition(Production, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  industry excluding construction")

# x-11
x11_dcmp <- production |>
  model(x11 = X_13ARIMA_SEATS(Production ~ x11())) |>
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of total industry excluding construction using X-11")

# STL
dcmp <- production %>% model(stl = STL(Production ~ trend(window = 3) + season(window = 'periodic'), robust = TRUE))
dcmp %>% components(dcmp) %>% autoplot() 


# 4. models
# ETS
# model development

fit_ets1 <- production %>% model(ETS(Production ~ error("M") + trend("A") + season("A")))
report(fit_ets1)
# AIC      AICc     BIC 
# 2255.341 2256.666 2326.295

fit_ets2 <- production %>% model(ETS(Production ~ error("A") + trend("A") + season("A")))
report(fit_ets2)
# AIC      AICc     BIC 
# 2299.667 2300.991 2370.621

fit_ets3 <- production %>% model(ETS(Production ~ error("M") + trend("A") + season("M")))
report(fit_ets3)
# AIC      AICc     BIC 
# 2270.324 2271.649 2341.279

fit_ets4 <- production %>% model(ETS(Production ~ error("A") + trend("A") + season("M")))
report(fit_ets4)
# AIC      AICc     BIC 
# 2325.111 2326.435 2396.065

#ETS (M, Ad, M)
fit_ets5 <- production %>% model(ETS(Production ~ error("M") + trend("Ad") + season("M")))
report(fit_ets5)
# AIC      AICc     BIC 
# 2229.041 2230.525 2304.169

fit_ets6 <- production %>% model(ETS(Production ~ error("M") + trend("Ad") + season("A")))
report(fit_ets6)
# AIC      AICc     BIC
# 2244.905 2246.389 2320.033 
# The best ETS model is fit_est5.

# Forecast plot of the best ETS MODEL
forecast(fit_ets5, h=12) %>%
  autoplot(production) +
  labs(title = "Forecasts of Production: Total industry excluding construction
       using the ETS (M,Ad,M) model")

# ARIMA
# differencing plot
production %>% gg_tsdisplay(difference(Production, 12), plot_type='partial', lag=20) ###### ANITHA
#Double differencing
production %>% gg_tsdisplay(difference(difference(Production, 12)), plot_type='partial', lag=120)
#the PACF is suggestive of an AR (2) model, the ACF suggests an MA(2) model.

# non seasonal differencing
production <- production %>% mutate(diff_prod = difference(Production))
production %>% autoplot(diff_prod)
production %>% ACF(diff_prod, lag_max = 120) %>% autoplot()
production %>% PACF(diff_prod, lag_max = 120) %>% autoplot()

# seasonal differencing
production <- production %>% mutate(sdiff_prod = difference(Production, 12))
production %>% autoplot(sdiff_prod)
production %>% ACF(sdiff_prod, lag_max = 120) %>% autoplot()
production %>% PACF(sdiff_prod, lag_max = 120) %>% autoplot()

# stationary test
production %>% features(Production, unitroot_kpss)
#p-value 0.01, the data are not stationary

production %>% features(diff_prod, unitroot_kpss)
#p-value 0.1, the differenced data appear stationary

production %>% features(sdiff_prod, unitroot_kpss)
#p-value 0.1, the differenced data appear stationary

production |> features(Production, unitroot_ndiffs)
#ndiffs
#<int>
#  1      1
#As we saw from the KPSS tests above, one difference is required to make the data stationary.

# Display the ACF/PACF of seasonally differenced data
production %>% gg_tsdisplay(sdiff_prod, plot_type='partial', lag = 120)

# Display the ACF/PACF of double differenced data
production %>% gg_tsdisplay(sdiff_prod %>% difference(), plot_type='partial', lag = 120)

# model development
fit_sarima1 <- production %>% model(ARIMA(Production ~ pdq(2, 1, 2) + PDQ(0, 1, 1)))
report(fit_sarima1)
# AIC=622.26   AICc=622.45   BIC=647.14

fit_sarima2 <- production %>% model(ARIMA(Production ~ pdq(1, 1, 1) + PDQ(0, 1, 1)))
report(fit_sarima2)
# AIC=623.36   AICc=623.44   BIC=639.940

fit_sarima3 <- production %>% model(ARIMA(Production ~ pdq(2, 1, 2) + PDQ(1, 1, 1)))
report(fit_sarima3)
# AIC=624.19   AICc=624.44   BIC=653.22

fit_sarima4 <- production %>% model(ARIMA(Production ~ pdq(1, 1, 1) + PDQ(1, 1, 1)))
report(fit_sarima4)
# AIC=625.34   AICc=625.47   BIC=646.07

fit_sarima5 <- production %>% model(ARIMA(Production ~ pdq(1, 1, 2) + PDQ(1, 1, 1)))
report(fit_sarima5)
# AIC=622.2   AICc=622.38   BIC=647.07

fit_sarima6 <- production %>% model(ARIMA(Production ~ pdq(0, 1, 2) + PDQ(0, 1, 1)))
report(fit_sarima6)
# AIC=619.93   AICc=620.01   BIC=636.51
# The best ARIMA model so far is fit6.

# Forecast plot of the best SARIMA model
forecast(fit_sarima6, h=12) %>%
  autoplot(production)

# STL_+ ETS
STLF <- decomposition_model(
  STL(Production ~ season(window = Inf)),
  ETS(season_adjust ~ season("N"))
)

# Prophet
proph = prophet(
  Production ~ season(period = "year", order = 2) + 
    season(period="month", order = 2))



# 5. evaluation
#comparing models
cv <- production %>% slice(-n()) %>% stretch_tsibble()
cv %>% model(
  ets = ETS(Production ~ error("M") + trend("Ad") + season("M")),
  stlf = STLF,
  arima = ARIMA(Production ~ pdq(0, 1, 2) + PDQ(0, 1, 1)),
  proph = prophet(
    Production ~ season(period = "year", order = 2) + season(period="month", order = 2))) %>% 
  mutate(
    combination = (ets + stlf + arima + proph) / 4) %>% 
  forecast(h = 1) %>% 
  accuracy(production)

#.model      .type       ME  RMSE   MAE     MPE  MAPE  MASE RMSSE  ACF1
#<chr>       <chr>    <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>
#1 arima       Test  -0.00765 0.484 0.329 -0.0368  1.19 0.306 0.352 0.114
#2 combination Test  -0.00831 0.586 0.404 -0.0650  1.47 0.375 0.427 0.404
#3 ets         Test   0.0103  0.508 0.349  0.0227  1.27 0.324 0.370 0.240
#4 proph       Test  -0.0444  2.14  1.19  -0.211   4.84 1.11  1.56  0.427
#5 stlf        Test   0.0319  0.646 0.432  0.117   1.64 0.402 0.470 0.168

fit_sarima6 |> 
  gg_tsresiduals(lag=36) 



# 6. forecasting
fit_sarima6 %>% forecast(h = 60) %>% autoplot(production) +
  labs(title = "Forecasts of Production: Total industry excluding construction
       using the ARIMA (0,1,2)(0,1,1) model")

fit_sarima6 %>% forecast(h = 60) %>%
  autoplot(production  |>   filter_index("2003 Jan" ~ .)) +
  labs(title = "Forecasts of Production: Total industry excluding construction
       using the ARIMA (0,1,2)(0,1,1) model")

