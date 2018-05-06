# Date: 5-6 May 2018;
# About: Cryptodata coding challenge;
# Reference: data was obtained from: https://www.cryptocompare.com/api/#;
# _note: this data is about bitcoin price against EUR;
# Aim-1: To predict if the price goes up or down in the next hour;
# Aim-2: To predict if the price will go up or down for the next 6 hours,;
# _i.e., one prediction per hour;
# Workflow (main steps);
# I. Fetch histohour data;
# II. Data preparation;
# III. Model fitting;
# IV. Forecasting;
# V. Save this workspace;


library(jsonlite)
library(anytime)
library(stringi)
library(ggplot2)
library(forecast)
library(tseries)


# ##############################################################################################################;
# I. Fetch the histohour data ___________________________________________________________________________________;
# ______________________________________________________________________________________________________________;
# setwd("/Users/tenzin/Documents/Wattx_Coding_Challenge")

# 1. directly read JSON data;
BTC_EUR_ls <- fromJSON("https://min-api.cryptocompare.com/data/histohour?fsym=BTC&tsym=EUR&limit=2000")
class(BTC_EUR_ls)
# [1] "list"

# @relevant infos;
names(BTC_EUR_ls)
# [1] "Response" ;  "Type"; "Aggregated"; "Data"; "TimeTo"; "TimeFrom"; "FirstValueInArray";
# [8] "ConversionType"

# _@extract timestamp range of the data;
(BTC_data_timeRange <- paste("Contains BTCs hourly data from ", 
                            anytime(BTC_EUR_ls$TimeFrom), 
                            " to ", 
                            anytime(BTC_EUR_ls$TimeTo), ";" , sep = ""))
# [1] "Contains BTCs hourly data from 2018-02-11 09:00:00 to 2018-05-05 18:00:00;";

# 2. Extract the main data for the analysis;
BTC_EUR_data <- BTC_EUR_ls$Data
dim(BTC_EUR_data)
# [1] 2001    7;

# @get the data structure;
str(BTC_EUR_data)
# 'data.frame':	2001 obs. of  7 variables:
#   $ time      : int  1518336000 1518339600 1518343200 1518346800 1518350400 1518354000 1518357600 1518361200 1518364800 1518368400 ...
# $ close     : num  6497 6664 6587 6719 6796 ...
# $ high      : num  6512 6666 6683 6740 6873 ...
# $ low       : num  6424 6441 6548 6548 6694 ...
# $ open      : num  6512 6500 6664 6587 6719 ...
# $ volumefrom: num  1402 1033 923 975 1242 ...
# $ volumeto  : num  9059280 6739388 6114230 6467002 8437453 ...

# _@get the summary;
summary(BTC_EUR_data)
# time               close           high           low            open        volumefrom          volumeto       
# Min.   :1.518e+09   Min.   :5249   Min.   :5327   Min.   :5247   Min.   :5249   Min.   :   55.61   Min.   :  310424  
# 1st Qu.:1.520e+09   1st Qu.:6509   1st Qu.:6552   1st Qu.:6464   1st Qu.:6509   1st Qu.:  376.75   1st Qu.: 2648745  
# Median :1.522e+09   Median :7234   Median :7273   Median :7190   Median :7233   Median :  610.26   Median : 4463351  
# Mean   :1.522e+09   Mean   :7234   Mean   :7281   Mean   :7183   Mean   :7233   Mean   :  797.29   Mean   : 5785724  
# 3rd Qu.:1.524e+09   3rd Qu.:7939   3rd Qu.:7994   3rd Qu.:7859   3rd Qu.:7938   3rd Qu.:  998.66   3rd Qu.: 7262548  
# Max.   :1.526e+09   Max.   :9529   Max.   :9573   Max.   :9492   Max.   :9529   Max.   :12148.49   Max.   :74774770 
# NOTE: this also confirms the absence of missing values!;

# __@crosscheck if the hourly data is reported;
(BTC_EUR_data_timeChk1 <- anytime(BTC_EUR_data$time[2]) - anytime(BTC_EUR_data$time[1]))
# Time difference of 1 hours;
(BTC_EUR_data_timeChk2 <- anytime(BTC_EUR_data$time[2001]) - anytime(BTC_EUR_data$time[2000]))
# Time difference of 1 hours;
# NOTE: confirms hourly data;



# ##############################################################################################################;
# II. Data preparation _________________________________________________________________________________________;
# ______________________________________________________________________________________________________________;

# 1. include human readble date column mainly for the plots and data prep, whilst preserving the original data; 
BTC_EUR_data_hr <- BTC_EUR_data
BTC_EUR_data_hr$fullTime <- anytime(BTC_EUR_data$time)

# @separate Date column;
BTC_EUR_data_hr$Date <- as.Date(stri_split_fixed(BTC_EUR_data_hr$fullTime, " ", simplify = TRUE)[,1])

# _@separate Time column;
BTC_EUR_data_hr$hrTime <- stri_split_fixed(BTC_EUR_data_hr$fullTime, " ", simplify = TRUE)[,2]

# __@dim check;
dim(BTC_EUR_data_hr)
# [1] 2001   10;

# 2. include another column for time series closing price;
BTC_EUR_data_hr$TS_Close <- with(BTC_EUR_data_hr, ts(data = close, start = time[1]))
BTC_EUR_data_hr$tsClose_6hr <- ma(BTC_EUR_data_hr$TS_Close, order=6)
BTC_EUR_data_hr$tsClose_1day <- ma(BTC_EUR_data_hr$TS_Close, order=24)
BTC_EUR_data_hr$tsClose_7day <- ma(BTC_EUR_data_hr$TS_Close, order=168)

# @visualize 1: "TS_Close" to get an idea of the data; 
P1_tsClose <- ggplot(BTC_EUR_data_hr, aes(x=fullTime, y=TS_Close, colour = "Close: Time series")) +
  geom_line() + scale_x_datetime(date_minor_breaks = "1 day", date_labels = "%d %h %Y") + 
  ylab("BTC: hourly closing price") + xlab("Months")

# _@visualize 2: TS_Close vs moving-average (MA) 6 hours:
P2_tsClose_MA6hr <- P1_tsClose + 
  geom_line(data = BTC_EUR_data_hr, aes(x=fullTime, y = tsClose_6hr, colour = "MA: 6 hours")) 

# __@visualize 3: TS_Close vs MA 1 day:
P3_tsClose_MA_6hr_1day <- P2_tsClose_MA6hr +
  geom_line(data = BTC_EUR_data_hr, aes(x=fullTime, y = tsClose_1day, colour = "MA: 1 day"))

# __@visualize 4: TS_Close vs MA 1 day:
P4_tsClose_MA_6hr_1_7days <- P3_tsClose_MA_6hr_1day +
  geom_line(data = BTC_EUR_data_hr, aes(x=fullTime, y = tsClose_7day, colour = "MA: 7 days"))

# ___@save;
pdf("P4_tsClose_MA_6hr_1_7days.pdf", width = 15, height = 10)
P4_tsClose_MA_6hr_1_7days
dev.off()

# 3. data stationarity check @per hour using @ADF test (augmented Dickey-Fuller);
(BTCclose_1hr_adfTst <- adf.test(BTC_EUR_data_hr$TS_Close, alternative = "stationary"))
# Augmented Dickey-Fuller Test
# data:  BTC_EUR_data_hr$TS_Close
# Dickey-Fuller = -1.3053, Lag order = 12, p-value = 0.8724
# alternative hypothesis: stationary
#
# Note: p-value = 0.8724, confirms non-stationary or trend data; 
# --note: them ARIMA test (requires stationary data) possible after differencing;

# 4. another stationarity check using autocorrelation plots, helps in choosing model order;
par(mfrow=c(1,2))
Acf(BTC_EUR_data_hr$TS_Close, main="BTC closing price MA per hour")
Pacf(BTC_EUR_data_hr$TS_Close, main="BTC closing price MA per hour")
# dev.off()

# 5. differencing @1;
BTCclose_1hour_d1 <- diff(BTC_EUR_data_hr$TS_Close, differences = 1)

# @visualize;
plot(BTCclose_1hour_d1)

# _@another adf test;
(BTCclose_1hour_d1_adfTst <- adf.test(BTCclose_1hour_d1, alternative = "stationary"))
# Augmented Dickey-Fuller Test
# data:  BTCclose_1hour_d1
# Dickey-Fuller = -12.643, Lag order = 12, p-value = 0.01
# alternative hypothesis: stationary

# __@another stationarity check using differenced values;
par(mfrow=c(1,2))
Acf(BTCclose_1hour_d1, main="BTC closing price MA per hour min d1")
Pacf(BTCclose_1hour_d1, main="BTC closing price MA per hour min d1")
dev.off()


# ##############################################################################################################;
# III. Model fitting ___________________________________________________________________________________________;
# ______________________________________________________________________________________________________________;

# 1. using auto arima function;
BTC_EUR_data_hr_autoFit <- auto.arima(BTC_EUR_data_hr$TS_Close, seasonal = FALSE)

# @visualize the fit;
tsdisplay(residuals(BTC_EUR_data_hr_autoFit))
          
# 2. another fit at lag 14;
BTC_EUR_data_hr_Fit2 <- arima(BTC_EUR_data_hr$TS_Close, order = c(1,1,14))

# @visualize;
tsdisplay(residuals(BTC_EUR_data_hr_Fit2), lag.max=15)

# 3. Model selection;
BTC_EUR_data_hr_autoFit$aic
# [1] 23059.72;
BTC_EUR_data_hr_Fit2$aic
# [1] 23068.77;
# NOTE: smaller AIC and better fit model is @BTC_EUR_data_hr_autoFit;



# ##############################################################################################################;
# IV. Forecasting  _____________________________________________________________________________________________;
# ______________________________________________________________________________________________________________;

# 1. Aim-1: To predict if the price goes up or down in the next hour;
(BTC_EUR_data_hr_autoFit_forcst_1hr <- forecast(BTC_EUR_data_hr_autoFit, h=1))
# Point Forecast    Lo 80   Hi 80    Lo 95    Hi 95
# 1518338001       8267.958 8169.137 8366.78 8116.824 8419.093
accuracy(BTC_EUR_data_hr_autoFit_forcst_1hr)
plot(BTC_EUR_data_hr_autoFit_forcst_1hr)

# @rbind the tail of main data and forecast data, clear visualization;
tail(BTC_EUR_data_hr[,1:11], 1)
#             time   close    high     low    open volumefrom volumeto            fullTime       Date   hrTime TS_Close;
# 2001 1525536000 8268.36 8268.36 8211.20 8261.55     441.49  3632055 2018-05-05 18:00:00 2018-05-05 18:00:00  8268.36;

# __@summary about the forecast 1hours;
(BTC_EUR_1hr_forCst_Message <- paste0(with(BTC_EUR_data_hr, paste0("BTC closing price at ", anytime(time[nrow(BTC_EUR_data_hr)]), 
                                                                  " is ",  close[nrow(BTC_EUR_data_hr)])),
                                     "; arima closing price forcast for the next hour is ", 
                                     round(c(BTC_EUR_data_hr_autoFit_forcst_1hr$mean), 2) ))
# [1] "BTC closing price at 2018-05-05 18:00:00 is 8268.36; arima closing price forcast for the next hour is 8267.96";


# 2. Aim-2: To predict if the price will go up or down for the next 6 hours,;
(BTC_EUR_data_hr_autoFit_forcst_6hr <- forecast(BTC_EUR_data_hr_autoFit, h=6))
#     Point         Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 1518338001       8267.958 8169.137 8366.780 8116.824 8419.093
# 1518338002       8267.976 8131.206 8404.745 8058.805 8477.147
# 1518338003       8267.975 8101.599 8434.351 8013.524 8522.425
# 1518338004       8267.975 8076.521 8459.429 7975.171 8560.779
# 1518338005       8267.975 8054.367 8481.583 7941.290 8594.660
# 1518338006       8267.975 8034.304 8501.646 7910.606 8625.344
accuracy(BTC_EUR_data_hr_autoFit_forcst_6hr)
plot(BTC_EUR_data_hr_autoFit_forcst_6hr)

# @check the tail of main data, clear visualization;
tail(BTC_EUR_data_hr[,1:11], 1)
#             time   close    high     low    open volumefrom volumeto            fullTime       Date   hrTime TS_Close;
# 2001 1525536000 8268.36 8268.36 8211.20 8261.55     441.49  3632055 2018-05-05 18:00:00 2018-05-05 18:00:00  8268.36;

# __@summary about the forecast 6hours;
(BTC_EUR_6hr_forCst_Message <- paste0(with(BTC_EUR_data_hr, paste0("BTC closing price at ", anytime(time[nrow(BTC_EUR_data_hr)]), 
                                                                   " is ",  close[nrow(BTC_EUR_data_hr)])),
                                      "; arima closing price forcast for the next 6 hours: ", 
                                      paste(round(c(BTC_EUR_data_hr_autoFit_forcst_6hr$mean), 2), collapse = ", ") ))
# [1] "BTC closing price at 2018-05-05 18:00:00 is 8268.36; arima closing price forcast for the next 6 hours: 8267.96, 8267.98, 8267.97, 8267.97, 8267.97, 8267.97";


# ##############################################################################################################;
# V. Save this workspace  ______________________________________________________________________________________;
# ______________________________________________________________________________________________________________;
# save.image("CryptoCoding_Challenge_WATTX.RData")
# load("CryptoCoding_Challenge_WATTX.RData")
# END __________________________________________________________________________________________________________;
