# Here is my answer to The Challenge.
library(readr)
library(dplyr)
library(ggeasy)
library(tidyverse) #Load library tidyverse
library(seasonal)
library(lubridate)
library(forecast)
library(fable)
library(fable.prophet)
library(tsibble)

library(tidymodels)
library(modeltime)
library(timetk)

###
# loading libraries at once:
suppressPackageStartupMessages({
  list.of.packages <- c("stringr","qdap","tidytext","tm", "caret", "e1071", "ggplot2",
  "digest", "gbm", "randomForest", "rpart", "rpart.plot","Rtsne", "TTR","matrixStats",
  "zoo", "RcppRoll")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
 if (length(new.packages)) install.packages(new.packages)

   lapply(list.of.packages, require, character.only = TRUE,)
})

#read the data:
df_dates = read_csv('PredictionChallenge_Dates.csv')
df_SKU_props = read_csv('PredictionChallenge_SKU_Characteristics.csv')
df_SKU_qntts = read_csv('PredictionChallenge_SKU_Quantities.csv')

# dates as Date format (we will use tsibble later and required them as Date)
head(df_SKU_qntts, n=2)

# 1. exploratory data analysis
# the quantity data will be joined with characteristics and information about SKU categories:
dates_qntts_cat <- df_SKU_qntts %>% left_join(df_SKU_props)
underwear <- dates_qntts_cat %>% filter(Category=='Underwear')
which.max(underwear$qty)
underwear[127218,]
# 920 SKUs:
head(dates_qntts_cat, n=3)
tail(dates_qntts_cat, n=1)
# date details (workday, holiday and so on) from 2018-01-01 to 2019-12-31: to enable us to map these data to the time series.
head(df_dates, n=3)
#
tail(df_dates, n=1)
str(df_dates)
#I would like to check the data consistency and look on the sales by dates and categories.
# It is easier to get explore data in aggregated categories:
date_sales_by_category <- dates_qntts_cat %>% group_by(date,Category) %>% summarise(qty=sum(qty))
date_sales_by_size <- dates_qntts_cat %>% group_by(date,Size) %>% filter(Size == "XXL") %>% summarise(qty=sum(qty))
date_sales_by_size <- dates_qntts_cat %>% group_by(date,Size)  %>% summarise(qty=sum(qty))
date_sales_by_colors <- dates_qntts_cat %>% group_by(date,Color)  %>% summarise(qty=sum(qty))
# As in 8_Outro.pdf (page 9):
outro_plot_1 <- ggplot(date_sales_by_category, aes(x=date, y = qty, color=Category)) + geom_line()+ggtitle("Sales by Dates and Categories") + geom_point() + ggeasy::easy_center_title()
outro_plot_1
# Outliers on 2019-06-30 cannot be seen on 2018-06-30: it might be a result of a one-time event in 2019. It is better to plot this sold quantities as separate plots:
outro_plot_1 + facet_grid(vars(Category), scales = "free_y") + scale_x_date( date_labels = "%y-%m-%d", limits=as.Date(c("2019-04-13", "2019-06-30")))
outro_plot_1 + facet_grid(vars(Category), scales = "free_y")
plot_size <- ggplot(date_sales_by_size, aes(x=date, y = qty, color=Size)) + geom_line()+ggtitle("Sales by Size") + geom_point() + ggeasy::easy_center_title()
plot_size
plot_size + facet_grid(vars(Size), scales = "free_y")

plot_color <- ggplot(date_sales_by_colors, aes(x=date, y = qty, color=Color)) + geom_line()+ggtitle("Sales by Color") + geom_point() + ggeasy::easy_center_title()
plot_color
#Observations: SKUs quantities of categories Curves and Thermo are all zero. Their forecast could not be made and will be transferred as zeros to the final forecast. The categories Curves and Thermo might be removed from the data (w/o loosing the prediction quality) with the aim to of data set reduction.
# SKUs of categories bed linen, Breastfeeding lingeries, Fine stockings, swimwear towels have no sales quantity records or no sales in June 2018, so we cannot rely on the historical data in the same season in 2018 for these categories.
# Observations: Outliers close to January - higher sales quantities in preparation for the holiday season (Hanukkah, X-mas, New Year's Eve). Such jumps are in all categories with recorded quantities.
# Furthermore, we see the same behavior on June 30 2019. Hence, these outliers are hardly a mistake in records.
# Further investigation of outliers:
outliers_check_df <- date_sales_by_category %>% left_join(df_dates)
plot_with_dates_cat <- ggplot(outliers_check_df, aes(x=date, y = qty, color=Category)) + geom_line()+ ggtitle("Sales by Dates and Categories") + ggeasy::easy_center_title() + geom_point(colour = "black", size = 4.5) + geom_point(colour = "pink", size = 4)
plot_with_dates_cat + geom_point(colour = "black",aes(shape = factor(weekend)))
# Observation: the bigger outliers are at weekends or around them.
plot_with_dates_cat + geom_point(colour = "black",aes(shape = factor(school)))
# Observation: the outlier on June 30 2019 is a weekend and a school holiday. It is an usual event, since we do not observe it in 2018.

# Now we look at the our initial data:
summary(date_sales_by_category)
#The data has a wide rage of values.
# As in 8_Outro.pdf (page 10):
qty_count <- dates_qntts_cat %>% left_join(df_dates) %>% group_by(SKU,CalendarWeek) %>% summarise(qty=sum(qty)) %>% count(qty) %>% group_by(qty) %>% summarise(count=sum(n))
ggplot(qty_count, aes(qty, count)) + geom_col(fill= "pink", color ="black")
# The aggregated plot (weekly qty of sold SKUs vs. number of sale events) shows that the data are right-skewed - there are a lot of observations with zero values for aggregated weekly quantities and some weeks with high numbers for quantities sold
library(moments)
skewness(date_sales_by_category$qty)
# 12.02 - the qty values are right-skewed. 
# Let's look on the histograms of the individual categories and correlation between categories of SKUs:
# We will use package GGally:
library(GGally)

date_sales_by_category %>% select(-date) %>%  pivot_wider(values_from = qty, names_from = Category) %>% GGally::ggpairs(columns = 2:12)
#We could see on the combined plot that all data within categories are right-skewed (even categories where we have a full data set demonstrate a slow-moving nature)
# Transformation:
df_SKU_logQty <- df_SKU_qntts %>% mutate(logqty = log(1+qty)) %>% select(-qty)
df_SKU_logQty
hist(df_SKU_logQty$logqty)
head(df_SKU_logQty, n=3)
dates_logQntts_cat <- df_SKU_logQty %>% left_join(df_SKU_props)
head(dates_logQntts_cat)
bodies_qty <- dates_qntts_cat %>% filter(Category=='Bodies')
bodies_logqty <- dates_logQntts_cat %>% filter(Category=='Bodies')
hist(bodies_qty$qty)
skewness(bodies_qty$qty)
hist(bodies_logqty$logqty)
skewness(bodies_logqty$logqty)
# log initial data
date_logsales_by_category <- date_sales_by_category %>% mutate(logqty = log(1+qty)) %>% select(-qty)
log_plot_1 <- ggplot(date_logsales_by_category, aes(x=date, y = logqty, color=Category)) + geom_line()+ggtitle("log sales by Dates and Categories") + geom_point() + ggeasy::easy_center_title()
log_plot_1
date_logsales_by_category  %>% select(-date) %>%  pivot_wider(values_from = logqty, names_from = Category) %>% GGally::ggpairs(columns = 2:12)
# log transformation is a slight improvement in comparison with the initial data skewness.
# What are the findings? We have low counts - way too many zeros in the data. Hence, to explore a weekly data aggregation might result in improved time series.

# Curves and Thermo could be removed from the data set:
summary(dates_logQntts_cat)
dates_logQntts_cat_red <- dates_logQntts_cat %>% filter(Category != "Curves"& Category != "Thermo" )
dates_qntts_cat_red <- dates_qntts_cat %>% filter(Category != "Curves"& Category != "Thermo" )
curves_thermo_SKU_forecast <- dates_qntts_cat %>% filter(Category == "Curves"| Category == "Thermo" ) %>% group_by(SKU) %>% summarise(yhat1 = 0) %>% ungroup()
head(curves_thermo_SKU_forecast)
curves_thermo_skus <- dates_logQntts_cat %>% filter(Category == "Curves"| Category == "Thermo" )
summary(dates_qntts_cat_red)
summary(curves_thermo_skus)
head(curves_thermo_skus)

### Investigation of the dataset:
library(fpp3)
sales_by_cat <- dates_qntts_cat %>% group_by(date,Category) %>% summarise(qty=sum(qty))

tsbl_sales_by_cat <- sales_by_cat %>% as_tsibble(index=date, key=Category)
df_SKU_qntts %>% as_tsibble(index=date, key=SKU) %>% filter(SKU=="1008-20") %>% gg_tsdisplay(qty)
df_SKU_qntts %>% as_tsibble(index=date, key=SKU) %>% filter(SKU=="1063-16") %>% gg_tsdisplay(qty)

ts_df_SKU_qntts <- df_SKU_qntts %>% as_tsibble(index=date, key=SKU) 
decomp_106316 <- ts_df_SKU_qntts  %>% filter(SKU=="1008-20") %>% model(stl = STL(qty))
components(decomp_106316) %>% as_tsibble() %>% autoplot(qty, colour="gray") + geom_line(aes(y=trend), colour = "#D55E00")
tsbl_sales_by_cat %>% filter(Category=="Underwear") %>% gg_subseries(period="month")
tsbl_sales_by_cat %>% filter(Category=='Underwear') %>% gg_season(qty)
tsbl_sales_by_cat %>% filter(Category=="Underwear") %>% gg_season(qty, period = "week")
tsbl_sales_by_cat %>% filter(Category=="Underwear") %>% gg_season(qty, period = "month")
tsbl_sales_by_cat %>% filter(Category=="Underwear") %>% gg_tsdisplay()


# Forecast like in Outro.pdf

# Let's try the same naive method from the outro (and compare it with validation interval later):
#Data granularity - weekly data.
SKUmeans_full <- dates_qntts_cat %>% left_join(df_dates) %>% group_by(SKU,CalendarWeek) %>% summarise(qty=sum(qty)) %>% group_by(SKU) %>% summarize(yhat1 = mean(qty)) %>% ungroup()
SKUmeans_red <- dates_qntts_cat_red %>% left_join(df_dates) %>% group_by(SKU,CalendarWeek) %>% summarise(qty=sum(qty)) %>% group_by(SKU) %>% summarize(yhat1 = mean(qty)) %>% ungroup()

# Quality assurance: combining remaining data with curves and thermo categories works correct:
# To use this code later when joining the forecast with zero values of Thermo and Curves:
SKUmean_binded <- bind_rows(SKUmeans_red, curves_thermo_SKU_forecast) %>% arrange(SKU)
all_equal(SKUmeans_full,SKUmean_binded)
#
# Calculating category means (model from the outro):
Categorymeans_red <- dates_qntts_cat_red %>% left_join(df_dates) %>% filter(Year==2018, CalendarWeek ==27) %>% group_by(SKU) %>% summarise(qty=sum(qty)) %>% left_join(df_SKU_props) %>% group_by(Category) %>% summarise(yhat2 = mean(qty))
# Calculating prediction based on the model in Outro.pdf (page xx)
Prediction_W27_2019_red <- SKUmeans_red %>% left_join(df_SKU_props) %>% left_join(Categorymeans_red) %>% mutate(yhat3 = .4*yhat1 + .6*yhat2)
Prediction_W27_2019 <- bind_rows(Prediction_W27_2019_red, curves_thermo_SKU_forecast) %>% arrange(SKU)
summary(Prediction_W27_2019)

head(Prediction_W27_2019, n=3)
#How good is this model? 
#To answer this question, we should partition our data set into a training data set and (in case of a time series - a period) and into a validation data set.
#The initial data set will be divided into two periods. We are going to use the validation period to choose the best model for the future (first week of July 2019).
# When the best (from available ones in the given work) forecast model is chosen, the training set and validation set will be merged used as a whole to predict weekly SKU quantities in the future period.
# To mimic the future period, it was decided to take a week 26 (NB: week count in 2019 starts with 0) for validation.

Categorymeans_W26 <- dates_qntts_cat %>% left_join(df_dates) %>% filter(Year==2018, CalendarWeek ==26) %>% group_by(SKU) %>% summarise(qty=sum(qty)) %>% left_join(df_SKU_props) %>% group_by(Category) %>% summarise(yhat2 = mean(qty))
Prediction_W26_2019<- SKUmeans_full %>% left_join(df_SKU_props) %>% left_join(Categorymeans_W26) %>% mutate(yhat3 = .4*yhat1 + .6*yhat2)

head(Prediction_W26_2019)

validate_for_mean_combination <- dates_qntts_cat %>% left_join(df_dates) %>% filter(Year == 2019 & CalendarWeek ==25) %>% group_by(SKU) %>% summarise(real_qty=sum(qty))
RMSE_mean_method <- validate_for_mean_combination %>% left_join(Prediction_W26_2019) %>% mutate(v = (real_qty - yhat3)^2)
RMSE_value <- sqrt(mean(RMSE_mean_method$v))
RMSE_value
library(Metrics)
RMSE_of_mean_method_v2 <- rmse(validate_for_mean_combination$real_qty,Prediction_W26_2019$yhat3)
# we can do better: 17.3149
#NB:
#Lai et al (2022), https://doi.org/10.1038/s41597-022-01120-z (page 10): Of note, week numbers in the weekly time series datasets were calculated by year and contain a week 0 for
#some years; the days of that week should therefore be included in the last week of the preceding year."
# Larisa: the week #27 is the first week in 2019. The first week of July 2016 has a number 26.

#head(dates_logQntts_cat)
library(lubridate)
library(tsibble)
library(fable)
library(fpp3)
# Statistical methods for benchmarking (ARIMA) and deployment of prophet for later usage with ML results:
set_for_tsibble <- dates_qntts_cat %>% left_join(df_dates) %>% as_tsibble(index = date, key=SKU)
# We use index_by() to assign new week index. Now the weeks starts with 1 in each year:
set_for_tsibble_weeks <- set_for_tsibble %>% group_by_key() %>% index_by(tsWeeks = ~yearweek(.))
set_for_tsibble_weeks_2 <- set_for_tsibble %>% group_by_key() %>% index_by(tsWeeks = ~yearweek(.)) %>% summarise(wqty=sum(qty))
log_set_for_tsibble_weeks_2 <- set_for_tsibble %>% group_by_key() %>% index_by(tsWeeks = ~yearweek(.)) %>% summarise(wqty=sum(qty)) %>% mutate(logwqty=log(1+wqty))
hist(set_for_tsibble_weeks_2$wqty)
hist(log_set_for_tsibble_weeks_2$logwqty)

# Here I try to work with the weekly data to get a feeling for statistical methods:
train <- set_for_tsibble_weeks_2 %>% filter_index(. ~ '2019 W25') %>% filter(SKU=="1008-20")
validate <- set_for_tsibble_weeks_2 %>% filter_index('2019 W26' ~ .) %>% filter(SKU=="1008-20")

#let us try prophet
# We try use at once ets, arima and prophet:
fit_with_prophet <- train %>% model(ets=ETS(wqty), arima=ARIMA(wqty),
                                    prophet = prophet(wqty ~ season(period = "year", type = "multiplicative")))
my_fc_with_prophet <- fit_with_prophet %>% forecast(h=1)

#my_fc_accuracy_prophet <- accuracy(my_fc_with_prophet, validate, measures = list(point_accuracy_measures,interval_accuracy_measures,distribution_accuracy_measures))

'my_fc_accuracy_prophet %>%
  group_by(.model) %>%
  summarise(
    RMSE = mean(RMSE),
    MAE = mean(MAE),
    MASE = mean(MASE),
    Winkler = mean(winkler),
    CRPS = mean(CRPS)
  ) %>%
  arrange(RMSE)'

fit_with_prophet_v2 <- train %>% model(ets=ETS(wqty), arima=ARIMA(wqty),
                                       prophet = prophet(wqty))
my_fc_with_prophet_v2 <- fit_with_prophet_v2 %>% forecast(h=1)

#
# To improve the forecasting quality of the results a combination of method was implemented:

library(fable.prophet)
library(prophet)
library(fpp3)

data_1008_20_for_prophet_qty <- data_for_prophet_qty %>% filter(SKU=='1008-20') %>% as_tsibble(index=date, key=SKU)
fit_days_with_prophet <- data_1008_20_for_prophet_qty %>% model(prophet = prophet(qty))

### all 920 curves:
ts_data_for_prophet_qty <- data_for_prophet_qty %>% as_tsibble(index=date, key=SKU)
all_fit_prophet <- ts_data_for_prophet_qty %>% model(prophet = prophet(qty))
#fit_with_prophet_days_ho <- data_1008_20 %>% model(prophet = prophet(qty ~ season(period = "day", order = 10) +
# + season(period = "week", order = 5), holidays = holidays))
# Here I tested dummy variables for the prophet:
#fit_with_prophet_days_ho <- prophet(data_1008_20, holidays = holidays)
#my_fc_1008_20_prophet_no_h <- fit_with_prophet_days %>% forecast(h=7)

# Fitting all 920 curves with Prophet:
my_fc_alldata_prophet_no_h <- all_fit_prophet %>% forecast(h=7)

my_aggregated_data_prophet <- my_fc_alldata_prophet_no_h %>% group_by(SKU) %>% as_tibble() %>% summarise(qty=round(sum(.mean)))
# I write the data to an external file for safety:
write.csv(my_aggregated_data_prophet,"prophet.csv")
my_fc_1008_20_prophet_no_h %>% autoplot(data_1008_20_for_prophet_qty)
#

# Machine Learning part

library(caret)
# Testing a feature of converting category variable into o and 1:
#dummy <- dummyVars(" ~ Category", data=df_SKU_props, sep = ".")
#my_onehot_props <- data.frame(predict(dummy, newdata=df_SKU_props)) %>%  as_tibble()

reduced_props_df <- df_SKU_props %>% select(SKU) %>% bind_cols(my_onehot_props)
head(reduced_props_df, n=3)
# We do need to decompose the date since Dr. Zimmerman delivered a nice df_dates
# However, I want to adjust the week numbering: we get a reality distortion in the first week of January (since there are just six days in 2019)m but we get consistent week numbers in the remaining 2019.
#adjusted_wk_dates_df <- df_dates %>%  mutate(case_when(Year==2019 ~ CalendarWeek=CalendarWeek+1))
# We clean up the SKUs to remove those with identified all-zero values in Categories:
empty_SKU_forecast <- dates_qntts_cat %>% filter(Category == "Curves"| Category == "Thermo" ) %>% group_by(SKU) %>% summarise(yhat1 = 0) %>% ungroup()
#
summary(df_SKU_qntts)
#each of 920 SKUs has 546 data points (rows). We are going to reduced the data set removing the SKU subsets with 0 values, but need to record the order of SKUs for evaluation later:
dates_qntts_reduced_for_ML <- dates_qntts_cat %>% filter(Category != "Curves"& Category != "Thermo" )
SKUs_for_ML <- dates_qntts_cat %>% filter(Category != "Curves"& Category != "Thermo" ) %>% group_by(SKU)

# Performing log transformation of the qty:
dates_qntts_reduced_for_ML %<>% mutate(qty = log(1+qty))

# Create lags and lag features:
Create_lags <- function(df, start_lag, num_lags) {
lags = seq(from = start_lag, to=start_lag + num_lags)
lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag="0"), sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
print(lag_functions)
df = df  %>% arrange(SKU) %>% group_by(SKU) %>% mutate_at(vars(qty), funs_(lag_functions))
       print(colnames(df)) 
return (df)
}
test_lags <- Create_lags(dates_qntts_reduced_for_ML,1,7)

#create rolling means:
library(zoo)

test_lags_rollmeans <- test_lags %>%
  dplyr::arrange(SKU) %>% 
  dplyr::group_by(SKU) %>% 
  dplyr::mutate(rmean_03 = zoo::rollmean(qty, k = 3, fill = NA),
                rmean_05 = zoo::rollmean(qty, k = 5, fill = NA),
                rmean_07 = zoo::rollmean(qty, k = 7, fill = NA),
                rmean_15 = zoo::rollmean(qty, k = 15, fill = NA),
                rmean_21 = zoo::rollmean(qty, k = 21, fill = NA)) %>% 
  dplyr::ungroup()

# Forecast method evaluation.
# test w/o category information.
# The initial dataset will be divided into into train and validation data sets:

SKUs_for_ML
TS_names_to_process <- unique(SKUs_for_ML$SKU)

single_future_prediction  <- as_tibble(data.frame(Characters=character(),Doubles=double()))
single_future_prediction2  <- as_tibble(data.frame(Characters=character(),Doubles=double()))
single_future_prediction_NN  <- as_tibble(data.frame(Characters=character(),Doubles=double()))
colnames(single_future_prediction) = c("SKU","qty")
colnames(single_future_prediction_NN) = c("SKU","NN")
#for (ts_name1 in TS_names_to_process)
#{
#  print(ts_name1)
#}

inverse_tran <- function(x){
  value = exp(x) -1.
  return(value)
}

ts_name <- "1008-20"
single_future_prediction2 <-rbind(single_future_prediction2,ts_name)
temp_initial_df <- df_SKU_qntts %>% filter(SKU==ts_name)
#
SKUs_for_ML %>% select(-Color,-Category,-Type,-Size)
dates_for_runs <- SKUs_for_ML %>% select(-Color,-Category,-Type,-Size) %>% left_join(df_dates)
colnames(test_lags_rollmeans)
temp_lags_rollmeans <- test_lags_rollmeans %>% filter(SKU==ts_name) %>% select(-Color,-Category,-Type,-Size)
temp_lags_rollmeans_ext <- temp_lags_rollmeans %>% extend_timeseries(.id_var = SKU, .date_var = date, .length_future = 7)
temp_for_NN <- temp_lags_rollmeans_ext  %>% left_join(df_dates)
tail(temp_for_NN, n=7 )

#ggplot(temp_for_NN, aes(date, qty)) + geom_line(color = "red")+ ylab("TS")+ xlab("Date")+ggtitle(ts_name)
temp_for_NN[is.na(temp_for_NN)] <- 0
tail(temp_for_NN, n=3)

N_temp = nrow(temp_for_NN)
temp_cut = N_temp - 14
train = temp_for_NN[1:temp_cut, ]
validate = temp_for_NN[(temp_cut+1):(temp_cut+7),  ]
future = temp_for_NN[(temp_cut+8):N_temp,  ]
my_data <- subset(temp_for_NN, select = -c(qty,date,SKU))
train_data_x <- subset(train , select = -c(qty,date,SKU))
train_data_y <- subset(train, select = c(qty))
val_data_x <- subset(validate , select = -c(qty,date,SKU))
val_data_y <- subset(validate, select = c(qty))

future_x <- subset(future , select = -c(qty,date,SKU))

# normalize the data (features) for training:
my_mean <- apply(my_data, 2, mean)
my_sd <- apply(my_data, 2, sd)
# normalize features

scaled_train_data_x <- scale(train_data_x, center= my_mean, scale = my_sd)
scaled_val_data_x <- scale(val_data_x, center= my_mean, scale = my_sd)

scaled_future_x <- scale(future_x, center= my_mean, scale = my_sd)

eval_test_y <- val_data_y$qty
y_train <-train_data_y$qty

library(tensorflow)
library(keras)
set.seed(42)

#Building a model
# keras has two network building types: 1. adding one hidden layer after another one. 2. API
# Define a model
# 1. initialize a model
# 2. add layers
#Since we use rmse as the wuality evaluation
my_custom_metric_rmse <-
  custom_metric("my_rmse", function(y_true, y_pred) {
    k_sqrt(k_mean((y_pred-y_true)^2)) }
  )

my_model <- keras_model_sequential() %>% 
  layer_dense(units = 64, activation = "relu", input_shape = 22) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)
#comments
#
#compile the model
my_model %>% compile(optimizer = "adam", loss = "mse", metrics = my_custom_metric_rmse)
my_model

my_history <- my_model %>% fit(
  #we specify our training set
  scaled_train_data_x, y_train,
  batch_size = 30,
  epochs = 550,
  verbose = 0,
  #validation set:
  validation_data = list(scaled_val_data_x,eval_test_y)
)
#using GPU:
#tf$config$list_physical_devices("GPU")

my_temp_data <- predict(my_model,scaled_future_x)
SKU_names = c(ts_name,ts_name,ts_name,ts_name,ts_name,ts_name,ts_name)
temp_future <- data.frame(SKU_names, my_temp_data)
colnames(temp_future) <-c('SKU', 'NN')

temp_future[temp_future$qty<0]<-0
data_NN <- temp_future
data_NN['NN'] <- lapply(data_NN['NN'], inverse_tran) 
data_NN <- data_NN %>% group_by(SKU) %>% summarise(NN=round(sum(NN)))
single_future_prediction_NN <- bind_rows(single_future_prediction_NN,data_NN)
my_history
# Plot loss (MSE) and metrics (MAE)
plot(my_history)
#
#library(ggplot2)
my_loss_history <- my_history$metrics$val_loss

#Hyperparameter tuning
# deploy library tfruns:
library(tfruns)
runs_2 <- tuning_run("tuning_2.R", flags = list(dense_units1=c(32,64),dense_units2=c(32,64),dense_units3=c(32,64)))
#
runs_2[,c(4:10)]
#The best performing model?: units = 64 for hidden_layer1 and units = 64 for hidden_layer2
# Observation: I have to improve performance and switch to the LSTM!!!
#
#Choose the best model and evaluate it on the test set. What is the estimated generalization error of your final model?
best_run <-ls_runs(order = my_custom_metric_rmse, decreasing = FALSE, runs_dir = "runs")[1,]


#Training all SKUs except in categories Thermo and Curves for week 26 2019.
#remaining curves:

future_prediction_NN  <- as_tibble(data.frame(Characters=character(),Doubles=double()))
#future_prediction
colnames(future_prediction_NN) = c("SKU","NN")
counter<-0
for (ts_name in TS_names_to_process)
{
 
#black slip:
#ts_name <- "1008-20"

#SKUs_for_ML %>% select(-Color,-Category,-Type,-Size)
#dates_for_runs <- SKUs_for_ML %>% select(-Color,-Category,-Type,-Size) %>% left_join(df_dates)

temp_lags_rollmeans <- test_lags_rollmeans %>% filter(SKU==ts_name) %>% select(-Color,-Category,-Type,-Size)
temp_lags_rollmeans_ext <- temp_lags_rollmeans %>% extend_timeseries(.id_var = SKU, .date_var = date, .length_future = 7)
temp_for_NN <- temp_lags_rollmeans_ext  %>% left_join(df_dates)
# The qty has been log transformed before.
#ggplot(temp_for_NN, aes(date, qty)) + geom_line(color = "red")+ ylab("TS")+ xlab("Date")+ggtitle(ts_name)

temp_for_NN[is.na(temp_for_NN)] <- 0
temp_for_NN

N_temp = nrow(temp_for_NN)
temp_cut = N_temp - 14
train = temp_for_NN[1:temp_cut, ]
validate = temp_for_NN[(temp_cut+1):(temp_cut+7),  ]
future = temp_for_NN[(temp_cut+8):N_temp,  ]
my_data <- subset(temp_for_NN, select = -c(qty,date,SKU))
train_data_x <- subset(train , select = -c(qty,date,SKU))
train_data_y <- subset(train, select = c(qty))
val_data_x <- subset(validate , select = -c(qty,date,SKU))
val_data_y <- subset(validate, select = c(qty))

train_for_future_x <- bind_rows(train_data_x, val_data_x)
train_for_future_y <- bind_rows(train_data_y,val_data_y)
future_x <- subset(future , select = -c(qty,date,SKU))

# normalize the data (features) for training:
my_mean <- apply(my_data, 2, mean)
my_sd <- apply(my_data, 2, sd)
# normalize features:
scaled_train_data_x_f <- scale(train_for_future_x, center= my_mean, scale = my_sd)
scaled_future_x <- scale(future_x, center= my_mean, scale = my_sd)
y_train_f <-train_for_future_y$qty
#y_train <-train_for_future_y
#y_train_f

#Building a model
# Keras has two network building types: 
#1. adding one hidden layer after another one. 
#2. API
# Define a model
# 1. initialize a model
# 2. add layers
#We use rmse as the quality evaluation, since it is used in final evaluation.

my_history <- my_model %>% fit(
  #we specify our training set
  scaled_train_data_x_f, y_train_f,
  batch_size = 30,
  epochs = 550,
  verbose = 0,
  #validation set:
  validation_data = list(scaled_train_data_x_f,y_train_f)
)
#using GPU:
#tf$config$list_physical_devices("GPU")

my_temp_data <- predict(my_model,scaled_future_x )
SKU_names = c(ts_name,ts_name,ts_name,ts_name,ts_name,ts_name,ts_name)
temp_future <- data.frame(SKU_names, my_temp_data)
colnames(temp_future) <-c('SKU', 'NN')
temp_future[temp_future$qty<0]<-0

data_NN <- temp_future
data_NN['NN'] <- lapply(data_NN['NN'], inverse_tran) 
data_NN <- data_NN %>% group_by(SKU) %>% summarise(NN=round(sum(NN)))
future_prediction_NN <- bind_rows(single_future_prediction_NN,data_NN)

counter <- counter + 1
print(counter)
}
future_prediction_NN
# Writing results to an external file:
#write.csv(future_prediction_NN,"E:/nosave/challenge/NN_for_w26_2019.csv")

################################################
################ XGBOOST #######################
################################################
#Extreme Gradient Boosting (xgboost) is similar to gradient boosting framework but more efficient. 
#It has both linear model solver and tree learning algorithms. So, what makes it fast is its capacity to do parallel computation on a single machine.
#This makes xgboost at least 10 times faster than existing gradient boosting implementations. 
#It supports various objective functions, including regression, classification and ranking.
#Since it is very high in predictive power but relatively slow with implementation, “xgboost” becomes an ideal fit for many competitions. It also has additional features for doing cross validation and finding important variables. There are many parameters which needs to be controlled to optimize the model. We will discuss about these factors in the next section.

single_prediction_XGB <- as_tibble(data.frame(Characters=character(),Doubles=double()))
single_prediction_XGB
colnames(single_prediction_XGB) = c("SKU","XGB")

library(xgboost)
library(caret)

ts_name <- "1008-20"
temp_initial_df <- df_SKU_qntts %>% filter(SKU==ts_name)
# extend_timeseries - it extends a time series.
temp_lags_rollmeans <- test_lags_rollmeans %>% filter(SKU==ts_name) %>% select(-Color,-Category,-Type,-Size)
temp_lags_rollmeans_ext <- temp_lags_rollmeans %>% extend_timeseries(.id_var = SKU, .date_var = date, .length_future = 7)
temp_dataset <- temp_lags_rollmeans_ext  %>% left_join(df_dates)
# The data was log transformed before.
temp_dataset[is.na(temp_dataset)] <- 0
temp_dataset
ggplot(temp_dataset, aes(date, qty)) + geom_line(color = "red")+ ylab("TS")+ xlab("Date")+ggtitle(ts_name)

#Temporal data partition (training, test/validation and future)
N_temp = nrow(temp_dataset)
# 7 days for test and 7 days 
temp_cut = N_temp - 14
train = temp_for_NN[1:temp_cut, ]
validate = temp_for_NN[(temp_cut+1):(temp_cut+7),  ]
future = temp_for_NN[(temp_cut+8):N_temp,  ]
my_data <- subset(temp_for_NN, select = -c(qty,date,SKU))
train_data_x <- subset(train , select = -c(qty,date,SKU))
train_data_y <- subset(train, select = c(qty))
val_data_x <- subset(validate , select = -c(qty,date,SKU))
val_data_y <- subset(validate, select = c(qty))

future_x <- subset(future , select = -c(qty,date,SKU))

# normalize the data (features) for training:
my_mean <- apply(my_data, 2, mean)
my_sd <- apply(my_data, 2, sd)
# normalize features

scaled_train_data_x <- scale(train_data_x, center= my_mean, scale = my_sd)
scaled_val_data_x <- scale(val_data_x, center= my_mean, scale = my_sd)

scaled_future_x <- scale(future_x, center= my_mean, scale = my_sd)

eval_test_y <- val_data_y$qty
y_train <-train_data_y$qty
scaled_train_data_x
#x_train_xgbst <- xgboost::xgb.DMatrix(as.matrix(scaled_train_data_x), label=y_train)
x_train_xgbst <- xgboost::xgb.DMatrix(scaled_train_data_x, label=y_train)
#(data = train_x, label = train_y)
#x_val_xgbst <- xgboost::xgb.DMatrix(as.matrix(scaled_val_data_x), label=eval_test_y)
x_val_xgbst <- xgboost::xgb.DMatrix(scaled_val_data_x, label=eval_test_y)

xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)

xgb_grid <- base::expand.grid(
  list(
    nrounds = c(30, 40, 50, 100),
    #max_depth = c(10, 15, 20,30,50), # maximum depth of a tree
    max_depth = c(4,6),
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    #eta = 0.1, # learning rate
    eta=c(0.05,0.1,0.2),
    gamma = 0, # minimum loss reduction
    min_child_weight = c(1,10,100),  # minimum sum of instance weight (hessian) needed in a child
    subsample = c(0.5,0.75, 1) # subsample ratio of the training instances
    
  ))
# After hyperparameter evaluation the xgb_grid_tuned contained optimal parameter values:
xgb_grid_tuned <- base::expand.grid(
  list(
    nrounds = c(30, 40, 50),
    #max_depth = c(10, 15, 20,30,50), # maximum depth of a tree
    max_depth = c(4,6),
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    #eta = 0.1, # learning rate
    eta=c(0.05,0.1),
    gamma = 0, # minimum loss reduction
    min_child_weight = c(1,10,100),  # minimum sum of instance weight (hessian) needed in a child
    subsample = c(0.75, 1) # subsample ratio of the training instances
    
  ))

watchlist = list(train=x_train_xgbst, test=x_val_xgbst)
xgb_model <- caret::train(x_train_xgbst, y_train, trControl = xgb_trcontrol, tuneGrid = xgb_grid_tuned, method = "xgbTree", watchlist =watchlist,verbose=0)
# nthread = 6,  - number of cpu, when removed xgboost uses all cpus.
xgb_model$bestTune
y_hat <- predict(xgb_model$finalModel, x_val_xgbst)

SKU_names = c(ts_name,ts_name,ts_name,ts_name,ts_name,ts_name,ts_name)
temp_xgb <- data.frame(SKU_names, y_hat)
colnames(temp_xgb) <-c('SKU', 'XGB')
temp_xgb[temp_xgb$qty<0]<-0

data_XGB <- temp_xgb
data_XGB['XGB'] <- lapply(data_XGB['XGB'], inverse_tran) 
data_XGB <- data_XGB %>% group_by(SKU) %>% summarise(XGB=round(sum(XGB)))
single_prediction_XGB <- bind_rows(single_prediction_XGB,data_XGB)
#}
single_prediction_XGB

y_hat

xgb_names <- dimnames(x_train_xgbst)[[2]]
xgb_names
importance_matrix_1 <- xgb.importance(xgb_names, model=xgb_model$finalModel)
#importance_matrix <-xgboost::xgb.importance(xgb_model$finalModel$feature_names,model=xgb_model$finalModel)
xgb.plot.importance(importance_matrix_1[1:20,], xlab = "Relative importance")
# PNG device (saving pictures to the importance)
#png(paste("importance_",ts_name,".png", sep = ""))
# Code
#xgb.plot.importance(importance_matrix_1[1:20,], xlab = "Relative importance")
# Close device
#dev.off()
#library(pdp)
#partial(xgb_model, pred.var ="lag_1", plot = TRUE,rug = TRUE)
################################################
################ XGBOOST #######################
################################################
# Predicting all time series except those of SKUs in Thermo and Curves:

prediction_XGB <- as_tibble(data.frame(Characters=character(),Doubles=double()))
prediction_XGB
colnames(prediction_XGB) = c("SKU","XGB")

library(xgboost)
library(caret)
library(tictoc)
tic()
for (ts_name in TS_names_to_process)
{
#ts_name <- "1008-20"
temp_initial_df <- df_SKU_qntts %>% filter(SKU==ts_name)

dates_for_runs <- SKUs_for_ML %>% select(-Color,-Category,-Type,-Size) %>% left_join(df_dates)
temp_lags_rollmeans <- test_lags_rollmeans %>% filter(SKU==ts_name) %>% select(-Color,-Category,-Type,-Size)
temp_lags_rollmeans_ext <- temp_lags_rollmeans %>% extend_timeseries(.id_var = SKU, .date_var = date, .length_future = 7)
temp_dataset <- temp_lags_rollmeans_ext  %>% left_join(df_dates)
# we deal with skewness and variation:
temp_dataset <- temp_dataset %>% mutate(qty = log(1+qty))
# flag for log transformation
log_trans <- 1
temp_dataset[is.na(temp_dataset)] <- 0
#temp_dataset
#ggplot(temp_dataset, aes(date, qty)) + geom_line(color = "red")+ ylab("TS")+ xlab("Date")+ggtitle(ts_name)

N_temp = nrow(temp_dataset)
temp_cut = N_temp - 14
train = temp_for_NN[1:temp_cut, ]
validate = temp_for_NN[(temp_cut+1):(temp_cut+7),  ]
future = temp_for_NN[(temp_cut+8):N_temp,  ]
my_data <- subset(temp_for_NN, select = -c(qty,date,SKU))
train_data_x <- subset(train , select = -c(qty,date,SKU))
train_data_y <- subset(train, select = c(qty))
val_data_x <- subset(validate , select = -c(qty,date,SKU))
val_data_y <- subset(validate, select = c(qty))

future_x <- subset(future , select = -c(qty,date,SKU))

# normalize the data (features) for training:
my_mean <- apply(my_data, 2, mean)
my_sd <- apply(my_data, 2, sd)
# normalize features

scaled_train_data_x <- scale(train_data_x, center= my_mean, scale = my_sd)
scaled_val_data_x <- scale(val_data_x, center= my_mean, scale = my_sd)

scaled_future_x <- scale(future_x, center= my_mean, scale = my_sd)

eval_test_y <- val_data_y$qty
y_train <-train_data_y$qty

x_train_xgbst <- xgboost::xgb.DMatrix(as.matrix(scaled_train_data_x), label=y_train)
#(data = train_x, label = train_y)
x_val_xgbst <- xgboost::xgb.DMatrix(as.matrix(scaled_val_data_x), label=eval_test_y)

xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)

xgb_grid_tuned <- base::expand.grid(
  list(
    nrounds = c(30, 40, 50),
    #max_depth = c(10, 15, 20,30,50), # maximum depth of a tree
    max_depth = c(4,6),
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    #eta = 0.1, # learning rate
    eta=c(0.05,0.1),
    gamma = 0, # minimum loss reduction
    min_child_weight = c(1,10,100),  # minimum sum of instance weight (hessian) needed in a child
    subsample = c(0.75, 1) # subsample ratio of the training instances
    
  ))

watchlist = list(train=x_train_xgbst, test=x_val_xgbst)
xgb_model <- caret::train(x_train_xgbst, y_train, trControl = xgb_trcontrol, tuneGrid = xgb_grid_tuned, method = "xgbTree", watchlist =watchlist,nthread = 6, verbose=0)
xgb_model$bestTune
#nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
#43      30         4 0.1     0                1               10      0.75
y_hat <- predict(xgb_model$finalModel, x_val_xgbst)
y_hat
SKU_names = c(ts_name,ts_name,ts_name,ts_name,ts_name,ts_name,ts_name)
temp_xgb <- data.frame(SKU_names, y_hat)
colnames(temp_xgb) <-c('SKU', 'XGB')
# the NA will be changed to 0.
temp_xgb[temp_xgb$qty<0]<-0

data_XGB <- temp_xgb
data_XGB['XGB'] <- lapply(data_XGB['XGB'], inverse_tran) 
data_XGB <- data_XGB %>% group_by(SKU) %>% summarise(XGB=round(sum(XGB)))
prediction_XGB <- bind_rows(prediction_XGB,data_XGB)
}
toc()
prediction_XGB

# now the products from the categories Thermo and Curves will be added to the final data set:
zero_ts_for_XGB <- curves_thermo_SKU_forecast
colnames(zero_ts_for_XGB) <- c('SKU', 'XGB')
prediction_XGB_binded <- bind_rows(prediction_XGB, zero_ts_for_XGB) %>% arrange(SKU)
summary(prediction_XGB_binded)
df_prophet = read_csv('Prophet.csv')
########## Writing final values to an external ascii file:

combine_XGB_Prophet <- prediction_XGB_binded %>% left_join(df_prophet)
combine_XGB_Prophet

df_to_export <- combine_XGB_Prophet %>% mutate(`ForecastedSales` = round(rowMeans(select(., XGB, prophet)))) %>% select(-XGB,-prophet)


write.csv(df_to_export,"LarisaPodkolzina_ForecastedSales_v2.csv", row.names = FALSE)

##########
