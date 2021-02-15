require(jsonlite)
require(httr)
require(data.table)
require(forecast)
require(tseries)
require(xts)
library(tidyverse)
library(zoo)
library(dplyr)
library(lubridate)
require(tsbox)
require(astsa)
library(MLmetrics)
get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    if(i<nrow(predictions)){
      post_string=sprintf("%s%s,",post_string,predictions$forecast[i])
    } else {
      post_string=sprintf("%s%s)",post_string,predictions$forecast[i])
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if('forecast' %in% names(predictions)){
      if(nrow(predictions)==24){
        if(all(is.numeric(predictions$forecast))){
          print("Format OK")
          return(TRUE)
        } else {
          print("forecast information is not numeric")
          return(FALSE)                
        }
      } else {
        print("Forecasts for 24 hours should be provided, current number of rows:")
        print(nrow(predictions))
        return(FALSE)     
      }
    } 
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://46.101.124.77'

u_name = "Group13"
p_word = "NnFgYzOyEvYYMhaM"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)

# this part is where you need to provide your forecasting function / or set of R codes
date <- c("2021-01-30")
datehour <- c("2021-01-29 23:00:00")

bulkdata <- read_csv("bulk.csv")
organized <- data %>% arrange(event_date, event_hour)
colnames(bulkdata)[1] <- "event_date"
colnames(bulkdata)[2] <- "event_hour"
colnames(bulkdata)[3] <- "consumption"
colnames(bulkdata)[4] <- "t_1"
colnames(bulkdata)[5] <- "t_2"
colnames(bulkdata)[6] <- "t_3"
colnames(bulkdata)[7] <- "t_4"
colnames(bulkdata)[8] <- "t_5"
colnames(bulkdata)[9] <- "t_6"
colnames(bulkdata)[10] <- "t_7"
allconsumption <- dplyr::bind_rows(bulkdata, organized)
allconsumption$Date <- paste(allconsumption$event_date, allconsumption$event_hour)
allconsumption$Date = as.POSIXct(strptime(allconsumption$Date, format = "%Y-%m-%d %H"))
xts_t1 = apply.daily(xts(allconsumption$t_1, order.by = allconsumption$Date), mean)
xts_t2 = apply.daily(xts(allconsumption$t_2, order.by = allconsumption$Date), mean)
xts_t3 = apply.daily(xts(allconsumption$t_3, order.by = allconsumption$Date), mean)
xts_t4 = apply.daily(xts(allconsumption$t_4, order.by = allconsumption$Date), mean)
xts_t5 = apply.daily(xts(allconsumption$t_5, order.by = allconsumption$Date), mean)
xts_t6 = apply.daily(xts(allconsumption$t_6, order.by = allconsumption$Date), mean)
xts_t7 = apply.daily(xts(allconsumption$t_7, order.by = allconsumption$Date), mean)
ts_t1 <- ts_ts(xts_t1[index(xts_t1) <= date])
ts_t2 <- ts_ts(xts_t2[index(xts_t2) <= date])
ts_t3 <- ts_ts(xts_t3[index(xts_t3) <= date])
ts_t4 <- ts_ts(xts_t4[index(xts_t4) <= date])
ts_t5 <- ts_ts(xts_t5[index(xts_t5) <= date])
ts_t6 <- ts_ts(xts_t6[index(xts_t6) <= date])
ts_t7 <- ts_ts(xts_t7[index(xts_t7) <= date])

fts_t1 <- ts_ts(xts_t1[index(xts_t1) > date])
fts_t2 <- ts_ts(xts_t2[index(xts_t2) > date])
fts_t3 <- ts_ts(xts_t3[index(xts_t3) > date])
fts_t4 <- ts_ts(xts_t4[index(xts_t4) > date])
fts_t5 <- ts_ts(xts_t5[index(xts_t5) > date])
fts_t6 <- ts_ts(xts_t6[index(xts_t6) > date])
fts_t7 <- ts_ts(xts_t7[index(xts_t7) > date]) 
future_regressors <- cbind(fts_t1, fts_t2, fts_t3, fts_t4, fts_t5, fts_t6, fts_t7)
colnames(future_regressors)[colnames(future_regressors) == "fts_t1"] <- "ts_t1"
colnames(future_regressors)[colnames(future_regressors) == "fts_t2"] <- "ts_t2"
colnames(future_regressors)[colnames(future_regressors) == "fts_t3"] <- "ts_t3"
colnames(future_regressors)[colnames(future_regressors) == "fts_t4"] <- "ts_t4"
colnames(future_regressors)[colnames(future_regressors) == "fts_t5"] <- "ts_t5"
colnames(future_regressors)[colnames(future_regressors) == "fts_t6"] <- "ts_t6"
colnames(future_regressors)[colnames(future_regressors) == "fts_t7"] <- "ts_t7"


xts_consumption = apply.daily(xts(allconsumption$consumption, order.by = allconsumption$Date), sum)
train_consumption <- ts_ts(xts_consumption[index(xts_consumption) <= date])

train_regressors <- cbind(ts_t1, ts_t2, ts_t3, ts_t4, ts_t5, ts_t6, ts_t7)

organized$Date <- paste(organized$event_date, organized$event_hour)
organized$Date = as.POSIXct(strptime(organized$Date, format = "%Y-%m-%d %H"))
organized <- organized %>%
  group_by(event_date) %>%
  mutate(Mean = mean(consumption))
organized$consumption <- (organized$consumption - organized$Mean)

xts_organized= xts(organized$consumption, order.by = organized$Date)
organized_train_consumption <- ts_ts(xts_organized[index(xts_organized) <= datehour])
attr(train_consumption, 'frequency')<- 7
#Daily model
fitted <- arima(train_consumption, xreg = train_regressors, order = c(7,1,2))
forecast1 <- predict(fitted, newxreg = future_regressors)
#Hourly model
dailyfit <- arima(organized_train_consumption, order = c(0,0,1), seasonal = list(order = c(0, 1, 1 ),period=24))

hourlyforecast <- forecast(dailyfit, h=48)
hnextday <- as.matrix(hourlyforecast$mean)

truetest<- organized[organized$Date <"2021-02-06 00:00:00",]
truetest <- truetest[truetest$Date >"2021-02-04 23:00:00",]
addmeanback <- matrix(truetest$Mean, nrow = 24, ncol = 1)
finaltest <-as.list(truetest$consumption + addmeanback)
vectortest <- unlist(finaltest)

hmean <- as.matrix(forecast1$pred)
div <- c(24)
hmean[2]<- hmean[2]/div
add2 <- matrix(hmean[2], nrow=48, ncol=1)
final <- as.list(hnextday+ add2)
submit <- as.list(final[index(final)>24])
vector <- unlist(submit)

predictions=data.table(Date=rep(as.Date(Sys.time())+1,24),Hour=0:23)
# be sure if ordered
predictions=predictions[order(Date,Hour)]
MAPE(vector,vectortest)
plot(vector)
# dummy forecast
predictions[,forecast:=vector]

send_submission(predictions, token, url=subm_url, submit_now=T)
