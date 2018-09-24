library(xlsx)
library(tidyr)
library(Metrics)
library(smooth)
library(TTR)
library(tseries)

setwd('D:\\KE Notes\\Developing intelligent systems for BA\\Day3\\2a')
data <- read.csv("FinancialData.csv")
# creating a new dataframe using week, sg->d, p->d only
new_data <- data[c('Week','SG.D','P.D')]
# creating sg_ts and pound_ts using the exchange rates
sg_ts <- ts(new_data$SG.D, start = c(2006,1), frequency = 52) 
pound_ts <- ts(new_data$P.D, start = c(2006,1), frequency = 52) 
# plotting 
ts.plot(sg_ts,pound_ts,col=c("blue","red"))

# creating the initial train test , leaving 104 observations for test
sg_train <- window(sg_ts,start=c(2006,1),end=c(2015,34))
pound_train <- window(pound_ts,start=c(2006,1),end=c(2015,34))
sg_test <- window(sg_ts,start=c(2015,35))
pound_test <- window(pound_ts,start=c(2015,35))
ts.plot(sg_train,pound_train,col=c("blue","red"))

# triple exponential smoothing
es_tr_model <- HoltWinters(pound_train)
es_tr_pred <- predict(es_tr_model, 104, prediction.interval = FALSE)
ts.plot(pound_ts,es_tr_pred,col=c("red","blue"))

#pred_sg_xch <- data.frame()
#pred_pound_xch <- data.frame()

pred_xch <- data.frame(sgd_e=double(), pd_e=double(),
                       sgd_a=double(), pd_a=double(),
                       sgd_l=double(), pd_l=double())
for (i in c(0:103)){
  end_m = (34 + i)%%52
  end_y = 2015 + (34 + i)%/%52
  if (end_m == 0){
    end_m = 52
    end_y = end_y-1
  }
  loop_sg_train <- window(sg_ts,start=c(2006,1),end=c(end_y,end_m))
  loop_pound_train <- window(pound_ts,start=c(2006,1),end=c(end_y,end_m))
  estr_sg_model <- HoltWinters(loop_sg_train, gamma = FALSE)
  estr_pound_model <- HoltWinters(loop_pound_train, gamma = FALSE)
  estr_sg_pred <- predict(estr_sg_model, 1)
  estr_pound_pred <- predict(estr_pound_model, 1)
  #print(paste(502+i,502+1+i,es_tr_pred[1]))
  #pred_sg_xch[(1+i):104,paste('wk',toString(503+i))] <- estr_sg_pred
  #pred_pound_xch[(1+i):104,paste('wk',toString(503+i))] <- estr_pound_pred
  pred_xch[i+1,c('sgd_e','pd_e')] <- list(estr_sg_pred[1],estr_pound_pred[1])
}
#write.csv(x =pred_sg_xch,file = 'pred_sg_xch.csv',row.names = FALSE )
#write.csv(x =pred_pound_xch,file = 'pred_pound_xch.csv',row.names = FALSE )
pred_e_sg_ts <- ts(pred_xch$sgd_e, start = c(2015,35), frequency = 52)
pred_e_pound_ts <- ts(pred_xch$pd_e, start = c(2015,35), frequency = 52)
ts.plot(sg_test,pred_e_sg_ts,col=c("red","blue"))

mape(data[503:606,'P.D'],estr_pound_pred)


library(forecast)

#pred_xch <- data.frame(sgd=double(), pd=double())
for (i in c(0:103)){
  end_m = (34 + i)%%52
  end_y = 2015 + (34 + i)%/%52
  if (end_m == 0){
    end_m = 52
    end_y = end_y-1
  }
  loop_sg_train <- window(sg_ts,start=c(2006,1),end=c(end_y,end_m))
  loop_pound_train <- window(pound_ts,start=c(2006,1),end=c(end_y,end_m))
  arima_sg_model <- auto.arima(loop_sg_train)
  arima_pound_model <- auto.arima(loop_pound_train)
  arima_sg_pred <- forecast(arima_sg_model, 1)$mean[1]
  arima_pound_pred <- forecast(arima_pound_model, 1)$mean[1]
  #print(paste(502+i,502+1+i,es_tr_pred[1]))
  #pred_sg_xch[(1+i):104,paste('wk',toString(503+i))] <- estr_sg_pred
  #pred_pound_xch[(1+i):104,paste('wk',toString(503+i))] <- estr_pound_pred
  pred_xch[i+1,c('sgd_a','pd_a')] <- list(arima_sg_pred,arima_pound_pred)
}

pred_a_sg_ts <- ts(pred_xch$sgd_a, start = c(2015,35), frequency = 52)
pred_a_pound_ts <- ts(pred_xch$pd_a, start = c(2015,35), frequency = 52)

ts.plot(red_a_sg_ts,sg_test,col=c("red","blue","green"))


for (i in c(0:103)){
  loop_sg_train <- new_data[new_data$Week<=502+i,c('Week',"SG.D")]
  loop_sg_test <- new_data[new_data$Week>502+i,c('Week',"SG.D")]
  loop_pound_train <- new_data[new_data$Week<=502+i,c('Week',"P.D")]
  loop_pound_test <- new_data[new_data$Week>502+i,c('Week',"P.D")]
  lm_sg_model <- lm(SG.D ~ Week,loop_sg_train)
  lm_pound_model <- lm(P.D ~ Week,loop_pound_train)
  lm_sg_pred <- predict(lm_sg_model,loop_sg_test)[1]
  lm_pound_pred <- predict(lm_pound_model,loop_pound_test)[1]
  #print(paste(502+i,502+1+i,es_tr_pred[1]))
  #pred_sg_xch[(1+i):104,paste('wk',toString(503+i))] <- estr_sg_pred
  #pred_pound_xch[(1+i):104,paste('wk',toString(503+i))] <- estr_pound_pred
  pred_xch[i+1,c('sgd_l','pd_l')] <- list(lm_sg_pred,lm_pound_pred)
}

pred_l_sg_ts <- ts(pred_xch$sgd_l, start = c(2015,35), frequency = 52)
pred_l_pound_ts <- ts(pred_xch$pd_l, start = c(2015,35), frequency = 52)

ts.plot(pred_l_pound_ts,pound_test,col=c("red","blue"))
ts.plot(pred_l_sg_ts,sg_test,col=c("red","blue"))
