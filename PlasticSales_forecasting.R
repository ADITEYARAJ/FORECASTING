sales <- read.csv(file.choose())
View(sales)
summary(sales)
str(sales)
plot(sales$Sales,type = "o")
# So creating 12 dummy variables 
X <- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names 
View(X)
sales.p <- cbind(sales,X)
View(sales.p)
colnames(sales.p)
sales.p["t"] <- c(1:60)
sales.p["log_sales"] <- log10(sales$Sales)
sales.p["t_square"] <- sales$Sales*sales$Sales

#partition
train <- sales.p[1:45,]
test <- sales.p[46:60,]
########################### LINEAR MODEL #############################
linear.model <- lm(Sales ~ t,data=train)
summary(linear.model)
linear.pred <- data.frame(predict(linear.model, interval='predict', newdata =test))
rmse.linear <- sqrt(mean((test$Sales-linear.pred$fit)^2, na.rm = T))
AIC(linear.model)
rmse.linear#241
######################### Exponential #################################

expo.model <- lm(log_sales ~ t, data = train)
summary(expo.model)
expo.pred <- data.frame(predict(expo.model, interval='predict', newdata = test))
rmse.expo <- sqrt(mean((test$Sales-exp(expo.pred$fit))^2, na.rm = T))
AIC(expo.model)
rmse.expo#1335
######################### Quadratic ####################################

Quad.model <- lm(Sales ~ t + t_square, data = train)
summary(Quad.model)
Quad.pred <- data.frame(predict(Quad.model, interval='predict', newdata=test))
rmse.Quad <- sqrt(mean((test$Sales-Quad.pred$fit)^2, na.rm=T))
AIC(Quad.model)
rmse.Quad#44
######################### Additive Seasonality #########################

sea.add.model <- lm(Sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(sea.add.model)
sea.add.pred <- data.frame(predict(sea.add.model, newdata=test, interval = 'predict'))
rmse.sea.add <- sqrt(mean((test$Sales-sea.add.pred$fit)^2, na.rm = T))
AIC(sea.add.model)
rmse.sea.add#257.13
######################## Additive Seasonality with Quadratic #################

Add.sea.Quad.model <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add.sea.Quad.model)
Add.sea.Quad.pred <- data.frame(predict(Add.sea.Quad.model, interval='predict', newdata=test))
rmse.Add.sea.Quad <- sqrt(mean((test$Sales - Add.sea.Quad.pred$fit)^2, na.rm=T))
AIC(Add.sea.Quad.model)
rmse.Add.sea.Quad#42067
######################## Multiplicative Seasonality #########################

multi.sea.model <- lm(log_sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi.sea.model)
multi.sea.pred <- data.frame(predict(multi.sea.model, newdata=test, interval='predict'))
rmse.multi.sea <- sqrt(mean((test$Sales-exp(multi.sea.pred$fit))^2, na.rm = T))
AIC(multi.sea.model)
rmse.multi.sea#1337.67
# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse.linear,rmse.expo,rmse.Quad,rmse.sea.add,rmse.Add.sea.Quad,rmse.multi.sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic has least RMSE value
write.csv(sales.p, file="plastic_sales.csv", row.names = F)
getwd()
############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add.sea.Quad.model <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add.sea.Quad.model)
AIC(Add.sea.Quad.model)
####################### Predicting new data #############################
View(test)
pred_new <- predict(Add.sea.Quad.model, newdata = test, interval = 'predict')
pred_new <- as.data.frame(pred_new)

plot(Add.sea.Quad.model)
acf(Add.sea.Quad.model$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot

A <- arima(Add.sea.Quad.model$residuals, order = c(2,0,0))
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 10)

# predicting next 12 months errors using arima( order =c(1,0,0))

library(forecast)
errors_12 <- forecast(A, h = 12)

View(errors_12)

future_errors <- data.frame(errors_12)
class(future_errors)
future_errors <- future_errors$Point.Forecast

# predicted values for new data + future error values 

predicted_new_values <- pred_new + future_errors

write.csv(predicted_new_values, file = "predicted_new_values.csv", row.names = F)
getwd()


