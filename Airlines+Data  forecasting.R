pas <- read_excel(file.choose())
View(pas)
summary(pas)
str(pas)
plot(pas$Passengers,type="o")
# Pre Processing

# So creating 12 dummy variables 
X <- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names 
View(X)
airline.pas <- cbind(pas,X)
View(airline.pas)
colnames(airline.pas)
#input t
airline.pas["t"] <- c(1:96)
airline.pas["log_passengers"] <- log(airline.pas$Passengers)
airline.pas["t_square"] <- airline.pas$t*airline.pas$t

# partitioning
train <- airline.pas[0:77,]
test <- airline.pas[78:96,]
attach(airline.pas)

########################### LINEAR MODEL #############################
linear.model <- lm(Passengers ~ t,data=train)
summary(linear.model)
linear.pred <- data.frame(predict(linear.model, interval='predict', newdata =test))
rmse.linear <- sqrt(mean((test$Passengers-linear.pred$fit)^2, na.rm = T))
rmse.linear#58.148
######################### Exponential #################################

expo.model <- lm(log_passengers ~ t, data = train)
summary(expo.model)
expo.pred <- data.frame(predict(expo.model, interval='predict', newdata = test))
rmse.expo <- sqrt(mean((test$Passengers-exp(expo.pred$fit))^2, na.rm = T))
rmse.expo#47.8896
######################### Quadratic ####################################

Quad.model <- lm(Passengers ~ t + t_square, data = train)
summary(Quad.model)
Quad.pred <- data.frame(predict(Quad.model, interval='predict', newdata=test))
rmse.Quad <- sqrt(mean((test$Passengers-Quad.pred$fit)^2, na.rm=T))
rmse.Quad#58.92
######################### Additive Seasonality #########################

sea.add.model <- lm(Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(sea.add.model)
sea.add.pred <- data.frame(predict(sea.add.model, newdata=test, interval = 'predict'))
rmse.sea.add <- sqrt(mean((test$Passengers-sea.add.pred$fit)^2, na.rm = T))
rmse.sea.add#133.315
######################## Additive Seasonality with Quadratic #################

Add.sea.Quad.model <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add.sea.Quad.model)
Add.sea.Quad.pred <- data.frame(predict(Add.sea.Quad.model, interval='predict', newdata=test))
rmse.Add.sea.Quad <- sqrt(mean((test$Passengers - Add.sea.Quad.pred$fit)^2, na.rm=T))
rmse.Add.sea.Quad#39.617
######################## Multiplicative Seasonality #########################

multi.sea.model <- lm(log_passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi.sea.model)
multi.sea.pred <- data.frame(predict(multi.sea.model, newdata=test, interval='predict'))
rmse.multi.sea <- sqrt(mean((test$Passengers-exp(multi.sea.pred$fit))^2, na.rm = T))
rmse.multi.sea#138.75
# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse.linear,rmse.expo,rmse.Quad,rmse.sea.add,rmse.Add.sea.Quad,rmse.multi.sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)
# Additive seasonality with Quadratic has least RMSE value

write.csv(airline.pas, file="airline_Passenger.csv", row.names = F)
getwd()
############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add.sea.Quad.model <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add.sea.Quad.model)
