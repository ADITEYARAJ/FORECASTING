library(readxl)
sales <- read_excel(file.choose())
View(sales)
summary(sales)
str(sales)
plot(sales$Sales,type = "o")
# Pre Processing

# So creating 12 dummy variables 
X <- data.frame(outer(rep(month.abb,length = 42), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names 
View(X)
sales.coo <- cbind(sales,X)
View(sales.coo)
colnames(sales.coo)
#input t
sales.coo["t"] <- c(1:42)
sales.coo["log_sales"] <- log(sales$Sales)
sales.coo["t_square"] <- sales.coo$t*sales.coo$t

#partition
train <- sales.coo[1:30,]
test <- sales.coo[31:42,]
attach(sales.coo)
########################### LINEAR MODEL #############################
linear.model <- lm(Sales ~ t,data=train)
summary(linear.model)
linear.pred <- data.frame(predict(linear.model, interval='predict', newdata =test))
rmse.linear <- sqrt(mean((test$Sales-linear.pred$fit)^2, na.rm = T))
AIC(linear.model)
rmse.linear#741.148
######################### Exponential #################################

expo.model <- lm(log_sales ~ t, data = train)
summary(expo.model)
expo.pred <- data.frame(predict(expo.model, interval='predict', newdata = test))
rmse.expo <- sqrt(mean((test$Sales-exp(expo.pred$fit))^2, na.rm = T))
AIC(expo.model)
rmse.expo#552.281
######################### Quadratic ####################################

Quad.model <- lm(Sales ~ t + t_square, data = train)
summary(Quad.model)
Quad.pred <- data.frame(predict(Quad.model, interval='predict', newdata=test))
rmse.Quad <- sqrt(mean((test$Sales-Quad.pred$fit)^2, na.rm=T))
AIC(Quad.model)
rmse.Quad#646.27
######################### Additive Seasonality #########################

sea.add.model <- lm(Sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(sea.add.model)
sea.add.pred <- data.frame(predict(sea.add.model, newdata=test, interval = 'predict'))
rmse.sea.add <- sqrt(mean((test$Sales-sea.add.pred$fit)^2, na.rm = T))
AIC(sea.add.model)
rmse.sea.add#1770.515
######################## Additive Seasonality with Quadratic #################

Add.sea.Quad.model <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add.sea.Quad.model)
Add.sea.Quad.pred <- data.frame(predict(Add.sea.Quad.model, interval='predict', newdata=test))
rmse.Add.sea.Quad <- sqrt(mean((test$Sales - Add.sea.Quad.pred$fit)^2, na.rm=T))
AIC(Add.sea.Quad.model)
rmse.Add.sea.Quad#637.368
######################## Multiplicative Seasonality #########################

multi.sea.model <- lm(log_sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi.sea.model)
multi.sea.pred <- data.frame(predict(multi.sea.model, newdata=test, interval='predict'))
rmse.multi.sea <- sqrt(mean((test$Sales-exp(multi.sea.pred$fit))^2, na.rm = T))
AIC(multi.sea.model)
rmse.multi.sea#1820.766
# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse.linear,rmse.expo,rmse.Quad,rmse.sea.add,rmse.Add.sea.Quad,rmse.multi.sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)
# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse.linear,rmse.expo,rmse.Quad,rmse.sea.add,rmse.Add.sea.Quad,rmse.multi.sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)
# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse.linear,rmse.expo,rmse.Quad,rmse.sea.add,rmse.Add.sea.Quad,rmse.multi.sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)
# Additive seasonality with Quadratic has least RMSE value
write.csv(sales.coo, file="cocacola_sales.csv", row.names = F)
getwd()
############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add.sea.Quad.model <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add.sea.Quad.model)
AIC(Add.sea.Quad.model)
