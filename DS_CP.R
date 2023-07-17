#Decision Tree

library(rpart)
library(rpart.plot)
library(randomForest)
data=read.csv("Gold_pp_final_dataset.csv")
data=data[1:1376,]
tree=rpart(Adj_Close~.,data)
a=data.frame(SP_open=c(126.510002),SP_high=c(126.529999),SP_low=c(124.730003),SP_close=c(124.830002),SP_Ajclose=c(108.411423),SP_volume=c(119107100),DJ_open=c(12288.84961),DJ_high=c(12299.11035),DJ_low=c(12140.16992),DJ_close=c(12151.41016),DJ_Ajclose=c(12151.41016),DJ_volume=c(84010000),EU_Price=c(1.2936),EU_open=c(1.307),
             EU_high=c(1.3081),EU_low=c(1.291))
a1=data.frame(SP_open=data$SP_open,SP_high=data$SP_high,SP_low=data$SP_low,SP_close=data$SP_close,SP_Ajclose=data$SP_Ajclose,SP_volume=data$SP_volume,DJ_open=data$DJ_open,DJ_high=data$DJ_high,DJ_low=data$DJ_low,DJ_close=data$DJ_close,DJ_Ajclose=data$DJ_Ajclose,DJ_volume=data$DJ_volume,EU_Price=data$EU_Price,EU_open=data$EU_open,EU_high=data$EU_high,EU_low=data$EU_low)
result_test=a1[1377:1719,]
result=predict(tree,a)
result_all=predict(tree,result_test)
print(result_all)
result1=predict(tree,a1)
comp=data.frame(actual_val=data$Adj_Close,predicted_val=result1)
print(comp)
mse=(sum((comp$actual_val-comp$predicted_val)^2))/1718
mae=(sum(abs(comp$actual_val-comp$predicted_val)))/1718
rmse=sqrt(mse)
r2=sum((comp$predicted_val-mean(comp$actual_val))^2)/sum((comp$actual_val-mean(comp$actual_val))^2)
actual=mean(data$Adj_Close)
rpart.plot(tree)

#Random Forest
library(randomForest)
set.seed(1234)
output.forest=randomForest(Adj_Close~.,data=data,ntree=2001.)
print(output.forest)
b=data.frame(SP_open=c(126.510002),SP_high=c(126.529999),SP_low=c(124.730003),SP_close=c(124.830002),SP_Ajclose=c(108.411423),SP_volume=c(119107100),DJ_open=c(12288.84961),DJ_high=c(12299.11035),DJ_low=c(12140.16992),DJ_close=c(12151.41016),DJ_Ajclose=c(12151.41016),DJ_volume=c(84010000),EU_Price=c(1.2936),EU_open=c(1.307),
             EU_high=c(1.3081),EU_low=c(1.291))
b1=data.frame(SP_open=data$SP_open,SP_high=data$SP_high,SP_low=data$SP_low,SP_close=data$SP_close,SP_Ajclose=data$SP_Ajclose,SP_volume=data$SP_volume,DJ_open=data$DJ_open,DJ_high=data$DJ_high,DJ_low=data$DJ_low,DJ_close=data$DJ_close,DJ_Ajclose=data$DJ_Ajclose,DJ_volume=data$DJ_volume,EU_Price=data$EU_Price,EU_open=data$EU_open,EU_high=data$EU_high,EU_low=data$EU_low)
ans1=predict(output.forest,b)
print(ans1)
ans2=predict(output.forest,b1)
comp1=data.frame(actual_rf=data$Adj_Close,predicted_rf=ans2)
mse.rf=(sum((comp1$actual_rf-comp1$predicted_rf)^2))/1718
mae.rf=(sum(abs(comp1$actual_rf-comp1$predicted_rf)))/1718
rmse.rf=sqrt(mse.rf)
r2.rf=sum((comp1$predicted_rf-mean(comp1$actual_rf))^2)/sum((comp1$actual_rf-mean(comp1$actual_rf))^2)

#--------------SVM----------------------
library(e1071)
svm_regressor=svm(formula = data$Adj_Close ~ .,
                  data=data,
                  type='eps-regression')
c=data.frame(SP_open=c(126.510002),SP_high=c(126.529999),SP_low=c(124.730003),SP_close=c(124.830002),SP_Ajclose=c(108.411423),SP_volume=c(119107100),DJ_open=c(12288.84961),DJ_high=c(12299.11035),DJ_low=c(12140.16992),DJ_close=c(12151.41016),DJ_Ajclose=c(12151.41016),DJ_volume=c(84010000),EU_Price=c(1.2936),EU_open=c(1.307),
             EU_high=c(1.3081),EU_low=c(1.291))
result_svm=predict(svm_regressor,c)

c1=data.frame(SP_open=data$SP_open,SP_high=data$SP_high,SP_low=data$SP_low,SP_close=data$SP_close,SP_Ajclose=data$SP_Ajclose,SP_volume=data$SP_volume,DJ_open=data$DJ_open,DJ_high=data$DJ_high,DJ_low=data$DJ_low,DJ_close=data$DJ_close,DJ_Ajclose=data$DJ_Ajclose,DJ_volume=data$DJ_volume,EU_Price=data$EU_Price,EU_open=data$EU_open,EU_high=data$EU_high,EU_low=data$EU_low)
result3=predict(svm_regressor,c1)
comp2=data.frame(actual_val=data$Adj_Close,predicted_val=result3)
mse.svm=(sum((comp2$actual_val-comp2$predicted_val)^2))/1718
mae.svm=(sum(abs(comp2$actual_val-comp2$predicted_val)))/1718
rmse.svm=sqrt(mse.svm)
r2.svm=sum((comp2$predicted_val-mean(comp2$actual_val))^2)/sum((comp2$actual_val-mean(comp2$actual_val))^2)

#----------------RESULT-------------------------
cat("\n\n\nThe value Predicted by Decision Tree is ",result,"\nMAE = ",mae,"\nRMSE = ",rmse,"\nR2 = ",r2,"\n
    The value Predicted by Random Forest is ",ans1,"\nMAE = ",mae.rf,"\nRMSE = ",rmse.rf,"\nR2 = ",r2.rf,"\n
    The value Predicted by SVM is ",result_svm,"\nMAE = ",mae.svm,"\nRMSE = ",rmse.svm,"\nR2 = ",r2.svm,"
    Actual Value is 151.029999
")