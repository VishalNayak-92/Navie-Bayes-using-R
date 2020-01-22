rm(list=ls(all=TRUE))
setwd("/Users/nilesh/Downloads/20190921_Batch70_CSE7402c_Naive_Bayes/")

#Read the data into R

data=read.csv(file="FlightDelays.csv", header=T)
colnames(data)
head(data)
###Column Names and Description :
# CARRIER Name
# Departure Time(DEP_TIME)
# Carrier Destination(DEST)
# Flight Date(FL_DATE)
# Flight Number(FL_NUM)
# Weather(Code of 1 represents there was a weather-related delay)
# Tail Number
# Flight Status(where it’s ontime or delayed)



data = subset(data,select=-c(FL_DATE,FL_NUM,TAIL_NUM))

head(data)

str(data)


DEP_TIME_BIN=0
for(i in 1:2201) {
  if(data$DEP_TIME[i]<600){DEP_TIME_BIN[i]="0500-0559"} 
  else if(data$DEP_TIME[i]<700) {DEP_TIME_BIN[i]="0600-0659"}
  else if(data$DEP_TIME[i]<800) {DEP_TIME_BIN[i]="0700-0759"}
  else if(data$DEP_TIME[i]<900) {DEP_TIME_BIN[i]="0800-0859"}
  else if(data$DEP_TIME[i]<1000) {DEP_TIME_BIN[i]="0900-0959"}
  else if(data$DEP_TIME[i]<1100) {DEP_TIME_BIN[i]="1000-1059"}
  else if(data$DEP_TIME[i]<1200) {DEP_TIME_BIN[i]="1100-1159"}
  else if(data$DEP_TIME[i]<1300) {DEP_TIME_BIN[i]="1200-1259"}
  else if(data$DEP_TIME[i]<1400) {DEP_TIME_BIN[i]="1300-1359"}
  else if(data$DEP_TIME[i]<1500) {DEP_TIME_BIN[i]="1400-1459"}
  else if(data$DEP_TIME[i]<1600) {DEP_TIME_BIN[i]="1500-1559"}
  else if(data$DEP_TIME[i]<1700) {DEP_TIME_BIN[i]="1630-1659"}
  else if(data$DEP_TIME[i]<1800) {DEP_TIME_BIN[i]="1700-1759"}
  else if(data$DEP_TIME[i]<1900) {DEP_TIME_BIN[i]="1800-1859"}
  else if(data$DEP_TIME[i]<2000) {DEP_TIME_BIN[i]="1900-1959"}
  else if(data$DEP_TIME[i]<2100) {DEP_TIME_BIN[i]="2000-2059"}
  else if(data$DEP_TIME[i]<2200) {DEP_TIME_BIN[i]="2100-2159"}
  else if(data$DEP_TIME[i]<2300) {DEP_TIME_BIN[i]="2200-2259"}
  else {DEP_TIME_BIN[i]="2300-2359"}
}

table(DEP_TIME_BIN)

Flight.Status = factor(ifelse(data$Flight.Status == "delayed", 1, 0))
Flight.Status

data2 = data.frame(data,DEP_TIME_BIN)
head(data2)
data2$DEP_TIME = NULL

head(data2)
str(data2)

unique(data2$Weather)
data2$Weather = factor(data2$Weather)

### Split data in Train and test

#rows=seq(1,nrow(data2),1)
set.seed(123)


### Train-Test Split 

library(caret)
trainRows=createDataPartition(data$CARRIER,p = 0.6,list = FALSE,)

data_train = data2[trainRows,] 
data_test=data2[-trainRows,] 


### Model Building

library(e1071)
model = naiveBayes(Flight.Status ~ CARRIER+DEP_TIME_BIN+DEST+ORIGIN, data = data_train)
model

pred = predict(model, data_train)
tabtrain=table(pred, data_train$Flight.Status)
tabtrain
confusionMatrix(tabtrain)   #Confusion Matrix
#Accuracy on Train : 0.8266


pred = predict(model, data_test)
tabtest = table(pred, data_test$Flight.Status)
tabtest

confusionMatrix(tabtest)
#Accuracy on Test : 0.828



colnames(data_train)
model2 = naiveBayes(Flight.Status ~ ., data = data_train)
model2

pred2 = predict(model2, data_train)
tabtrain1 = table(pred2, data_train$Flight.Status)
confusionMatrix(tabtrain1)
#Accuracy on Train: 0.84

pred2 = predict(model2, data_test)
tabtest1 = table(pred2, data_test$Flight.Status)
confusionMatrix(tabtest1)
#Accuracy on Test : 0.8394         
