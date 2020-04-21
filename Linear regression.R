#waist vs AT model - Class room model
colnames(WC_AT)
model<- lm(AT~Waist,data =WC_AT)
summary(model)
new_WC=data.frame(Waist=c(40,70,200))
AT1 =predict(model,newdata=new_WC)
AT1


# SALARY VS EXPERIANCE MODEL  Build a prediction model for Salary_hike
library(readxl)
df_Sal <- read.csv("Salary_Data.csv",header = T,sep = ",")
View(df_Sal)

colnames(df_Sal)
model1<- lm(Salary~YearsExperience,data =df_Sal)
summary(model1)
new_Sal=data.frame(YearsExperience=c(6,8))
Sal =predict(model1,newdata=new_Sal)
Sal

#Delivery time Model Predict delivery time using sorting time

df_delt <- read.csv("delivery_time.csv",header = T,sep = ",")
colnames(df_delt)

model2<- lm(Delivery.Time~Sorting.Time,data =df_delt)
summary(model2)
new_time=data.frame(Sorting.Time=c(6.4,8.3))
DelN =predict(model2,newdata=new_time)
DelN

#Emp_data -> Build a prediction model for Churn_out_rate 

df_emp <- read.csv("emp_data.csv",header = T,sep = ",")
View(df_emp)

colnames(df_emp)

model3<- lm(Churn_out_rate~Salary_hike,data =df_emp)
summary(model3)
new_Salary_hike=data.frame(Salary_hike=c(1549,1768))
NewChurn =predict(model3,newdata=new_Salary_hike)
NewChurn

#Calories_consumed-> predict weight gained using calories consumed

df_cal <- read.csv("calories_consumed.csv",header = T,sep = ",")
View(df_cal)

colnames(df_cal)

names(df_cal)[1] <- "WeightGained"
names(df_cal)[2] <- "CaloriesConsumed"

colnames(df_cal)
model4<- lm(WeightGained~CaloriesConsumed,data =df_cal)
summary(model4)
new_cal=data.frame(CaloriesConsumed=c(1456,1873))
Nweightg =predict(model4,newdata=new_cal)
Nweightg