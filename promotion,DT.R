promotion.raw<- read.csv("C:/Users/user/Desktop/promotion.csv", header=TRUE)
head(promotion.raw)
#--------------------------------------------------------------------------------
str(promotion.raw)#54808 obs. of  14 variables          
promotion<-promotion.raw
promotion<- subset(promotion,select = - employee_id) #去除不要得變數
#--------------------------------------------------------------------------------
#資料預處理:
#outier##
#標準化分數判斷: 
require(datasets) 
data <- promotion
data <- subset(data,select = - department )# 去除不是數值型態的column
data <- subset(data,select = - region)
data <- subset(data,select = - education)
data <- subset(data,select = - gender)
data <- subset(data,select = - recruitment_channel) 

data <- subset(data,select = - KPIs_met..80.)#### 去除二元數值型態的column 
data <- subset(data,select = - awards_won.)
data <- subset(data,select = - is_promoted )
str(data)#只有數值#var.=5

scale_data <- scale(data, center = TRUE, scale = TRUE) # 標準化表單中的數值
scale_data <- as.data.frame(scale_data) # 轉成Data frame型態
names(scale_data)                               #重新命名scale 後的variables
names(scale_data)[1]<-"scale_no_of_trainings"
names(scale_data)[2]<-"scale_age"
names(scale_data)[3]<-"scale_previous_year_rating"
names(scale_data)[4]<-"scale_length_of_service"
names(scale_data)[5]<-"scale_avg_training_score"

scale_data <- cbind(promotion,scale_data)
scale_data <- subset(scale_data, scale_no_of_trainings< 2 & scale_age < 2 & scale_previous_year_rating < 2 & scale_length_of_service < 2 & scale_avg_training_score < 2) # 留下全部欄位中，Z分數小於2的值
scale_data <- subset(scale_data, scale_no_of_trainings> -2 & scale_age > -2 & scale_previous_year_rating > -2 & scale_length_of_service > -2 & scale_avg_training_score > -2) # 留下全部欄位中，Z分數大於-2的值
str(scale_data)#43526 obs. of  18 variables

#遺漏值處理  ###直接移除有遺漏值的資料
complete.cases(scale_data)
summary(complete.cases(scale_data))#沒有missing value
finaldata<-scale_data
#split the dataset :training dataset/testing dataset (80% vs. 20%)##
require(caTools)
set.seed(123)  
sample = sample.split(finaldata,SplitRatio = 0.8)
train =subset(finaldata,sample ==TRUE) 
test=subset(finaldata, sample==FALSE)

#Decision Tree##=======================================================
require(rpart)
cart.model<- rpart(is_promoted ~. ,data=train)

# 輸出各節點的細部資訊(呈現在console視窗)
cart.model
#rpart有專屬的繪圖套件rpart.plot，函式是prp()
#說真的，用prp()畫出來的決策樹，比較好看一些
require(rpart.plot) 
prp(cart.model,         # 模型
    faclen=1,           # 0/1呈現的變數以縮寫(不然太寬)
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  
    extra=1)
#predic#
pred <- predict(cart.model, newdata=test)
table(real=test$is_promoted, predict=pred)
# 計算預測準確率 = 對角線的數量/總數量
confus.matrix <- table(real=test$is_promoted, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix) # 對角線的數量/總數量
#準確率只有0.2691274 :很低

