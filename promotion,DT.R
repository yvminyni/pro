promotion.raw<- read.csv("C:/Users/user/Desktop/promotion.csv", header=TRUE)
head(promotion.raw)
#--------------------------------------------------------------------------------
str(promotion.raw)#54808 obs. of  14 variables          
promotion<-promotion.raw
promotion<- subset(promotion,select = - employee_id) #�h�����n�o�ܼ�
#--------------------------------------------------------------------------------
#��ƹw�B�z:
#outier##
#�зǤƤ��ƧP�_: 
require(datasets) 
data <- promotion
data <- subset(data,select = - department )# �h�����O�ƭȫ��A��column
data <- subset(data,select = - region)
data <- subset(data,select = - education)
data <- subset(data,select = - gender)
data <- subset(data,select = - recruitment_channel) 

data <- subset(data,select = - KPIs_met..80.)#### �h���G���ƭȫ��A��column 
data <- subset(data,select = - awards_won.)
data <- subset(data,select = - is_promoted )
str(data)#�u���ƭ�#var.=5

scale_data <- scale(data, center = TRUE, scale = TRUE) # �зǤƪ��椤���ƭ�
scale_data <- as.data.frame(scale_data) # �নData frame���A
names(scale_data)                               #���s�R�Wscale �᪺variables
names(scale_data)[1]<-"scale_no_of_trainings"
names(scale_data)[2]<-"scale_age"
names(scale_data)[3]<-"scale_previous_year_rating"
names(scale_data)[4]<-"scale_length_of_service"
names(scale_data)[5]<-"scale_avg_training_score"

scale_data <- cbind(promotion,scale_data)
scale_data <- subset(scale_data, scale_no_of_trainings< 2 & scale_age < 2 & scale_previous_year_rating < 2 & scale_length_of_service < 2 & scale_avg_training_score < 2) # �d�U������줤�AZ���Ƥp��2����
scale_data <- subset(scale_data, scale_no_of_trainings> -2 & scale_age > -2 & scale_previous_year_rating > -2 & scale_length_of_service > -2 & scale_avg_training_score > -2) # �d�U������줤�AZ���Ƥj��-2����
str(scale_data)#43526 obs. of  18 variables

#��|�ȳB�z  ###������������|�Ȫ����
complete.cases(scale_data)
summary(complete.cases(scale_data))#�S��missing value
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

# ��X�U�`�I���ӳ���T(�e�{�bconsole����)
cart.model
#rpart���M�ݪ�ø�ϮM��rpart.plot�A�禡�Oprp()
#���u���A��prp()�e�X�Ӫ��M����A����n�ݤ@��
require(rpart.plot) 
prp(cart.model,         # �ҫ�
    faclen=1,           # 0/1�e�{���ܼƥH�Y�g(���M�Ӽe)
    fallen.leaves=TRUE, # ����K�H�����覡�e�{
    shadow.col="gray",  
    extra=1)
#predic#
pred <- predict(cart.model, newdata=test)
table(real=test$is_promoted, predict=pred)
# �p��w���ǽT�v = �﨤�u���ƶq/�`�ƶq
confus.matrix <- table(real=test$is_promoted, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix) # �﨤�u���ƶq/�`�ƶq
#�ǽT�v�u��0.2691274 :�ܧC
