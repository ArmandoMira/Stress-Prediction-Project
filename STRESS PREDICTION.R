library(dplyr)
library(caTools)
library(gmodels)
library(class)
library(naniar)
library(ggvis)
library(tidyr)
library(readxl)
library(corrplot)
library(tidyverse)
library(naivebayes)
library(ggplot2)
library(psych)
library(ggplot2)
library(corrplot)
library(corrplot)
install.packages("Amelia")
library(Amelia)
stress<-read.csv("C:/Users/arman/Documents/Advanced Data Analytics (R)/stress_df_final.csv")
head(data)
summary(stress)
missmap(stress, main = "Missing values vs observed")

#Gender vs Condition
ggplot(stress, aes(x=gender, fill=Condition))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Gender Group') +
  scale_y_continuous(labels = scales::percent)
#ggplot(stress,aes(x=gender, fill=as.factor(Condition)))+
 # geom_bar(position='dodge')
table(data.frame(gender=stress$gender, Condition=stress$Condition))
chisq.test(stress$gender, stress$Condition)
table(stress$gender)

#Major vs Condition
ggplot(stress, aes(x=major, fill=as.factor(Condition)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Major') +
  scale_y_continuous(labels = scales::percent)
table(data.frame(major=stress$major, Condition=stress$Condition))
chisq.test(stress$major, stress$Condition)
length(unique(stress$major))

#Married vs Condition
ggplot(stress, aes(x=married, fill=as.factor(Condition)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Married') +
  scale_y_continuous(labels = scales::percent)
table(data.frame(married=stress$married, Condition=stress$Condition))
#ggplot(stress,aes(x=married, fill=as.factor(Condition)))+
 # geom_bar(position='dodge')
chisq.test(stress$married, stress$Condition)


#Age Group vs Condition
ggplot(stress, aes(x=Age_Groups, fill=as.factor(Condition)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Age Group') +
  scale_y_continuous(labels = scales::percent)
table(data.frame(Age_Groups=stress$Age_Groups, Condition=stress$Condition))
chisq.test(stress$Age_Groups, stress$Condition)

#Native Tongue English vs Condition
ggplot(stress, aes(x=engnat, fill=as.factor(Condition)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Engnat') +
  scale_y_continuous(labels = scales::percent)
table(data.frame(engnat=stress$engnat, Condition=stress$Condition))
chisq.test(stress$engnat, stress$Condition)


#Education vs Condition
ggplot(stress, aes(x=education, fill=as.factor(Condition)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Critical-Quarrelsome') +
  scale_y_continuous(labels = scales::percent)
table(data.frame(education=stress$education, Condition=stress$Condition))
chisq.test(stress$education, stress$Condition)

#Urban vs Condition
ggplot(stress, aes(x=urban, fill=as.factor(Condition)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Critical-Quarrelsome') +
  scale_y_continuous(labels = scales::percent)
table(data.frame(urban=stress$urban, Condition=stress$Condition))
chisq.test(stress$urban, stress$Condition)

#Family Size vs Condition
ggplot(stress, aes(x=familysize, fill=as.factor(Condition)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Critical-Quarrelsome') +
  scale_y_continuous(labels = scales::percent)
table(data.frame(familysize=stress$familysize, Condition=stress$Condition))
chisq.test(stress$familysize, stress$Condition)

#Getting upset(Q1) vs Condition
ggplot(stress, aes(x=Q1A, fill=as.factor(Condition)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Q1') +
  scale_y_continuous(labels = scales::percent)
table(data.frame(Q1A=stress$Q1A, Condition=stress$Condition))
chisq.test(stress$Q1A, stress$Condition)

#Critical-quarrelsome vs Condition
ggplot(stress, aes(x=Critical.quarrelsome, fill=as.factor(Condition)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Critical-Quarrelsome') +
  scale_y_continuous(labels = scales::percent)
table(data.frame(Critical.quarrelsome=stress$Critical.quarrelsome, Condition=stress$Condition))

#correlation matrix
res <- cor(stress[,c('education','urban','gender','engnat','Age_Groups_cn','married','familysize','Condition_cn')])
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
summary(stress)


chisq.test(stress$Critical.quarrelsome, stress$Condition)
chisq.test(stress$Q1A, stress$Condition)



ggplot(data = stress, aes(x=Age_Groups, y=Condition)) +
  geom_boxplot(fill="steelblue") +
  labs(title="Age_Groups vs Condition", x="Age_Groups", y="Condition")

ggplot(stress, aes(Condition), y=age, fill=Condition) + 
  geom_boxplot() + 
  labs(subtitle="age by condition")
lm.education <- lm(stress$education ~ stress$Condition)
summary(lm.education)
#--------------------------------------------------- ARMANDO
stress$major
length(unique(stress$major))
table(stress$major)
# business, language, engineering, nat res, music,hard sciences, hosp & management
# medicine, political science, No Degree, behavioral sciences, religion, animal health, arts, education,
# communication

colnames(stress)

#Open to new experiences-complex vs Condition
ggplot(stress, aes(x=Open.to.new.experiences.complex, fill=as.factor(Condition)))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Open-Complex') +
  scale_y_continuous(labels = scales::percent)
table(data.frame(open_complex=stress$Open.to.new.experiences.complex, Condition=stress$Condition))
chisq.test(stress$Open.to.new.experiences.complex, stress$Condition)
#--------------------------------------------------- ARMANDO


#----------------------Model fitting-------------------------------------
stress_df <- read.csv(file.choose(),header = TRUE, stringsAsFactors = FALSE)
head(stress_df)
#distinct value counts
table(stress_df$Condition_cn)
summary(stress_df)
dim(stress_df)
sum(is.na(stress_df))
typeof(stress_df)
str(stress_df)
drop = c("Condition","Age_Groups","age","major","Total_Count")
stress_new_df = stress_df[,!(names(stress_df) %in% drop)]
#stress_new_df$Condition_cn = as.numeric(stress_new_df$Condition_cn)
dim(stress_new_df)
str(stress_new_df)
ncol(stress_new_df)
names(stress_new_df)
stress_new_df <- lapply(stress_new_df, as.factor)
#stress_new_df <- lapply(stress_new_df, as.numeric)
#stress_new_df$Condition_cn =as.factor(stress_new_df$Condition_cn)
stress_new_df <- data.frame(stress_new_df)

#-----------------------------------------------------------------
#Random Forest
install.packages("randomForest")
install.packages("randomForestExplainer")
library(randomForest)
library(randomForestExplainer)
set.seed(122)
#rows1<-sample(nrow(stress_new_df))
#stress_rf<-stress_new_df[rows1,]

# split Train data and test data
#train1 <- stress_rf[1:31820,]
#test1 <- stress_rf[31821:39775,]

split <- sample.split(stress_new_df, SplitRatio = 0.8)
split

train_reg <- subset(stress_new_df, split == "TRUE")
test_reg <- subset(stress_new_df, split == "FALSE")

errorvalues <- vector()
for (i in 3:10){
  temprf <- randomForest(Condition_cn~.,data = train_reg,ntree = 1500,mtry = i)
  errorvalues[i] <- temprf$err.rate[nrow(temprf$err.rate),1]
}

#set.seed(2017)
#forest <- randomForest(Condition_cn~.,data = stress_new_df, localImp = TRUE)
#explain_forest(forest, interactions = TRUE, data = train_reg)

#The distribution of the mean minimal depth allows us to appreciate 
#the variable’s role in the random forest’s structure and prediction.
#The smaller the mean minimal depth, the more important the variable 
#is and the higher up the y-axis the variable will be.

min_depth_frame <- min_depth_distribution(temprf)
plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 31)
#plot_min_depth_distribution(min_depth_frame)

importance_frame <- measure_importance(temprf)
importance_frame

y_pred = predict(temprf, newdata = test_reg)
table(y_pred)
typeof(y_pred)
str(y_pred)
library(caret)
confusion_mtx = confusionMatrix(y_pred,test_reg$Condition_cn)
confusion_mtx
plot(errorvalues)
importance(temprf)
varImpPlot(temprf)
plot(temprf)
#------------------------------------------------------------
#logistic

set.seed(42)
install.packages("caTools")    # For Logistic regression
install.packages("ROCR")
install.packages("glmnet")
library(caTools)
library(ROCR) 
library(glmnet)
require(nnet)
split <- sample.split(stress_new_df, SplitRatio = 0.8)
split

train_reg <- subset(stress_new_df, split == "TRUE")
test_reg <- subset(stress_new_df, split == "FALSE")
multinom_model <- multinom(Condition_cn ~ ., data = train_reg)
summary(multinom_model)

# Predicting the values for train dataset
test_reg$ClassPredicted <- predict(multinom_model, newdata = test_reg, "class")
# Building classification table
tab <- table(test_reg$Condition_cn, test_reg$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#logistic regression curve
library(ggplot2)

#plot logistic regression curve
ggplot(test$reg$ClassPredicted, aes(x=hp, y=vs)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", 
              se=FALSE, method.args = list(family=binomial))
#confusion matrix graph
#any other which explains the model (error values, importance variable)
#-------------------------------------------------------------------------------

#Decision tree
install.packages("party")
library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)

split <- sample.split(stress_new_df, SplitRatio = 0.8)
train_reg <- subset(stress_new_df, split == "TRUE")
test_reg <- subset(stress_new_df, split == "FALSE")
str(train_reg)
model<- ctree(Condition_cn ~ ., train_reg)


predict_model<-predict(model, test_reg)

# creates a table to count how many are classified
# as native speakers and how many are not
m_at <- table(test_reg$Condition_cn, predict_model)
m_at
plot(m_at)

ac_Test <- sum(diag(m_at)) / sum(m_at)
print(paste('Accuracy for test is found to be', ac_Test))

#confusion matrix graph
#any other which explains the model (error values, importance variable)

#------------------------------------------------------------------------------


# table rows: models, columns: accuracy, precssion, f1, recall