
### Group Project Assignment - YouTube Channel

rm(list=ls())
setwd("/Users/lexyc/Desktop/692\ Marketing\ analytics/Assignment/group")

devtools::install_github("lchiffon/wordcloud2")

library(ggplot2)
library(stringr)
library(rpart)
library(textdata)
library(dplyr)
library(tidyr)
library(tidytext)
library(wordcloud) 
library(wordcloud2)
library(devtools)
library(caret)
library(MLmetrics)
library(ROCR)
library(randomForest)


yt <- read.csv("YoutubeChannelsTop5000.csv",encoding = "UTF-8")


## 1. Data Preparation

str(yt)
head(yt)

#Preprocessing the "Rank" variable
yt$Rank<-as.character(yt$Rank)# The class of "Rank" is already Character in my dataset. Might be not nessecery for you as well
yt$Rank<-gsub(",","",yt$Rank)# Remove the punctuation ","
yt$Rank<-str_sub(yt$Rank,1,-3)# Remove the letters "st", "nd", "rd","th

#Preprocessing the "Subscribers" and "Video Uploads" variables
yt$Subscribers<-gsub("--","",yt$Subscribers)# Remove the punctuation "--"
yt$Video.Uploads<-gsub("--","",yt$Video.Uploads)# Remove the punctuation "--"

#Convert the calss of variables, preparing for the model analysis
yt$Rank<-as.numeric(as.character(yt$Rank))
yt$Video.Uploads<-as.numeric(as.character(yt$Video.Uploads))
yt$Subscribers<-as.numeric(as.character(yt$Subscribers))
yt$Video.views<-as.numeric(as.character(yt$Video.views))
yt$Channel.name<-as.character(yt$Channel.name)# The class of "Channel name" is already Character in my dataset. Might be not nessecery for you as well

yt$Grade<-as.factor(yt$Grade)
str(yt)

#Checking the missing data
sapply(yt, function(x) sum(is.na(x)))# There are three variables exsiting missing data.The "Grade" and "Video Uploads" variables are missing data for the same "Channel", which means that there are total 6 rows of missing data in "Grade" and "Video Uploads" variables. Will removed these 6 rows since imputing those missing valus is ineffecient and non-sense. For the "Subscribers" variable, there is a relatively large missing value of 387 and will be processed. 


sapply(yt, function(x) sum(is.na(x)/nrow(yt)))# missing data of Video uploads only occupies 0.12% of the total data, whereas, missing data of Subscribers takes 7.74% of the total data. So, we will delete the missing data of Video Uploads directly and using a prediction model to fill the missing data of Subscribers.


# Impute the missing values of "Subscribers". Predict and fill in the missing subscriber data through a linear regression model built by non NA data.
missing_Sub <- lm(Subscribers ~ Rank + Video.Uploads + Video.views, data = yt[!is.na(yt$Subscribers),])
yt$Subscribers[is.na(yt$Subscribers)] <- predict.lm(missing_Sub, yt[is.na(yt$Subscribers),])

#delete the data with low missing rate
yt<-yt[complete.cases(yt),]

sapply(yt, function(x) sum(is.na(x))) #Every missing data is clean now

# Text mining for "Channel name" variables to categorize channel types

# Combine all 4994 Channel names into one text.
yt$Channel.name<-sapply(yt$Channel.name, tolower)#cases do not matter
text<-sapply(list(yt$Channel.name),function(x) paste(x,collapse=" "))
str(text)

text=as.data.frame(text)
str(text)

text$text=as.character(text$text)
str(text)

#Separate word individually, combine the same word, and sort frequency
text.word=text%>%unnest_tokens(word,text)%>%group_by(word)%>%summarise(count=n())%>%arrange(-count)
head(text.word,10)

text.word

head(stop_words)

#remove from the list the stop wards and words whose length smaller than 2 

text.word=text.word%>%anti_join(stop_words)%>%filter(nchar(word) > 1)
head(text.word, 20)


#wordcloud creation via wordcloud2
text.word%>%wordcloud2::wordcloud2(size = .3,color="#00AA90",backgroundColor = "white",minRotation = -pi/6, maxRotation = -pi/6, shape="sqaure",rotateRatio = 1)


#======================================================



# create dummy for channel names include the hot word

yt$kid=grepl("kid",fixed = FALSE,yt$Channel.name)
yt$kid<-as.integer(yt$kid)
sum(yt$kid)

yt$music=grepl("music",fixed = FALSE,yt$Channel.name)
yt$music<-as.integer(yt$music)
sum(yt$music)

yt$tv=grepl("tv",fixed = FALSE,yt$Channel.name)
yt$tv<-as.integer(yt$tv)
sum(yt$tv)

yt$toy=grepl("toy",fixed = FALSE,yt$Channel.name)
yt$toy<-as.integer(yt$toy)
sum(yt$toy)

yt$official=grepl("official",fixed = FALSE,yt$Channel.name)
yt$official<-as.integer(yt$official)
sum(yt$official)

yt$news=grepl("news",fixed = FALSE,yt$Channel.name)
yt$news<-as.integer(yt$news)
sum(yt$news)

yt$movie=grepl("movie",fixed = FALSE,yt$Channel.name)
yt$movie<-as.integer(yt$movie)
sum(yt$movie)

yt$channel=grepl("channel",fixed = FALSE,yt$Channel.name)
yt$channel<-as.integer(yt$channel)
sum(yt$channel)

yt$entertainment=grepl("entertainment",fixed = FALSE,yt$Channel.name)
yt$entertainment<-as.integer(yt$entertainment)
sum(yt$entertainment)


yt$india=grepl("india",fixed = FALSE,yt$Channel.name)
yt$india<-as.integer(yt$india)
sum(yt$india)


## 2. Part 1: Regression Model on the factors that affect the ranking 

#Considering to add other related variables, such as "views per vedio", in this part

head(yt)

yt$Views.per.video<-as.numeric(yt$Video.views/yt$Video.Uploads)# add views per video as a varible


pairs(~Rank+Video.Uploads+Subscribers+Video.views+Views.per.video,data=yt,main="Plots")

####for1=as.formula(paste(names(yt)[1],"~",paste(names(yt)[4:19],collapse="+")))#application of as formula

#linear regression with origial three variables.
fit<-lm(Rank~Video.Uploads+Video.views+Subscribers,data=yt)
summary(fit)
#linear regression with "views per video" instead of "video views", the R sqaure is smaller than previous model "fit", so we will not use views per video.
fit_v.p.v<-lm(Rank~Video.Uploads+Subscribers+Views.per.video,data=yt)
summary(fit_v.p.v)

##linear regression with top 10 key word as dummy and three numeric variables
fit0<-lm(Rank~tv+kid+music+toy+official+news+channel+movie+entertainment+india+Video.Uploads+Video.views+Subscribers,data=yt)
summary(fit0)# tv, kid, toy and three numeric variables are significant to the model.

#remove the non-significant variables
fit1<-lm(Rank~tv+kid+toy+Video.Uploads+Subscribers+Video.views,data=yt)
summary(fit1) 

yt$GradeRange<-as.factor(str_sub(yt$Grade,1,1))#Change levels of Grade into two range, A an B because logistic regression model is binomial

# Change dummy to factors
cols <- c(names(yt[7:16]))
yt[cols] <- lapply(yt[cols], factor)
str(yt)

##logistic regression with origial three variables.
logfit<-glm(GradeRange~Video.Uploads+Video.views+Subscribers,data=yt,family=binomial)
summary(logfit)#Compare to linear regression model "fit", "Video Uploads" is not longer significant anymore. This is because in the linear regression model, the number of video uploads is sensitive to the specific ranking number. However, as Grade range is a wide range of ranking, the number of video upload is not significant enought to impact the grade range.

##linear regression with top 10 key word as dummy and three numeric variables
logfit0<-glm(GradeRange~tv+kid+music+toy+official+news+channel+movie+entertainment+india+Video.Uploads+Video.views+Subscribers,data=yt,family=binomial)
summary(logfit0)

#remove non-significant variables
logfit1<-glm(GradeRange~tv+kid+toy+movie+Video.views+Subscribers,data=yt,family=binomial)
summary(logfit1)#movie becomes significant in the regression model.


## 3. Part 2: Prediction model on the letter grades 


set.seed(111)
rv=sample(1:nrow(yt),round(nrow(yt)*.7,0))
train.yt=yt[rv,]
test.yt=yt[-rv,]

#random guess

p=sum(train.yt$GradeRange=="A")/nrow(train.yt)#a naive guess Fraud probability based on percentage
p
rg.pred <- sample(c("A", "B"), nrow(train.yt), prob=c(1-p,p),replace = TRUE)#random guess prediction
str(rg.pred)
rg.pred =as.factor(rg.pred)
summary(rg.pred)
mean(rg.pred==yt$GradeRange)# random guess around

logfit1<-glm(GradeRange~tv+kid+toy+movie+Video.views+Subscribers,data=train.yt,family=binomial)
summary(logfit1)

predprob<-predict.glm(logfit1, test.yt, type = "response")

threshold=0.5
logit.pred <- as.factor(ifelse(predprob>=threshold,"B","A"))
mean(logit.pred ==test.yt$GradeRange)
table(logit.pred,test.yt$GradeRange)# after testing, threshold=0.5 gives the highest accuracy.

confusionMatrix(logit.pred,test.yt$GradeRange, positive = "A")

F1_Score(y_pred = logit.pred, y_true = test.yt$GradeRange, positive = "A")

pred.roc <-ROCR::prediction(predprob, test.yt$GradeRange)
perf.roc <- performance(pred.roc, "tpr", "fpr")
plot(perf.roc,col="black")
abline(a=0, b=1, col="#8AB63F")

logit.auc<- performance(pred.roc,  c("auc"))
logit.auc@y.values[1]

#Random Forest Model

vid=which( colnames(yt)=="GradeRange" )
vid
names(train.yt)
train.yt=as.data.frame(train.yt)
rf.res=tuneRF(train.yt[,4:17], train.yt[,vid], stepFactor=1.5)
mtry_opt=rf.res[,"mtry"][which.min(rf.res[,"OOBError"])]
mtry_opt

rf.fit <- randomForest(GradeRange~tv+kid+music+toy+official+news+channel+movie+entertainment+india+Subscribers+Video.views+Video.Uploads, data=train.yt, ntree=201, mtry=mtry_opt, importance=TRUE)


print(rf.fit)
plot(rf.fit)

#Summarize Importance
importance(rf.fit) 

#Plot Variable Importance
varImpPlot(rf.fit)
varImpPlot(rf.fit, sort=T, n.var=10, main="Top 10 Variable Importance")

#Prediction and Confusion Matrix
rf.pred <- predict(rf.fit,newdata=test.yt[,4:17],type="class")
confusionMatrix(rf.pred, test.yt$GradeRange)
mean(rf.pred==test.yt$GradeRange)

#Plot ROC
rf.predProb <- predict(rf.fit, newdata=test.yt, type = "prob")
predRF.roc <- ROCR::prediction(rf.predProb[,2], test.yt$GradeRange) 
perfRF.roc <- performance(predRF.roc, "tpr", "fpr") 

plot(perfRF.roc,col="black")
abline(a=0, b=1, col="#8AB63F")

#Compute AUC
rf.auc<- performance(predRF.roc, c("auc"))
rf.auc@y.values[1]

#Comparison accuracy
cat("Comparison of prediction accuracy", "\n",
    "Logistic",mean(logit.pred ==test.yt$GradeRange),"\n",
    "Random Forests", mean(rf.pred == test.yt$GradeRange))

#Comparing AUC
cat("Comparison of AUC", "\n",
    "Logistic",unlist(logit.auc@y.values[1]),"\n",
    "Random Forests", unlist(rf.auc@y.values[1]))
###Random Forest model is better than logistic 
