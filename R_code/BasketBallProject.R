setwd("G:/FDA PROJECT/march-madness-analytics-2020/MPlayByPlay_Stage2")  #Location Of The Data
#data<-read.csv("MEvents2015.csv",stringsAsFactors=FALSE)                #data of the 2015 th data reading in to data attribute
MEvents2015<-read.csv("MEvents2015.csv",stringsAsFactors=FALSE)                #data of the 2015 th data reading in to data attribute#MEvents2016<-read.csv("MEvents2016.csv",stringsAsFactors=FALSE)                #data of the 2016 th data reading in to data attribute
MEvents2016<-read.csv("MEvents2016.csv",stringsAsFactors=FALSE)                #data of the 2016 th data reading in to data attribute
MEvents2017<-read.csv("MEvents2017.csv",stringsAsFactors=FALSE)                #data of the 2017 th data reading in to data attribute
MEvents2018<-read.csv("MEvents2018.csv",stringsAsFactors=FALSE)                #data of the 2018 th data reading in to data attribute
MEvents2019<-read.csv("MEvents2019.csv",stringsAsFactors=FALSE)                #data of the 2019 th data reading in to data attribute this data is not enable to merge due to overloaded

data=rbind(MEvents2015,MEvents2016) # to merge two seaon details
data=rbind(data,MEvents2017)    # to merge two seaon details
data=rbind(data,MEvents2018)    # to merge two seaon details
data=rbind(data,MEvents2019)    # to merge two seaon details

View(data)
dim(data)#to view the 'data' in the data attribute


#############################Applying ML Algorithim Before preprocessing ###########################
##########there is 11681180 rows so from those all data only one season data is taken for applying the ML Algorithim
##################################################Multiple Regression#################
#MEvents2015 is taken for the prediction
View(MEvents2015)
####################for WFinalScore as classLabel
#splitting data to 60 : 40 ratio
sample = sample.split(MEvents2015,SplitRatio = 0.60) # splits the data in the ratio of 60% for train and remaing 40% for test. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
#above statement error has been occure due to high size so just first 1 lakh data is taken 
data_sample_2015=MEvents2015[1:50000,]
sample = sample.split(data_sample_2015,SplitRatio = 0.60) # splits the data in the ratio of 60% for train and remaing 40% for test. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(data_sample_2015,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(data_final, sample==FALSE)
model_before=lm(WFinalScore~+Season+DayNum+WTeamID+LTeamID+LFinalScore,train)
summary(model_before)                 #Multiple R-squared:  0.1701,	Adjusted R-squared:  0.17
################################here when we reducing the data rhe accurecy also decreasing so less data of orginal will give high accuracy
######But there is problem like these predictions because where there is 1 set of team have almost2k to 3k rows so for the same set of team will get different priditions
##for one row it is win and for aother row it will loose...*by this calculation may not be predictable
#for after preprocessing the data R2 is better than the present one among 20%  
y_pred = predict(model_before, data = test)
solution1<-data.frame(WTeamID=test$WTeamID,LTeamID=test$LTeamID,WFinalScore=test$WFinalScore)
View(solution1)
#Finding accuracy
actuals_preds <- data.frame(cbind(actuals=test$WFinalScore, predicted=y_pred))    #this is the data of predicted and actual data
View(actuals_preds)
#####################for LFinalScore
#splitting data to 60 : 40 ratio
model_before_L=lm(LFinalScore~+Season+DayNum+WTeamID+LTeamID+WFinalScore,train)
summary(model_before_L)                 #Multiple R-squared:  0.1868,	Adjusted R-squared:  0.186
#for after preprocessing the data R2 is better than the present one among 25%  
y_pred = predict(model_before_L, data = test)
solution1<-data.frame(WTeamID=test$WTeamID,LTeamID=test$LTeamID,WFinalScore=test$WFinalScore)
View(solution1)
#Finding accuracy
actuals_preds_L <- data.frame(cbind(actuals=test$WFinalScore, predicted=y_pred))    #this is the data of predicted and actual data

#####for accuracy
dim(actuals_preds)          #to know the size of the rows
##### winning or lost is noteing in the format of 1's and 0's in the Actual_won label
actual_won_before=actuals_preds$actuals
for(i in c(1:58824))
{
  if(actuals_preds[i,1] >=actuals_preds_L[i,1])
  {
    actual_won_before[i]=1
  }
  else
  {
    actual_won_before[i]=0
  }
}
View(actual_won_before)
class(actuals_preds[,1])
#####For Lost
dim(actuals_preds_LFinal)          #to know the size of the rows
##### winning or lost is noteing in the format of 1's and 0's in the Actual_won label
predicted_won_before=actuals_preds$predicted
for(i in c(1:58824))
{
  if(actuals_preds[i,2] >actuals_preds_L[i,2])
  {
    predicted_won_before[i]=1
  }
  else
  {
    predicted_won_before[i]=0
  }
}
View(predicted_won_before)

#appling confusion matrix for the actualwons and predicted ones
tb=table(actual_won_before,predicted_won_before)
View(tb)

####MAPE###
######################################Decision tree##################
data_sample_2015=MEvents2015[1:50000,]
sample = sample.split(data_sample_2015,SplitRatio = 0.60) # splits the data in the ratio of 80% for train and remaing 20% for test. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(data_final,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(data_final, sample==FALSE)
View(train)
View(test)

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
#destree = rpart(formula = WFinalScore ~ Season+DayNum+WTeamID+LTeamID+ElapsedSeconds+EventTeamID+EventPlayerID,data = train)

#penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)

# building the classification tree with rpart
#1
tree <- rpart(WFinalScore~.,data=train)                         #this graph is giving so many branches and it is good but 
summary(tree)  #Here MSE(Mean Square Error) should be low then more accuracy MSE=51%          #in the summary of tree it is mainly depended up on 2 to 4 colums 
###so accuracy is 49%



#in the data attribute we have all the data of the seasons
#####################################Common preprocess for across all data and not usefull data###############################
#Pre-Processing Tecniques for the all (2015,2016,2017,2018,2019) data 

#removing un necesary rows from the data
#the down removing coloums is having all 200lakhs data is 0 only and we can be filled it because every thing is 0 so removing those colums
data=data[,-c(15,16,17)]      #removing the un usefull data for our predictions    Colums:X,Y,Area

#####there is so much of data columns so removing the null values and we cannot be predicted the Event Subtypes ######
data=data[-(which(data$EventSubType=="")),]    #Removing null values from event subtype or empty value in that column rows are removed,there is no use with that rows

#there are some missing values in the EventPlayerId from data table
#####summary of the data
summary(data)
#for Wcurrent score and LCurrentScore
# chi-square test before reducing
tb2<-table(data$WCurrentScore,data$LCurrentScore)
chisq.test(tb2)
#in the Wcurrentscore and Lcurrent score there is no use of both 0's at same row because no action was taken place nothing is predicted from those 0's
#so removing those 0's
data=data[-(which(data$WCurrentScore==0 & data$LCurrentScore==0)),]
# chi-square test After  reducing
tb2<-table(data$WCurrentScore,data$LCurrentScore)
chisq.test(tb2)
#using Mice Package to predict the missing valuesby comparing more than 1  colums
#install.packages("mice")
library(mice)
x=mice(data1[,c(11:14)],m=1,method = 'pmm',seed=500)
#to know the pattern of Missing data
md.pattern(data)
#Graph of the missing data pattern to visualize easily
#install.packages("VIM")
library(VIM)
aggr_plot <- aggr(MEvents2015, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#after analyising the data there are no missing rows in the data



####################################################################################
####################################################################################
####################################################################################
####################################################################################
#this pre processing process has been done initially but with these preprocessing the accuracy or prediction is not good so 
#so finnaly i have did in another way of pre processing finnaly
#By this down pre processing the data also more upto 200 lakhs of data is reduced to 30 to 50 lakhs of data
#removed rows and colums also useless data only 
#but prediction of this is not good so took the another method of approch for predictions
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
#To remove un wanted data in the event sub type 
table(data$EventSubType)   # here after observing there is no missed data so no need to do any pre processing tecqunic
#to remove un useful data from event type
table(data$EventType)  #by observing the output there we need only made1,made2 and made3 where points will come 
#remove remaing data in it
data=data[-(which(data$EventType=="foul")),]             #to remove the foul event type data
data=data[-(which(data$EventType=="miss1")),]             #to remove the miss1 event type data
data=data[-(which(data$EventType=="miss2")),]             #to remove the miss2 event type data
data=data[-(which(data$EventType=="miss3")),]             #to remove the miss3 event type data
data=data[-(which(data$EventType=="reb")),]             #to remove the reb event type data
data=data[-(which(data$EventType=="sub")),]             #to remove the sub event type data
data=data[-(which(data$EventType=="timeout")),]             #to remove the timeout event type data
data=data[-(which(data$EventType=="turnover")),]             #to remove the turnover event type data
factor(data$EventType)  #to know levels of that colum
#changing the string data into numeric data by recode the data into 1,2,3 as made1=1point,made2=2poins and made3=3points 
#recode function is not applicable for the string types
data$EventType[data$EventType=="made1"]<-1
#data$EventType[is.na(data$EventType)] <-"made1" this method is used to replace the na values in data frame
#To find the"NA" values in every colum of data file
colSums(is.na(data))
#After exxecuting above command we found there is No NA values but there is missing values 
install.packages("reshape2")      #installing reshape2 package
library('reshape2')
teams <- melt(data, id.vars = c("DayNum", "WTeamID"))         #As per the dayNum how many teams played in the whole day will be displayed at first and next day so...on
View(teams)
#to remove the missing colums from  the data.Because Event player id is unable to find and the data is more than 25 crores just in 1 file and total years should be calculated is 5 years
#For column data$EventPlayerId
data=data[-(which(data$EventPlayerID==0)),]
#Recode the data$season in to smaller number 
#To know how many diff types of values present in the Season Column
table(data$Season)
#as it has only 5 types of season by indicationg 2015,2016,2017,2018,2019 into 1,2,3,4,5 correspondingly
install.packages("tidyverse")
library(tidyverse)
#install.packages("dplyr")
library(car)
data<-recode(data$Season,"2015=1;2016=2;2017=2;2018=3") #recoding the season as there are big numbers so...
distinct(data, WTeamID,LTeamID, .keep_all= TRUE)
table(status)
#checking outliers
library(outliers)



####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################


########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
#####APPLYING PREPROCESSING PROCESS OF ANOTHER WAY##########
#here we are going to do the preprocess of removing therows or colums which are not implementing the class labels are removing
#by observing the data there are so much of not usefull data 


####In the view of the class label the main impacting colums are Season,Day,WTEamID,LTeamId,and including classlabels WfinalScore and LFinalScore
#here current score is increasing by the depending on event id but this is not usefull to predict the Wfinal score or Lfinal score
#here Wfinalscore will be the heightst value in the max(current score of the particular team so untill current score reaches to it...it will move on)
#so those are the duplicate rows of the WteamID and LTeamID
#This duplcate rows are not useful for the predictions
library(tidyverse)
data2=arrange(data,desc(WCurrentScore),desc(LCurrentScore))  #arranging the current scores in decreasing order
data_final=distinct(data2,WTeamID,LTeamID,.keep_all=TRUE) #by removing duplicate rows of the class label almost the data reduced from 170lakhs to 10,000 
##here accuracy will be there because there wont be duplicated set of team 
#################*****************  here almost 150 lakhs of data is removed    ***************************##################
View(data_final)
#to see how many missing values are present in the EventPlayerId
table(data_final$EventPlayerID)

##here no need of event type and eventsub type so removing them for predict the data
data_final=data_final[,-c(13,14)]
View(data_final)

#removing event id also because no use of it any more to predict
table(data_final$Season)

#for this almost WCurrentscore And Wfinal score is same so no need of WCurrentscore And Lcurrent score
data_final=data_final[,-c(1,8,9)]



################################################################################################################################


#######################################################APPLYING MACHINE LEARNING ALGORITHMS #####################################
                        ############################# Multiple REGRESSION ########################################
                                ############CLASS LABELS ARE 2:1)WFinalScore,2)LfinalScore########

############# class label is WFinalScore
#########
###### for prediction 1

#spliting the data into 80 % for training the data and 20% for testing the data

library("caTools")
sample = sample.split(data_final,SplitRatio = 0.80) # splits the data in the ratio of 80% for train and remaing 20% for test. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(data_final,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(data_final, sample==FALSE)
View(train)
View(test)

##########************Machine Learning Algorithm  Multiple REGRESSION***********############
regression = lm(WFinalScore ~ Season+DayNum+WTeamID+LTeamID+LFinalScore+ElapsedSeconds+EventTeamID+EventPlayerID,data = train)
summary(regression)                       # Multiple R-squared:  0.3687,	Adjusted R-squared:  0.3681
sigma(model)/mean(data_final$WFinalScore) #here we got just 0.1129 so 11% is the error rate
# Predicting the Test set results
y_pred = predict(regression, data = test)
solution1<-data.frame(WTeamID=test$WTeamID,LTeamID=test$LTeamID,WFinalScore=test$WFinalScore)
View(solution1)
#Finding accuracy
actuals_preds <- data.frame(cbind(actuals=test$WFinalScore, predicted=y_pred))    #this is the data of predicted and actual data
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)
View(actuals_preds)
#actuals predicted
#2      144  85.42871
#3      131  97.94861
#5      127 106.82008
#6      126  79.06679
#7      125  76.29101
#11     121  81.45334
mean(actuals_preds$actuals)  #mean is 84.67
mean(actuals_preds$predicted)  #mean is 81.7103
#confusionMatrix(actuals_preds$actuals, actuals_preds$predicted)




y=which((test$WFinalScore!=solution1$WFinalScore) && (test$WTeamID=solution1$WTeamID))
plot(solution1)
 #for removing another column
model = lm(WFinalScore ~ Season+DayNum+WTeamID+LTeamID+LFinalScore+EventTeamID+EventPlayerID,data = train)
summary(model)
#to find the error ratio 
#RSE=9.232
#The error rate can be estimated by dividing the RSE(Residual Standard Error) by the mean outcome variable:
sigma(model)/mean(data_final$WFinalScore) #here we got just 0.1126 so 11% is the error rate

#removing another column
mode2 = lm(WFinalScore ~ Season+DayNum+WTeamID+LTeamID+LFinalScore+EventPlayerID,data = train)
summary(model)               #Multiple R-squared:  0.3611,	Adjusted R-squared:  0.3606
#to find the error ratio 
#RSE=9.232
#The error rate can be estimated by dividing the RSE(Residual Standard Error) by the mean outcome variable:
sigma(model)/mean(data_final$WFinalScore) #here we got just 0.1126 so 11% is the error rate
#here error rate is same so not impacting the column EventTeamId

model = lm(WFinalScore ~ Season+DayNum+WTeamID+LTeamID+LFinalScore,data = train)
summary(model)           #Multiple R-squared:  0.3609,	Adjusted R-squared:  0.3605
#to find the error ratio 
#RSE=9.232
#The error rate can be estimated by dividing the RSE(Residual Standard Error) by the mean outcome variable:
sigma(model)/mean(data_final$WFinalScore) #here we got just 0.1126 so 11.2% is the error rate

model = lm(WFinalScore ~ Season+DayNum+WTeamID+LTeamID+LFinalScore+ElapsedSeconds,data = train)
summary(model)          #Multiple R-squared:  0.3685,	Adjusted R-squared:  0.3681
#to find the error ratio 
#RSE=9.232
#The error rate can be estimated by dividing the RSE(Residual Standard Error) by the mean outcome variable:
sigma(model)/mean(data_final$WFinalScore) #here we got just 0.1116 so 11.1% is the error rate
y_pred = predict(model, data = test)


#Finding accuracy
actuals_preds <- data.frame(cbind(actuals=test$WFinalScore, predicted=y_pred))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)
View(actuals_preds)
mean(actuals_preds$actuals)  #mean is 84.67
mean(actuals_preds$predicted)  #mean is 81.7103
#mean(min(actuals_preds$actuals, actuals_preds$predicted)/max(actuals_preds$actuals, actuals_preds$predicted))
#actuals predicted
#2      144  85.42871
#3      131  97.94861
#5      127 106.82008
#6      126  79.06679
#7      125  76.29101
#11     121  81.45334
accuracy(actuals_preds)


#################################These are the graphs for testing purpose ###########################

library("ggplot2")


bargraph.actual<-ggplot(actuals_preds,aes(x=actuals))    #frequency  graph for actual predicted data
bargraph+geom_freqpoly()

bargraph.prediction<-ggplot(actuals_preds,aes(predicted))   #frequency graph for predicted values of the class label
bargraph.prediction+geom_freqpoly()


bargraph.actual<-ggplot(actuals_preds,aes(x=actuals))    #bar graph for actual predicted data
bargraph.actual+geom_bar()

bargraph.prediction<-ggplot(actuals_preds,aes(predicted))   #bar graph for predicted values of the class label
bargraph.prediction+geom_bar()

bargraph.actual+geom_density(fill="#FFBCDE")
bargraph.prediction+geom_density(fill="#FFBCDE")

jittergraph.actual<-ggplot(actuals_preds,aes(x=actuals,y=predicted))    #bar graph for actual predicted data
jittergraph.actual+geom_jitter()

#binding_data=test
#binding_data=cbind(binding_data,actuals_preds$predicted)
############################################################################################################################

##finally after observing both graphs are almost 70 % same 
###############################################################################################################################################
###############################################################################################################################################
################################# class label is LFinalScore  #############################
#########
############# for prediction 1
#to separate the WTeamID,LTeamID with their score by unique to calculate the which team wins more times with another team to find the probability
###################################################here test and train data same where we splited at the time of class label=WFinalScore####
#Machine Learning Algorithm  Multiple REGRESSION
#1
regression = lm(LFinalScore ~ Season+DayNum+WTeamID+LTeamID+ElapsedSeconds+EventTeamID+EventPlayerID,data = train)
summary(regression)                  #  Multiple R-squared:  0.1991,	Adjusted R-squared:  0.1985
sigma(model)/mean(data_final$WFinalScore) #here we got just 0.1129 so 11% is the error rate
##############################   2    ###########################
regression = lm(LFinalScore ~.,data = train)
summary(regression)                  #  Multiple R-squared:  0.4646,	Adjusted R-squared:  0.4642             ###this is the best R-Square value among all so this is the final 
sigma(model)/mean(data_final$WFinalScore) #here we got just 0.1129 so 11% is the error rate
# Predicting the Test set results
y_pred = predict(regression, data = test)
###########
tb=table(actuals_preds$actuals,actuals_preds$predicted)
View(tb)
class(actuals_preds$predicted)
##########
solution1<-data.frame(WTeamID=test$WTeamID,LTeamID=test$LTeamID,WFinalScore=test$WFinalScore)
View(solution1)
#Finding accuracy
actuals_preds_LFinal <- data.frame(cbind(actuals=test$LFinalScore, predicted=y_pred))
correlation_accuracy_Lfinal <- cor(actuals_preds_LFinal)
View(correlation_accuracy_Lfinal)
#accuracy is 
#         actuals      predicted
#actuals	1.00000000	0.03034069
#predicted	0.03034069	1.00000000
#almost 99% is showing

head(actuals_preds_LFinal)
View(actuals_preds_LFinal)
#actuals predicted
#2      144  85.42871
#3      131  97.94861
#5      127 106.82008
#6      126  79.06679
#7      125  76.29101
#11     121  81.45334
mean(actuals_preds$actuals)  #mean is 84.67
mean(actuals_preds$predicted)  #mean is 81.7103
#confusionMatrix(actuals_preds$actuals, actuals_preds$predicted)
########################################################################
#3
regression = lm(LFinalScore ~.-LTeamID,data = train)
summary(regression)                  #  Multiple R-squared:  0.4645,	Adjusted R-squared:  0.4641
sigma(model)/mean(data_final$WFinalScore) #here we got just 0.1129 so 11% is the error rate
y_pred = predict(regression, data = test)
actuals_preds_LFinal <- data.frame(cbind(actuals=test$LFinalScore, predicted=y_pred))

#4
regression = lm(LFinalScore ~ Season+DayNum+WTeamID+WFinalScore+ElapsedSeconds,data = train)
summary(regression)                  #  Multiple R-squared:  0.464,	Adjusted R-squared:  0.4638
sigma(model)/mean(data_final$WFinalScore) #here we got just 0.1129 so 11% is the error rate
y_pred = predict(regression, data = test)
actuals_preds_LFinal <- data.frame(cbind(actuals=test$LFinalScore, predicted=y_pred))

#5
regression = lm(LFinalScore ~ Season+DayNum+WTeamID+LTeamID+WFinalScore+ElapsedSeconds+EventTeamID,data = train)
summary(regression)                  #  Multiple R-squared:  0.4642,	Adjusted R-squared:  0.4639
sigma(model)/mean(data_final$WFinalScore) #here we got just 0.1129 so 11% is the error rate
y_pred = predict(regression, data = test)
actuals_preds_LFinal <- data.frame(cbind(actuals=test$LFinalScore, predicted=y_pred))


###########################*********************imp calculations***********************############################
###########Finding of WFinalScore and LFinal Score of Both actual and prediced values whether they won or lost
dim(actuals_preds)          #to know the size of the rows
##### winning or lost is noteing in the format of 1's and 0's in the Actual_won label
actual_won=actuals_preds$actuals
for(i in c(1:9442))
{
  if(actuals_preds[i,1] >=actuals_preds_LFinal[i,1])
  {
    actual_won[i]=1
  }
  else
  {
    actual_won[i]=0
  }
}
View(actual_won)
class(actuals_preds[,1])
#####For Lost
dim(actuals_preds_LFinal)          #to know the size of the rows
##### winning or lost is noteing in the format of 1's and 0's in the Actual_won label
predicted_won=actuals_preds$predicted
for(i in c(1:9442))
{
  if(actuals_preds[i,2] >=actuals_preds_LFinal[i,2])
  {
    predicted_won[i]=1
  }
  else
  {
    predicted_won[i]=0
  }
}
View(predicted_won)

#appling confusion matrix for the actualwons and predicted ones
tb=table(actual_won,predicted_won)
View(tb)
# actual_won  Predicted_won freq 
#1	   1	          0	       400
#2	   1	          1      	9042
####to know percentage of accuracy from above data 9042 values are predicted correctly and 400 is not correct 
###to know percentage of exactly predicted is (9042*100)/9442 = 95.76361 correctly predicted 

#############################################Applying the regression is predicted 95.7% exactly###############################

##############################################################################################################################
                        #############################  DESCISION TREE  ########################################
                                ############CLASS LABELS ARE 2:1)WFinalScore,2)LfinalScore########

############# class label is WFinalScore
#########
###### for prediction 2
######################Using Decission tree

library("tidyverse")
data2=arrange(data,desc(WCurrentScore),desc(LCurrentScore))
data_final=distinct(data2,WTeamID,LTeamID,.keep_all=TRUE) #by removing duplicate rows of the class label almost the data from 200lakhs to 10,000 data is reduced
View(data_final)
#to see how many missing values are present in the EventPlayerId
table(data_final$EventPlayerID)

##here no need of event type and eventsub type so removing them for predict the data
data_final=data_final[,-c(13,14)]
View(data_final)

#removing event id also because no use of it any more to predict
table(data_final$Season)

#for this almost WCurrentscore And Wfinal score is same so no need of WCurrentscore And Lcurrent score
data_final=data_final[,-c(1,8,9)]

#spliting the data into 80 % for training the data and 20% for testing the data
library("caTools")
sample = sample.split(data_final,SplitRatio = 0.80) # splits the data in the ratio of 80% for train and remaing 20% for test. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(data_final,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(data_final, sample==FALSE)
View(train)
View(test)

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
#destree = rpart(formula = WFinalScore ~ Season+DayNum+WTeamID+LTeamID+ElapsedSeconds+EventTeamID+EventPlayerID,data = train)

#penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)

# building the classification tree with rpart
#1
tree <- rpart(WFinalScore~.,data=train)                         #this graph is giving so many branches and it is good but 
summary(tree)                                                   #in the summary of tree it is mainly depended up on 2 to 4 colums 
# Visualize the decision tree with rpart.plot                   #only so by removing those column names from prediction applying the process again in the below
rpart.plot(tree, nn=TRUE)
#Testing the model
pred <- predict(object=tree,test)
View(pred)
actuals_preds <- data.frame(cbind(actuals=test$WFinalScore, predicted=pred))     #storing the values of acurate and predicted of WfinalTeam
View(actuals_preds)
predicted_data.1_W=cbind(test$WFinalScore,pred)
View(predicted_data.1)      #by observing the data it is almost 50 to 60 % accuratly predicted


#2
#After the removing un impacted data
tree <- rpart(WFinalScore~ WTeamID+LTeamID+ElapsedSeconds+LFinalScore,data=train)    #creating model of data of desicion tree ML algorithm
summary(tree)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)
#Testing the model
pred <- predict(object=tree,test)
View(pred)
predicted_data.2_W=cbind(test$WFinalScore,pred)
View(predicted_data.2)      #by observing the data it is almost 50 to 60 % accuratly predicted



#3
#here less number of branches are comming and less columns are impacting almost 1 or 2 columns only after seeing summary
tree <- rpart(WFinalScore~ WTeamID+LTeamID+ElapsedSeconds+EventTeamID+EventPlayerID,data=train)
summary(tree)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)
#Testing the model
pred <- predict(object=tree,test)
View(pred)
predicted_data.3_W=cbind(test$WFinalScore,pred)
View(predicted_data.3)      #by observing the data it is almost 50 to 60 % accuratly predicted

#confision matrix
t <- table(test$WFinalScore,pred)
View(t)
#t <- table(test$WFinalScore,pred) 
# confusionMatrix(t)
library(caret)
#Confusion Matrix and Statistics
# confusionMatrix(   test,   t,   positive = NULL,   dnn = c("Prediction", "Reference"),   prevalence = NULL,   mode = "sens_spec",... )
#result=predict(regression,newdata=test,type="response")
 ########################################



############# class label is LFinalScore
#########
###### for prediction 2
######################Using Decission tree
#install.packages("rpart.plot")
#library(rpart)
#library(rpart.plot)
# building the classification tree with rpart

#1
tree <- rpart(LFinalScore~.,data=train)                         #this graph is giving so many branches and it is good but 
summary(tree)                                                   #in the summary of tree it is mainly depended up on 2 to 4 colums 
# Visualize the decision tree with rpart.plot                   #only so by removing those column names from prediction applying the process again in the below
rpart.plot(tree, nn=TRUE)
#Testing the model
pred <- predict(object=tree,test)
View(pred)
##############
actuals_preds_LFinal <- data.frame(cbind(actuals=test$LFinalScore, predicted=pred))     #storing the values of acurate and predicted of WfinalTeam
View(actuals_preds_LFinal)    
#above and below data is equal 
#o we can go with less number of colums used to predict and those colums only main impacting the data
#2
#After the removing un impacted data
tree <- rpart(WFinalScore~ WTeamID+LTeamID+ElapsedSeconds+LFinalScore,data=train)    #creating model of data of desicion tree ML algorithm
summary(tree)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)
#Testing the model
pred <- predict(object=tree,test)
View(pred)
predicted_data_2_L=cbind(test$LFinalScore,pred)
View(predicted_data_2_L)      #by observing the data it is almost 50 to 60 % accuratly predicted
#3
#here less number of branches are comming and less columns are impacting almost 1 or 2 columns only after seeing summary
tree <- rpart(WFinalScore~ WTeamID+LTeamID+ElapsedSeconds+EventTeamID+EventPlayerID,data=train)
summary(tree)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)
#Testing the model
pred <- predict(object=tree,test)
View(pred)
predicted_data.3_L=cbind(test$LFinalScore,pred)
View(predicted_data.3_L)      #by observing the data it is almost 50 to 60 % accuratly predicted

#############################here finally comparing with the WFinalScore and LFinalScore OF Orginal data
#############################And WFinalScore and LFinalScore OF predicted data

#######tables are predicted_data_1_W and predicted_data_1_L  or predicted_data_2_W and predicted_data_2_L   both are equal
dim(actuals_preds)          #to know the size of the rows
##### winning or lost is noteing in the format of 1's and 0's in the Actual_won label
actual_won=actuals_preds$actuals
for(i in c(1:2697))
{
  if(actuals_preds[i,1] >=actuals_preds_LFinal[i,1])
  {
    actual_won[i]=1
  }
  else
  {
    actual_won[i]=0
  }
}
View(actual_won)
dim(predicted_won)
class(actuals_preds[,1])
#####For Lost
dim(actuals_preds_LFinal)          #to know the size of the rows
##### winning or lost is noteing in the format of 1's and 0's in the Actual_won label
predicted_won=actuals_preds$predicted
for(i in c(1:2697))
{
  if(actuals_preds[i,2] >=actuals_preds_LFinal[i,2])
  {
    predicted_won[i]=1
  }
  else
  {
    predicted_won[i]=0
  }
}
View(predicted_won)

#appling confusion matrix for the actualwons and predicted ones
tb=table(actual_won,predicted_won)
View(tb)
dim(actual_won)
# actual_won  Predicted_won freq 
#1	   1	         0      	101
#2	   1	         1        2596
####to know percentage of accuracy from above data 2596 values are predicted correctly and 101 is not correct among 2697 
###to know percentage of exactly predicted is (2596*100)/2697 = 96.2551% correctly predicted 
#################**********************predictions with decession tree ML got 96% correct predictions*************#########
######################################2.Decession Tree Algorithm is applied success fully ##################################
#############################################################################################################################
#############################################################################################################################
######################################################3.RANDOM FOREST ###########################
install.packages("randomForest")
library(randomForest)
#creating the data
sample = sample.split(data_final,SplitRatio = 0.80) # splits the data in the ratio of 80% for train and remaing 20% for test. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(data_final,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(data_final, sample==FALSE)
View(train)
View(test)
#making model
randam_model=randomForest(WFinalScore~.,data=train)
#testing the model
prediction <-predict(randam_model,newdata = test)
View(prediction)
predicted_data.r_W=data.frame(cbind(actuals=test$WFinalScore,predicted=prediction))
View(predicted_data.r_W)

##########classLabel LFinalScore###########
#Making Model for LFinal Score  
randam_model=randomForest(LFinalScore~.,data=train)     #####model prepared using the random forest
summary(randam_model)
#testing the model
prediction <-predict(randam_model,newdata = test)      #####predicting the values
summary(prediction)
View(prediction)
predicted_data.r_L=data.frame(cbind(actuals=test$LFinalScore,predicted=prediction))  #combinig to one table of Actuals and predicted
View(predicted_data.r_L)

###############################scores are predicted but whethere they are giving accurate results can be test by finding won or lost##
dim(predicted_data.r_W)          #to know the size of the rows
##### winning or lost is noteing in the format of 1's and 0's in the Actual_won label
actual_won_R=predicted_data.r_W$actuals
for(i in c(1:2697))
{
  if(predicted_data.r_W[i,1] >=predicted_data.r_L[i,1])
  {
    actual_won_R[i]=1
  }
  else
  {
    actual_won_R[i]=0
  }
}
View(actual_won_R)        #data is in the form of 1's and 0's represent win or loose correspondingly
dim(predicted_data.r_L)
class(actuals_preds[,1])
#####For Lost
##### winning or lost is noteing in the format of 1's and 0's in the Actual_won label
predicted_won_R=predicted_data.r_L$predicted
for(i in c(1:2697))
{
  if(predicted_data.r_W[i,2] >=predicted_data.r_L[i,2])
  {
    predicted_won_R[i]=1
  }
  else
  {
    predicted_won_R[i]=0
  }
}
View(predicted_won_R)

#appling confusion matrix for the actualwons and predicted ones
tb=table(actual_won_R,predicted_won_R)
View(tb)
dim(actual_won)
# actual_won  Predicted_won freq 
#1	   1          	0       	56
#2	   1          	1     	2641
####to know percentage of accuracy from above data 2596 values are predicted correctly and 101 is not correct among 2697 
###to know percentage of exactly predicted is (2641*100)/2697 = 97.92362% correctly predicted 
#################**********************predictions with decession tree ML got 97.92% correct predictions*************#########
############################################successfully completed the 3 rd ML algorithim 


######$$$$$$$$%%%%%%%%%************ABOUTH THE PROJECT**********%%%%%%%%%%%%$$$$$$$$$#################
##In our project i have iplemented the 3 ML ALGORITHIMS 1.MultiLinear Repression 2.Decision tree 3.RandamForest Algorithim
##AS we have 20 class labels are the continuous prediction will be accurate and cant able find accuracy so actually finnaly the project should able to predict the Won or Lost 
##SO by predicting WFinalScore and LFinalScore For every ML Algorithm finally by those predicted value we can check the win or lost by apply condition in for loop
##those 1 and 0 are predicted for both orginal values and predicted value and compared them haw many got correct wins and wrong result
##In this result i have calculated the percentage of got correct prediction on win or lost finally for every ML algorithm 
###Results are below 
##1.MultiLinear Regression   - got  95.7%correctly got win or lost data (**this is not the class label from prediction class label final acevement is calculated)
##2.Decision tree            - got 96.26%correctly got wins data
##3.Randam Forest            - got 97.9236% correctly values
###############################*****************************************************#######################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

 ####################################################
#library(caret)
#install.packages("klaR")
#library(klaR)
# load the iris dataset
# define an 80%/20% train/test split of the dataset
#split=0.80
#trainIndex <- createDataPartition(data_final$WFinalScore, p=split, list=FALSE)
#data_train <- data_final[ trainIndex,]
#data_test <- data_final[-trainIndex,]
# train a naive bayes model
#model <- NaiveBayes(WFinalScore~., data=data_train)
# make predictions
#x_test <- data_test[,1:4]
#y_test <- data_test[,5]
#predictions <- predict(model, x_test)
# summarize results
#confusionMatrix(predictions$class, y_test)

########################################
#For Handling the data and Separate the DayNum and WTeamID With Frequencies Of Every Colum
#importing the players names with ids to a players attribute
players=read.csv("MPlayers.csv")
#displaying the players data using View method
View(players)
colnames(data)
#dividing the data 
install.packages("caTools")
library("caTools")