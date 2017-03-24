library(ggplot2)

library(stringr)

library(scales)

library(gridExtra)

library(lubridate)

library(caret)

library(caTools)

library(e1071)

library(party)


dataset <- read.csv("E:/R Workspace/Data Mining Project/No-show-Issue-Comma-300k.csv", stringsAsFactors = TRUE, header = TRUE, na.strings = c("", " ", "NA"))

summary(dataset)

#dataset <- cbind(data)
#############################################################
#################Data Understanding##########################
#############################################################

# plotting histogram of relation between Age and Status
ggplot(dataset,aes(x=Age,group=Status,fill=Status))+geom_histogram(position="identity",alpha=.4,bins=40)+scale_fill_brewer(palette="Set1")

#plotting boxplot of relation between Age and Status
ggplot(dataset,aes(x=Status,y=Age,colour=Status))+geom_boxplot(size=1)+scale_colour_brewer(palette="Pastel1")

#taking absolute of AwaitingTime to make it positive
dataset$AwaitingTime <- abs(dataset$AwaitingTime)

#plotting density of effect of AwaitingTime on status
ggplot(dataset[dataset$AwaitingTime<=100,],aes(x=AwaitingTime,group=Status))+geom_density(aes(fill=Status),alpha=.3,colour=NA)

#plotting the bar chart of relation between Gender and Status
p1 <- ggplot(dataset, aes(x=Gender, fill=Gender)) + geom_bar(position="dodge")
p2 <- ggplot(dataset, aes(x=Gender, fill=Status)) + geom_bar(position="dodge")
grid.arrange(p1, p2,ncol=2, top='Gender distribution')

gender_table <- table(dataset$Gender, dataset$Status)
addmargins(gender_table)


#plotting relationship of days of the week and status on bar chart
ggplot(dataset, aes(x=DayOfTheWeek, fill=Status)) + geom_bar(position="dodge")

days_table <- table(dataset$DayOfTheWeek, dataset$Status)
addmargins(days_table)

#plotting relation of appointment month and status

dataset$ApointmentData <- ymd_hms(dataset$ApointmentData)

ggplot(dataset, aes(x=month(ApointmentData,label=TRUE), fill=Status)) + geom_bar(position="dodge")

month_table <- table(month(dataset$ApointmentData), dataset$Status)
addmargins(month_table)

#plotting the effect of SMS reminders on Status

ggplot(dataset, aes(x=Sms_Reminder, fill=Status)) + geom_bar(position="dodge")


#######################################################
#############Preparing the Data########################

#normalization the data

dataset$Age <- (dataset$Age - min(dataset$Age))/(max(dataset$Age) - min(dataset$Age))


dataset$AwaitingTime <- (dataset$AwaitingTime - min(dataset$AwaitingTime))/(max(dataset$AwaitingTime) - min(dataset$AwaitingTime))

#removing variables

dataset <- dataset[,-c(3,4)]

#splitting the dataset into training and testing sets

split_index <-createDataPartition(dataset$Status,p=0.7,list=FALSE)
Train.data <-dataset[split_index,]
Test.data <-dataset[-split_index,]

# seperating the class labels
Train.Status <- Train.data$Status
Test.Status<-Test.data$Status


##########################################################
###################Modelling##############################

#implementing Logistic Regression to classify Status

Model.2 <- glm(Status~., data = Train.data, family = binomial("logit"))
summary(Model.2)


pred_m2 <- predict(Model.2, Test.data, type="response")
colAUC(pred_m2, Test.data$Status, plotROC=TRUE)

#implementing Decision Tree to classify Status

# Give the chart file a name.
png(file = "decision_tree.png")

# Create the classification tree.
output.tree <- ctree(Status~., data = Train.data)

# Plot the tree.
plot(output.tree)


# Predictions on test data
pred_tree <- predict(output.tree, Test.data)
#Printing confusion matrix
confusionMatrix(pred_tree, Test.data$Status)

# Save the file.
dev.off()


#implementing Support Vector Machine for classification

Model.1 <- svm(Status~., data = Train.data)

summary(Model.1)

pred_m1 <- predict(Model.1, Test.data, type="response")
colAUC(pred_m1, Test.data$Status, plotROC=TRUE)





