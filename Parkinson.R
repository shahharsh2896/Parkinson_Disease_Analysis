library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(ggcorrplot)
library(tidyr)
library(e1071)
library(factoextra)
library(ggfortify)
library(tidyverse)

#Loading dataset into R
Data <- read.csv("Parkinson.csv")
#View(Data)
Dataset<-Data[,-c(1,2)]
str(Dataset)
#Converting Variables to factors
Dataset$status<-as.factor(Dataset$status)    #Dependent Variable

#Check for missing values
sum(is.na(Dataset))   #No missing values

#Univariate Analysis (Outliers and skewness)

Dataset[,-17] %>%
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +     # In separate panels
  geom_density()                           # as density


par(mfrow=c(2,2))
out<-boxplot.stats(Dataset$MDVP.Fhi.Hz.)$out
boxplot(Dataset$MDVP.Fhi.Hz.,
        ylab = "MDVP.Fhi.Hz.",
        main = "Boxplot")
mtext(paste("Outliers: ", paste(sort(out), collapse = ", ")))
####
out<-boxplot.stats(Dataset$MDVP.Jitter...)$out
boxplot(Dataset$MDVP.Jitter...,
        ylab = "MDVP.Jitter...",
        main = "Boxplot")
mtext(paste("Outliers: ", paste(sort(out), collapse = ", ")))
####
out<-boxplot.stats(Dataset$MDVP.Jitter.Abs.)$out
boxplot(Dataset$MDVP.Jitter.Abs.,
        ylab = "MDVP.Jitter.Abs.",
        main = "Boxplot")
mtext(paste("Outliers: ", paste(sort(out), collapse = ", ")))
####
out<-boxplot.stats(Dataset$MDVP.RAP)$out
boxplot(Dataset$MDVP.RAP,
        ylab = "MDVP.RAP",
        main = "Boxplot")
mtext(paste("Outliers: ", paste(sort(out), collapse = ", ")))

#Bivariate Analysis
Dataset_m=melt(Dataset,id.vars = "status")
p <- ggplot(data = Dataset_m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=status))
p + facet_wrap( ~ variable, scales="free")

#Correlation
#Cor Matrix
Train_num<-Dataset
Train_num$status<-as.numeric(Train_num$status)
r <- cor(Train_num, use="complete.obs")
round(r,2)
library(ggcorrplot)
ggcorrplot(r, hc.order = TRUE, type = "lower" , lab = T, lab_size = 2.5 )

#T-Test
pvn<-data.frame(lapply(Train_num[,-c(1:15,17)], function(x) t.test(x ~ Train_num$status, var.equal = TRUE)$p.value))
View(round(pvn,6))

#Wilcox test (Right skewed)
pvn1<-data.frame(lapply(Train_num[,c(1:15)], function(x) wilcox.test(x ~ Train_num$status, var.equal = TRUE)$p.value))
View(round(pvn1,6))

#PCA
Data.pca1 <- prcomp(Dataset[,c(9:14,16)], center = T, scale = T) #Shimmer
Data.pca2 <- prcomp(Dataset[,c(4:8,15)], center = T, scale = T) #Jitter
fviz_eig(Data.pca1)
summary(Data.pca1)
str(Data.pca1)
pca1.plot <- autoplot(Data.pca1, data = Dataset, colour = 'status')
pca1.plot$data
comps1 <- pca1.plot$data[,c(1,2,11:16,18)]
cor(comps1[,c(3:9)], comps1[,c(1:2)])


fviz_eig(Data.pca2)
summary(Data.pca2)
str(Data.pca2)
pca2.plot <- autoplot(Data.pca2, data = Dataset, colour = 'status')
pca2.plot$data
comps2 <- pca2.plot$data[,c(1,2,6:10,17)]
cor(comps2[,c(3:8)], comps2[,c(1,2)])

pca1<-(Dataset[c(9:14,16)])
pca2<-(Dataset[c(4:8,15)])
pca1$PCA1<-rowMeans(pca1)
pca2$PCA2<-rowMeans(pca2)

Dataset.new<-cbind(Dataset, pca1$PCA1 , pca2$PCA2)
Dataset.new$PCA1<-Dataset.new$`pca1$PCA1`
Dataset.new$PCA2<-Dataset.new$`pca2$PCA2`
Dataset.new<-Dataset.new[,-c(4:16,24,25)]
Dataset.new$PPE<-NULL

#Normalizing Data
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

#Dataset<-as.data.frame(scale(Dataset.new[,-4]))
Dataset <- as.data.frame(lapply(Dataset.new[,-4], normalize))
Dataset<-cbind(Dataset,Dataset.new$status)
Dataset$status<-Dataset$`Dataset.new$status`
Dataset$`Dataset.new$status`<-NULL
describe(Dataset)
View(Dataset)

#T-Test
pvnf<-data.frame(lapply(Dataset[,-11], function(x) t.test(x ~ Dataset$status, var.equal = TRUE)$p.value))
View(round(pvnf,6))
Dataset$status<-as.numeric(Dataset$status)
r <- cor(Dataset, use="complete.obs")
round(r,2)

library(ggcorrplot)
ggcorrplot(r, hc.order = TRUE, type = "lower" , lab = T, lab_size = 2.5 )
Dataset$status<-as.factor(Dataset$status)

###Simple Model
Model<-glm(formula = status ~ ., 
           family = "binomial", data = Dataset)
options(scipen = 99)
alias(Model)   
car::vif(Model) 
summary(Model)

#80% Train, 20% Test. (75% 1's, 25% 0's maintained in DV)
library(caret)
set.seed(123)
train.index <- createDataPartition(Dataset$status, p = .80, list = FALSE)
TrainData  <- Dataset[ train.index,]
TestData<- Dataset[-train.index,]
table(TrainData$status)
table(TestData$status)

###Removing variables with VIF>5 and running glm model
TrainData_V1<-TrainData
TestData_V1<-TestData
null <- glm(status ~ 1, data= TrainData_V1,family="binomial") # only includes one variable
full <- glm(status ~ (.*.), 
            data= TrainData_V1,family="binomial") # includes all the variables
logitModel <- step(null, scope = list(lower = null, upper = full), direction = "both")
summary(logitModel)

Model1<-glm(formula = status ~ spread1 + D2 + DFA + 
              PCA1:spread2, family = "binomial", 
            data = TrainData_V1)
options(scipen = 99)
alias(Model1)   
car::vif(Model1) 
summary(Model1)

pred_prob <- predict(Model1,newdata = TestData_V1, type = "response")

#Getting Probability cut off point using ROC curve
library(ROCR)
pred <- prediction( predictions = pred_prob, TestData_V1$status)
perf <- performance(pred,"tpr","fpr")
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x-0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)}

print(opt.cut(perf, pred))

#Prediction and performance on TrainData
pre <- as.numeric(predict(Model1,type="response")>0.5193006)
confusionMatrix(table(pre,TrainData_V1$status), positive = "1")
#Prediction and performance on TestData
pre <- as.numeric(predict(Model1,newdata=TestData_V1[,-4],type="response")>0.5193006)
confusionMatrix(table(pre,TestData_V1$status), positive = "1")
options(scipen = 99)
#Coefficients - Log of ODDS
exp(coefficients(Model1))

###SVM
#Building a Linear Model
library(e1071)
set.seed(123)
svmmodel=tune(svm,status~.,data=TrainData_V1,
             ranges = list(cost=c(0.001,0.01,0.1,1,2,4,6,8,10,100),gamma=c(0.1,0.3,0.5,0.7,1,2)),
             kernel="radial")
svmmodel<-svmmodel$best.model
svmmodel$gamma
svmmodel$cost

pred=predict(svmmodel,TestData_V1)
tr=table(predicted=pred,Actual=TestData_V1$status)
sum(diag(tr))/sum(tr)
confusionMatrix(table(pred,TestData_V1$status), positive = "1")


###Random Forest
library(randomForest)
set.seed(123)
rfmodel<-randomForest(status~ .,data=TrainData_V1, mtry=2, ntree=100,
                      importance=TRUE,proximity=TRUE)

#Testing the model
p=predict(rfmodel,TestData_V1)
confusionMatrix(p, TestData_V1$status, positive = "1")

###K-NN
library(ggvis)
library(class)
library(caret)
set.seed(123)
#Dataset$status<-as.factor(Dataset$status)
training = TrainData_V1[, 1:10]
test = TestData_V1[, 1:10]

trainLabels = TrainData_V1[, 11]
testLabels = TestData_V1[, 11]

#build model on training data for FACTOR as TARGET
pred = knn(train = training, test = test, cl = trainLabels, k=5)
confusionMatrix(pred ,as.factor(testLabels), positive = "1")