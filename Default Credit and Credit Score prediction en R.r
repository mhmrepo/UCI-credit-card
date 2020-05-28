
library(ggplot2)
library(dplyr)
library(caret)
library(lattice)
library(corrplot)
library(gridExtra)
library(e1071)
install.packages("ROCR",dep=T)
library(ROCR)
install.packages("corrplot")
library(corrplot)
library(rpart)

options("scipen" = 10) # make sure all the plot have no exponential mark -> show real values

raw.credit <- read.csv("C:\\201X_Classes\\201X_DATOS\\CASOS DE USO\\UCI_Credit_Card.csv", header=T, sep=',', stringsAsFactors=F)
colnames(raw.credit) <- tolower(colnames(raw.credit))

#Note that we have removed **id** from datasets as there's no meaning in this context.
#We will proceed to next section before assigning any factor class to the variables.

convert.to.factor <- function(df, lst){
  for (i in lst){
    df[[i]] <- as.factor(df[[i]])
  }
  return(df)
}

convert.to.numeric <- function(df, lst){
  for (i in lst){
    df[[i]] <- as.numeric(df[[i]])
  }
  return(df)
}


raw.credit <- select(raw.credit,-id)
#raw.credit <- select(raw.credit)

#1. Look at Demographical variable data
# For education, we will merge 0, 4, 5, 6 to 4 (others)
# For marriage, we will merge 0 to 3 (others)

raw.credit[which(raw.credit$education %in% c(0, 4, 5, 6)),]$education <- 4 # Flag to others
raw.credit[which(raw.credit$marriage == 0),]$marriage <- 3 # Flag to other

list.to.factor <- c("sex", "education", "marriage", "default.payment.next.month")

raw.credit <- convert.to.factor(raw.credit, list.to.factor)

dim(raw.credit)

#2. Age data - the data looks ok

#3. Quick look on the limit balance

#4. What about repayment status
#With this, I **assume** that -2, -1 and 0 have the same meaning. We will merge them to 0.
for (i in seq(6, 11, 1)) {
  raw.credit[which(raw.credit[[i]] %in% c(-2, -1, 0)),][[i]] <- 0
}

lets.plot <- function(df, fun, ii, ncol) {
  pp <- list()
  for (i in ii) {
    p <- fun(df=df, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

barPlot <- function(df, i) {
  d <- data.frame(x=df[[i]]) 
  p <- ggplot(d, aes(factor(x))) + 
    stat_count() + 
    xlab(colnames(df)[i]) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)
          , legend.position = "bottom"
          , legend.title = element_blank()
    )  
  return(p)
}

lets.plot(raw.credit, fun=barPlot, ii=2:4, ncol=2)

#6. Lastly, let's look at the number of default payment next month.
lets.plot(raw.credit, fun=barPlot, ii=24, ncol=1)
prop.table(table(raw.credit$default.payment.next.month))

#Separate the dataset onto training (70%) and testing (30%).
intraining <- createDataPartition(y=raw.credit$default.payment.next.month, p=0.7, list=F)
train.batch <- raw.credit[intraining,]
test.batch <- raw.credit[-intraining,]

train.batch[1:5,]

#1. Traditional Score using Logistic Regression
glm.m <- glm(default.payment.next.month ~ ., data=train.batch, family=binomial)
summary(glm.m)

glm.final <- glm(default.payment.next.month ~ limit_bal+sex+education+marriage+
                   pay_0+pay_2+pay_3+pay_5+pay_6+pay_amt1+pay_amt2, data=train.batch,
                 family=binomial)
summary(glm.final)

#Hacer la estimacion del Test
test.x <- select(test.batch, - default.payment.next.month)
test.batch$glm_scor1 <- predict(glm.final, type="response", test.x)
pred <- prediction(test.batch$glm_scor1, test.batch$default.payment.next.month)


# Predict 
pred.prob = predict(glm.m, test.batch, type = "response")
pred.glm = rep(0, length(pred.prob))
pred.glm[pred.prob > 0.8] = 1
pred.table = table(pred.glm, test.batch$default.payment.next.month)
pred.table

# Sensitivity: TP/P = TPR
Sensitivity = pred.table[1,1] / sum(pred.table[,1])

# Specificity: TN/N = TNR
Specificity = pred.table[2,2] / sum(pred.table[,2])

# Accuracy: (TP + TN)/(P + N)
Accuracy = sum(pred.table[1,1], pred.table[2,2]) / sum(pred.table[,])

# Total Error Rate: (FP + FN)/(P + N)
TotalError = sum(pred.table[1,2],pred.table[2,1]) / sum(pred.table[,])
glm.Confusion = round(data.frame(Sensitivity, Specificity, Accuracy, TotalError),4)

row.names(glm.Confusion) = "GLM"

paste("The accuracy using Logistic regression is", Accuracy)

formulaQ3 = 
  as.formula(default.payment.next.month~marriage+pay_0+pay_2+pay_3+
                  bill_amt1+pay_amt1)

######################################################################################
# Generalized Linear Model with Logistic Regression
fit.glm = glm(formulaQ3, data = train.batch, family = binomial)

# Predict 
pred.prob = predict(fit.glm, test.batch, type = "response")
pred.glm = rep(0, length(pred.prob))
pred.glm[pred.prob > 0.8] = 1
pred.table = table(pred.glm, test.batch$default.payment.next.month)
pred.table

# Sensitivity: TP/P = TPR
Sensitivity = pred.table[1,1] / sum(pred.table[,1])

# Specificity: TN/N = TNR
Specificity = pred.table[2,2] / sum(pred.table[,2])

# Accuracy: (TP + TN)/(P + N)
Accuracy = sum(pred.table[1,1], pred.table[2,2]) / sum(pred.table[,])

# Total Error Rate: (FP + FN)/(P + N)
TotalError = sum(pred.table[1,2],pred.table[2,1]) / sum(pred.table[,])
glm.Confusion = round(data.frame(Sensitivity, Specificity, Accuracy, TotalError),4)

row.names(glm.Confusion) = "GLM"

paste("The accuracy using Logistic regression is", Accuracy)

# a) Calcular la sensitivity, Specificity, Accuracy, Total Error Rate:
Hacer la matriz de confusión con el cutoff de 0.8

colnames(train.batch[5,])
attach(train.batch)

varnames <- c("limit_bal","age",'bill_amt1','bill_amt2','bill_amt3','bill_amt4','bill_amt5','bill_amt6',
              'pay_amt1','pay_amt2','pay_amt3','pay_amt4','pay_amt5','pay_amt6')

resto <- c("sex","education","marriage","default.payment.next.month")
# index vector of columns which must not be scaled
index <- names(train.batch) %in% varnames



# scale only the columns not in index
temp <- scale(train.batch[, varnames])
temp1<-cbind(train.batch[,resto],temp)

train.batch.scale <- temp1
train.batch.scale[10,]

glm.m <- glm(default.payment.next.month ~ ., data=train.batch.scale, family=binomial)
summary(glm.m)

glm.final <- glm(default.payment.next.month ~ limit_bal+sex+education+marriage+
                   pay_0+pay_2+pay_3+pay_5+pay_6+pay_amt1+pay_amt2, data=train.batch,
                 family=binomial)
summary(glm.final)

data.frame(limit_bal=train.batch$limit_bal,
sex=train.batch$sex,      
education=train.batch$education,
marriage=train.batch$marriage,
pay_0=train.batch$pay_0,     
pay_2=train.batch$pay_2,     
pay_3=train.batch$pay_3,     
pay_5=train.batch$pay_5,     
pay_6=train.batch$pay_6,     
pay_amt1=train.batch$pay_amt1,  
pay_amt2=train.batch$pay_amt2,Fitted=fitted(glm.final),credit.score=fitted(glm.final)*800)

#Hacer la curva ROC

glm.perf <- performance(pred, "tpr", "fpr")
glm.perf

# ROC
plot(glm.perf, lty=1, col="blue", main="Logistic Regression Performance")
abline(a=0, b= 1)



#2. Decision Tree **(rpart library)**
rpart.model <- rpart(default.payment.next.month ~.,data=train.batch)
              
plot(rpart.model);text(rpart.model)
plotcp(rpart.model)
printcp(rpart.model)

rpart.pred <- predict(rpart.model, test.x, type="class")
confusionMatrix(rpart.pred, test.batch$default.payment.next.month, positive = '1')

test.batch$rpart_scor1 <- predict(rpart.model,type='prob',test.x)
pred2 <- prediction(test.batch$rpart_scor1[, 2], test.batch$default.payment.next.month)
rpart.perf1 <- performance(pred2,"tpr","fpr")


plot(rpart.perf1, col='red', main='Rpart Tree performance')
abline(a=0, b= 1)

#3. Random Forest

install.packages("randomForest")
library(randomForest)

rand.model <- randomForest(default.payment.next.month~.,data=train.batch, importance=T,
                            ntree=500, keep.forest=T)

varImpPlot(rand.model)

rand.pred <- predict(rand.model, test.x, type="class")
confusionMatrix(rand.pred, test.batch$default.payment.next.month, positive='1')

rand.prob <- predict(rand.model, test.x, type='prob')[,2]
pred4 <- prediction(rand.prob,test.batch$default.payment.next.month)

rf.perf1 <- performance(pred4,"tpr","fpr")



plot(glm.perf,col='red',lty=1, main='ROC Logistic VS. Rpart tree VS. Random Forest'); 
plot(rpart.perf1, col='blue',lty=2,add=T); 
plot(rf.perf1, col='green', lty=3, add=T);
legend(0.6,0.6,c('logistic regression','rpart tree', 'randomForest'),
       col=c('red','blue','green'),lwd=3)


