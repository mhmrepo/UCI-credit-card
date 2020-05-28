
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


#1 Calcular la sensitivity, Specificity, Accuracy, Total Error Rate: Hacer la matriz de confusión con el cutoff de 0.8
#2 Usar esta nueva formula de regresión y comparar los mismos cálculos que el apartado a): default.payment.next.month~marriage+pay_0+pay_2+pay_3+bill_amt1+pay_amt1
#3 Hacer la regresión haciendo una standarización de variables y comparar con el anterior:
#4 Tomar uno de los modelos calculados y hacer el calculo de credit score
#5 Hacer la curva ROC: Hacer distintos escenarios de cutoff y generar las tablas de confusión
#6 Hacer la curva ROC para las regresiones logisticas
#7 Hacer un arbol de decisión
#8 Hacer un random Forest
#9 el ROC de todos los modelos

