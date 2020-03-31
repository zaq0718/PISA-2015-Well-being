library(caret)
library(mlbench)
library(randomForest)
library(dplyr)
library(naniar)

#1. Data
setwd("~\\Well-Being Project_PISA 2015\\Analysis\\Cultural Regions\\Anglo")
dta <- read.csv("PV1.txt",sep="")
colnames(dta) <- c("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP",
                   "PVSCIE","ST016Q01NA","SENWT","IMMIG2","IMMIG3")
dta <- as.data.frame(dta[,c("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PVSCIE","ST016Q01NA")])

dta <- dta%>%replace_with_na_all(condition = ~.x==9999)
dta <- dta%>%mutate_at(c("ST004D01T","IMMIG"),as.factor)
dta <- dta%>%mutate_at(c("ST016Q01NA"),as.numeric)
train <- sample(1:nrow(dta),floor(nrow(dta)*0.7),replace = FALSE)
training <- dta[train,]
testing <- dta[-train,]
ST016Q01NA.test <- as.vector(unlist(testing[,10]))

#2. cross-validation
mtry <- floor(sqrt(ncol(dta))) #Default calculation for the mtry :3
control <- trainControl(method="cv", number=5)
metric <- "Rsquared"
tunegrid1<- expand.grid(.mtry=c(1:9))
rfresults <- rep(list(NULL),nrow(tunegrid1))
oldtime <- Sys.time()
for( i in 1:nrow(tunegrid1)){
  set.seed(1234)
  tunegrid<- expand.grid(.mtry=tunegrid1[i,])
  tryCatch(train(ST016Q01NA~., data=training, method="rf", metric=metric, na.action = na.omit,
               tuneGrid=tunegrid, trControl=control),warning=function(w) print(i))
  output <- train(ST016Q01NA~., data=training, method="rf", metric=metric, na.action = na.omit,
                  tuneGrid=tunegrid, trControl=control)
  rfresults[[i]] <- output$results[c("mtry","RMSE","Rsquared","MAE")]
  
}

output <- as.data.frame(matrix(unlist(rfresults),ncol=4,nrow=12,byrow = TRUE))#mtry=2
colnames(output) <- c("mtry","RMSE","Rsquared","MAE")

#4.Tuning ntree
tunegrid1 <- expand.grid(.mtry=2)
numbert <- seq(100,1000,100)
ntree<- rep(list(NULL),length(numbert))

treetime <- Sys.time()

for (j in 1:length(nt)) {
  set.seed(1234)
  tryCatch(train(ST016Q01NA~., data=training, method="rf", metric=metric, na.action = na.omit,
                 tuneGrid=tunegrid1, trControl=control,ntree=numbert[j]),warning=function(w) print(i))
  fit <- train(ST016Q01NA~., data=training, method="rf", metric=metric, na.action = na.omit,
               tuneGrid=tunegrid1, trControl=control,ntree=numbert[j])
  ntree[[j]] <- fit$results[c("mtry","RMSE","Rsquared","MAE")]
}

treeout<- as.data.frame(matrix(unlist(ntree),ncol=4,nrow=length(numbert),byrow = TRUE))#mtry=2
colnames(treeout) <- c("mtry","RMSE","Rsquared","MAE")#400

#3. loop for tuning mytry 
mse<- rep(NA,length(Number))
r2 <- rep(NA,length(Number))

for ( i in 1:length(Number)){
  tryCatch(randomForest(ST016Q01NA~.,data=training,importance=TRUE,na.action=na.omit,mtry=try[i]),warning=function(w) print(i))
  model <- randomForest(ST016Q01NA~.,data=training,importance=TRUE,na.action=na.omit,mtry=try[i])
  Predicition <-  predict(model,newdata=dta[-train,])
  mse[i] <- round(mean((Predicition-ST016Q01NA.test)^2,na.rm = TRUE),digits = 3)
  r2[i] <- round(sum((ST016Q01NA.test-Predicition)^2,na.rm = TRUE)/sum((ST016Q01NA.test-mean(ST016Q01NA.test,na.rm = TRUE))^2,na.rm=TRUE),digits = 3)
  #need to check 
}
mse
#4. Th final one
final<- randomForest(ST016Q01NA~.,data=training,importance=TRUE,na.action=na.omit,mtry=2,ntree=400)
final
final_p <- predict(final,newdata=dta[-train,])
round(mean((final_p-ST016Q01NA.test)^2,na.rm = TRUE),digits = 3)
