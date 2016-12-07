library(mlbench)
library(caret)
library(corrplot)
library(class)
install.packages("gmodels")
library(gmodels)

# load the data
df <- read.csv("~/Google Drive/GMU/Current Classes/R Data Sets/Serial Killers Filtered1.csv")

# deleted NA rows and converted to numeric
df <- na.omit(df)
df <- data.frame(sapply(df,function (x) as.numeric(x)))

# new column for danger classifications based on number of victims
df$danger <- rep(NA, nrow(df))
df[df$NumVics  >= 7, ][, "danger"] <- "Prolific"
df[df$NumVics   < 7, ][, "danger"] <- "Common"
df[df$NumVics  < 3, ][, "danger"] <- "Sparce"
table(df$danger)

#stored df as dfl
dfl<- df

# df <- df[-1]
df <- df [-117]

# training scheme for KNN [10 fold cross validation]-> cycle = 5
ctrl <- trainControl(method="repeatedcv", number=10, repeats=5)
# train the model with scale as preProcess type, can also use "pca" for PCA
model <- train(NumVics~., data=df, method="knn", preProcess="scale", trControl=ctrl)
# estimate variable importance with scaling option TRUE
importance <- varImp(model, scale=T)
# summarize importance  [shows best 20]
print(importance)
# simple plot of importance 
plot(importance, top = 30, main = "Top 30 Predictors")
# Names of best 20 Features for "NumVic" , because graph looks messy
a<-order(importance$importance, decreasing = T)[1:30]
names(df[,a])

# merged importance data frame with danger level corresponded to ID
newdf <- data.frame(df[,a])
newdf <- merge(newdf, data.frame(dfl$NumVics,dfl$ID), by.x = "ID", by.y = "dfl.ID")

#created new danger level
newdf$danger <- rep(NA, nrow(newdf))
newdf[newdf$dfl.NumVics  >= 7, ][, "danger"] <- "Prolific"
newdf[newdf$dfl.NumVics < 7, ][, "danger"] <- "Common"
newdf[newdf$dfl.NumVics < 3, ][, "danger"] <- "Sparce"
table(newdf$danger)

## Model 

#create random training and test data
set.seed(11)
ind <- sample(2, nrow(newdf), replace=TRUE, prob=c(0.70, 0.30))
df.training <- newdf[ind==1, 2:31]
df.test <- newdf[ind==2, 2:31]

#stored labels
df.trainLabels <- newdf[ind==1, 32]
df.testLabels <- newdf[ind==2, 32]

# List append function to store error at different K values
l = 0
lst.append <- function(l,x){
  n <- length(l)
  l[n+1]<- x
  return(l)
}
# Getting sqrt value of length of rows 
sqrt(dim(newdf)[1])

# finding the best value for K
for(i in 1:19){
  new_pred <- knn(train = df.training, test = df.test, cl = df.trainLabels, k=i)
  # check % error for crosstable
  m<-mean(df.trainLabels != new_pred)
  l<- lst.append(l,m)
}
# removing initial value form l
l<- l[2:20]
# plotting error at different values of 'K' 
plot(l, type = 'l', main = "Error rate for different 'K' values")

new_pred <- knn(train = df.training, test = df.test, cl = df.trainLabels, k=which.min(l))
CrossTable(x = df.testLabels, y = new_pred, prop.chisq=T)
# error in CrossTable
mean(df.trainLabels != new_pred)
