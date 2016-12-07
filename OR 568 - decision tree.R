library(ISLR)
library(tree)

`Serial.Killers.(1)` <- read.csv("~/Google Drive/GMU/Current Classes/R Data Sets/Serial Killers (1).csv")
serial=`Serial.Killers.(1)`

set.seed(12345)
tree.serial=tree(NumVics~.,serial)
plot(tree.serial)
text(tree.serial,pretty=4)
summary(tree.serial)

cv.serial=cv.tree(tree.serial)
plot(cv.serial$size,cv.serial$dev,type='b')
prune.serial=prune.tree(tree.serial,best=5)
plot(prune.serial)
text(prune.serial,pretty=5)
