df <- read.csv("~/Google Drive/GMU/Current Classes/R Data Sets/Serial Killers Filtered1.csv")

# deleted NA rows and converted to numeric
df <- na.omit(df)
df <- data.frame(sapply(df,function (x) as.numeric(x)))
View(df)
df.lm = df[-c(1,5,6)]
View(df.lm)
model <- lm(NumVics ~.,data = df.lm)
summary(model)
