df <- readRDS("~/kehra/data/EnglandDB_1979_2014.rds")

library(caret)
set.seed(3456)
trainIndex <- createDataPartition(y=df$CVD60, p=0.60, list=FALSE, times = 1)
training <- df[trainIndex,]
testing <- df[-trainIndex,]

# saveRDS(training, "training.rds")
# saveRDS(testing, "testing.rds")
as.data.frame(names(training))

# CHECK VARIABLES
hist(training$o3,main="",xlab="ave. capital run length")

# STANDARDISE all the variables
preObj <- preProcess(training[,-c(5:12)],method=c("center","scale"))
trainCVD60 <- predict(preObj,training[,-c(5:12)])$CVD60
mean(trainCVD60, na.rm = TRUE)
sd(trainCVD60, na.rm = TRUE)
