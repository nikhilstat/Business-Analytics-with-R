# setwd("~/Documents/2019 Fall/Business Analytics with R/Project_new/dataset")
# import packages
library("stringr")
library("ggplot2")
library("tidyr")
library("reshape")
library("dplyr")
df.raw = read.csv("FIFA2018.csv")


#### Data Cleaning ####

#Remove useless columns
useless_columns = c(1, 4, 6, 8, 10, 48:75)  
df = df.raw[, -useless_columns]
df[, 10:41] = sapply(df[, 10:41], as.numeric)

#Remove currency symbols
df$Value = as.numeric(str_extract(df$Value, "\\d+\\.*\\d*"))
df$Wage = as.numeric(str_extract(df$Wage, "\\d+\\.*\\d*"))

# Remove the rows with wage that is smaller than 2K/week
df = df[-which(df$Wage <= 2), ]    

#Create 2 buckets for wage
df.num = select_if(df, is.numeric)
df.num$WageRange = ifelse(df$Wage < 10,"<10K", ">10K")
df.num$WageRange = factor(df.num$WageRange, levels = c("<10K",">10K"))

#Run correlation matrix
library("corrplot")
cor = cor(df.num[, c(1,4:36)], method = "pearson", use = "complete.obs")
corrplot(cor, tl.cex= 0.5, tl.col = "gray50")

#Plot relationship between Age and Wage
age_nationality = ggplot(df.num, aes(df$Age, df$Wage))
age_nationality + geom_point(position = "jitter") + xlab("Age") + ylab("Wage")

#Plot relationship between Overall and Wage
overall_wage = ggplot(df.num, aes(df$Overall, df$Wage))
overall_wage + geom_point(position = "jitter") + xlab("Overall") + ylab("Wage")

#Plot density of Wage
plt = ggplot(df, aes(df$Wage))
plt + geom_density(adjust = 1) + 
  labs(title = "Density of Wage", x = "Wage (K)")
theme(plot.title = element_text(hjust = 0.5, face = "bold"))

##Modeling
set.seed(321)
train = sample((1:nrow(df)), 0.7*nrow(df))
df.train = df.num[train, ]  # training data set
df.valid = df.num[-train, ] # validation data set

df.tree = rpart(WageRange ~. , data = df.train, method = "class", 
                parms = list(split = 'information'))
rpart.plot(df.tree, extra = "auto")

train_df.pred = predict(df.tree, type = "class")
confusionMatrix(df.train$WageRange, train_df.pred)
valid_df.pred = predict(df.tree, df.valid, type = "class")
confusionMatrix(df.valid$WageRange, valid_df.pred)



####### How many clusters? #######
withinss = c()
for (i in 2:30){
  k = kmeans(df.num[, 2:ncol(df.num)], i)
  withinss = c(withinss, k$tot.withinss)
}
plot(withinss)
lines(withinss)

# Choose 6 clusters
df.cl = kmeans(scale(df.num[2:ncol(df.num)]), 6)

####### Scaling #######
df.num = cbind(df.num, Cluster = df.cl$cluster)
maximum = sapply(df.num[, 2:30], max)
minimum = sapply(df.num[, 2:30], min)
df.num[, 2:29] = as.data.frame(sapply(df.num[, 2:29], scale))
set.seed(999)

#### tree 1 ####
df.cl1 = df.num[df.num$Cluster == 1, ]
train = sample((1:nrow(df.cl1)), 0.7*nrow(df.cl1))
df.cl1.train = df.cl1[train, ]  # training data set
df.cl1.valid = df.cl1[-train, ] # validation data set
df.tree1 = rpart(WageRange ~. , data = df.cl1.train, method = "class", 
                 parms = list(split = 'information'))
fancyRpartPlot(df.tree1, caption = "")
pred1 = predict(df.tree1, type = "class")   # training
confusionMatrix(df.cl1.train$WageRange, pred1)
pred1 = predict(df.tree1, df.cl1.valid, type = "class") # validation
confusionMatrix(df.cl1.valid$WageRange, pred1)
printcp(df.tree1)

#### tree 2 ####
df.cl2 = df.num[df.num$Cluster == 2,]
train = sample((1:nrow(df.cl2)), 0.7*nrow(df.cl2))
df.cl2.train = df.cl2[train, ]  # training data set
df.cl2.valid = df.cl2[-train, ] # validation data set
df.tree2 = rpart(WageRange ~. , data = df.cl2.train, method = "class", 
                 parms = list(split = 'information'))
fancyRpartPlot(df.tree2, caption = "")
pred2 = predict(df.tree2, type = "class")   # training
confusionMatrix(df.cl2.train$WageRange, pred2)
pred2 = predict(df.tree2, df.cl2.valid, type = "class") # validation
confusionMatrix(df.cl2.valid$WageRange, pred2)

#### tree 3 ####
df.cl3 = df.num[df.num$Cluster == 3,]
train = sample((1:nrow(df.cl3)), 0.7*nrow(df.cl3))
df.cl3.train = df.cl3[train, ]
df.cl3.valid = df.cl3[-train, ]
df.tree3 = rpart(WageRange ~. , data = df.cl3.train, method = "class", 
                 parms = list(split = 'information'))
fancyRpartPlot(df.tree3, caption = "")
pred3 = predict(df.tree3, type = "class")   # training
confusionMatrix(df.cl3.train$WageRange, pred3)
pred3 = predict(df.tree3, df.cl3.valid, type = "class") # validation
confusionMatrix(df.cl3.valid$WageRange, pred3)

#### tree 4 ####
df.cl4 = df.num[df.num$Cluster == 4,]
train = sample((1:nrow(df.cl4)), 0.7*nrow(df.cl4))


df.cl4.train = df.cl4[train, ]
df.cl4.valid = df.cl4[-train, ]
df.tree4 = rpart(WageRange ~. , data = df.cl4.train, method = "class", 
                 parms = list(split = 'information'))
fancyRpartPlot(df.tree4, caption = "")
pred4 = predict(df.tree4, type = "class")   # training
confusionMatrix(df.cl4.train$WageRange, pred4)
pred4 = predict(df.tree4, df.cl4.valid, type = "class") # validation
confusionMatrix(df.cl4.valid$WageRange, pred4)

#### tree 5 ####
df.cl5 = df.num[df.num$Cluster == 5, ]
train = sample((1:nrow(df.cl5)), 0.7*nrow(df.cl5))
df.cl5.train = df.cl5[train, ]
df.cl5.valid = df.cl5[-train, ]
df.tree5 = rpart(WageRange ~. , data = df.cl5.train, method = "class", 
                 parms = list(split = 'information'))
fancyRpartPlot(df.tree5, caption = "")
pred5 = predict(df.tree5, type = "class")   # training
confusionMatrix(df.cl5.train$WageRange, pred5)
pred5 = predict(df.tree5, df.cl5.valid, type = "class") # validation
confusionMatrix(df.cl5.valid$WageRange, pred5)

#### tree 6 ####
df.cl6 = df.num[df.num$Cluster == 6,]
train = sample((1:nrow(df.cl6)), 0.7*nrow(df.cl6))
df.cl6.train = df.cl6[train, ]
df.cl6.valid = df.cl6[-train, ]
df.tree6 = rpart(WageRange ~. , data = df.cl6.train, method = "class", 
                 parms = list(split = 'information'))
fancyRpartPlot(df.tree6, caption = "")
pred6 = predict(df.tree6, type = "class")   # training
confusionMatrix(df.cl6.train$WageRange, pred6)
pred6 = predict(df.tree6, df.cl6.valid, type = "class") # validation
confusionMatrix(df.cl6.valid$WageRange, pred6)


library("randomForest")

#### random forest 1 ####
df.rf1 = randomForest(WageRange ~ ., data = df.cl1.train, ntree = 10000)
rf.pred1 = predict(df.rf1, type = "class")  # training
confusionMatrix(rf.pred1, df.cl1.train$WageRange)

rf.pred1 = predict(df.rf1, df.cl1.valid, type = "class")    # validation
confusionMatrix(rf.pred1, df.cl1.valid$WageRange)

#### random forest 2 ####
df.rf2 = randomForest(WageRange ~ ., data = df.cl2.train, ntree = 10000)
rf.pred2 = predict(df.rf2, type = "class")  # training
confusionMatrix(rf.pred2, df.cl2.train$WageRange)
rf.pred2 = predict(df.rf2, df.cl2.valid, type = "class")    # validation
confusionMatrix(rf.pred2, df.cl2.valid$WageRange)

#### random forest 3 ####
df.rf3 = randomForest(WageRange ~ ., data = df.cl3.train, ntree = 10000)
rf.pred3 = predict(df.rf3, type = "class")  # training
confusionMatrix(rf.pred3, df.cl3.train$WageRange)
rf.pred3 = predict(df.rf3, df.cl3.valid, type = "class")    # validation
confusionMatrix(rf.pred3, df.cl3.valid$WageRange)

#### random forest 4 ####
df.rf4 = randomForest(WageRange ~ ., data = df.cl4.train, ntree = 10000)
rf.pred4 = predict(df.rf4, type = "class")  # training
confusionMatrix(rf.pred4, df.cl4.train$WageRange)
rf.pred4 = predict(df.rf4, df.cl4.valid, type = "class")    # validation
confusionMatrix(rf.pred4, df.cl4.valid$WageRange)

#### random forest 5 ####
df.rf5 = randomForest(WageRange ~ ., data = df.cl5.train, ntree = 10000)
rf.pred5 = predict(df.rf5, type = "class")  # training
confusionMatrix(rf.pred5, df.cl5.train$WageRange)
rf.pred5 = predict(df.rf5, df.cl5.valid, type = "class")    # validation
confusionMatrix(rf.pred5, df.cl5.valid$WageRange)

#### random forest 6 ####
df.rf6 = randomForest(WageRange ~ ., data = df.cl6.train, ntree = 10000)
rf.pred6 = predict(df.rf6, type = "class")  # training
confusionMatrix(rf.pred6, df.cl6.train$WageRange)
rf.pred6 = predict(df.rf6, df.cl6.valid, type = "class")    # validation
confusionMatrix(rf.pred6, df.cl6.valid$WageRange)
