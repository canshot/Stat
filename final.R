#Raw Data Manipulation 

library(tidytext)
library(dplyr)
library(stringr)
library(gridExtra) #grid table
library(ggplot2)
library(stats)

#Read six csv files 
book1 = read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf1.csv", stringsAsFactors = FALSE)
book2 = read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf2.csv", stringsAsFactors = FALSE)
book3 = read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf3.csv", stringsAsFactors = FALSE)
book4 = read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf4.csv", stringsAsFactors = FALSE)
book5 = read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf5.csv", stringsAsFactors = FALSE)
book6 = read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf6.csv", stringsAsFactors = FALSE)

#Date format identification 
#%Y: 4-digit year (1982), %y: 2-digit year (82), %m: 2-digit month (01), %d: 2-digit day of the month (13), 
#%A: weekday (Wednesday)%a: abbreviated weekday (Wed), %B: month (January), %b: abbreviated month (Jan)
book1$dates = as.Date(book1$dates, format = "%B %d, %Y")
book2$dates = as.Date(book2$dates, format = "%d-%b-%y")
book3$dates = as.Date(book3$dates, format = "%d-%b-%y")
book4$dates = as.Date(book4$dates, format = "%d-%b-%y")
book5$dates = as.Date(book5$dates, format = "%d-%b-%y")
book6$dates = as.Date(book6$dates, format = "%d-%b-%y")

#Google Trend of "Man Booker" over time

#Google Trend represents search interest relative to the highest point 
#on the chart for the given region and time. A value of 100 is 
#the peak popularity for the term. A value of 50 means that 
#the term is half as popular. 

google = read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/googletrend.csv", stringsAsFactors = FALSE)
grid.table(head(google))
grid.table(tail(google))

#Create combined dataframe
library(reshape)
book=merge_recurse(list(book1,book2,book3,book4,book5,book6))
book$id <- seq.int(nrow(book))
book$X = NULL

#Column order change
book = book[c("id", "ratings", "dates","formats", "titles","texts")]

#Order by date
library(dplyr)
book = arrange(book,dates)

#Book review text character count
library(qdap)

#Count the number of character and create the responding column
book$count = as.numeric(character_count(book$texts))

#Count the number of character by format
library(dplyr)
count.format = book %>%
  group_by(formats) %>%
  summarise(sum=sum(count), mean=mean(count), sd=sd(count))

grid.table(count.format)

qplot(count.format$formats, count.format$mean, 
      xlab = "format",
      ylab = "mean",
      geom=c("point","line"),
      main = "Mean of Character Count by Format")

############# Sentiment analysis ##############
library(dplyr)
library(tidyverse)
library(tidytext)
library(lubridate)
library(plyr)

review_words = book %>%
  select(id,ratings,dates,formats,titles,texts,count) %>%
  unnest_tokens(word, texts) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

AFINN = sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn.score = score)

library(tidyverse)                
sentiment = review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(id, ratings, dates, formats) %>%
  summarise(sentiment = mean(afinn.score)) %>%
  mutate(method = "AFINN")

#Creating another dataset for Google Trend task
sentiment.date = review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(id, ratings, dates, formats) %>%
  summarise(sentiment = mean(afinn.score)) %>%
  mutate(method = "AFINN") 

########################################

#Merge sentiment dataframe with book dataframe 
sentiment.merge = merge(sentiment, book, by = "id")

#Delete the overlapped column
sentiment.merge$ratings.y = NULL
sentiment.merge$dates.y = NULL
sentiment.merge$formats.y = NULL
sentiment.merge$method = NULL

#Change the column name
colnames(sentiment.merge)[which(names(sentiment.merge) == "dates.x")] <- "dates"
colnames(sentiment.merge)[which(names(sentiment.merge) == "ratings.x")] <- "ratings"
colnames(sentiment.merge)[which(names(sentiment.merge) == "formats.x")] <- "formats"
colnames(sentiment.merge) = c("id","rating","date", "format", "sentiment", "title", "text", "count")

#create a final dataset 
book.final = sentiment.merge[-c(6,7)] #2,245 observations
View(book.final)
dim(book.final)
summary(book.final)

############ Descriptive Statistics ###############

#Count the number of each format
table(book.final$format)

#Create the histogram of each format
library(ggplot2)

theme_set(theme_bw())
ggplot(book.final, aes(x=format)) +
  geom_bar() + 
  ylab("number") + 
  xlab("format") + 
  ggtitle("The Number of Book Format")

#[plot] character count by date
qplot(book.final$date, book.final$count, 
      xlab = "date",
      ylab = "character count", 
      main = "Character Count by Date")

#[plot] sentiment by date
qplot(book.final$date, book.final$sentiment, 
      xlab = "date",
      ylab = "sentiment", 
      main = "Sentiment by Date",
      geom = "line")

#[plot]rating by date 
qplot(book.final$date, book.final$rating, 
      xlab = "date",
      ylab = "numeric rating", 
      main = "Numeric Rating by Date",
      geom = "point")

qplot(book.final$date, book.final$rating, 
      xlab = "date",
      ylab = "numeric rating", 
      main = "Numeric Rating by Date",
      geom = "jitter") #to avoid overplotting

#[plot]character count by format
qplot(book.final$format, book.final$count, 
      xlab = "format",
      ylab = "character count", 
      main = "Character Count by format",
      geom = "point")

qplot(book.final$format, book.final$count, 
      xlab = "format",
      ylab = "character count", 
      main = "Character Count by format",
      geom = "jitter")

#What is jitter? 
#It adds a small amount of random variation to the 
#location of each point, and is a useful way of handling 
#overplotting caused by discreteness in smaller datasets.

#[plot]rating by format
qplot(book.final$format, book.final$rating, 
      xlab = "format",
      ylab = "rating", 
      main = "Numeric rating by format",
      geom = "jitter")

#[plot]sentiment by format
qplot(book.final$format, book.final$sentiment, 
      xlab = "format",
      ylab = "sentiment", 
      main = "The degree of sentiment by format",
      geom = "point")

qplot(book.final$format, book.final$sentiment, 
      xlab = "format",
      ylab = "sentiment", 
      main = "The degree of sentiment by format",
      geom = "boxplot") #to see the median and quartiles in a clear way

#[plot]Google Trend
google$date <- as.Date(google$date, '%m/%d/%Y')
require(ggplot2)
ggplot(data=google, aes(date,trend)) + geom_line() + ggtitle("Google Trend of 'Man Booker' by Date")

#To analyze the relationship between Google Trend and review features,
#data needs to be manipulated on a monthly basis

library(lubridate)
library(plyr)
library(gridExtra)

#Google Trend by month
google$date = floor_date(google$date, "month")
google.month = ddply(google, "date", summarise, trend = sum(trend))
View(google.month)

#sentiment & ratings by month
sentiment.date$dates = floor_date(sentiment.date$dates, "month")
sentiment.month = ddply(sentiment.date, "dates", summarise, 
                        sentiment = mean(sentiment), 
                        ratings = mean(ratings))
View(sentiment.month)

#allign setiment & rating with sales
sentiment.month=sentiment.month[-c(1:7),]

#plot of sentiment by month 
require(ggplot2)
theme_set(theme_bw())
ggplot(sentiment.month, aes(dates, sentiment, group = dates)) +
  geom_point() + 
  ylab("sentiment scores") + 
  xlab("date") + 
  ggtitle("Sentiment by Month")

#plot of ratings by month 
require(ggplot2)
theme_set(theme_bw())
ggplot(sentiment.month, aes(dates, ratings, group = dates)) +
  geom_point() + 
  ylab("rating") + 
  xlab("dates") + 
  ggtitle("Rating by Month")

############ Statistical Learning ################

#3.6.2 Simple Linear Regression 

#The effect of sentiment on rating
lm.fit = lm(rating~sentiment,data = book.final)
summary(lm.fit)

coef(lm.fit)
confint(lm.fit)

plot(book.final$sentiment, book.final$rating,
     pch=20, col="black",cex = 1,
     main="Simple Linear Regresion of Sentimen & Rating",
     ylab = "rating", xlab = "sentiment")
abline(lm.fit, col="red")

par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,1))
plot(predict(lm.fit), residuals(lm.fit)) #residuals
plot(predict(lm.fit), rstudent(lm.fit)) #studentized residuals

#leverage statistics 
plot(hatvalues(lm.fit)) 
#identifying which observation has the largest leverage statistic
which.max(hatvalues(lm.fit)) 


#The effect of Google Trend on rating
lm.fit.g.r = lm(sentiment.month$ratings~google.month$trend)
summary(lm.fit.g.r)

plot(sentiment.month$ratings,google.month$trend,
     pch=20, col = "black", cex = 2, 
     main="Simple Linear Regresion of Goolge Trend & Rating",
     ylab = "treand", xlab = "rating")
abline(lm.fit.g.r, col="red")

#The effect of Google Trens on sentiment
lm.fit.g.s = lm(sentiment.month$sentiment~google.month$trend)
summary(lm.fit.g.s)
names(lm.fit.g.s)
coef(lm.fit.g.s)
confint(lm.fit.g.s)

plot(google.month$trend,sentiment.month$sentiment,
     pch=20, col = "black", cex = 2,
     main="Simple Linear Regresion of Google Trend & Sentiment",
     ylab = "sentiment", xlab = "trend")
abline(lm.fit.g, col="red")

#3.6.3 Multiple Linear Regression 

#The effect of sentiment + count on rating
lm.fit.multi = lm(rating~sentiment+count,data = book.final)
summary(lm.fit.multi)

par(mfrow = c(2, 2))
plot(lm.fit.multi, pch=15, col="blue")

#The effect of all variables on rating
lm.fit.all = lm(rating~.,data = book.final)
summary(lm.fit.all)

par(mfrow = c(2, 2))
plot(lm.fit.all, pch=15, col="blue")

#3.6.4 Interaction Terms
lm.fit.inter = lm(rating~sentiment*count,data = book.final)
summary(lm.fit.inter)

#3.6.5 Non-linear Transformations of the Predictors
lm.fit.nonlinear = lm(rating~sentiment+I(sentiment^2), data = book.final)
summary(lm.fit.nonlinear)

anova(lm.fit, lm.fit.nonlinear)#comparing two models

#drawing confidence interval plot
plot(coef(lm.fit.g), ylim=range(confint(lm.fit.g)))
y = coef(lm.fit.g)
x = seq_along(y)
ci = confint(lm.fit.g)
plot(y, ylim=range(ci))
#drawing confidence interval plot - the ci's:
arrows(x,ci[,1],x,ci[,2], code=3, angle=90, length=0.05)
##drawing confidence interval plot - improved plot
xlim = range(x) + c(-0.5,0.2)
ylim = range(ci)
ylim = ylim + 0.1*c(-1,+1)*diff(ylim) # extend it a little
ylab = bquote(hat(beta))
xlab = "coefficient"
par(mar=c(4.5,5,1,1), las=1)
plot(y, pch=16, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, xaxt="n", bty="n")
axis(1, at=x, labels=names(y), tick=FALSE)
abline(h=0, lty=3)
arrows(x,ci[,1],x,ci[,2], code=3, angle=90, length=0.05)

############## Logistic Regression ###############

#4.6.1 Corrplot & Creating binary variable

library(corrplot)
par(mfrow = c(1, 1))
corrplot(cor(book.final[,-c(1,3,4)]), method = "circle")
corrplot.mixed(cor(book.final[,-c(1,3,4)]),lower.col = "black", number.cex = .7)

#For the classification task, create a binary variable  
#If Kindle format = "digital", otherwise = "physical" 

book.final$digital = as.factor(ifelse(book.final$format == "Kindle Edition", "digital", "physical"))
contrasts(book.final$digital)

#4.6.2 Logistic Regression

#As a first step, we will split the data into testing and training observation. 
#The data is split into 60-40 ratio 
set.seed(1)
row.number = sample(1:nrow(book.final), 0.6*nrow(book.final))
train = book.final[row.number,]
test = book.final[-row.number,]
dim(train)
dim(test)
contrasts(book.final$digital)

#Building a model

glm.fit1=glm(digital~sentiment, data=book.final, family=binomial)
summary(glm.fit1)

glm.fit2=glm(digital~rating, data=book.final, family=binomial)
summary(glm.fit2)

glm.fit3=glm(digital~count, data=book.final, family=binomial)
summary(glm.fit3)

#plot for glm.fit3
plot(digital~count, 
     data = book.final,
     xlab="count", 
     ylab="digital", 
     pch=19)

#another plot
library(ggplot2)
ggplot(glm.fit3, aes(x=book.final$count, y=book.final$digital)) + geom_jitter() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)
     

#find training accuracy with training data
attach(train)
pred.prob = predict(glm.fit3, newdata = train, type="response")
pred.prob = ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, train$digital)
(761+111)/1347 

#65.2%, physical = 1, digital = 0

#find test accuracy with test data
attach(test)
pred.prob = predict(glm.fit3, newdata = test, type="response")
pred.prob = ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, test$digital)
(517+74)/898 


#######################################################
glm.probs=predict(glm.fit3,type = "response")
glm.probs[1:10]

glm.pred = rep("physical", 1201)
glm.pred[glm.probs > 0.5] = "digital"
table(glm.pred, book.final$digital)
(91+375)/720

mean(glm.pred == digital)

#training error rate = 100-66 = 34%

train = (book1.final$id<683)
book1.id683=book1.final[!train,]
dim(book1.id683)

digital.id683 = book1.final$digital[!train]

glm.fit2 = glm(book1.final$digital~rating+sentiment+count, 
               data = book1.final, family = binomial, subset = train)

glm.probs2 = predict(glm.fit2, book1.id683, type="response")

glm.pred2 = rep("physical", 621)

glm.pred2[glm.probs2 >.5] = "digital"

table(glm.pred2,digital.id683)

(328+23)/621

mean(glm.pred2 == "digital")
mean(glm.pred2 != "digital")
#######################################################


#4.6.3 Linear Discriminant Analysis

#Linear Discriminant analysis is a classification (and dimension reduction) method. 
#It finds the (linear) combination of the variables that separate the target variable classes. 
#The target can be binary or multiclass variable.

library(MASS)
lda.fit = lda(digital~rating+sentiment+count, data=train)
lda.fit
plot(lda.fit)

#Predicting training results.
predmodel.train.lda = predict(lda.fit, data=train)
table(Predicted=predmodel.train.lda$class, train$digital)
(785+70)/1347 

#or to calculate the accuracy  
mean(predmodel.train.lda$class==train$digital)

#The plot shows how the response class has been classified by the LDA classifier. 
#The X-axis shows the value of line defined by the co-efficient of linear discriminant for LDA model. 
#The two groups are the groups for response classes.
ldahist(predmodel.train.lda$x[,1], g= predmodel.train.lda$class)

#Predicting test results.
predmodel.test.lda = predict(lda.fit, newdata=test)
table(Predicted=predmodel.test.lda$class, test$digital)
(531+46)/898 

#or to calculate the accuracy  
mean(predmodel.test.lda1$class==test1$digital)

#4.6.4 Quadratic Discriminant Analysis

attach(train)
qda.fit = qda(digital~rating+sentiment+count, data=train)
qda.fit

#Predicting training results.
predmodel.train.qda = predict(qda.fit, data=train)
table(Predicted=predmodel.train.qda$class, train$digital)
(774+91)/1347

#or to calculate the accuracy  
mean(predmodel.train.qda1$class == train1$digital)

##Predicting test results.
attach(test)
predmodel.test.qda = predict(qda.fit, newdata=test)
table(Predicted=predmodel.test.qda$class, test$digital)
(523+60)/898

#or to calculate the accuracy  
mean(predmodel.test.qda1$class==test1$digital)

#####4.6.5 K-Nearest Neighbors 

#scatterplot of digital
library(ggvis)
book.final %>% 
  ggvis(~rating, ~sentiment, fill = ~digital) %>% 
  layer_points()

#scatterplot of format
library(ggvis)
book.final %>% 
  ggvis(~rating, ~sentiment, fill = ~format) %>% 
  layer_points()

#knn of format
library(class)

names(train) #to find the index[4] of "format" 

cl = train[,4,drop=TRUE] #class for knn model
testcl = test[,4,drop=TRUE] #class for knn confusion matrix

knn = knn(train[,5, drop=FALSE], test[,5, drop=FALSE], cl, k=1) #knn model when k=1
table(knn, testcl) #confusion matrix
(45+489)/898

knn5 = knn(train[,5, drop=FALSE], test[,5, drop=FALSE], cl, k=5) #knn model when k=5
table(knn5, testcl) #confusion matrix
(44+510)/898 

knn10 = knn(train[,5, drop=FALSE], test[,5, drop=FALSE], cl, k=10) #knn model when k=10
table(knn10, testcl) #confusion matrix
(41+500)/898 

k = sqrt(nrow(train))
knn.sqrt = knn(train[,5, drop=FALSE], test[,5, drop=FALSE], cl, k) #knn model when k=sqrt of nrow of train
table(knn.sqrt, testcl) #confusion matrix
(15+533)/898 

# Finding best "K" 
library(caret)
library(e1071)
model <- train(
  format~., data = train, method = "knn",
  trControl = trainControl("cv", number = 5),
  preProcess = c("center","scale"),
  tuneLength = 10)
plot(model) #plot of accuracy

# Print the best tuning parameter k that maximizes model accuracy
model$bestTune #best K = 7 

knn7 = knn(train[,5, drop=FALSE], test[,5, drop=FALSE], cl, k=7) #knn model when k=7
table(knn7, testcl) #confusion matrix
(41+511)/898 

#########################################################
#knn along with digital
library(class)

names(train1) #find the index[7] of "digital" 

cl = train1[,7,drop=TRUE] #class for knn model
testcl = test1[,7,drop=TRUE] #class for knn confusion matrix

knn1 = knn(train1[,5, drop=FALSE], test1[,5, drop=FALSE], cl, k=1) #knn model when k=1
table(knn1, testcl) #confusion matrix
(244+33)/720 #38.5%

knn1 = knn(train1[,5, drop=FALSE], test1[,5, drop=FALSE], cl, k=5) #knn model when k=5
table(knn1, testcl) #confusion matrix
(258+21)/720 #38.8%

knn1 = knn(train1[,5, drop=FALSE], test1[,5, drop=FALSE], cl, k=10) #knn model when k=10
table(knn1, testcl) #confusion matrix
(247+16)/720 #36.5%

k = sqrt(nrow(train1))
knn1 = knn(train1[,5, drop=FALSE], test1[,5, drop=FALSE], cl, k) #knn model when k=sqrt of nrow of train1
table(knn1, testcl) #confusion matrix
(265+10)/720 #38.2%


# Finding best "K" 
library(caret)
library(e1071)
model.digital <- train(
  digital~., data = train1, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 20)
plot(model)

# Print the best tuning parameter k that maximizes model accuracy
model$bestTune #best K = 5
#########################################################


#5.3.1 The Validation Set Approach 

set.seed(1)
row.number = sample(1:nrow(book.final), 0.5*nrow(book.final)) #0.5
train.valid = book.final[row.number,]
test.valid = book.final[-row.number,]

dim(test.valid)

lm.fit.valid = lm(rating~sentiment, data=train.valid) #validation set 

attach(test.valid)
mean((rating - predict(lm.fit.valid,train.valid))^2) #MSE

#The estimated test MSE for the linear regression fit is 2.57

#5.3.2 Leave-One-Out Cross-Validation 

library(boot) #for cv.glm() function
glm.fit = glm(rating~sentiment, data = book.final) #summary(glm.fit) = summary(lm.fit)

#cross-validation estimate fot the test error
cv.err = cv.glm(book.final, glm.fit)
cv.err$delta 

#LOOCV estimate = 1.817393

#5.3.3 k-Fold Cross-Validation 
cv.err.10 = cv.glm(book.final, glm.fit, K=10)
cv.err.10$delta 


#10-Fold CV estimate = 1.817851

#5.3.4 The Bootstrap

#5.3.4.1 Estimating the Accuracy of a Statistic of Interest 

alpha.fn = function(book.final,index){
  sentiment = book.final$sentiment[index]
  rating = book.final$rating[index]
  return((var(rating)-cov(sentiment,rating))/(var(sentiment) + var(rating)-2*cov(sentiment,rating)))
} #to indicate which observations should be used to estimate alpha 

alpha.fn(book.final, 1:100) 

set.seed(1)
alpha.fn(book.final, sample(100,100,replace = T))

boot(book.final, alpha.fn, R=1000) 

#using the original data, alpha = 0.3798012 
#and the bootstrap estimate for SE(alpha) is 0.01209684   

#5.3.4.2 Estimating the Accuracy of a Linear Regression Model  

boot.fn = function(book.final,index)
  return(coef(lm(rating~sentiment, data = book.final, subset = index)))

boot.fn(book.final, 1:2245)

boot(book.final, boot.fn, 1000)


#8.3.1 Fitting Regression Trees

library(tree)
tree.rating = tree(rating~.-id,book.final)

summary(tree.rating)

plot(tree.rating)
text(tree.rating,pretty=0)
title("Classification Tree for 'rating'")

#Evaluating the performance of the regression tree
set.seed(2)
row.number = sample(1:nrow(book.final), 0.6*nrow(book.final))
train.tree = book.final[row.number,]
tree.book = tree(rating~.-id, book.final, subset = unlist(train.tree))
summary(tree.book)

plot(tree.book)
text(tree.book,pretty=0)
title("Classification Tree for 'rating' with training data")

cv.book = cv.tree(tree.book)#cross-validation for optimal tree complexity  
names(cv.book)
cv.book$size
cv.book$dev

plot(cv.book$size, cv.book$dev, type = 'b')
prune.book = prune.tree(tree.book, best = 7)
plot(prune.book)
text(prune.book,pretty=0)
title("Classification Tree for 'rating' after pruning")


#Prediction on the test set
yhat = predict(tree.book, newdata = book.final[-row.number,])
test.tree = book.final[-row.number,"sentiment"]
plot(yhat, test.tree)
abline(0,1)
mean((yhat-test.tree)^2) 


#8.3.3 Bagging and Random Forest
library(randomForest)
library(party)
set.seed(1)
bag.book = randomForest(rating ~ sentiment+count,data = book.final) 
bag.book
plot(bag.book)

print(importance(bag.book,type = 2))
varImpPlot(bag.book,type=2)


bag.book2 = randomForest(digital ~ rating+sentiment+count,data = book.final) 
bag.book2
plot(bag.book2)
legend(450, 0.65, legend=c("rating", "sentiment","count"),
       col=c("green", "black", "red"), lty=1:2, cex=0.8)

print(importance(bag.book2,type = 2))
varImpPlot(bag.book2,type=2)

#8.3.4 Boosting
library(gbm)
set.seed(1)
boost.book = gbm(rating~.-id-date-format, data = book.final, 
                distribution = "gaussian",
                n.trees = 5000, 
                interaction.depth = 4)
summary(boost.book)

par(mfrow=c(1,2))
plot(boost.book, i = "sentiment")
plot(boost.book, i = "count")

#9.6.1 Support Vector Classifier

library(e1071)
svmfit = svm(digital~., data = book.final, kernel = "linear", cost = 10, scale = FALSE)
summary(svmfit)

set.seed(1)
tune.out = tune(svm, digital~., data = book.final, kernel = "linear", 
                ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)

bestmod = tune.out$best.model
summary(bestmod)
plot(svmfit, book.final)
