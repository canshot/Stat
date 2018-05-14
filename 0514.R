#Data preparation 

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

#Create combined dataframe
library(reshape)
book=merge_recurse(list(book1,book2,book3,book4,book5,book6))
book$id <- seq.int(nrow(book))
book$X = NULL

#Column order change
book = book[c("id", "ratings", "dates","formats", "titles","texts")]
View(book)

#Order by date
library(dplyr)
book = arrange(book,dates)

#grid table
tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
grid.table(head(book),theme=tt)

###### Book review text character count ######
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
      main = "Mean of Character Count by Date")
###############################################

############# Sentiment analysis ##############
library(dplyr)
library(tidyverse)
library(tidytext)
library(lubridate)
library(plyr)

review_words <- book %>%
  select(id,ratings,dates,formats,titles,texts,count) %>%
  unnest_tokens(word, texts) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

View(review_words)

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn.score = score)

sentiment <- review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(id, ratings, dates, formats) %>%
  summarise(sentiment = mean(afinn.score)) %>%
  mutate(method = "AFINN")

sentiment.date <- review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(id, ratings, dates, formats) %>%
  summarise(sentiment = mean(afinn.score)) %>%
  mutate(method = "AFINN") #same with 'sentiment'dateset

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

############ Descriptive Statistics ###############

#Count the number of each format
table(book.final$format)
40+19+715+1369+102 #2245

#Create the histogram of each format
library(ggplot2)

theme_set(theme_bw())
ggplot(book.final, aes(x=format)) +
  geom_bar() + 
  ylab("number") + 
  xlab("format") + 
  ggtitle("The Number of Book Format")

#[plot]character count by date
qplot(book.final$date, book.final$count, 
      xlab = "date",
      ylab = "character count", 
      main = "Character Count by Date")

#[plot]sentiment by date
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
      geom = "point")

qplot(book.final$format, book.final$rating, 
      xlab = "format",
      ylab = "rating", 
      main = "Numeric rating by format",
      geom = "jitter") #to avoid overplotting

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

#############################################################
#To analyze the relationship between Google Trend and review features,
#data needs to be manipulated on a monthly basis

library(lubridate)
library(plyr)
#Google Trend by month
google$date = floor_date(google$date, "month")
google.month = ddply(google, "date", summarise, trend = sum(trend))
grid.table(google.month)

#sentiment & ratings by month

sentiment.date$dates = floor_date(sentiment.date$dates, "month")
sentiment.month = ddply(sentiment.date, "dates", summarise, 
                        sentiment = mean(sentiment), 
                        ratings = mean(ratings))
View(sentiment.month)

#allign setiment & rating with sales
sentiment.month=sentiment.month[-c(1:7),]

#plot of sentiment by month 
library(ggplot2)
theme_set(theme_bw())
ggplot(sentiment.month, aes(dates, sentiment, group = dates)) +
  geom_point() + 
  ylab("sentiment scores") + 
  xlab("date") + 
  ggtitle("Sentiment Score by Month")

#plot of ratings by month 
library(ggplot2)
theme_set(theme_bw())
ggplot(sentiment.month, aes(dates, ratings, group = dates)) +
  geom_point() + 
  ylab("rating") + 
  xlab("dates") + 
  ggtitle("Rating by Month")

########## Linear Regression ########## 

#3.6.2 Simple Linear Regression (sentiment on rating)
lm.fit = lm(rating~sentiment,data = book.final)
summary(lm.fit)

coef(lm.fit)
confint(lm.fit)

plot(book.final$rating,book.final$sentiment,
     pch=20, col="black",cex = 1,
     main="Simple Linear Regresion of SENTIMENT & RATING",
     ylab = "sentiment", xlab = "rating")
abline(lm.fit, col="red")

par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))

which.max(hatvalues(lm.fit))

#3.6.3 Multiple Linear Regression 
lm.fit.multi = lm(rating~sentiment+count,data = book.final)
summary(lm.fit.multi)

lm.fit.all = lm(rating~.,data = book.final)
summary(lm.fit.all)

#3.6.4 Interaction Terms
lm.fit.inter = lm(rating~sentiment*count,data = book.final)
summary(lm.fit.inter)

#3.6.5 Non-linear Transformations of the Predictors
lm.fit.nonlinear = lm(rating~sentiment+I(sentiment^2), data = book.final)
summary(lm.fit.nonlinear)

anova(lm.fit, lm.fit.nonlinear)

#The lm of sentiment on Google Trend
lm.fit.g = lm(google.month$trend~sentiment.month$sentiment)
lm.fit.g
summary(lm.fit.g)
names(lm.fit.g)
coef(lm.fit.g)
confint(lm.fit.g)

plot(sentiment.month$sentiment,google.month$trend,
     pch=20, col = "red", cex = 2,
     main="Simple Linear Regresion of Google Trend & Sentiment",
     ylab = "trend", xlab = "sentiment")
abline(lm.fit.g, col="red")

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

#The lm of ratings on Google Trends
lm.fit.g.r = lm(google.month$trend~sentiment.month$ratings)
lm.fit.g.r
summary(lm.fit.g.r)
names(lm.fit.g.r)
coef(lm.fit.g.r)
confint(lm.fit.g.r)
plot(sentiment.month$ratings,google.month$trend,
     pch=20, col = "red", cex = 2, 
     main="Simple Linear Regresion of Goolge Trend & Rating",
     ylab = "treand", xlab = "rating")
abline(lm.fit.g.r, col="red")

#Multiple lm 
lm.fit.g.m = lm(google.month$trend~sentiment.month$sentiment+sentiment.month$ratings)
lm.fit.g.m
summary(lm.fit.g.m)
par(mfrow = c(2, 2))
plot(lm.fit.g.m, pch=15, col="blue")

##########################################################


############## Logistic Regression ###############

#4.6.1
names(book1.final)
dim(book1.final)
summary(book1.final)
cor(book1.final[,-c(1,2,4,6,7)])

library(corrplot)
corrplot(cor(book1.final[,-c(1,2,4)]), method = "circle")
corrplot.mixed(cor(book1.final[,-c(1,2,4)]),lower.col = "black", number.cex = .7)

#Create a binary variable for classification task 
#If Kindle format = "digital", otherwise = "physical" 

#book1.dummy = as.numeric(book1.final$format == "Kindle Edition")
#book1.final$kindle = as.numeric(book1.dummy)

book1.final$digital = as.factor(ifelse(book1.final$format == "Kindle Edition", "digital", "physical"))
contrasts(book1.final$digital)

#4.6.2 Logistic Regression

#Now our data is data is ready to create the model. 
#As a first step, we will split the data into testing and training observation. 
#The data is split into 60-40 ratio 
#so there are 720 observations for training the model and 481 observation for evaluating the model.
set.seed(1)
row.number = sample(1:nrow(book1.final), 0.6*nrow(book1.final))
train1 = book1.final[row.number,]
test1 = book1.final[-row.number,]
dim(train1)
dim(test1)
contrasts(book1.final$digital)

#Building a model

glm.fit1=glm(digital~rating+sentiment+count, data=book1.final, family=binomial)
summary(glm.fit1)
coef(glm.fit1)

#find training accuracy with training data
attach(train1)
pred.prob = predict(glm.fit1, newdata = train1, type="response")
pred.prob = ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, train1$digital)
(421+49)/720 #65.2%, physical = 1, digital = 0

#find test accuracy with test data
attach(test1)
pred.prob = predict(glm.fit1, newdata = test1, type="response")
pred.prob = ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, test1$digital)
(291+36)/481 #67.9% 

#######################################################
glm.probs=predict(glm.fit1,type = "response")
glm.probs[1:10]

glm.pred = rep("physical", 1201)
glm.pred[glm.probs > 0.5] = "digital"
table(glm.pred, book1.final$digital)
(36+368)/720

mean(glm.pred == "digital")

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
lda.fit1 = lda(digital~rating+sentiment+count, data=train1)
lda.fit1
plot(lda.fit1)

#Predicting training results.
predmodel.train.lda1 = predict(lda.fit1, data=train1)
table(Predicted=predmodel.train.lda1$class, train1$digital)
(430+23)/720 #62.9%

#or to calculate the accuracy  
mean(predmodel.train.lda1$class==train1$digital)

#The plot shows how the response class has been classified by the LDA classifier. 
#The X-axis shows the value of line defined by the co-efficient of linear discriminant for LDA model. 
#The two groups are the groups for response classes.
ldahist(predmodel.train.lda1$x[,1], g= predmodel.train.lda1$class)

predmodel.test.lda1 = predict(lda.fit1, newdata=test1)
table(Predicted=predmodel.test.lda1$class, test1$digital)
(298+18)/481 #65.7%

#or to calculate the accuracy  
mean(predmodel.test.lda1$class==test1$digital)

#4.6.4 Quadratic Discriminant Analysis

attach(train)
qda.fit1 = qda(digital~rating+sentiment+count, data=train1)
qda.fit1

#Predicting training results.
predmodel.train.qda1 = predict(qda.fit1, data=train1)
table(Predicted=predmodel.train.qda1$class, train1$digital)
(424+46)/720 #65.3%

#or to calculate the accuracy  
mean(predmodel.train.qda1$class == train1$digital)

##Predicting test results.
attach(test1)
predmodel.test.qda1 = predict(qda.fit1, newdata=test1)

table(Predicted=predmodel.test.qda1$class, test1$digital)
(293+34)/481 #68.0%

#or to calculate the accuracy  
mean(predmodel.test.qda1$class==test1$digital)

#4.6.5 K-Nearest Neighbors 

#scatterplot by format
library(ggvis)

book1.final %>% 
  ggvis(~rating, ~sentiment, fill = ~format) %>% 
  layer_points()

#knn along with format
library(class)

names(train1) #find the index of "format[4]" 

cl = train1[,4,drop=TRUE] #class for knn model
testcl = test1[,4,drop=TRUE] #class for knn confusion matrix

knn1 = knn(train1[,5, drop=FALSE], test1[,5, drop=FALSE], cl, k=1) #knn model when k=1
table(knn1, testcl) #confusion matrix
(27+277)/481 #63.2%

knn1 = knn(train1[,5, drop=FALSE], test1[,5, drop=FALSE], cl, k=5) #knn model when k=5
table(knn1, testcl) #confusion matrix
(25+282)/481 #57.4%

knn1 = knn(train1[,5, drop=FALSE], test1[,5, drop=FALSE], cl, k=10) #knn model when k=10
table(knn1, testcl) #confusion matrix
(18+288)/481 #63.6%

k = sqrt(nrow(train1))
knn1 = knn(train1[,5, drop=FALSE], test1[,5, drop=FALSE], cl, k) #knn model when k=sqrt of nrow of train1
table(knn1, testcl) #confusion matrix
(6+299)/481 #63.4%

#########################################################
# Finding best "K" 
library(caret)
library(e1071)
model <- train(
  format~., data = train1, method = "knn",
  trControl = trainControl("cv", number = 5),
  preProcess = c("center","scale"),
  tuneLength = 10)
plot(model) #plot of accuracy

# Print the best tuning parameter k that maximizes model accuracy
model$bestTune #best K = 23

knn1 = knn(train1[,5, drop=FALSE], test1[,5, drop=FALSE], cl, k=) #knn model when k=9
table(knn1, testcl) #confusion matrix
(27+273)/481 #62.4%
#########################################################

#scatterplot by digital
library(ggvis)

book1.final %>% 
  ggvis(~rating, ~sentiment, fill = ~digital) %>% 
  layer_points()

#knn along with format
library(class)

names(train1) #find the index of "digital[7]" 

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

#########################################################
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


#8.3.1 Fitting Regression Trees

library(tree)
tree.rating = tree(rating~.-id,book1.final)
summary(tree.rating)
plot(tree.rating)
text(tree.rating,pretty=0)
title("Classification Tree for 'rating'")

#Evaluating the performance of the regression tree
set.seed(2)
row.number = sample(1:nrow(book1.final), 0.6*nrow(book1.final))
train1.tree = book1.final[row.number,]
tree.book1 = tree(rating~.-id, book1.final, subset = unlist(train1.tree))
summary(tree.book1)
plot(tree.book1)
text(tree.book1,pretty=0)

cv.book1 = cv.tree(tree.book1)
plot(cv.book1$size, cv.book1$dev, type = 'b')
prune.book1 = prune.tree(tree.book1, best = 5)

plot(prune.book1)
text(prune.book1,pretty=0)

#Prediction on the test set
yhat = predict(tree.book1, newdata = book1.final[-row.number,])
test1.tree = book1.final[-row.number,"sentiment"]
plot(yhat, test1.tree)
abline(0,1)
mean((yhat-test1.tree)^2) #13.3

#8.3.3 Bagging and Random Forest
library(randomForest)
library(party)
set.seed(1)
bag.book1 = randomForest(rating ~ sentiment+count,data = book1.final) 
bag.book1
plot(bag.book1)

bag.book2 = randomForest(digital ~ rating+sentiment+count,data = book1.final) 
bag.book2
print(bag.book2)
print(importance(bag.book2,type = 2))
varImpPlot(bag.book2,type=2)

#8.3.4 Boosting
library(gbm)
set.seed(1)
boost.book1 = gbm(rating~.-id-date-format, data = book1.final, 
                  distribution = "gaussian",
                  n.trees = 5000, 
                  interaction.depth = 4)
summary(boost.book1)

par(mfrow=c(1,2))
plot(boost.book1, i = "sentiment")
plot(boost.book1, i = "count")

#9.6.1 Support Vector Classifier

library(e1071)
svmfit = svm(count~rating, data = book.final, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit)
summary(svmfit)







