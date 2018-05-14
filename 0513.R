library(tidytext)
library(dplyr)
library(stringr)

##Read csv file and create book1 dataset 
book1 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf1.csv", stringsAsFactors = FALSE)

######Character Count######
library(qdap)

#Character count and add the column
book1.cc = character_count(book1$texts)
book1$count = as.numeric(book1.cc)

#Count the number of each format
table(book1$formats)

#Extract the subset of Hardcover format from book1 
hard1 = subset(book1, formats == "Hardcover")
kindle1 = subset(book1, formats == "Kindle Edition")
audible1 = subset(book1, formats == "Audible Audio Edition")
paper1 = subset(book1, formats == "Paperback")
audio1 = subset(book1, formats == "Audio CD")

#Create histogram of formats
library(ggplot2)

theme_set(theme_bw())
ggplot(book1, aes(x=formats)) +
  geom_bar() + 
  ylab("number") + 
  xlab("format") + 
  ggtitle("[Book1] The Number of Book Format")

######Manipulate "date"######

# %Y: 4-digit year (1982), %y: 2-digit year (82), %m: 2-digit month (01), %d: 2-digit day of the month (13), 
# %A: weekday (Wednesday)%a: abbreviated weekday (Wed), %B: month (January), %b: abbreviated month (Jan)

book1$dates = as.Date(book1$dates, format = "%B %d, %Y")
hard1$dates = as.Date(hard1$dates, format = "%B %d, %Y")
kindle1$dates = as.Date(kindle1$dates, format = "%B %d, %Y")
audible1$dates = as.Date(audible1$dates, format = "%B %d, %Y")
paper1$dates = as.Date(paper1$dates, format = "%B %d, %Y")
audio1$dates = as.Date(audio1$dates, format = "%B %d, %Y")

#order by date

library(dplyr)
hard1=arrange(hard1, dates)
kindle1=arrange(kindle1, dates)
audible1=arrange(audible1, dates)
paper1=arrange(paper1, dates)
audio1=arrange(audio1, dates)

#Character count by each format

hard1.count = character_count(hard1$texts)
#delete outlier
#hard1.small = after deleting outlier
hard1.small=hard1[-243,]
hard1.cc = character_count(hard1.small$texts)

kindle1.cc=character_count(kindle1$texts)
audible1.cc=character_count(audible1$texts)
paper1.cc=character_count(paper1$texts)
audio1.cc=character_count(audio1$texts)

hard1.cc.length = length(hard1.cc)
hard1.cc.sum = sum(hard1.cc)
hard1.cc.mean = mean(hard1.cc)
hard1.cc.sd = sd(hard1.cc)
hard1.cc.df = data.frame(hard1.cc.length,hard1.cc.sum,hard1.cc.mean,hard1.cc.sd)

kindle1.cc.length = length(kindle1.cc)
kindle1.cc.sum = sum(kindle1.cc)
kindle1.cc.mean = mean(kindle1.cc)
kindle1.cc.sd = sd(kindle1.cc)

audible1.cc.length = length(audible1.cc)
audible1.cc.sum = sum(audible1.cc)
audible1.cc.mean = mean(audible1.cc)
audible1.cc.sd = sd(audible1.cc)

paper1.cc=character_count(paper1$texts)
paper1.cc.length = length(paper1.cc)
paper1.cc.sum = sum(paper1.cc)
paper1.cc.mean = mean(paper1.cc)
paper1.cc.sd = sd(paper1.cc)

audio1.cc=character_count(audio1$texts)
audio1.cc.length = length(audio1.cc)
audio1.cc.sum = sum(audio1.cc)
audio1.cc.mean = mean(audio1.cc)
audio1.cc.sd = sd(audio1.cc)

#create character count dataframe - book1.cc.df
book1.cc.df = cbind(hard1.cc.length, hard1.cc.sum, hard1.cc.mean, hard1.cc.sd, 
                    kindle1.cc.length, kindle1.cc.sum, kindle1.cc.mean, kindle1.cc.sd,
                    audible1.cc.length, audible1.cc.sum, audible1.cc.mean, audible1.cc.sd,
                    paper1.cc.length, paper1.cc.sum, paper1.cc.mean, paper1.cc.sd,
                    audio1.cc.length, audio1.cc.sum, audio1.cc.mean, audio1.cc.sd)

#create plot of character count

qplot(hard1$dates, hard1$count, 
      xlab = "date",
      ylab = "character count", 
      main = "[Book1] Hardcover format")

View(hard1)

qplot(kindle1$dates, kindle1$count, 
     xlab = "date",
     ylab = "character count", 
     main = "[Book1] Kindle format")

qplot(audible1$dates, audible1$count, 
     xlab = "date",
     ylab = "character count", 
     main = "[Book1] Audible format")

qplot(paper1$dates, paper1$count, 
     xlab = "date",
     ylab = "character count", 
     main = "[Book1] Paper format")

qplot(audio1$dates, audio1$count, 
     xlab = "date",
     ylab = "character count", 
     main = "[Book1] Audio format")

#Numeric ratings 

qplot(hard1$dates, hard1$ratings, 
     xlab = "date",
     ylab = "rating", 
     main = "[Book1] Hardcover format")

qplot(kindle1$dates, kindle1$ratings, 
     xlab = "date",
     ylab = "rating", 
     main = "[Book1] Kindle format")

qplot(audible1$dates, audible1$ratings, 
     xlab = "date",
     ylab = "rating", 
     main = "[Book1] Audible format")

qplot(paper1$dates, paper1$ratings, 
     xlab = "date",
     ylab = "rating",
     main = "[Book1] Paper format")

qplot(audio1$dates, audio1$ratings, 
     xlab = "date",
     ylab = "rating", 
     main = "[Book1] Audio format")

#sentiment analysis
library(tidyverse)
library(tidytext)
library(lubridate)
library(dplyr)
library(plyr)

review_words1 <- book1 %>%
  select(X, dates, ratings, formats, titles, texts, count) %>%
  unnest_tokens(word, texts) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

View(review_words1)

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn.score = score)

sentiment1 <- review_words1 %>%
  inner_join(AFINN, by = "word") %>%
  group_by(X, dates, ratings, formats) %>%
  summarise(sentiment = mean(afinn.score)) %>%
  mutate(method = "AFINN")

View(sentiment1)

#Merge sentiment and book1
sentiment1.merge = merge(sentiment1, book1, by = "X")
sentiment1.merge$ratings.y = NULL
sentiment1.merge$dates.y = NULL
sentiment1.merge$formats.y = NULL
sentiment1.merge$method = NULL

colnames(sentiment1.merge)[which(names(sentiment1.merge) == "dates.x")] <- "dates"
colnames(sentiment1.merge)[which(names(sentiment1.merge) == "ratings.x")] <- "ratings"
colnames(sentiment1.merge)[which(names(sentiment1.merge) == "formats.x")] <- "formats"

View(sentiment1.merge)
qplot(sentiment1.merge$sentiment, sentiment1.merge$ratings)

#order 'review_sentiment1' by date
# %Y: 4-digit year (1982), %y: 2-digit year (82), %m: 2-digit month (01), %d: 2-digit day of the month (13), 
# %A: weekday (Wednesday), %a: abbreviated weekday (Wed), %B: month (January), %b: abbreviated month (Jan)

reviews_sentiment1$dates = as.Date(reviews_sentiment1$dates, format = "%B %d, %Y")
review_sentiment1.dates=arrange(reviews_sentiment1, dates)

View(review_sentiment1.dates)

#sentiment & ratings by month

review_sentiment1.dates$dates <- floor_date(review_sentiment1.dates$dates, "month")
review_sentiment1.month = ddply(review_sentiment1.dates, "dates", summarise, 
                                sentiment = mean(sentiment), 
                                ratings = mean(ratings))

#allign setiment & rating with sales
review_sentiment1.month=review_sentiment1.month[-c(1:3),]

#plot of sentiment by month 
library(ggplot2)
theme_set(theme_bw())
ggplot(review_sentiment1.month, aes(dates, sentiment, group = dates)) +
  geom_density() + 
  ylab("Sentiment scores") + 
  xlab("Dates") + 
  ggtitle("Sentiment Score by Month")

#plot of ratings by month 
library(ggplot2)
theme_set(theme_bw())
ggplot(review_sentiment1.month, aes(dates, ratings, group = dates)) +
  geom_density() + 
  ylab("Ratings") + 
  xlab("Dates") + 
  ggtitle("Ratings by Month")

#sales
sales1 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/sales1.csv", stringsAsFactors = FALSE)

sales1 = sales1[-15,]

mean(sales1$sales)

theme_set(theme_bw())
ggplot(sales1, aes(date, sales, group = date)) +
  geom_density() + 
  ylab("Sales (book counts sold)") + 
  xlab("Date") + 
  ggtitle("[Book1] Sales by Month")

#column name cleansing
colnames(sentiment1.merge) = c("id","date","rating", "format", "sentiment", "title", "text", "count")
book1.final = sentiment1.merge[-c(6,7)]
View(book1.final)

########## Linear Regression ########## 

#3.6.2 Simple Linear Regression (sentiment on rating)
lm.fit1 = lm(rating~sentiment,data = book1.final)
summary(lm.fit1)

coef(lm.fit1)
confint(lm.fit1)

plot(book1.final$rating,book1.final$sentiment,
     pch=20, col="black",cex = 1,
     main="Simple Linear Regresion of SENTIMENT & RATING",
     ylab = "sentiment", xlab = "rating")
abline(lm.fit1, col="red")

par(mfrow=c(2,2))
plot(lm.fit1)

plot(predict(lm.fit1), residuals(lm.fit1))
plot(predict(lm.fit1), rstudent(lm.fit1))
plot(hatvalues(lm.fit1))

which.max(hatvalues(lm.fit1))

#3.6.3 Multiple Linear Regression 
lm.fit1.multi = lm(rating~sentiment+count,data = book1.final)
summary(lm.fit1.multi)

lm.fit1.all = lm(rating~.,data = book1.final)
summary(lm.fit1.all)

#3.6.4 Interaction Terms
lm.fit1.inter = lm(rating~sentiment*count,data = book1.final)
summary(lm.fit1.inter)

#3.6.5 Non-linear Transformations of the Predictors
lm.fit1.nonlinear = lm(rating~sentiment+I(sentiment^2), data = book1.final)
summary(lm.fit1.nonlinear)

anova(lm.fit1, lm.fit1.nonlinear)










#The lm of sentiment on sales
View(review_sentiment1.month)
lm.fit.sales1 = lm(sales1$sales~review_sentiment1.month$sentiment)
lm.fit.sales1
summary(lm.fit.sales1)
names(lm.fit.sales1)
coef(lm.fit.sales1)
confint(lm.fit.sales1)
plot(review_sentiment1.month$sentiment,sales1$sales,
     pch=20, col = "red", cex = 2,
     main="Simple Linear Regresion of SALES1 & SENTIMENT1",
     ylab = "sales", xlab = "sentiment")
abline(lm.fit.sales1, col="red")

#drawing confidence interval plot
plot(coef(lm.fit.sales1), ylim=range(confint(lm.fit.sales1)))
y = coef(lm.fit.sales1)
x = seq_along(y)
ci = confint(lm.fit.sales1)
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

#The lm of ratings on sales
lm.fit.sales1.ratings = lm(sales1$sales~review_sentiment1.month$ratings)
lm.fit.sales1.ratings
summary(lm.fit.sales1.ratings)
names(lm.fit.sales1.ratings)
coef(lm.fit.sales1.ratings)
confint(lm.fit.sales1.ratings)
plot(review_sentiment1.month$ratings,sales1$sales,
     pch=20, col = "red", cex = 2, 
     main="Simple Linear Regresion of SALES1 & RATING1",
     ylab = "Sales", xlab = "Ratings")
abline(lm.fit.sales1.ratings, col="red")

#Multiple lm 
lm.fit.multi1 = lm(sales1$sales~review_sentiment1.month$sentiment+review_sentiment1.month$ratings)
lm.fit.multi1
summary(lm.fit.multi1)
par(mfrow = c(2, 2))
plot(lm.fit.multi1, pch=15, col="blue")

#https://drsimonj.svbtle.com/visualising-residuals


##############Logistic Regression / page 155###############

#4.6.1
names(book1.final)
dim(book1.final)
summary(book1.final)
cor(book1.final[,-c(1,2,4,6,7)])

library(corrplot)
corrplot(cor(book1.final[,-c(1,2,4)]), method = "circle")
corrplot.mixed(cor(book1.final[,-c(1,2,4)]),lower.col = "black", number.cex = .7)

#create a dummy for digital book(Kindle Edition). If Kindle format = "digital", otherwise = "physical" 

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


#8.3.1 Fitting Classification Trees

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



