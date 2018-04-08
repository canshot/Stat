library(tidytext)
library(dplyr)
library(stringr)

View(book1)

review_words1 <- book1 %>%
  select(X, dates, ratings, formats, titles, texts) %>%
  unnest_tokens(word, texts) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

View(review_words1)

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

reviews_sentiment1 <- review_words1 %>%
  inner_join(AFINN, by = "word") %>%
  group_by(X, dates, ratings, formats) %>%
  summarize(sentiment = mean(afinn_score))

View(reviews_sentiment1)

library(ggplot2)
theme_set(theme_bw())
ggplot(reviews_sentiment1, aes(formats, sentiment, group = formats)) +
  geom_boxplot() + 
  ylab("Average sentiment score") + 
  xlab("Format") + 
  ggtitle("Sentiment Score by Format [Title: Lincoln in the Bardo]")

#Simple Linear Regression 

lm.fit = lm(reviews_sentiment1$ratings~reviews_sentiment1$sentiment)
summary(lm.fit)
coef(lm.fit)
confint(lm.fit)
abline(lm.fit, lwd=3)
