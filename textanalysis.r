# Word Cloud of Injury Description from Claims
# Source: https://datascienceplus.com/building-wordclouds-in-r/
# Source: http://onepager.togaware.com/TextMiningO.pdf

# install following packages, if they are not already installed
#   tm
#   SnowballC
#   wordcloud
#   RColorBrewer

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# load the above packages

# read the csv file ClaimsInjuryDescription.csv (download from Canvas before reading)
# while reading, UNCHECK the StringsAsFactors box

claimsData <- ClaimsInjuryDescription

# the following commands convert the description column into corpus
# and use functions from tm package to prepare for the word cloud

incidentCorpus <- Corpus(VectorSource(claimsData$IncidentDescription))
incidentCorpus <- tm_map(incidentCorpus, content_transformer(tolower))
incidentCorpus <- tm_map(incidentCorpus, removePunctuation)
incidentCorpus <- tm_map(incidentCorpus, PlainTextDocument)
incidentCorpus <- tm_map(incidentCorpus, removeWords, stopwords('english'))
incidentCorpus <- tm_map(incidentCorpus, stemDocument)
incidentCorpus <- Corpus(VectorSource(incidentCorpus))

wordcloud(incidentCorpus, max.words = 25, 
          random.order = FALSE, 
          colors = brewer.pal(6, "Dark2"))

#Data = Jeopardy_CSV

jeocorpus = Corpus(VectorSource(jeo$Question))
jeocorpus = tm_map(jeocorpus, content_transformer(tolower))
jeocorpus = tm_map(jeocorpus, removePunctuation)
jeocorpus = tm_map(jeocorpus, PlainTextDocument)
jeocorpus = tm_map(jeocorpus, removeWords, stopwords('english'))
jeocorpus = tm_map(jeocorpus, stemDocument)
jeocorpus = Corpus(VectorSource(jeocorpus))

wordcloud(jeocorpus, max.words = 50, random.order = FALSE, 
          colors = brewer.pal(4, "Dark2"))




