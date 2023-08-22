# Text-mining-and-sentiment-analysis
This is a project of a text analysis using R and some specific code

1) Download file frComposteOpSy.RData. This will be useful when you need to break down the text from compound forms.
2) Download file utility.R. It contains some functions as:
   
# search_reddit      search reddit using RedditExtractoR
# search_reddit2     search reddit using RedditExtractoR, selecting from subreddit without keywords, and UTF-8 encoding of txt field
# reddit.users.info  return information about reddit users
# data2timestamp     convert date to timestamp
# timestamp2data     convert timestamp to date
# det.lang.2         determine if text is in the selected language
# search.Pushshift   search reddit using pushshiftR
# cleanText          text cleaning
# corNGram           modify compound forms based on dictionary (Italian only)
# visNGram           visualize n-grams to identify compound forms
# corMultWord        correction of compound forms from Excel file
# corFrmComp         modify compound forms from correction vector
# multWordPOS        find compound forms based on grammatical analysis
# lemmaUDP           lemmatization
# make.tweet.df      create a data.frame from the result of a function search_tweets or get_timeline of rtweet
# my_get_timeline    get_timeline for multiple twitter users

4) Download diz_polarity.RData, IT_stopwwords.RData, mySntIT.RData, SentimentFunctions.R for sentiment analysis.
5) Download file dfRecHoEoFw.Rdata . There is the data.frame dfRecHoEoFw which contains 900 reviews of three
 telecommunications and internet service companies: Ho-mobile, Eolo, Fastweb. For each review, the complete text
 (text field), date (date), the company it refers to (company), and a rating from 1 to 5 (vote) are provided.

6) download treebank
italian-vit-ud-2.5-191206.udpipe

7) Use library:
library(RedditExtractoR)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rtweet)
library(tm)
library(udpipe)
library(ggraph)
library(igraph)
library(textplot)
library(pushshiftR)
library(wordcloud)
library(RColorBrewer)
library(proxy)
library(factoextra)
library(ngram)
library(igraph)
library(proxy)
library(rpart)









