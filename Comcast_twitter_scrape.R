library(twitteR)
library(ROAuth)
library(httr)
library (ggplot2)
library(plyr)
library(stringr)

#API Keys go to https://apps.twitter.com/ to acess
api_key <- "xxxxxxxxxxxx"
api_secret <- "xxxxxxxxxx"
access_token <- "xxxxxxxxxx"
access_token_secret <- "xxxxxxxxxx"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Time to grab the latest tweets and determine the amount you want
tweets_comcast <- searchTwitter('@comcast', n=1500)

feed_comcast= laply(tweets_comcast, function(t) t$getText())

# Read in dictionary of positive and negative works credit to 
#http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
yay = scan('pos_words.txt',
           what='character', comment.char=';')
boo = scan('neg_words.txt',
           what='character', comment.char=';')
bad_text = c(boo, 'wtf', 'crashing', 'expensive')
good_text = c(yay, 'upgrade', ':)', 'fast')

score.sentiment = function(sentences, good_text, bad_text, .progress='none')
{
  scores = laply(sentences, function(sentence, good_text, bad_text) {
    
    # use gsub to clean
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub("\\D|\\d{3,}", '', sentence)
    #to remove emojis
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    sentence = tolower(sentence)        
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, "^\\s+|\\s+$")
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, good_text)
    neg.matches = match(words, bad_text)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, good_text, bad_text, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

comcastfeels <- score.sentiment(feed_comcast, good_text, bad_text, 
                               .progress='text')
##I noticed a ton of tutorials left out actually scanning the positive and neg sentiments! that's this command below

comcastdf2 <- score.sentiment(feed_comcast, boo, yay, .progress='text')

qplot(factor(score), data=comcastdf2, geom="bar", 
      xlab = "Sentiment Score")

pl=ggplot(comcastdf2, aes(x=comcastdf2$score))+geom_bar(fill='white', alpha=0.75, color='dodgerblue2')
+theme_fivethirtyeight()+theme_fivethirtyeight()+ggtitle('@comcast tweets sentiment analysis')

pl2=pl+ labs(title = "Comcast Twitter Sentiment Analysis",
             subtitle = "1500 tweets to @comcast on 10/22/2017",
             caption = "Source: Twitter API", 
             x = "score") 