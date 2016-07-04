source("sentiment.r")
vcsSomeText <- readLines("tweets.txt")
# clean data
# remove retweet entities
vcsSomeText = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", vcsSomeText)
# remove at twitter people
vcsSomeText = gsub("@\\w+", "", vcsSomeText)
# remove punctuation
vcsSomeText = gsub("[[:punct:]]", "", vcsSomeText)
# remove numbers
vcsSomeText = gsub("[[:digit:]]", "", vcsSomeText)
# remove html links
vcsSomeText = gsub("http\\w+", "", vcsSomeText)
# remove unnecessary spaces
vcsSomeText = gsub("[ \t]{2,}", "", vcsSomeText)
vcsSomeText = gsub("^\\s+|\\s+$", "", vcsSomeText)
# lower case using try.error with sapply
vcsSomeText = tolower(vcsSomeText)

dfrPolarity <- classify_polarity(vcsSomeText)
dfrEmotions <- classify_emotions(vcsSomeText)

vcsEmotions <- dfrEmotions[,7]
vcsPolarity <- dfrPolarity[,4]
dfrSentAnal <- data.frame(text=vcsSomeText, emotions=vcsEmotions,
                          polarity=vcsPolarity, stringsAsFactors=FALSE)

dfrSentAnal.Tmp <- filter(dfrSentAnal, emotions != "unknown")
# plot distribution of emotions
ggplot(dfrSentAnal.Tmp, aes(x=emotions)) +
  geom_bar(stat="count", aes(colur=emotions, fill=emotions)) +
  labs(title="Sentiment Analysis of Tweets\nClassification by Emotions") +
  labs(x="Emotion Categories") +
  labs(y="Frequency Count")

# plot distribution of polarity
ggplot(dfrSentAnal, aes(x=polarity)) +
  geom_bar(stat="count", aes(colur=polarity, fill=polarity)) +
  labs(title="Sentiment Analysis of Tweets\nClassification by Polarity") +
  labs(x="Polarity") +
  labs(y="Frequency Count")

table(dfrSentAnal.Tmp$emotions)
table(dfrSentAnal.Tmp$polarity)
