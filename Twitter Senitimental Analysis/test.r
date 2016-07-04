# test.r
# r script to test sentiment.r

# clear environment & history
rm(list = ls())
write("", file=".blank")
loadhistory(".blank")
unlink(".blank")

# if not already installed
# install.packages("tm")
# install.packages("NLP")
# install.packages("Rstem")
# install.packages("RColorBrewer")

# load library
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# load sentiment.r
source("sentiment.r")

# define some text to check
vcsSomeText <- c("The quick brown fox jumps over lazy dog")
# check sentiment
dfrPolarity <- classify_polarity(vcsSomeText)
dfrEmotions <- classify_emotions(vcsSomeText)

# read file
vcsSomeText <- readLines("data/tweets.txt")
head(vcsSomeText)
length(vcsSomeText)

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

# check sentiment
dfrPolarity <- classify_polarity(vcsSomeText)
dfrEmotions <- classify_emotions(vcsSomeText)

# store results in vectors
vcsEmotions <- dfrEmotions[,7]
vcsPolarity <- dfrPolarity[,4]
# also store result data frame
dfrSentAnal <- data.frame(text=vcsSomeText, emotions=vcsEmotions,
                         polarity=vcsPolarity, stringsAsFactors=FALSE)


# sort data frame
dfrSentAnal <- arrange(dfrSentAnal, emotions)

# plot distribution of emotions
ggplot(dfrSentAnal, aes(x=emotions)) +
    geom_bar(stat="count", aes(colur=emotions, fill=emotions)) +
    labs(title="Sentiment Analysis of Tweets\nClassification by Emotions") +
    labs(x="Emotion Categories") +
    labs(y="Frequency Count")

#
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

# separate the text by emotions
vcsEmoWords <- levels(factor(dfrSentAnal$emotions))
vciEmoCount <- length(vcsEmoWords)
vcsEmoCorps <- rep("", vciEmoCount)
for (i in 1:vciEmoCount)
{
    tmp = vcsSomeText[vcsEmotions == vcsEmoWords[i]]
    vcsEmoCorps[i] = paste(tmp, collapse=" ")
}

# remove stopwords
vcsEmoCorps = removeWords(vcsEmoCorps, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(vcsEmoCorps))
tdmEmoWords = TermDocumentMatrix(corpus)
tdmEmoWords = as.matrix(tdmEmoWords)
colnames(tdmEmoWords) = vcsEmoWords

# visualize the words with a comparison cloud
# comparison word cloud
comparison.cloud(tdmEmoWords, colors=brewer.pal(vciEmoCount, "Dark2"),
                 scale=c(2,.5), random.order=FALSE, title.size=1)
