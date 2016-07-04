# 11-server.r
library(shiny)
library(stringr)

# IMPORTANT
# by default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

# server
shinyServer(function(input, output) {

    # header pres checkbox
    output$inpFileName <- renderUI({
        fileInput('inpFileName', 'Choose Tweets File', accept=c('text/plain', '.txt'))
    })

    # show data file selected
    output$ShowData <- renderDataTable({
        # move to local variable
        dfrFileName <- input$inpFileName
        # check for null
        if (is.null(dfrFileName))
            return(NULL)
        # open data file
        dfrDataFile <- readLines(dfrFileName$datapath[1])
        # move file to wd()
        # subject to OS level permissions
        file.rename(dfrFileName$datapath[1], dfrFileName$name[1])
        vcsSomeText <- dfrDataFile
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
        # return
        vcsX <<- vcsSomeText
        dfrTweet <<- read.table(text=vcsSomeText, sep = "\n")
        # split
        lstTweetLines <- str_split(vcsSomeText," ")
        # words per line
        vciTweetLines <- unlist(lapply(lstTweetLines, length))
        # unlist to get vector of words
        vcsTweetWords <- unlist(lstTweetLines)
        # total word count = length of vector
        intWordCount <- length(vcsTweetWords)
        #number of lines
        intLineCount <- length(vcsSomeText)
        
        #Show Line count
        output$LineCountText <- renderText({
          
          paste("Number of Lines:",as.character(intLineCount), sep=" ")
        })
        
        #Show Total word count
        output$TotalWordCount <- renderText({
          paste("Total Word Count:",as.character(intWordCount), sep=" ")
        })
        
        #Show Average Words per Line
        output$AvgWordsPerLine <- renderText({
          paste("Average Words per line:",as.character(intWordCount/intLineCount), sep=" ")
        })
        
        dfrTweet
    })
    
    #Quantitative Analysis
    output$HistSigWords <- renderPlot({
      # move to local variable
      dfrFileName <- input$inpFileName
      # check for null
      if (is.null(dfrFileName))
        return(NULL)
      # open data file
      dfrDataFile <- readLines(dfrFileName$name[1])
      vcsSomeText <- dfrDataFile
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
      #split
      lstUNPrfLines <- str_split(vcsSomeText," ")
      vciUNPrfWperL <- unlist(lapply(lstUNPrfLines, length))
      vcsUNPrfWords <- unlist(lstUNPrfLines)
      vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="[[:digit:]]", "")
      vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="[[:punct:]]", "")
      vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="[[:space:]]", "")
      vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="[~@#$%&-_=<>]", "")
      vcsUNPrfWords <- vcsUNPrfWords[vcsUNPrfWords != ""]
      vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="$", "")
      dfrUNPrfWords <- data.frame(vcsUNPrfWords)
      colnames(dfrUNPrfWords) <- c("Words")
      dfrUNPrfWords$Words <- as.character(dfrUNPrfWords$Words)
      dfrUNPrfWords <- filter(dfrUNPrfWords, str_length(Words)>2)
      vcsCmnWords <- c("all","also","and","any","are","but","can","cant","cry","due","etc","few","for","get","had","has","hasnt","have","her","here","hers","herself","him","himself","his","how","inc","into","its","ltd","may","nor","not","now","off","once","one","only","onto","our","ours","out","over","own","part","per","put","see","seem","she","than","that","the","their","them","then","thence","there","these","they","this","those","though","thus","too","top","upon","very","via","was","were","what","when","which","while","who","whoever","whom","whose","why","will","with","within","without","would","yet","you","your","yours","the")
      dfrUNPrfWords <- filter(dfrUNPrfWords, !(Words %in% vcsCmnWords))
      # remove all bad words ...
      # original found at http://en.wiktionary.org/wiki/Category:English_swear_words
      vcsBadWords <- c("arse","ass","asshole","bastard","bitch","bloody","bollocks","child-fucker","cunt","damn","fuck","goddamn","godsdamn","hell","motherfucker","shit","shitass","whore")
      dfrUNPrfWords <- filter(dfrUNPrfWords, !(Words %in% vcsBadWords))
      dfrUNPrfFreq <- dfrUNPrfWords %>% group_by(Words) %>% summarise(Freq=n())
      dfrUNPrfFreq <- arrange(dfrUNPrfFreq, desc(Freq))
      dfrUNPrfFreq <- filter(dfrUNPrfFreq, Freq>2)
      
      output$CloudSigWords <- renderPlot({
        wordcloud(dfrUNPrfFreq$Words[1:100], dfrUNPrfFreq$Freq[1:100], random.order=F, max.words=100, colors=brewer.pal(8, "Dark2"))
      })
      
      top30 <- head(dfrUNPrfFreq, 30)
      library(ggplot2)
      ggplot(top30, aes(x=Words, y=Freq)) +
        geom_bar(stat="identity", aes(color=Words, fill=Words)) +
        labs(title="Frequency of top 30 words") +
        labs(x="Word") +
        labs(y="Frequency") +
        coord_flip()
    }) 
   
    #Sentment Analysis
    output$HistEmotions <- renderPlot({
      source("sentiment.r")
      dfrFileName <- input$inpFileName
      # check for null
      if (is.null(dfrFileName))
        return(NULL)
      # open data file
      dfrDataFile <- readLines(dfrFileName$name[1])
      vcsSomeText <- dfrDataFile
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
      
      output$HistPolarity <- renderPlot({
        # plot distribution of polarity
        ggplot(dfrSentAnal, aes(x=polarity)) +
          geom_bar(stat="count", aes(colur=polarity, fill=polarity)) +
          labs(title="Sentiment Analysis of Tweets\nClassification by Polarity") +
          labs(x="Polarity") +
          labs(y="Frequency Count")
      })
      
      output$LineCountEmotions <- renderTable({
        table(dfrSentAnal.Tmp$emotions)
      })
      
      output$LineCountPolarity <-renderTable({
        table(dfrSentAnal.Tmp$polarity)
      })
      # plot distribution of emotions
      ggplot(dfrSentAnal.Tmp, aes(x=emotions)) +
        geom_bar(stat="count", aes(colur=emotions, fill=emotions)) +
        labs(title="Sentiment Analysis of Tweets\nClassification by Emotions") +
        labs(x="Emotion Categories") +
        labs(y="Frequency Count")
      
    })
    
})