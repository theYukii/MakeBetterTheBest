# ============================= #
#  Student Name: Weixuezi WANG  #
# ============================= #
rm(list = ls())
setwd("~/Desktop/Final Data")
install.packages("text2vec")
install.packages("openNLPmodels.en")
install.packages("openNLP")
install.packages("tseries")
library("NLP")
library("tm")
library("text2vec")
library("tseries")

# -------------------------------- #
#  Load the data in the dataframe  #
# -------------------------------- #

# Use for loop to read all the data into one dataframe
fb <- data.frame(text = NA) 
fbid <- data.frame(id = NA)
for (i in 1:5) {
  for (j in 1:12) {
    fileyear <- paste("201",toString(i),sep ="")
    filemonth <- j
    file <- paste("fpost-",fileyear,"-",filemonth,".csv", sep = "")
    df <- readLines(con = file)
    df <-as.data.frame(df)
    colnames(df) <- "text"
    fb <- rbind(fb,df)
    fbid <- rbind(fbid, data.frame(id = rep((paste(fileyear,filemonth,sep ="")),nrow(df))))
  }
}

fb_all <- data.frame(fbid,fb) # combine the text and the id we created
fb_all <- fb_all[-1,] # remove the first line with "NA" value

write.csv(fb_all,"df_final.csv")

# ------------------------------------------- #
#  Find the term which has the top frequency  #
# ------------------------------------------- #

# find top frequency words in each year and we use the ingredient file 
# which has enhanced with vegetable and fruit words.
ingredient = readLines("ingredients.txt")
dic = tolower(ingredient)  # we use the ingredient we created to be the dictionary
topword <- c()
for (i in 1:5) {
  for (j in 1:12) {
    fileyear <- paste("201",toString(i),sep ="")
    filemonth <- j
    dfname <- paste(fileyear,filemonth, sep = "")
    fb_sub <- fb_all[fb_all$id == dfname,]
    colnames(fb_sub) <- c("doc_id","text")
    docs <- Corpus(DataframeSource((fb_sub)))
    #mystop = c('act', 'actually', 'aka', 'and', 'so', 'via','great', 'don', 'dont','the',
               #'and','for','just','make','can','day', 'food', 'just', 'http', 'www', 'can',
               #'com', 'will',  'one', 'get','like', 'love','recip','back','bell','best','cook','eat','good','today')
    #dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, 
                       #stopwords=c(stopwords("english"),stopwords("spanish"),stopwords("portuguese"),mystop),
                       #stemming=T, weighting=weightTfIdf)
    dtm <- DocumentTermMatrix(docs, list(dictionary = dic)) 
    #based on the dictionary we can find the top term related to food
    dtm <- removeSparseTerms(dtm,0.99)
    topword <- c(topword, findFreqTerms(dtm, lowfreq=300)[1:5]) # choose the top 5 words
  }
}
topword
unique(topword) # Find the unique word in topword list

# ---------------------------------------------------------------------------------- #
#  Plot the time series for words which have top frequency and we are interested in  #
# ---------------------------------------------------------------------------------- #
keywords <- c('apple','chocolate','cake','rice','pumpkin','cheese','taco','potato','cauliflower rice','vegetable noodle')
df_keyword = matrix(0, 60, 1)

k <- keywords[6]
k
for(i in 1:5){
  for (j in 1:12){
      filename = paste("fpost-201", i, "-", j, ".csv", sep='')
      fbdata <- read.csv(filename, header=FALSE, sep=',', quote='"')
      fbdata <- paste(fbdata$V1, fbdata$V2, fbdata$V3)
      value <- sum(grepl(k, fbdata, ignore.case = T))
      row_num <- 12*(i-1) + j
      df_keyword[row_num, 1] <- value
  }
} #grep the chosen words from each month and assign into a dataframe

keywaord_freq <- ts(df_keyword, start = c(2011, 1), frequency = 12) # plot the time trend for each highlighted words
plot(keywaord_freq, yax.flip = TRUE)
title(paste("Time Series for",k))

# ---------END-------- #

           


