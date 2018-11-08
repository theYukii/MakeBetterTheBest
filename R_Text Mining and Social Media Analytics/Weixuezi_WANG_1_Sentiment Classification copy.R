# ============================= #
#  Student Name: Weixuezi WANG  #
# ============================= #
rm(list = ls())
# Load the necessary package in R
install.packages("lexicon")
install.packages("sentimentr")
library("text2vec")
library("NLP")
library("tm")
library("lexicon")
library("sentimentr")
library("DBI")
library("RMySQL")
library("e1071")
library("tokenizers")

############################
# Sentiment Classification #
############################

# ------------------------#
#  Input the Raw Dataset  #
# ------------------------#
myhost <- "localhost"
mydb <- "studb"
myacct <- "cis434"
mypwd <- "LLhtFPbdwiJans8F@S207"

driver <- dbDriver("MySQL")
conn <- dbConnect(driver, host=myhost, dbname=mydb, myacct, mypwd)
airline_raw <- dbGetQuery(conn, "SELECT * FROM classification WHERE rtag = 'cC|q8z29^Zio'")
airline <- airline_raw[,c(1,3,5)]

# --------------------#
#  Clean the Dataset  #
# --------------------#
airline$tweet <- tolower(airline$tweet)
airline$text <- gsub("http\\S+\\s*", " ", airline$tweet)
airline$text <- gsub("[#|@|-|&|.|!|?]", " ", airline$text)
airline$text <- gsub("[[:cntrl:]]", " ", airline$text)

# ----------------------------------------------------- #
#  Use Lexicon to find the positive and negative word   #
# ----------------------------------------------------- #
sentiment_word <- lexicon::hash_sentiment_jockers_rinker
positive <- sentiment_word[y > 0, 1]
negative <- sentiment_word[y < 0, 1]
addneg<- c("horribly","terrible","worse","shout","late","worst","bad","seriously","why not","fail","shit","shatterd","awful","shame","terrible",
          "stuck","sigh","force","inconvenient","sad","wasted","not happy","unitedsucks","sucks","disappointed","frustrated","screwed","screw",
          "screwing","poor","incompetent","rude","disrespectful","ridiculous","motherfuckers","strand","robot","unacceptable",
          "again","thieves","delay","delayed","no tv")
# transform the data into vector, in order to be used in function 'match'
positive <- unlist(as.list(positive))  
negative <- unlist(c(as.list(negative),addneg))

# ------------------------------------------------ #
#  Create a function to create the sentiment score   #
# ------------------------------------------------ #
require(plyr)
require(stringr)
scoresentiment <- function(text, positive, negative){
  scores = laply(text, function(text, positive, negative) {
    text = tolower(text) # transform all character into lowercase
    word_list = itoken(text, preprocessor=tolower, tokenizer=word_tokenizer) # splits the tweets by word in a list
    vocab <- create_vocabulary(word_list)[,1]
    #word.list = str_split(text, '\\s+')
    words = unlist(vocab) # turns the list into vector
    pos.matches = match(words, positive) ## returns matching 
    #values for words from list 
    neg.matches = match(words, negative)
    pos.matches = !is.na(pos.matches) ## converts matching values to true or false
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    return(score)
  },positive, negative)
}


# ------------------------------------------------ #
#  Create a function to find the sentiment score   #
# ------------------------------------------------ #

airline$sentiscore <- 0

for (i in 1:4583){
  airline$sentiscore[i]<- scoresentiment(airline$text[i],positive,negative)
}

airline$sentiment <- ifelse(airline$sentiscore > 2, 1, 0)

# Check whether the assigned sentiment is accurate
airline[airline$sentiment==1,]

# Count the number of no complaint
nrow(airline[airline$sentiment==1,])

not_complaints <- airline[airline$sentiment==1, c("id","tweet")]
write.csv(not_complaints,"not_complaints.csv")

# ============== IF we use the package sentimentr to find non complaint tweet ========== #
# --------------------------------------------------#
#  Use Sentimentr to Give each Sentence a Polarity  #
# --------------------------------------------------#
sentiment_all <- sentiment(airline$text, polarity_dt = lexicon::hash_sentiment_jockers_rinker, 
                           valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
                           amplifier.weight = 0.8, n.before = 1, n.after = 1,
                           question.weight = 1, adversative.weight = 0.85,
                           neutral.nonverb.like = FALSE, missing_value = 0)
airline_new <- airline
airline_new$sentiscore <- sentiment_all$sentiment

# Assign 1 to positive sentiment and 0 to negtive sentiment. The cutoff of polarity is 0.5
airline_new$sentiment <- ifelse(airline_new$sentiscore > 0.5, 1, 0)

# Check whether the assigned sentiment is accurate
airline_new[airline_new$sentiment==1,]

# Count the number of no complaint
nrow(airline_new[airline_new$sentiment==1,])

not_complaints_new <- airline_new[airline_new$sentiment==1, c("id","tweet")]
write.csv(not_complaints_new,"not_complaints_with package.csv")


