library(quanteda)
library(stringi)
library(data.table)
library(stringr)

setwd("C:/Users/sasmira.s.shetty/capstone")

if(!file.exists("./projectData")){
  dir.create("./projectData")
}
Url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

## Check if zip has already been downloaded in projectData directory?
if(!file.exists("./projectData/Coursera-SwiftKey.zip")){
  download.file(Url,destfile="./projectData/Coursera-SwiftKey.zip",mode = "wb")
}
## Check if zip has already been unzipped?
if(!file.exists("./projectData/final")){
  unzip(zipfile="./projectData/Coursera-SwiftKey.zip",exdir="./projectData")
}

# Once the dataset is downloaded start reading it 
path <- file.path("./projectData/final" , "en_US")
# Lets make a file connection of the twitter data set
con <- file("./projectData/final/en_US/en_US.twitter.txt", "r") 
lineTwitter<-readLines(con, skipNul = TRUE,encoding = "UTF-8")
# Close the connection handle when you are done
close(con)

# Lets make a file connection of the blog data set
con <- file("./projectData/final/en_US/en_US.blogs.txt", "r") 
lineBlogs<-readLines(con, skipNul = TRUE,encoding = "UTF-8")
# Close the connection handle when you are done
close(con)
# Lets make a file connection of the news data set
con <- file("./projectData/final/en_US/en_US.news.txt", open = "rb") 
lineNews<-readLines(con, skipNul = TRUE,encoding = "UTF-8")
# Close the connection handle when you are done
close(con)
# read profanity words
badwords<-readLines("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt",skipNul = TRUE,encoding = "UTF-8")
set.seed(2000)
#Get 30% of sample data
data.sample <- c(sample(lineBlogs, length(lineBlogs) * 0.30),
                 sample(lineNews, length(lineNews) * 0.30),
                 sample(lineTwitter, length(lineTwitter) * 0.30))
data.sample <- gsub("[^0-9A-Za-z///' ]", "", data.sample) # remove special characters

rm(lineBlogs)
rm(lineNews)
rm(lineTwitter)
#length(data.sample)

data.sample <- stri_trans_general(data.sample, "latin-ascii")
myCorpus <- corpus(data.sample)# Create Corpus

rm(data.sample)

#texts(myCorpus)[2]
#kwic(myCorpus, "limit")
#tokens(myCorpus)

#Create Tokens and remove numbers, puntuation, symbols, twitter, profanity and lower the words
tt<-tokens(myCorpus,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE,remove_twitter=TRUE)
tt<-tokens_tolower(tt)
#tt<-tokens_wordstem(tt)
#tt<-tokens_remove( tt,stopwords("english"))
tt<-tokens_select(tt, c("rt","lol","ought"), selection = "remove")
tt<-tokens_remove(tt, badwords)

rm(myCorpus)

#Create 1-Gram and remove terms with low frequency
mydf1<-dfm(tt,ngrams=1,concatenator = " " )
mydf1<-dfm_trim(mydf1, min_count = 50, min_docfreq = 5000)
df1 <- data.frame(Content = featnames(mydf1), Frequency = colSums(mydf1),
                  row.names = NULL, stringsAsFactors = FALSE)
df1<-df1[order(df1$Frequency,decreasing=TRUE),]
#nrow(df1)
#head(df1)
rm(mydf1)
save(df1, file="df1.RData")
rm(df1)

#Create 2-Gram and remove terms with low frequency 
mydf2<-dfm(tt,ngrams=2,concatenator = " " )
#mydf2[-2, ]
#mydf2[,1:4]
mydf2<-dfm_trim(mydf2, min_count = 10, min_docfreq = 2)
df2 <- data.frame(Content = featnames(mydf2), Frequency = colSums(mydf2),srcw = sub("^\\s*((?:\\S+\\s+){0}\\S+).*", "\\1", featnames(mydf2)),
                  row.names = NULL, stringsAsFactors = FALSE)
df2<-df2[order(df2$Frequency,decreasing=TRUE),]
rm(mydf2)
save(df2, file="df2.RData")
rm(df2)

#Create 3-Gram and remove terms with low frequency
mydf3<-dfm(tt,ngrams=3,concatenator = " " )
mydf3<-dfm_trim(mydf3, min_count = 5, min_docfreq = 2)
df3 <- data.frame(Content = featnames(mydf3), Frequency = colSums(mydf3), srcw = sub("^\\s*((?:\\S+\\s+){1}\\S+).*", "\\1",featnames(mydf3)),
                  row.names = NULL, stringsAsFactors = FALSE)
df3<-df3[order(df3$Frequency,decreasing=TRUE),]
save(df3, file="df3.RData")
#head(df3)
rm(mydf3)
rm(df3)
#gc()
#object.size()

#Create 4-Gram and remove terms with low frequency
mydf4<-dfm(tt,ngrams=4,concatenator = " " )
mydf4<-dfm_trim(mydf4, min_count = 5, min_docfreq = 2)
df4 <- data.frame(Content = featnames(mydf4), Frequency = colSums(mydf4), srcw = sub("^\\s*((?:\\S+\\s+){2}\\S+).*", "\\1",featnames(mydf4)),
                  row.names = NULL, stringsAsFactors = FALSE)
df4<-df4[order(df4$Frequency,decreasing=TRUE),]
save(df4, file="df4.RData")
#head(df4)
rm(mydf4)
rm(df4)

