#packages required

library(ggplot2)
library(downloader)
library(tidyverse)
library(dplyr)
library(stringi)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(sqldf)
library(data.table)









#download and unzip file

setwd('/Users/alan/Desktop/chickenPhotos')

#url = "https://www.kaggle.com/grikomsn/amazon-cell-phones-reviews/kernels"
#download(url, dest="dataset.zip", mode="wb") 

#unzip('dataset.zip')

#import files 
items = read.csv("20190928-items.csv")
reviews = read.csv("20190928-reviews.csv")


# merge the two datasets using a join
total <- left_join(items,reviews,by="asin")



#create new column combining title of review with content of review

total$full_review = stri_join(total$title.y,total$body,sep="_")


# drop unuseful columns
total$image = NULL
total$url = NULL
total$name = NULL
total$reviewUrl = NULL
total$title.y = NULL
total$body = NULL
total$rating.x = NULL
total$helpfulVotes = NULL



#phone values have significant numer of NA's so decision was made to not use for discussion so imputation was deemed innapropriate. more than half are NA's 
summary(total$prices)

#Visualisation 1, WORD CLOUD

# take a random sample of size 10000 from  dataset 
# sample without replacement
total_sample <- total[sample(1:nrow(total), 10000,
                          replace=FALSE),]




word_cloud_file = total_sample$full_review

# Writing  data
write.table(word_cloud_file, file = "word_file.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

# convert variable into a document
text <- readLines("word_file.txt")


# create a corpus from document 
documents <- Corpus(VectorSource(text))




#transform the text document into a usable format and remove excess characters into space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
documents <- tm_map(docs, toSpace, "/")
documents <- tm_map(docs, toSpace, "@")
documents <- tm_map(docs, toSpace, "\\|")


# Convert the text to lower case

documents <- tm_map(documents, content_transformer(tolower))

# Remove numbers from the variable

documents <- tm_map(documents, removeNumbers)

# Removes common English stopwords

documents <- tm_map(documents, removeWords, stopwords("english"))

# Removes stopwords that are too obvious from reviews

documents <- tm_map(documents, removeWords, c("the","got", "two", "of", "in", "a", "to" , "phone", "get", "use", "one", "get", "work", "just", "time", "buy", "can", "will")) 
# Removes punctuation

documents <- tm_map(documents, removePunctuation)

# Remove  white space

documents <- tm_map(documents, stripWhitespace)


#term document matrix shows which words are most frequent and this informs decision on stopword removal
docTermMatrix <- TermDocumentMatrix(documents)
matrix <- as.matrix(docTermMatrix)

# sorting the table in order
sorted <- sort(rowSums(matrix),decreasing=TRUE)
matrixTable <- data.frame(word = names(sorted),freq=sorted)
head(matrixTable, 10)



#wordcloud build
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3.5,0.25),
          colors=brewer.pal(8, "Dark2")) 


#VISUALISATION NUMBER 2
#visualisation of the major brands and their average rating 

#function is having difficulty reading coulmn name with full stop so renaming column
total$rating = total$rating.y

average_rating = sqldf('select Brand, avg(rating) as average_rating from total group by Brand')

avg_rating_plot = ggplot(data=average_rating, aes(x= brand, y=average_rating)) + geom_bar(stat="identity",aes(reorder(brand,average_rating),fill=brand), width = 1)

avg_rating_plot + labs( title= "Average rating of phones by Amazon Reviews", y="Rating", x = "Brands")







