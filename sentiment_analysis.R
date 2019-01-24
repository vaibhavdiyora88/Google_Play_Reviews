#Get Data

app_reviews<-read.csv("/Users/karmapatel/Dropbox/Purdue/Fall2/Web Data Analytics/Project/final_app_reviews.csv", sep = "\t", stringsAsFactors = FALSE)
app_details<-read.csv("/Users/karmapatel/Dropbox/Purdue/Fall2/Web Data Analytics/Project/final_app_details.csv", sep = "\t", stringsAsFactors = FALSE)

joined_merge<-merge(app_details,app_reviews, by="app_id")

app_id_list <- app_details$app_id
app_id_list <- gsub('\\b.\\b','',app_id_list)

app_reviews<-joined_merge[,c("app_id","rating.y","review")]
app_details<-joined_merge[,-c(19,20)]
app_details<-unique(app_details)


library(jsonlite)
library(tm)
library(SnowballC)
library(syuzhet)
library(wordcloud)
library(ggplot2)
library(reshape)
library(stringr)

#--------------------------------------------
#       Function to get Wordcloud
#--------------------------------------------

get.wordcloud <- function(docs) {
  
#  docs <- Corpus(VectorSource(data$reviews))
#  print(docs)
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, tolower)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeWords, c("app", "bad", "good", "game"))
  docs <- tm_map(docs, function (x) gsub('\\bcom', "", x))
#  docs <- tm_map(docs, function(x) nchar(x) > 10)
  docs <- tm_map(docs, function (x) ifelse(x %in% app_id_list, "", x))
  docs <- tm_map(docs, function(x) ifelse(is.na(x),"",x))
  
  # Create the document term matrix
  dtm <- TermDocumentMatrix(docs)
  mat <- as.matrix(dtm)
  
  # sort by frequency and print the first 10
  v <- sort(rowSums(mat), decreasing=TRUE)
  d <- data.frame(word = names(v), freq=v)
  d <- na.omit(d)
  head(d, 10)
  
  # Generate a word cloud
  set.seed(417)
  wordcloud(words = d$word, freq = d$freq, min.freq = 3, max.words = 250, 
            random.order = FALSE, rot.per = 0.35, colors=brewer.pal(8, "Dark2"))
  
}

#----------------------------------------------
#       Function to get app sentiment
#----------------------------------------------
get.appsentiment <- function(reviews) {
  
  # fetch sentiment 
  total.reviews<-length(reviews)
  sentiment <- get_nrc_sentiment(reviews)
  text <- cbind(reviews, sentiment)
  
  # total sentiment score by category
  total.sentiment <- data.frame(colSums(text[, c(2:11)]))
  names(total.sentiment) <- "count"
  total.sentiment <- cbind("sentiment" = rownames(total.sentiment), total.sentiment)
  rownames(total.sentiment) <- NULL
  
  total.sentiment$count<-total.sentiment$count/total.reviews
  # total sentiment score of all text
  ggplot(data=total.sentiment, aes(x=sentiment, y=count))+geom_bar(aes(fill=sentiment), stat="identity")+theme(legend.position="none") + xlab("Sentiment") + ylab("Total Count") + ggtitle("Total sentiment score") 
  
  rm(reviews, sentiment)
  
  #Reshape Data
  total.sentiment<-cbind(app_id,total.sentiment )
  total.sentiment<-cast(total.sentiment, app_id~sentiment)
  return(total.sentiment)
}

#----------------------------------------------
#       Function to get wordcloud for a genre
#----------------------------------------------

get.genre.wordcloud <- function(genres, app_all_sentiment){
  genre_subset<-app_all_sentiment[which(app_all_sentiment$category %in% genres),]
  genre_bad<-genre_subset[which((genre_subset$anger>0 | genre_subset$disgust>0 | genre_subset$fear>0 | 
                                   genre_subset$sadness>0 | genre_subset$negative>0) & genre_subset$joy<=0 & 
                                  genre_subset$surprise<=0 & genre_subset$trust<=0 & genre_subset$positive<=0),]
  get.wordcloud(Corpus(VectorSource(genre_bad$reviews)))
  
  genre_good<-social_subset[which(genre_subset$anger<=0 & genre_subset$disgust<=0 & genre_subset$fear<=0 & 
                                    genre_subset$sadness<=0 & genre_subset$negative<=0 & 
                                    (genre_subset$joy>0 | genre_subset$surprise>0 | genre_subset$trust>0 | 
                                       genre_subset$positive>0)),]
  get.wordcloud(Corpus(VectorSource(genre_good$reviews)))
  list(genre_good, genre_bad)
}

#----------------------------------------------
#       Function to get app associations
#----------------------------------------------

get.appassociations <- function(data, keyword) {
  
  #data<-bad
  reviews<-data$reviews
  docs <- Corpus(VectorSource(reviews))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, tolower)
  #docs <- tm_map(docs, stemDocument)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeWords, c("app","krstarplekstar","comaceshellproduction", "1nu", "bekaar"))
  docs <- tm_map(docs, function (x) gsub('\\bcom\\w+','',x))
  docs<- tm_map(docs, content_transformer(function(s){gsub(pattern = '[^a-zA-Z0-9\\s]+',
                                                           x = s,
                                                           replacement = " ",
                                                           ignore.case = TRUE,
                                                           perl = TRUE)}))
  
  
  # Create the document term matrix
  dtm <- TermDocumentMatrix(docs)
  
  # Assuming a search term co-occurence of 0.6
  word_list<-findAssocs(dtm, keyword,0.01)
  
  # sort by frequency and print the first 10
  
  v<-word_list
  d <- data.frame(word = "data", freq=v)
  d$word <- rownames(d)
  names(d)<-c("word","freq")
  d<-d[order(-d$freq),]
  
  head(d, 10)
  # Generate a word cloud
  set.seed(417)
  wordcloud(words = d$word, freq = d$freq, min.freq = 0.01, max.words = 250, random.order = FALSE, rot.per = 0.35, colors=brewer.pal(8, "Dark2"))
  
  return(word_list)
}

#----------------------------------------
#           Analysis
#----------------------------------------

#Get app sentiment array
df<-app_reviews
data_final<-data.frame()

for (app_id in app_details$app_id){
  current_df <- df[df$app_id == app_id,]
  total.sentiment<-get.appsentiment(current_df$review)
  
  #Join Dataframe and run regression
  app_details_subset<-app_details[app_details$app_id==app_id,]
  data_join<-merge(app_details_subset, total.sentiment, by="app_id")
  data_final<-rbind(data_final,data_join)
}
data<-data_final

#write.csv(data_final,"Data_Apps.csv")

#----------------------------------------
#           Word Clouds
#----------------------------------------
df<-app_reviews

# fetch sentiment 
reviews <- df$review
sentiment <- get_nrc_sentiment(reviews)
text <- cbind(joined_merge$app_id, reviews, sentiment)
write.csv(text, "Review_sentiment.csv")
try<-cbind(df[,c("app_id")],text)
names(try)[1]<-"app_id"
app_all_sentiment<-merge(app_details, try, by="app_id")
app_details$category <- app_details$genre
unique(app_details$category)

#----------------------------------------
#           Word Cloud images
#----------------------------------------

list(music_good, music_bad) <- get.genre.wordcloud(c("Music & Audio"), app_all_sentiment) # Music
list(games_good, games_bad) <-get.genre.wordcloud(c("Action", "Action & Adventure", 
                      "Adventure", "Arcade", "Board", 
                      "Brain Games", "Card", "Casino", 
                      "Casual", "Educational", "Music", 
                      "Puzzle", "Racing", "Role Playing", 
                      "Simulation", "Sports","Strategy", 
                      "Trivia", "Word"), app_all_sentiment) # Games
list(social_good, social_bad) <-get.genre.wordcloud(c("Social"), app_all_sentiment) # Social
list(food_good, food_bad) <-get.genre.wordcloud(c("Food & Drink"), app_all_sentiment) # Food & Drink
list(finance_good, finance_bad) <-get.genre.wordcloud(c("Finance"), app_all_sentiment) # Finance
list(lifestyle_good, lifestyle_bad) <-get.genre.wordcloud(c("Lifestyle"), app_all_sentiment) # Lifestyle
list(education_good, education_bad) <-get.genre.wordcloud(c("Education"), app_all_sentiment) # Education
list(video_good, video_bad) <-get.genre.wordcloud(c("Video Players & Editors"), app_all_sentiment) # Video Players & Editors
list(tools_good, tools_bad) <-get.genre.wordcloud(c("Tools"), app_all_sentiment) # Tools
list(personalization_good, personalization_bad) <-get.genre.wordcloud(c("Personalization"), app_all_sentiment) # Personalization
list(productivity_good, productivity_bad) <-get.genre.wordcloud(c("Productivity"), app_all_sentiment) # Productivity
list(photo_good, photo_bad) <-get.genre.wordcloud(c("Photography"), app_all_sentiment) # Photography
list(books_good, books_bad) <-get.genre.wordcloud(c("Books & Reference"), app_all_sentiment) # Books & Reference
list(entertainment_good, entertainment_bad) <-get.genre.wordcloud(c("Entertainment"), app_all_sentiment) # Entertainment
list(shop_good, shop_bad) <-get.genre.wordcloud(c("Shopping"), app_all_sentiment) # Shopping
list(weather_good, weather_bad) <-get.genre.wordcloud(c("Weather"), app_all_sentiment) # Weather
list(comm_good, comm_bad) <-get.genre.wordcloud(c("Communication"), app_all_sentiment) # Communication
list(travel_good, travel_bad) <-get.genre.wordcloud(c("Travel & Local"), app_all_sentiment) # Travel & Local
list(news_good, news_bad) <-get.genre.wordcloud(c("News & Magazines"), app_all_sentiment) # News & Magazines
list(auto_good, auto_bad) <-get.genre.wordcloud(c("Auto & Vehicles"), app_all_sentiment) # Auto & Vehicles
list(business_good, business_bad) <-get.genre.wordcloud(c("Business"), app_all_sentiment) # Business
list(library_good, library_bad) <-get.genre.wordcloud(c("Libraries & Demo"), app_all_sentiment) # Libraries & Demo
list(maps_good, maps_bad) <-get.genre.wordcloud(c("Maps & Navigation"), app_all_sentiment) # Maps & Navigation
list(medical_good, medical_bad) <-get.genre.wordcloud(c("Medical"), app_all_sentiment) # Medical
#list(events_good, events_bad) <-get.genre.wordcloud(c("Events"), app_all_sentiment) # Events
list(health_good, health_bad) <-get.genre.wordcloud(c("Health & Fitness"), app_all_sentiment) # Health & Fitness
list(art_good, art_bad) <-get.genre.wordcloud(c("Art & Design"), app_all_sentiment) # Art & Design

#----------------------------------------
#           App associations
#----------------------------------------

get.appassociations(genre_good, "love")
