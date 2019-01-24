data<-read.csv("/Users/juileebhosale/Desktop/PURDUE/Web Data Analytics/Final Project/file.csv")

data$X<-NULL

data$size_new<-NULL
data$size_1<-strsplit(as.character(data$size),'M')
data$size_2<-strsplit(as.character(data$size),'k')

data$size_1<-as.integer(data$size_1)
data$size_2<-as.integer(data$size_2)

data$multiplier<-ifelse(is.na(data$size_1), ifelse(is.na(data$size_2), 0, 1),1000)
data$size_new<-ifelse(is.na(data$size_1), ifelse(is.na(data$size_2), NA, data$size_2),data$size_1)
data$size_new<-as.integer(data$size_new)*as.integer(data$multiplier)

#data_copy<-data
data$size<-NULL
data$size_1<-NULL
data$size_2<-NULL
data$multiplier<-NULL
data$new<-NULL
data$test<-NULL
data$price<-NULL
data$app_url<-NULL

data$no_of_ratings<-gsub(',','',data$no_of_ratings)
data$ratings_distribution_5<-gsub(',','',data$ratings_distribution_5)
data$ratings_distribution_4<-gsub(',','',data$ratings_distribution_4)
data$ratings_distribution_3<-gsub(',','',data$ratings_distribution_3)
data$ratings_distribution_2<-gsub(',','',data$ratings_distribution_2)
data$ratings_distribution_1<-gsub(',','',data$ratings_distribution_1)

data$no_of_ratings<-as.integer(data$no_of_ratings)
data$ratings_distribution_5<-as.integer(data$ratings_distribution_5)
data$ratings_distribution_4<-as.integer(data$ratings_distribution_4)
data$ratings_distribution_3<-as.integer(data$ratings_distribution_3)
data$ratings_distribution_2<-as.integer(data$ratings_distribution_2)
data$ratings_distribution_1<-as.integer(data$ratings_distribution_1)

#Read sentiment file generated from the sentiment analysis code

#This dataset has apps and its corresponding sentiments
d<-read.csv("/Users/juileebhosale/Desktop/PURDUE/Web Data Analytics/Final Project/Data_Apps.csv")

d$X<-NULL
data_new<-d[,c(1,19:28)]
data_joined<-merge(data_new,data,by="app_id")

data_joined$app_id<-NULL
data_joined$installs<-NULL
data_joined$app_title<-NULL
data_joined$is_editors_choice<-as.factor(data_joined$is_editors_choice)

test <- multinom(popularity ~ ., data = data_joined)
model_coeff<-summary(test)$coefficients

z <- summary(test)$coefficients/summary(test)$standard.errors

p <- (1 - pnorm(abs(z), 0, 1)) * 2

write.csv(model_coeff,"Coefficients_sentiment.csv")
write.csv(p,"p_value_sentiment.csv")

data_subset<-data[which(data$category=='Education'),]
data_subset$category<-NULL

test_subset <- multinom(popularity ~ ., data = data_subset)
model_coeff_subset<-summary(test_subset)$coefficients

z_subset <- summary(test_subset)$coefficients/summary(test_subset)$standard.errors

p_subset <- (1 - pnorm(abs(z_subset), 0, 1)) * 2

write.csv(model_coeff_subset,"Coefficients_edu.csv")
write.csv(p_subset,"p_value_edu.csv")



