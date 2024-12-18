movie<-read.csv("D:\\Movie data\\Movie data.csv")
head(movie,5)
unique_genres<-unique(unlist(strsplit(paste(movie$Genre,collapse=","),",")))
unique_genres
one_hot_genres <- t(sapply(movie$Genre, function(genres) {
  sapply(unique_genres, function(genre) as.integer(genre %in% unlist(strsplit(genres, ","))))
}))
one_hot_genres
head(one_hot_genres)
library(cluster)
set.seed(123)
movie_kmeans<-kmeans(one_hot_genres,centers=4,nstart=100)
movie_kmeans
colnames(one_hot_genres)<-unique_genres
rownames(one_hot_genres)<-movie$title
head(one_hot_genres)
movie$cluster<-movie_kmeans$cluster
#Preferred Moives by the user
selected_movies<-c("Baby Driver","Lucy","Godzilla King of the Monsters","Mad Max Fury Road","Dont Breathe")
selected_clusters <- movie %>%
  filter(title %in% selected_movies) %>%
  pull(cluster)
common_cluster <- as.numeric(names(sort(table(selected_clusters), decreasing = TRUE)[1]))
common_cluster
recommendations <- movie %>%
  filter(cluster == common_cluster & status == "Released" & vote_average > 5) %>%
  select(original_title, vote_average)
head(recommendations,10)
str(recommendations)
recommendations[recommendations$original_title=="Dont Breathe",c(1,2)]
movie[movie$title=="Dont Breathe",16]
colnames(movie)
head(movie)
