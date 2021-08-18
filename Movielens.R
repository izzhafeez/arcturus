################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



#Attempt at using movie year as a parameter: too long to compute
# library(pbapply)
# titles<-edx$title
# years<-pbsapply(titles,function(x){
#   str_extract(x,regex('\\(\\d{4}\\)'))%>%substr(2,5)
# })
# edx$movie_year<-as.numeric(years)

library(lubridate)
library(plyr)
library(ModelMetrics)

train_set<-edx

#Default value if nas are reported
overall_average <- mean(train_set$rating)


#train_set <- train_set%>%mutate(rating_year=as.numeric(year(as_datetime(timestamp))),years_since_watched=rating_year-movie_year)

#Calculate average movie rating per movie
train_set$movie_avg <- ave(train_set$rating,train_set$movieId)

#Calculate how much the user overscored
train_set <- train_set%>%mutate(overscore=rating-movie_avg)

#Calculate how much on average each user overscores
train_set$tendency_overscore <- ave(train_set$overscore,train_set$userId)

#Ranks movies by average rating
train_set$movie_rank<-rank(train_set$movie_avg,train_set$movieId)

#Ranks users by tendency to overscore
train_set$user_rank<-rank(train_set$tendency_overscore,train_set$userId)

train_set<-as.data.frame(train_set)

#Training the model using the caret package
TrainData <- train_set%>%select(movie_avg,tendency_overscore,timestamp,movie_rank,user_rank)
TrainClasses <- train_set$rating
model<-train(TrainData,TrainClasses,method='lm',tuneLength=10,trControl=trainControl(method='boot'))

#Just a succinct way to transfer user and movie info to validation set
by_user<-train_set[!duplicated(train_set$userId),]%>%select(userId,tendency_overscore,user_rank)
by_movie<-train_set[!duplicated(train_set$movieId),]%>%select(movieId,movie_avg,movie_rank)

#Transferring user and movie info to validation set. temp_validation is used because i don't want to restart everything again when i make a mistake
temp_validation<-merge(x=validation,y=by_user,by='userId',all.x=TRUE)
temp_validation<-merge(x=temp_validation,y=by_movie,by='movieId',all.x=TRUE)
temp_validation<-temp_validation%>%mutate(rating_year=as.numeric(year(as_datetime(timestamp))),years_since_watched=rating_year-movie_year)

#Prediction
rating_hat<-as.vector(predict(model,temp_validation))

#RMSE = 0.8652998
rmse(temp_validation$rating,rating_hat)
