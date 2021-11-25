library(readxl)
library(caret)
library(forecast)
library(neuralnet)
#install.packages("corrplot")
library(corrplot)

#open files
movies <- read_excel("tmdb_5000_movies_08112021.xlsx")
#movies2 <- read_excel("tmdb_5000_movies_08112021_original.xlsx")
View(movies)
movies <- movies[,-c(2,3, 9,10,12, 33, 35)]

#normalise data
normalised_values <- preProcess(movies, method="range")
movies_norm <- predict(normalised_values, movies)

View(movies)
summary(movies)
head(movies)

#sample and split training and validation

set.seed(111)
train.index <- sample(row.names(movies_norm), 0.6*dim(movies_norm)[1])  
valid.index <- setdiff(row.names(movies_norm), train.index)  
train.df <- movies_norm[train.index, ]
valid.df <- movies_norm[valid.index, ]

View(train.df)

nn_movies <- neuralnet(factor(train.df$Profitable) ~ 
                       budget +
                       homepage +
                       original_language_english +
                       popularity +
                       Title_Match +
                       runtime +
                       vote_average +
                       vote_count +
                       NumberOfGenres +
                       Genre_Action +
                       Genre_Adventure +
                       Genre_Animation +
                       Genre_Comedy +
                       Genre_Crime +
                       Genre_Documentary +
                       Genre_Drama +
                       Genre_Family +
                       Genre_Fantasy +
                       Genre_Foreign +
                       Genre_History +
                       Genre_Horror +
                       Genre_Music +
                       Genre_Mystery +
                       Genre_Romance +
                       Genre_Science_Fiction +
                       Genre_Thriller +
                       Genre_War +
                       NumberOfProductionCompanies +
                       Production_Paramount_Pictures +
                       Production_Universal_Pictures +
                       Production_Columbia_Pictures +
                       Production_Twentieth_Century_Fox_Film_Corporation +
                       Production_New_Line_Cinema +
                       Production_Walt_Disney_Pictures +
                       Production_Miramax_Films +
                       Production_United_Artists +
                       Production_Village_Roadshow_Pictures +
                       Production_Warner_Bros +
                       language_count +
                       English +
                       Location_US +
                       Location_UK +
                       Location_France +
                       Location_Germany +
                       Location_Canada +
                       Location_Other,
                       data = train.df, linear.output = F, hidden = 2,stepmax = 1e+08)

                    
plot(nn_movies)
nn_movies$weights
# predictions on training and validation data
options(scipen = 0)
# training prediction probabilities
train.pred <- compute(nn_movies, train.df)
train.pred <- train.pred$net.result[,2]
# convert probabilities to classes
train.class <- (1* (train.pred>0.5))
confusionMatrix(factor(train.class), factor(train.df$Profitable), positive = "1")

# validation prediction probabilities
valid.pred <- compute(nn_movies, valid.df)
valid.pred <- valid.pred$net.result[,2]
# convert probabilities to classes
valid.class <- (1* (valid.pred>0.5))
# confusion matrix 
confusionMatrix(factor(valid.class), factor(valid.df$Profitable), positive = "1")

nn_movies2 <- neuralnet(factor(train.df$Profitable) ~ 
                          popularity + 
                          runtime + 
                          vote_average +
                          Genre_Foreign +
                          Genre_Horror +
                          Production_Paramount_Pictures +
                          Production_Universal_Pictures +
                          Production_Columbia_Pictures +
                          Production_Twentieth_Century_Fox_Film_Corporation +
                          Production_New_Line_Cinema +
                          Production_Walt_Disney_Pictures +
                          Production_United_Artists +
                          Location_US,
                          data = train.df, linear.output = F, hidden = 2)
plot(nn_movies2)
nn_movies2$weights
# predictions on training and validation data
options(scipen = 0)
# training prediction probabilities
train.pred <- compute(nn_movies2, train.df)
train.pred <- train.pred$net.result[,2]
# convert probabilities to classes
train.class <- (1* (train.pred>0.5))
confusionMatrix(factor(train.class), factor(train.df$Profitable), positive = "1")

# validation prediction probabilities
valid.pred <- compute(nn_movies2, valid.df)
valid.pred <- valid.pred$net.result[,2]
# convert probabilities to classes
valid.class <- (1* (valid.pred>0.5))
# confusion matrix 
confusionMatrix(factor(valid.class), factor(valid.df$Profitable), positive = "1")

