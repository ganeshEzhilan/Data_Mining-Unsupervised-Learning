#############################################################################
## The problem is to find the unspecified ratings of user 2 using user collaborative filtering
## Ganesh Ezhilan
## Created: February 16,2019
#############################################################################
library(recommenderlab)

rating_matrix = matrix(c(5,4,NA,7,1,6,NA,3,4,NA,7,3,4,3,3,4,NA,1,6,2,3,5,1,NA,2,NA,4,NA,4,5), nrow = 5)

## create user rating as matrix
R = as(rating_matrix, "realRatingMatrix")
## Part A ###################
## User Based Collaborative Filtering (Pearson Coefficient)
ubcf = Recommender(R, method = 'UBCF', parameter = list(normalize = "center", method = "pearson"))
user_ratings <- predict(ubcf, R, type = "ratings")
getRatingMatrix(user_ratings)

#Unspecified values using used based approch
as(user_ratings, 'matrix')

## Part B ####################
## Item Based Collaborative Filtering (Adjusted Cosine)
#Unspecified values using item based approch
ibcf = Recommender(R, method = "IBCF", parameter = list(method = "cosine"))
item_ratings = predict(ibcf, R, type = 'ratings')
getRatingMatrix(item_ratings)

#Unspecified values using item based approch
as(item_ratings, 'matrix')[,1:6]
