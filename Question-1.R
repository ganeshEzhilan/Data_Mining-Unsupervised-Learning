############################################################################################################
## The problem is to evaluate our own recommendation system and perform cross validation and analyze the inference
## Ganesh Ezhilan
## Created: February 16,2019
####################################################################################################
library(recommenderlab)

rm(list = ls())
data("MovieLense")

dim(MovieLense)
data = MovieLense
value_matrix = getRatingMatrix(data)
#length(unique(data$title))

# Converting the data frame into a matrix
data = as.matrix(data)

## Coverting matrix into a real rating matrix

data = as(data,'realRatingMatrix')
recommender_popularity <- Recommender(data, method = "UBCF")
user_ratings <- predict(recommender_popularity, data, type = "ratings")

getRatingMatrix(user_ratings)
pred_val = as(user_ratings,'matrix')
write.csv(pred_val, file = "user_rating.csv")


##############################################################################
## Performing Cross Validation to check the accuracy of the Recommender System
##############################################################################

## please note, this part has been referred from the recommender's lab provided to us

set.seed(2018)
scheme_1<- evaluationScheme(MovieLense, method="split", train=0.5, k=1, given=-5, goodRating=4)
scheme_1
algorithms<- list("random items" = list(name= "RANDOM", param=NULL),
                  "popular items"= list(name = "POPULAR", param =NULL),
                  "user-based CF"= list(name = "UBCF", param = list(nn=50)),
                  "item-based CF"= list(name = "IBCF", param = list(k=50)),
                  "SVD approximation" = list(name = "SVD", param= list(k=50)) )

results_1<- evaluate(scheme_1, algorithms, type="topNList", n=c(1,3,5,10,15,20))

names(results_1)
x11()
plot(results_1, annotate=c(1,3), legend="topleft")
x11()
plot(results_1, "prec/rec", annotate = TRUE)