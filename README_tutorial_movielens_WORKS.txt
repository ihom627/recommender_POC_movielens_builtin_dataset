
recommender POC, based on movielense dataset

https://www.rdocumentation.org/packages/recommenderlab/versions/0.2-5

NOTE: uses built in dataset, no external files required

#STEP1) install packages


install.packages("recommenderlab")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("devtools")

library(recommenderlab)
library(reshape2)
library(ggplot2)
library(devtools)

getwd()
setwd("/Users/hivan/work/recommender_POC")


#STEP2) Load movielens data and train recommender

data("MovieLense")
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]

>MovieLense100
358 x 1664 rating matrix of class ‘realRatingMatrix’ with 73610 ratings.

#Train a user-based collaborative filtering recommender using a small training set.
train <- MovieLense100[1:50]
rec <- Recommender(train, method = "UBCF")
rec
>Recommender of type ‘UBCF’ for ‘realRatingMatrix’ learned using 50 users.

#Create top-N recommendations for new users (users 101 and 102)
pre <- predict(rec, MovieLense100[101:102], n = 10)
pre

#Recommendations as ‘topNList’ with n = 10 for 2 users.
as(pre, "list")

$`291`
 [1] "Titanic (1997)"                        
 [2] "Contact (1997)"                        
 [3] "Alien (1979)"                          
 [4] "Amadeus (1984)"                        
 [5] "Godfather, The (1972)"                 
 [6] "Aliens (1986)"                         
 [7] "Sting, The (1973)"                     
 [8] "American Werewolf in London, An (1981)"
 [9] "Schindler's List (1993)"               
[10] "Glory (1989)"                          

$`292`
 [1] "Usual Suspects, The (1995)"               
 [2] "Amadeus (1984)"                           
 [3] "Titanic (1997)"                           
 [4] "Raising Arizona (1987)"                   
 [5] "Citizen Kane (1941)"                      
 [6] "Godfather: Part II, The (1974)"           
 [7] "Young Frankenstein (1974)"                
 [8] "Brazil (1985)"                            
 [9] "Butch Cassidy and the Sundance Kid (1969)"
[10] "Stand by Me (1986)"                     










############################# STOP HERE ##########################


# Read training file along with header
tr<-read.csv("train_v2.csv",header=TRUE)

# Just look at first few lines of this file
head(tr)

      ID user movie rating
1 610739 3704  3784      3
2 324753 1924   802      3
3 808218 4837  1387      4
4 133808  867  1196      4
5 431858 2631  3072      5
6 895320 5410  2049      4


# Remove 'id' column. We do not need it
tr<-tr[,-c(1)]


# Check, if removed
tr[tr$user==1,]

       user movie rating
34179     1  1907      4
64257     1  1287      5
68565     1  1566      4
71239     1   260      4
125237    1   919      4


# Using acast to convert above data as follows:
#       m1  m2   m3   m4
# u1    3   4    2    5
# u2    1   6    5
# u3    4   4    2    5

g<-acast(tr, user ~ movie)

# Check the class of g
class(g)


# Convert it as a matrix
R<-as.matrix(g)
 
# Convert R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(R, "realRatingMatrix")

#view r
r
6040 x 3676 rating matrix of class ‘realRatingMatrix’ with 750156 ratings.

# view r in other possible ways
as(r, "list")     # A list
as(r, "matrix")   # A sparse matrix
 
# I can turn it into data-frame
head(as(r, "data.frame"))
 
# normalize the rating matrix
r_m <- normalize(r)

r_m
6040 x 3676 rating matrix of class ‘realRatingMatrix’ with 750156 ratings.
Normalized using center on rows.

as(r_m, "list")

# Draw an image plot of raw-ratings & normalized ratings
#  A column represents one specific movie and ratings by users
#   are shaded.
#   Note that some items are always rated 'black' by most users
#    while some items are not rated by many users
#     On the other hand a few users always give high ratings
#      as in some cases a series of black dots cut across items

image(r, main = "Raw Ratings")       
image(r_m, main = "Normalized Ratings")



#STEP2) Generate recommender

# Can also turn the matrix into a 0-1 binary matrix
r_b <- binarize(r, minRating=1)
as(r_b, "matrix")


# Create a recommender object (model)
#   Run anyone of the following four code lines.
#     Do not run all four
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
rec=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
rec=Recommender(r[1:nrow(r)],method="POPULAR")


# selected UBCF
> rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
Warning: Unknown parameters: minRating
Available parameter (with default values):
method	 =  cosine
nn	 =  25
sample	 =  FALSE
normalize	 =  center
verbose	 =  FALSE


>print(rec)
Recommender of type ‘UBCF’ for ‘realRatingMatrix’ 
learned using 6040 users.

> names(getModel(rec))
[1] "description" "data"        "method"      "nn"          "sample"     
[6] "normalize"   "verbose" 

> getModel(rec)$nn
[1] 5



#STEP3, generate predictions

############Create predictions#############################
# This prediction does not predict movie ratings for test.
#   But it fills up the user 'X' item matrix so that
#    for any userid and movieid, I can find predicted rating
#     dim(r) shows there are 6040 users (rows)
#      'type' parameter decides whether you want ratings or top-n items
#         get top-10 recommendations for a user, as:
#             predict(rec, r[1:nrow(r)], type="topNList", n=10)
recom <- predict(rec, r[1:nrow(r)], type="ratings")
recom


########## Examination of model & experimentation  #############
########## This section can be skipped #########################
 
# Convert prediction into list, user-wise
as(recom, "list")
# Study and Compare the following:
as(r, "matrix")     # Has lots of NAs. 'r' is the original matrix
as(recom, "matrix") # Is full of ratings. NAs disappear
as(recom, "matrix")[,1:10] # Show ratings for all users for items 1 to 10
as(recom, "matrix")[5,3]   # Rating for user 5 for item at index 3
as.integer(as(recom, "matrix")[5,3]) # Just get the integer value
as.integer(round(as(recom, "matrix")[6039,8])) # Just get the correct integer value
as.integer(round(as(recom, "matrix")[368,3717])) 
 

# Convert all your recommendations to list structure
rec_list<-as(recom,"list")
head(summary(rec_list))

# Access this list. User 2, item at index 2
rec_list[[2]][2]

# Convert to data frame all recommendations for user 1
u1<-as.data.frame(rec_list[[1]])
attributes(u1)
class(u1)

# Create a column by name of id in data frame u1 and populate it with row names
u1$id<-row.names(u1)
# Check movie ratings are in column 1 of u1
u1
# Now access movie ratings in column 1 for u1
u1[u1$id==3952,1]



# Read test file
test<-read.csv("test_v2.csv",header=TRUE)
head(test)
# Get ratings list
rec_list<-as(recom,"list")
head(summary(rec_list))
ratings<-NULL
# For all lines in test file, one by one
for ( u in 1:length(test[,2]))
{
   # Read userid and movieid from columns 2 and 3 of test data
   userid <- test[u,2]
   movieid<-test[u,3]
 
   # Get as list & then convert to data frame all recommendations for user: userid
   u1<-as.data.frame(rec_list[[userid]])
   # Create a (second column) column-id in the data-frame u1 and populate it with row-names
   # Remember (or check) that rownames of u1 contain are by movie-ids
   # We use row.names() function
   u1$id<-row.names(u1)
   # Now access movie ratings in column 1 of u1
   x= u1[u1$id==movieid,1]
   # print(u)
   # print(length(x))
   # If no ratings were found, assign 0. You could also
   #   assign user-average
   if (length(x)==0)
   {
     ratings[u] <- 0
   }
   else
   {
     ratings[u] <-x
   }
 
}
length(ratings)
tx<-cbind(test[,1],round(ratings))
# Write to a csv file: submitfile.csv in your folder
write.table(tx,file="submitfile.csv",row.names=FALSE,col.names=FALSE,sep=',')
# Submit now this csv file to kaggle
########################################


























