# recommendation for amazon data set
# there are three methods I have done recommendations
# 1. Binary ratings
# 2. Score ratings
# 3. Recommendarlab package
# used libraries: 
# 1. tm 2. qdap 3.data.table 4. Shiny 5. Recommendarlab
############################################################################
# setting the working directory
datapath <- "C:/Users/c_ymelpati/Desktop/data science/data/data.world"
# for personal lappy
datapath <- "C:/Users/yeshu/Desktop/datascience_projects/amazon recommendation"
setwd(datapath)
#############################################################################
# loading the data
data <- read.csv('amazon.csv',stringsAsFactors = FALSE)

#############################################################################
## extracting the reviews column
user_rev <- data$customer_reviews
library(tm)
## function to clean the reviews with tm package
basic_clean <- function(user_rev){
  user_rev <- tolower(user_rev)
  user_rev <- removePunctuation(user_rev)
  user_rev <- removeNumbers(user_rev)
  user_rev <- stripWhitespace(user_rev)
  return(user_rev)
}
user_rev <- basic_clean(user_rev)

# to convert all special charecters with their transalation
user_rev <- iconv(user_rev, to = "ASCII//TRANSLIT")
# to remove all stopwords in engilsh
user_rev <- removeWords(user_rev,stopwords('en'))
library(qdap)
# function to clean reviews with qdap library
clean_qdap <- function(user_rev){
  user_rev <- bracketX(user_rev)
  user_rev <- replace_number(user_rev)
  user_rev <- replace_abbreviation(user_rev)
  user_rev <- replace_contraction(user_rev)
  user_rev <- replace_symbol(user_rev)
  user_rev <- removeWords(user_rev,stopwords('en'))
  user_rev <- gsub("\\W*\\b\\w\\b\\W*", " ", user_rev)
  user_rev <- removePunctuation(user_rev)
  return(user_rev)
}
user_rev <- clean_qdap(user_rev)
user_rev <- tolower(user_rev)

# cleaning by converting to Vector Corpus
rev_sr <- VectorSource(user_rev)
rev_cp <- VCorpus(rev_sr)

# function to clean the corpus
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  return(corpus)
}
rev_cl <- clean_corpus(rev_cp)
###################################################################
# for single word frequency
# making term document matrix
rev_tdm <- TermDocumentMatrix(rev_cl)
# making a sparse terms matrix
sparsmat <- removeSparseTerms(rev_tdm,sparse = 0.9)
rev_m <- as.matrix(sparsmat)
# calculating the frequencies of SINGLE WORDS
term_frequency <- rowSums(rev_m)
term_frequency <- sort(term_frequency,decreasing = TRUE)
################ the above is singple term frequency ##############

###################################################################
# for biwords frequency
### extracting the feature words i.e bi words 
library(RWeka)
# making the tokenizer function
tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

rev_f_tdm <- TermDocumentMatrix(
  rev_cl,
  control = list(tokenize = tokenizer)
)
# making a sparse terms matrix
sparsmat <- removeSparseTerms(rev_f_tdm,sparse = 0.99)
rev_f_m <- as.matrix(sparsmat)
rev_freq <- rowSums(rev_f_m)
rev_freq <- sort(rev_freq,decreasing = TRUE)
head(rev_freq)

# taking all biwords generated
totalbiwords <- totalbiwords <- rownames(rev_f_m)

# taking only high frequency biwords
topbiwords <- names(head(rev_freq,20))

# calculating the indeces of these top biwords in rev_f_m matrix
indtopbiwords <- which(totalbiwords %in% topbiwords)

# getting the matrix for these topbiwords
topmat <- rev_f_m[indtopbiwords,]
transtopmat <- t(topmat)
# transtopmat is products and their bi key words present or not
#####################################################################
# extracting the products and the ratings data frames
products <- subset(data, select = c(uniq_id,product_name,
                                    price,customer_reviews))
ratings <- subset(data,select = c(uniq_id,average_review_rating))

######################################################################
# products data frame processing
# removing customer_reviews un processed column
products$customer_reviews <- NULL
# adding the transtopmat processed reviews based on bikey words
products <- cbind(products,transtopmat)

######################################################################
### ratings data frame processing
# extracting the actual rating for a product in numeric form
ratings$average_review_rating <- as.numeric(substr(ratings$average_review_rating,1,3))
# add user_id columns to the ratings, as users column is not present in data set
# we will assume 100 users 
users <- sample(1:100,nrow(ratings),replace = TRUE)
ratings$user_id <- users
# changing the colnames 
colnames(ratings) <- c("product_id","rating","user_id")
## finding all na is rating column in ratings data frame and replace with 2
which(is.na(ratings$rating))

######################################################################
# method : 1
# binary ratings
binaryratings <- ratings
# changing all NA' s to value of 2
binaryratings[which(is.na(binaryratings$rating)),]$rating <- 2
which(is.na(binaryratings))
####### binary ratings start
# getting the NA's present in binaryratings
apply(binaryratings,2,function(x) which(is.na(x)))

# converting to binary ratings so that A user of rated <3 is disliked by him
for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,2] > 3){
    binaryratings[i,2] <- 1
  }
  else{
    binaryratings[i,2] <- -1
  }
}
library(data.table)
# convert binaryratings matrix to the correct format:
# dcast makes the gather related effect
binaryratings2 <- dcast(binaryratings, product_id~user_id, value.var = "rating", na.rm=FALSE)
# substituting NA with 0
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
# Rows are productId, cols are userIds
binaryratings2 = binaryratings2[,-1] #remove productId col. 

###################################################################
productsIds <- length(unique(products$uniq_id)) #10000
ratingproductIds <- length(unique(ratings$product_id)) #10000

uniq_users <- length(unique(ratings$user_id))
result = matrix(0,20,uniq_users) # here 20 is top bi key words and unique users are 100

## Calculate dot product for User Profiles
## for a user a product is liked then the products bi keywords are summed
## how much a user likes a bi key word
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(transtopmat)){
    result[i,c] <- sum( (transtopmat[,i])
                        * (binaryratings2[,c]) ) #ratings per genre
  }
}
################################################################
# Convert to Binary scale only for recommends in binary result
# for simplicity this is made 1 or 0
# we use median to bifurcate like for particular review
# this is not done for recommends by score result
med <- median(result)
for (c in 1:ncol(result)){
  for (i in 1:nrow(result)){
    if (result[i,c] < med ){
      result[i,c] <- 0
    }
    else {
      result[i,c] <- 1
    }
  }
}

#################################################################
# this method is after binary convert of result and matching most
# user 1: recommendations
result[,1]
matched <- c()
for( i in 1:nrow(transtopmat)){
  tempsum <- sum(transtopmat[i,] == result[,1] )
  matched <- c(matched,tempsum)
}
matchdf <- data.frame(matched , c(1:nrow(transtopmat)))
colnames(matchdf) <- c("matchsum","productId")
attach(matchdf)
sortmatchdf <- matchdf[order(-matchsum),]
ind <- (head(sortmatchdf)$productId)
# matchpercent is % how much his interest is matched with the product
matchpercent <- (head(sortmatchdf$matchsum)/20)*100
recommended_products <- products[ind,]
recommended_products$percentagematch <- matchpercent
recommended_products
#################################################################
## method : 2
# this method is calculating square
# first user recommendations using his score for each bi key word

ratings_without_na <- ratings
ratings_without_na[which(is.na(ratings_without_na$rating)),]$rating <- 0
which(is.na(ratings_without_na))
max(ratings_without_na$rating)
min(ratings_without_na$rating)
score_ratings <- dcast(ratings_without_na, product_id~user_id, value.var = "rating", na.rm=FALSE)

for( i in 1: ncol(score_ratings)){
  score_ratings[which(is.na(score_ratings[,i]) == TRUE),i] <- 0
}
score_ratings <- score_ratings[,-1]
score_ratings[,1] # user 1 ratings for each product

score_result = matrix(0,20,uniq_users) # here 20 is top bi key words and unique users are 100

## Calculate dot product for User Profiles
## for a user a product is liked then the products bi keywords are summed
## how much a user likes a bi key word
for (c in 1:ncol(score_ratings)){
  for (i in 1:ncol(transtopmat)){
    score_result[i,c] <- sum( (transtopmat[,i])
                        * (score_ratings[,c]) ) #ratings per genre
  }
}
# user 1 recommendations:
score_result[,1] # user 1 score for each bi key word
scorelist <- c()
for(i in 1:nrow(transtopmat)){
  tempscore <- sum(transtopmat[i,] * score_result[,1])
  scorelist <- c(scorelist,tempscore)
}
scoredf <- data.frame(scorelist , c(1:nrow(transtopmat)))
colnames(scoredf) <- c('score','productId')
attach(scoredf)
sortscoredf <- scoredf[order(-score),]
ind <- (head(sortscoredf)$productId)
scoredf[ind,]
# recommendations
recommended_products_score <- products[ind,]
recommended_products_score
#################################################################
# method 3:
library(recommenderlab)
lab_ratings <- dcast(ratings, user_id~product_id, value.var = "rating", na.rm=FALSE)
lab_ratings <- lab_ratings[,-1]
colnames(lab_ratings) <- 1:nrow(ratings)
rownames(lab_ratings) <- paste0('user',1:100)

# rows are products and columns are users
lab_ratings<- as.matrix(lab_ratings)
lab_real_rating <- as(lab_ratings, "realRatingMatrix")
ratingmat_norm <- normalize(lab_real_rating)
recommender_model <- Recommender(lab_real_rating, 
                                 method = "POPULAR")
model_details <- getModel(recommender_model)
names(model_details)



##################################################################
# user 1: method 3.

recom <- predict(recommender_model, 
                 lab_real_rating[1,], 
                 n=5) #Obtain top 5 recommendations for 1st user in dataset


recom_list <- as(recom, 
                 "list") #convert recommenderlab object to readable list
recom_list
lab_recommends <- products[as.integer(recom_list[[1]]),]
#################################################################
# function to make recommendations using recommendarlab package
lab_recommendation_fun <- function(value){
  recom <- predict(recommender_model, 
                   lab_real_rating[value,], 
                   n=5) #Obtain top 5 recommendations for 1st user in dataset
  recom_list <- as(recom, 
                   "list") #convert recommenderlab object to readable list
  lab_recommends <- products[as.integer(recom_list[[1]]),]
  lab_recommends
}
#################################################################
## making function for recommended products by score:
recommend_score <- function(value){
  scorelist <- c()
  for(i in 1:nrow(transtopmat)){
    tempscore <- sum(transtopmat[i,] * result[,value])
    scorelist <- c(scorelist,tempscore)
  }
  scoredf <- data.frame(scorelist , c(1:nrow(transtopmat)))
  colnames(scoredf) <- c('score','productId')
  attach(scoredf)
  sortscoredf <- scoredf[order(-score),]
  ind <- (head(sortscoredf)$productId)
  # recommendations
  recommended_products_score <- products[ind,]
  recommended_products_score
}
#########comparing results ######################################
transtopmat[ind[1],]
result[,1]
rbind(transtopmat[ind,],result[,1])
#################################################################
# making function for reocmmend by binary result:
recommend_function <- function(value){
  # value is the user id
  matched <- c()
  for( i in 1:nrow(transtopmat)){
    tempsum <- sum(transtopmat[i,] == result[,value] )
    matched <- c(matched,tempsum)
  }
  matchdf <- data.frame(matched , c(1:nrow(transtopmat)))
  colnames(matchdf) <- c("matchsum","productId")
  attach(matchdf)
  sortmatchdf <- matchdf[order(-matchsum),]
  ind <- (head(sortmatchdf)$productId)
  #######optinal finding percentage of match is optional#####
  matchpercent <- (head(sortmatchdf$matchsum)/20)*100
  recommended_products <- products[ind,]
  recommended_products$percentagematch <- matchpercent
  recommended_products
}

#################################################################
#### function to recommend as display UI shiny 
visualizerecommend(lab_recommendation_fun)
visualizerecommend(recommend_function) # visualize binary result recommendations
visualizerecommend(recommend_score) # visualize score result recommendation
#################################################################
# function to visualize UI shiny
visualizerecommend <- function(value){
  
  library(shiny)
  
  
  # Define UI for application that plots features of movies
  ui <- fluidPage(
    
    # Sidebar layout with a input and output definitions
    sidebarLayout(
      
      # Inputs
      sidebarPanel(
        n_total <- 100,
        # Text instructions
        HTML(paste("Enter a value i.e userid between 1 and",n_total,"to get recommmends")),
        
        # Numeric input for sample size
        numericInput(inputId = "n",
                     label = "User id:",
                     min = 1,
                     max = n_total,
                     value = 25,
                     step = 1)
        
      ),
      
      # Output: Show data table
      mainPanel(
        DT::dataTableOutput(outputId = "recommendstable")
      )
    )
  )
  
  
  server <- function(input, output) {
    
    # Create data table
    output$recommendstable <- DT::renderDataTable({
      req(input$n)
      recommends <- value(input$n)
      DT::datatable(data = recommends, 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    })
    
  }
  
  # Create a Shiny app object
  shinyApp(ui = ui, server = server)
  
}


