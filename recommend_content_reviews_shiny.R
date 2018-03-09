# recommendation for amazon data set

# setting the working directory
datapath <- "C:/Users/c_ymelpati/Desktop/data science/data/data.world"
setwd(datapath)

# loading the data
data <- read.csv('amazon.csv',stringsAsFactors = FALSE)

## extracting the reviews colun
user_rev <- data$customer_reviews
## reducing the size of reviews
#user_rev <- user_rev[1:100]
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

# cleaning by converting to Corpus
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
################ the aboe is singple term frequency ###############

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
# transtopmat is ~ genre_matrix2
#####################################################################
products <- subset(data, select = c(uniq_id,product_name,
                                    price,customer_reviews))
#products <- products[1:100,]
ratings <- subset(data,select = c(uniq_id,average_review_rating))

#ratings <- ratings[1:100,]


products$customer_reviews <- NULL
products <- cbind(products,transtopmat)

###################################################################
### ratings 
ratings$average_review_rating <- as.numeric(substr(ratings$average_review_rating,1,3))
# add user_id columns to the ratings

# we will assume 100 users 
users <- sample(1:100,nrow(products),replace = TRUE)
ratings$user_id <- users
# changing the colnames 
colnames(ratings) <- c("product_id","rating","user_id")

## finding all na is rating column in ratings data frame and replace with 2
which(is.na(ratings$rating))
ratings[which(is.na(ratings$rating)),]$rating <- 2


binaryratings <- ratings
####### binary ratings start
# getting the NA's present in binaryratings
apply(binaryratings,2,function(x) which(is.na(x)))


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
# Rows are movieIds, cols are userIds
binaryratings2 = binaryratings2[,-1] #remove movieIds col. 

#########################################################
productsIds <- length(unique(products$uniq_id)) #10000
ratingproductIds <- length(unique(ratings$product_id)) #10000

uniq_users <- length(unique(ratings$user_id))
result = matrix(0,20,uniq_users) # here, 668=no of users/raters, 18=no of genres

#Calculate dot product for User Profiles
## for a user a movie is liked then the movie genres are summed
## how much a user likes a genre
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(transtopmat)){
    result[i,c] <- sum( (transtopmat[,i])
                        * (binaryratings2[,c]) ) #ratings per genre
  }
}

#Convert to Binary scale
# for simplicity this is made 1 or 0
## we use median to bifurcate like for particular review
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
# user 1:
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
head(sortmatchdf)
ind <- (head(sortmatchdf)$productId)
matchpercent <- (head(sortmatchdf$matchsum)/20)*100
matchpercent
recommended_products <- products[ind,]
recommended_products$percentagematch <- matchpercent
recommended_products


#########comparing results
transtopmat[ind[1],]
result[,1]
rbind(transtopmat[ind,],result[,1])
####

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
user2 <- recommend_function(2)
user2
visualizerecommend(user2)

visualizerecommend <- function(value){
  
  library(shiny)
  mydata <- as.data.frame(value)
  
  # UI
  ui <- fluidPage(
    tableOutput(outputId = "datatable")
  )
  
  # Define server function required to create the scatterplot-
  server <- function(input, output, session) {
    # Take a reactive dependency on input$button, but not on any other inputs
    output$datatable <- renderTable({
      mydata
    })
  }
  
  # Create a Shiny app object
  shinyApp(ui = ui, server = server)
  
}


