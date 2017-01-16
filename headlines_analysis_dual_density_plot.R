library(tm)
library(magrittr)
library(Matrix)
library(glmnet)
library(ROCR)
library(ggplot2)


#Read the headlines dataset
headlines <- read.csv("headlines.csv", stringsAsFactors = FALSE)

str(headlines)

#Make "Date" column a date object
headlines$Date <- as.Date(headlines$Date)

str(headlines)

#Combine headlines into one text group for each day and add sentence separation.
headlines$All <- paste(headlines$HL1,  
                       headlines$HL2,
                       headlines$HL3,
                       headlines$HL4,
                       headlines$HL5,
                       headlines$HL6,
                       headlines$HL7,
                       headlines$HL8,
                       headlines$HL9,
                       headlines$HL10,
                       headlines$HL11,
                       headlines$HL12,
                       headlines$HL13,
                       headlines$HL14,
                       headlines$HL15,
                       headlines$HL16,
                       headlines$HL17,
                       headlines$HL18,
                       headlines$HL19,
                       headlines$HL20,
                       sep=" <s> ")
                       
# Get rid of those pesky b's and backslashes
headlines$All <- gsub('b"|b\'|\\\\|\\"', "", headlines$All)

# Get rid of all punctuation except headline separators
headlines$All <- gsub("([<>])|[[:punct:]]", "\\1", headlines$All)

# Reduce to only the three columns we need.
headlines <- headlines[, c('Date', 'Label', 'All')]

#Convert the text headlines into a document-term matrix via a Corpus object using tm.
#Each row of the document-term matrix will be the combined headlines for each day.
#Columns will be frequency counts of unigrams.
#The control object will tell the DocumentTermMatrix() function what we want to do with the text before converting to a term matrix.
#Punctuation and numbers are removed, everything is converted to lowercase, and 
#common words are removed since they will likely have little predictive power.

headlines_corpus <- list(removeNumbers = TRUE,
                         tolower = TRUE,
                         stopwords = c(stopwords(kind = "SMART"), "<s>"))

str(headlines_corpus)

dtm <- Corpus(VectorSource(headlines$All)) %>% DocumentTermMatrix(control = headlines_corpus)


#Now we split the data into train and test sets
split_index <- headlines$Date <= "2016-10-11"

ytrain <- as.factor(headlines$Label[split_index])
xtrain <- Matrix(as.matrix(dtm)[split_index, ], sparse=TRUE)

ytest <- as.factor(headlines$Label[!split_index])
xtest <- Matrix(as.matrix(dtm)[!split_index, ], sparse=TRUE)


#Train the model
glmnet.fit <- cv.glmnet(x=xtrain, y=ytrain, family='binomial', alpha=0)


#Generate predictions
preds <- predict(glmnet.fit, newx=xtest, type='response', s='lambda.min')

# Put results into dataframe for plotting.
results <- data.frame(pred=preds, actual=ytest)


#Plot the dual densities. 
#Here we are plotting densities of the predicted probabilites.
#We do this for all predicted probabilities for each of the true values 0 (down) or 1 (up or unchanged).
#As suspected there is no good value for the probability threshold. 

ggplot(results, aes(x=preds, color=actual)) + geom_density()


#Now we use the ROCR package to assess performance. 
#First we create two performance objects: 
#one for the true positive/false positive rates, and 
#one for the AUC score.

prediction <- prediction(preds, ytest)


#The returned result of prediction is an object class prediction, 
#which is an S4 object with a series of slots
#Let us look at the length of each slot and the class:
#We see that each slot has length 1 and is a list.
sn = slotNames(prediction)
sapply(sn, function(x) length(slot(prediction, x)))
sapply(sn, function(x) class(slot(prediction, x)))


#Performance Object for True positive/false positive rates
perf <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)


#Performance object for the AUC score.
auc <- performance(prediction, measure = "auc")
auc <- auc@y.values[[1]]

#The AUC score is 0.6064. This implies that prediction is a little bit better than random guess. 


#Plot the AUC curve
roc.data <- data.frame(fpr=unlist(perf@x.values), tpr=unlist(perf@y.values))

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_ribbon(alpha=0.2) +	
  geom_line(aes(y=tpr)) +
  geom_abline(slope=1, intercept=0, linetype='dashed') +
  ggtitle("ROC Curve") +
  ylab('True Positive Rate') +
  xlab('False Positive Rate')

#Plot shows that our model is not performing better than a null model of random guesses.
#Let's now do the same thing, but this time we will use bigrams rather than individual words.
#We'll use the NGramTokenizer from the RWeka package to build a bigram tokenizer, then feed it to our control list. 
#We will also set some bounds. We will only use bigrams that appear in at least 20 of the documents.
#This will eliminate some of the cruft. 
#We won't use bigrams that appear in more than 500 of the documents.
#This will keep the size of our DocumentTermMatrix under control, otherwise we may run into memory issues.

#Install package RWeka
library(RWeka)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

control <- list(
  tokenize=BigramTokenizer,
  bounds = list(global = c(20, 500)))

dtm <- Corpus(VectorSource(headlines$All)) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  DocumentTermMatrix(control=headlines_corpus)

split_index <- headlines$Date <= '2016-10-11'


ytrain <- as.factor(headlines$Label[split_index])
xtrain <- Matrix(as.matrix(dtm)[split_index, ], sparse=TRUE)

ytest <- as.factor(headlines$Label[!split_index])
xtest <- Matrix(as.matrix(dtm)[!split_index, ], sparse=TRUE)

#Fit a glmnet model exactly the same as before, setting alpha to 0 indicating that we want to use ridge regression.

# Train the model
glmnet.fit <- cv.glmnet(x=xtrain, y=ytrain, family='binomial', alpha=0)

# Generate predictions
preds <- predict(glmnet.fit, newx=xtest, type='response', s="lambda.min")

# Put results into dataframe for plotting.
results <- data.frame(pred=preds, actual=ytest)

#Plot the dual densities again. Looks like we may have a little bit of separation this time!
  
  ggplot(results, aes(x=preds, color=actual)) + geom_density()

  
#Now let's use the ROCR package to assess performance again.
prediction <- prediction(preds, ytest)
perf <- performance(prediction, measure = "tpr", x.measure = "fpr")
  
auc <- performance(prediction, measure = "auc")
auc <- auc@y.values[[1]]

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  geom_abline(slope=1, intercept=0, linetype='dashed') +
  ggtitle("ROC Curve") +
  ylab('True Positive Rate') +
  xlab('False Positive Rate')

#The AUC score is,0.6064, not better than the first try and therefore the prediction is little bit better than random guessing! 
#Also, that we dont know the standard deviation of this score.

#Next Steps
#1) The sample headlines and stock price data is for 4 months.There are 84 observations
#   42 observations show a upward (1) trend, and 42 observations show a downward (0) trend, are equally divided. 
#2) Larger sample size may improve the prediction performance. I propose to use 5 year data.
#3) Naive  Bayes and Support Vector Machines may give better results.
#4) Lagging the independent variables
#5) Vary the bounds of bigrams



