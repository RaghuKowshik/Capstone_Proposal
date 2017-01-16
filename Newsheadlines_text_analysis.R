
#0: load csv data into R data frame
headlines <- read.csv(file="headlines.csv", stringsAsFactors = FALSE)
str(headlines)

#Define new variable called "Up" to indicate when the stock trend is up
headlines$Up = as.factor(headlines$trend==1)
table(headlines$Up)

#Install package tm. and load package tm
install.packages("tm")
library(tm)

#Install SnowballC package
install.packages("SnowballC")
library(SnowballC)

#Convert the headlines into a "corpus" for pre-processing 
#using the "tm" package, using "corpus" and "vectorsource" functions
corpus = Corpus(VectorSource(headlines$HL1))
corpus
corpus[[1]]

#Convert the headlines to all lowercase
corpus = tm_map(corpus, tolower)
corpus[[1]]

#Remove punctuation from the headlines
corpus = tm_map(corpus, removePunctuation)
corpus[[2]]

#Remove the stopwords from the headlines
stopwords("english")[1:150]
corpus = tm_map(corpus, removeWords, c(stopwords("english")))

#Stem the document with StemDocument argument
corpus = tm_map(corpus, stemDocument)

#Extract word frequencies to be used for prediction
#Generate a matrix called "frequencies" with rows corrspond to documents (headlines) and
#columns correspond to words in the headlines. Value in the matrix is
#the number of times the word appears in the headlines

#To use the DocumentTermMatrix function first convert the corpus into  plain text document
corpus = tm_map(corpus, PlainTextDocument)

#Generate the document term matrix
frequencies = DocumentTermMatrix(corpus)

#Check the "frequencies" matrix
frequencies
#There are 520 words and 84 headlines in HL1

#Use the inspect function to check the matrix
inspect(frequencies[10:50, 10:100])


#Find the popular terms in the headlines
findFreqTerms(frequencies, lowfreq = 3)

#Specify sparsity threshold to remove sparse terms
sparse = removeSparseTerms(frequencies, 0.995)

#Check sparsity
sparse

#Convert sparse matrix into a datsframe to use in the prediction model
headlinesSparse = as.data.frame(as.matrix(sparse))

#Ensure that all variable names are apropriate for processing
colnames(headlinesSparse) = make.names(colnames(headlinesSparse))

#Add dependent variable "Up" to our dataframe, indicating the trend going up
headlinesSparse$Up = headlines$Up

#Add the caTools package to use the sapmple size split function
library(caTools)

#Set the seed
set.seed(123)

#split the sample data into training(70%) and test(30%) data sets
split = sample.split(headlinesSparse$Up, SplitRatio = 0.7)
trainSparse = subset(headlinesSparse, split==TRUE)
testSparse = subset(headlinesSparse, split==FALSE)

#Use CART and Logistic Regression to predict stock trend
#Load the necessary libraries
library(rpart)
library(rpart.plot)

#Build CART model
headlinesCART = rpart(Up ~.,data = trainSparse, method = "class")

#Plot using the prp function
prp(headlinesCART)


#Numerically evaluate the CART prediction
predictCART = predict(headlinesCART, newdata=testSparse, type = "class")
table(testSparse$Up, predictCART)
#     predictCART
#       FALSE TRUE
#FALSE     1   12
#TRUE      2   11

#Accuracy of our CART prediction model
(1+11)/(1+11+2+12)
# = 0.4615385 = 46% accuracy

#Baseline model accuracy
table(testSparse$Up)
#FALSE  TRUE 
#  13    13
13/(13+13)
# = 0.5 = 50%

#Build a Random Forest Model. 
library(randomForest)

headlinesRF = randomForest(Up ~.,data = trainSparse)

#Numerically evaluate the random forest model prediction
predictRF = predict(headlinesRF, newdata = testSparse)
table(testSparse$Up, predictRF)

#        predictRF
#        FALSE TRUE
# FALSE     5    8
# TRUE      3   10

#Accuracy of the random forest model
(5+10)/(5+10+3+8)
# = 0.5769231 = 57.7%


