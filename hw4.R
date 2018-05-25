Emails <- read.csv("C:/Users/subha/Documents/Spring Semester/Analytics Edge/EnronEmails.csv", stringsAsFactors = FALSE) 
head(Emails)

cat((Emails$Text[1]), sep="\n")
m = regexpr("^Message-ID",Emails$Text,ignore.case = FALSE)

grep(m, Emails$Text, ignore.case = TRUE)




EmailsShort = gsub("Subject:.*$", "", Emails$Text)
mean(nchar(EmailsShort))

matches = regexpr("Date:.*From:", EmailsShort)

DateStrings = regmatches(EmailsShort, matches)
DateStrings

DateStrings= gsub("(Date: )|(\nFrom:)", "", DateStrings)
DatesOnly = DateStrings

DatesOnly


Dates = strptime(DatesOnly, "%a, %d %b %Y %H:%M:%S %z ", tz='EST') 

min(Dates)
max(Dates) - min(Dates)



matches = regexpr("From:.*To:", EmailsShort)

FromStrings = regmatches(EmailsShort, matches)
mean(nchar(FromStrings))

FromStrings= gsub("(From: )|(\nTo:)", "", FromStrings)
sort(table(FromStrings))
mean(nchar(FromStrings))


matches= regexpr("(Date: )*(\nFrom: )", EmailsShort)
matches
DateStrings = regmatches(EmailsShort, matches) 

DateStrings

matches = regexpr("To:.*$", EmailsShort)
ToStrings = regmatches(EmailsShort, matches)

mean(nchar(ToStrings))

ToStrings= gsub("(To:)", "", ToStrings)
ToStrings
mean(nchar(ToStrings))

Recipient = ToStrings

Recipient_at = gsub("[^@]","", Recipient)

Recipient_at

install.packages(stringr)
library(stringr)
at_counter<- str_count(Recipient_at, "@")
at_counter
mean(at_counter)


RecipientShort= gsub("(\n)|(\t)|(,)", " ", Recipient)
RecipientShort
mean(nchar(RecipientShort))

matches= gregexpr("[^ ]+",RecipientShort)
RecipientList = regmatches(RecipientShort, matches)

RecipientList[1]

Recipient = unlist(RecipientList)
MsgNumber = rep(1:length(RecipientList), lengths(RecipientList))
RecipData = data.frame(MsgNumber, Recipient)

NewData = data.frame(FromStrings, Dates)
NewData$MsgNumber = 1:nrow(NewData)
EmailData = merge(RecipData, NewData, by="MsgNumber")
glimpse(EmailData)
which.max(table(EmailData$FromStrings))
which.max(EmailData$Recipeint)

which.max(table(EmailData$FromStrings))
which.max(table(EmailData$FromStrings))
EmailData[115,]

DatesOnly[1]
Dates= strptime(DatesOnly, "%a, %d %b %Y %H:%M:%S %z ", tz='EST')
Dates[1]-Dates[2]

library(tidyverse)
glimpse(DatesOnly)
DatesOnly
sort(Dates)


emails <- read.csv("C:/Users/subha/Documents/Spring Semester/Analytics Edge/emails.csv", stringsAsFactors=FALSE) 
sum(emails$spam)
max(nchar(emails$text))
min(nchar(emails$text))
which(nchar(emails$text)== min(nchar(emails$text)))
library(tm)
#install.packages("SnowballC")
library(SnowballC)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation,lazy=TRUE, mc.cores = 1)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
length(stopwords("english"))
frequencies = DocumentTermMatrix(corpus)
sparse = removeSparseTerms(frequencies, 0.95)
emailsSparse = as.data.frame(as.matrix(sparse))
emailsSparse$spam = as.factor(emails$spam)

library(caret)

set.seed(2018)

TrainRows = createDataPartition(emailsSparse$spam, p = 0.7, list = FALSE)

spl = rep(FALSE,nrow(emailsSparse))

spl[TrainRows] = TRUE

train = subset(emailsSparse, spl==TRUE)

test = subset(emailsSparse, spl==FALSE)

spamLog = glm(spam~., data=train, family="binomial")

library(rpart)
library(rpart.plot)

spamCART = rpart(spam~., data=train, method="class")

set.seed(2018)

library(randomForest)
spamRF = randomForest(spam ~., data=train)

emailsSparse= as.data.frame(as.matrix(sparse))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

sort(colSums(subset(emailsSparse, spam == 0)))


subset(emailsSparse, spam == 1)
sort(colSums(subset(emailsSparse, spam == 1)))

library(caret)
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(2018)

TrainRows = createDataPartition(emailsSparse$spam, p = 0.7, list = FALSE)

spl = rep(FALSE,nrow(emailsSparse))

spl[TrainRows] = TRUE

train = subset(emailsSparse, spl==TRUE)
train$spam = as.factor(train$spam)
test = subset(emailsSparse, spl==FALSE)
test$spam = as.factor(test$spam)
test$spam

predTestLog = predict(spamLog, newdata=test, type="response")

predTestCART = predict(spamCART, newdata=test)[,2]

predTestRF = predict(spamRF, newdata=test, type="prob")[,2]

table(test$spam, predTestLog > 0.5)

(1267+378)/1718
predictionTestSpamLog = prediction(predTestLog, test$spam)
as.numeric(performance(predictionTestSpamLog,"auc")@y.values)
table(test$spam, predTestCART >.5)
predictionTestCART = prediction(predTestCART, test$spam)
as.numeric(performance(predictionTestCART, "auc")@y.values)
table(test$spam, predTestRF >.5)
1300+403
1703/1718
predictionTestRF = prediction(predTestRF, test$spam)
as.numeric(performance(predictionTestRF, "auc")@y.values)
