library(readr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(rattle)
library(RColorBrewer)

training <- read.csv("./assets/Marriage_training.csv") # training data for rpart
testing <- read.csv("./assets/Marriage_testing_students1.csv") # predict from this data

#randomize dataset
set.seed(9850)
g <- runif(nrow(training))
train.r <- training[order(g),]

# create a column for age differences.
train.r$AgeDiff <- abs(train.r$GroomAge - train.r$BrideAge)

# create a personality index of sorts.  see whose are the same, whose are different. add a column.
train.r$PersIndex <- "same"
train.r$PersIndex[train.r$GroomPersonality != train.r$BridePersonality] <- "opp"

#now create a decision tree from the randomized dataset
m1 <-
  rpart(
    STATUS ~ AgeDiff + Joint.Income + PersIndex,
    # data = train.r[1:700, ], used for testing purposes
    data = train.r,
    method = "class",
    control = rpart.control(minsplit = 3, minbucket=1)
  )

# Validate my model
CrossValidation::cross_validate(train.r, m1, 2, 0.8)
summary(m1)

#plots for fun
par(oma=c(0,0,2,0))
prp(m1, extra=104, type = 2, box.palette = "auto", shadow.col = "gray", nn=TRUE, fallen.leaves = TRUE, main = "m1 Decision Tree")

# let's try it on the actual test data.
# first, create a working copy, add a status column to it
test.w <- testing

# populate this with the same predictors from before (agediff and persindex)
test.w$AgeDiff <- abs(test.w$GroomAge - test.w$BrideAge)
test.w$PersIndex <- "same"
test.w$PersIndex[test.w$GroomPersonality != test.w$BridePersonality] <- "opp"

p2 <- as.data.frame(predict(m1, test.w, method="class"))

# create a new column to store the aggregated results
p2[,4] <- ""
colnames(p2)[4] <- "STATUS"


p2$STATUS[p2$Divorced == "1"] <- "Divorced"
p2$STATUS[p2$Separated == "1"] <- "Separated"
p2$STATUS[p2$Married == "1"] <- "Married"

# transfer those results back to the test
test.w$STATUS <- as.factor(p2$STATUS)
test.w$PersIndex <- as.factor(test.w$PersIndex)

#write outcome vector, shove that into a csv, call it a day.
outcome <- data.frame(ID=test.w$ID, STATUS=test.w$STATUS)
write.csv(outcome, "kaggle_pred3.csv", row.names = F)

#scrap area

sub.same <- subset(test.w, PersIndex == "same", select = c(Joint.Income, STATUS, AgeDiff))
sub.opp <- subset(test.w, PersIndex == "opp")

#set the cut for small vs large difference in age, populate a new column with the results.
# breaks=c(-1, n, 50) where n is the cutoff in age difference. submitted sol'n uses n=9.9

sub.opp[,10] <- cut(sub.opp$AgeDiff, breaks=c(-1, 9.9, 50), labels=c("< 20",  "\u2265 10")) #10 year cutoff 
#sub.opp[,10] <- cut(sub.opp$AgeDiff, breaks=c(-1, 19.9, 50), labels=c("< 20",  "\u2265 20")) #20 year cutoff for comparison

colnames(sub.opp)[10]<-"QDiff"

# collect some info about means for presentation
mean(sub.opp$AgeDiff) # mean age difference for oppositely paired couples
mean(sub.same$AgeDiff) # mean age difference for similarly paired couples
median(sub.opp$AgeDiff) # median just cuz
mean(test.w$AgeDiff) # mean age difference, all couples
tapply(test.w$AgeDiff, test.w$STATUS, mean) # mean age difference by status
tapply(test.w$AgeDiff, test.w$STATUS, median) # median age difference by status
tapply(test.w$AgeDiff, test.w$PersIndex, mean) #mean age difference by parity
tapply(test.w$AgeDiff, test.w$PersIndex, median) #median age difference by parity

tapply(test.w$Joint.Income, test.w$STATUS, mean) # mean joint income by status
tapply(test.w$Joint.Income, test.w$PersIndex, mean) # mean joint income by parity
tapply(test.w$Joint.Income, test.w$STATUS, median) # median joint income by status
tapply(test.w$Joint.Income, test.w$PersIndex, median) # median joint income by parity

#plot counts and age diffs over marital status change cutoff in line above
#'breaks = c(-1,n,50)' and don't forget to change it in ggtitle too for the plot
ggplot(sub.opp, aes(x=STATUS)) + geom_bar(alpha=0.4, color="black", aes(fill=QDiff)) + ggtitle("Oppositely Paired Couples by Marital Status and Age Difference \n(10 Year Cutoff)") + xlab("Marital Status") + ylab("Couples") + labs(fill="Age \nDifference")
#uncomment the following line for 20 year age cutoff
#ggplot(sub.opp, aes(x=STATUS)) + geom_bar(alpha=0.4, color="black", aes(fill=QDiff)) + ggtitle("Oppositely Paired Couples by Marital Status and Age Difference \n(20 Year Cutoff)") + xlab("Marital Status") + ylab("Couples") + labs(fill="Age \nDifference")

