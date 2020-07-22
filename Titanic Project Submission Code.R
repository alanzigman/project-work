# Import libraries and datasets -------------------------------------------

library(tidyverse)
library(ggplot2)
library(e1071)

training_set <- read.csv('train.csv', na.strings = "")
test_set <- read.csv('test.csv', na.strings = "")

#Pull out PassengerId for submission file later
PassengerId <- test_set$PassengerId

#Combine into one data set

#test_set$Survived <- NA
#data <- rbind(training_set,test_set)

#Using bind_rows will automatically fill uneven columns with NA
#In this case the test set Survived column will be filled with NAs, saving a line of code shown above
data <- bind_rows(training_set,test_set)

# Exploratory data analysis -----------------------------------------------

summary(data)
str(data)

#Column 1 - PassengerId 
summary(data$PassengerId)
#Won't have an impact on the dependent variable. This column can be removed

#Column 2 - Survived variable - dependent variable
summary(data$Survived)
str(data$Survived)
data %>%
  group_by(data$Survived) %>%
  summarise(sum = n())
#418 missing values are from our test data set

#Column 3 - Pclass 
summary(data$Pclass)
str(data$Pclass)
data %>%
  group_by(data$Pclass) %>%
  summarise(sum = n())
table(data$Survived, data$Pclass)
#Split into three values: 1, 2, and 3. No missing values.
#Passengers in class 1 had the best chance of survival - each other class had more dead than survived
#In integer form - needs to be factorized

#Column 4 - Name
summary(data$Name)
str(data$Name)
data %>%
  group_by(data$Name) %>%
  summarise(sum = n())
#Names of the passengers - not going to have an impact on survival. While gender and age (which can be gleaned from Name) are captured in other variables
#This column can be removed

#Column 5 - Sex
summary(data$Sex)
str(data$Sex)
data %>%
  group_by(data$Sex) %>%
  summarise(sum = n())
table(data$Survived, data$Sex)
#Sex of the passengers split into two variables: female and male. No missing values. 
#Clearly females have a better chance of survival looking at the table
#Already in factor form - needs to be encoded

#Column 6 - Age
summary(data$Age)
str(data$Age)
data %>%
  group_by(data$Age) %>%
  summarise(sum = n())
ggplot(data, aes(x = Survived, y = Age, group = Survived)) + geom_boxplot()
#Age of passengers from 0.17 to 80. Includes 263 NA values. Younger passengers have slightly better odds to survive - but relatively even
#Will replace NA values with mean or median

#Column 7 - SibSp
summary(data$SibSp)
str(data$SibSp)
data %>%
  group_by(data$SibSp) %>%
  summarise(sum = n())
ggplot(data[1:891,], aes(x = SibSp, fill = factor(Survived))) + geom_bar(position = 'dodge') + scale_x_continuous(breaks = c(0:8))
table(data$Survived, data$SibSp)
#Number of siblings/spouse per passenger between 0 and 8. No missing values.
#Having 1 Sib/Sp is the only value where chances are survival are better than chance of death

#Column 8 - Parch
summary(data$Parch)
str(data$Parch)
data %>%
  group_by(data$Parch) %>%
  summarise(sum = n())
ggplot(data[1:891,], aes(x = Parch, fill = factor(Survived))) + geom_bar(position = 'dodge') + scale_x_continuous(breaks = c(0:8))
table(data$Survived, data$Parch)
#Number of parents/children per passenger between 0 and 9. No missing values.
#Having 1 to 3 Parch improves chance of survival compared to other values

#Column 9 - Ticket
summary(data$Ticket)
str(data$Ticket)
data %>%
  group_by(data$Ticket) %>%
  summarise(sum = n())
#Ticket name for each passenger. Includes combination of letters and numbers. 929 unique values. Will not impact survival and will remove column.

#Column 10 - 
summary(data$Fare)
str(data$Fare)
unique(data$Fare)
data %>%
  group_by(data$Fare) %>%
  summarise(sum = n())
#Price of fare for each passenger. Between 0 and 512.329. Includes one missing value
#Will replace missing value with Median value due to outlier high values

#Column 11 - 
summary(data$Cabin)
str(data$Cabin)
data %>%
  group_by(data$Cabin) %>%
  summarise(sum = n())
#There are 1014 blank/missing values in this column - as that is such a high percentage of overall observations in the column we will remove the column

#Column 12 -
summary(data$Embarked)
str(data$Embarked)
data %>%
  group_by(data$Embarked) %>%
  summarise(sum = n())
#Point of embarkation for each passenger. Contains 3 levels: S, Q, C. And 2 NAs
#Will replace NAs values with overwhelmingly most common variable, S, and then encode the variable

# Handling missing values and removing unnecessary variables -------------------------------------------------

data$PassengerId <- NULL
data$Name <- NULL
data$Ticket <- NULL
data$Cabin <- NULL

sort(colSums(is.na(data)), decreasing = TRUE)

data$Age <- replace_na(data$Age, mean(data$Age, na.rm = TRUE))

data$Fare <- replace_na(data$Fare, median(data$Fare, na.rm = TRUE))

data$Embarked <- replace_na(data$Embarked, 'S')

sort(colSums(is.na(data)), decreasing = TRUE)

# Encoding/factoring variables -------------------

data$Pclass <- factor(data$Pclass)

data$Sex <- factor(data$Sex, levels = c('male', 'female'), labels = c(1,2))

data$Embarked <- factor(data$Embarked, levels = c('Q', 'C', 'S'), labels = c(1,2,3))

#If we aren't looking to customize the levels and labels when factoring, can use:
#lapply(data[,c(numeric columns)], factor)
#This will factor all inputted columns in one line of code

# Some more EDA (don't need to run every time) -----------------------------------------------------------

#Correlation matrix for numeric columns
test <- data[1:891,c(1,4,5,6,7)]
cor(test)

#Convert factor variables to numeric and run correlation matrix again
test <- data[1:891,]
str(test)

test$Sex <- as.numeric(test$Sex)
test$Pclass <- as.numeric(test$Pclass)
test$Embarked <- as.numeric(test$Embarked)

cor(test)
#Most correlated variables with Survived are: 1. Sex, 2. Pclass, 3. Fare, 4. Embarked, 5. Parch, 6. Age, 7. SibSp

# Create classification model ---------------------------------------------

#Try running model with just top 3 variables (only ones with correlation over |0.25|)
#head(data)
#data <- data[,c(1,3,2,7)]
###Results were not as good as with entire dataset

training_set <- data[1:891,]
test_set <- data[892:1309,]

classifier <- svm(formula = Survived ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

#Predicting the Test set results
y_pred <- predict(classifier, newdata = test_set[-1])

submission <- data.frame(PassengerId = PassengerId, Survived = y_pred)

write.csv(submission, file = 'ksvmsubmission.csv', row.names = F)





