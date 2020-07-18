setwd("~/Desktop/R Projects and Libraries/nba-player-advanced-metrics-master")

dataset <- read.csv('nba-data-historical.csv')
dataset_2010_2020 <- dataset[dataset$year_id > 2009,]
dataset_2010_2019 <- dataset_2010_2020[dataset_2010_2020$year_id < 2020,]
dataset_2020 <- dataset_2010_2020[dataset_2010_2020$year_id == 2020,]
dataset_2020_names <- dataset_2010_2020[dataset_2010_2020$year_id == 2020,]

#remove Position column that we don't need - name and year id
dataset_2010_2019 <- dataset_2010_2019[,3:38]
dataset_2020 <- dataset_2020[,3:38]

#replace NA data with 0
dataset_2010_2019[is.na(dataset_2010_2019)] <- 0
dataset_2020[is.na(dataset_2020)] <- 0

#Split data 2010-2019 into test set and training set
library(caTools)
split = sample.split(dataset_2010_2019$AllNBA, SplitRatio = 0.80)
training_set = subset(dataset_2010_2019, split == TRUE)
test_set = subset(dataset_2010_2019, split == FALSE)

#Fitting multiple linear regression to the training set
regressor = lm(formula = AllNBA ~ ., data = training_set)
summary(regressor)

#Building the optimal model using Backward Elimination
regressor_opt = lm(formula = AllNBA ~ ., data = dataset_2010_2019)
summary(regressor_opt)
#Remove variables with highest P-value one by one until none are left with P > 0.05
regressor_opt = lm(formula = AllNBA ~ Min + P.36 + TS. + A.36 + TO.36 + Raptor.WAR + PIE. + AWS. + USG. + AST. + TOV. + ORB. + DRB. + TRB. + X.Pos + DRtg + FTAr, data = dataset_2010_2019)
summary(regressor_opt)
#Remove DRtg variable
regressor_opt = lm(formula = AllNBA ~ Min + P.36 + TS. + A.36 + TO.36 + Raptor.WAR + PIE. + AWS. + USG. + AST. + TOV. + ORB. + DRB. + TRB. + X.Pos + FTAr, data = dataset_2010_2019)
summary(regressor_opt)
#Remove X.Pos variable
regressor_opt = lm(formula = AllNBA ~ Min + P.36 + TS. + A.36 + TO.36 + Raptor.WAR + PIE. + AWS. + USG. + AST. + TOV. + ORB. + DRB. + TRB. + FTAr, data = dataset_2010_2019)
summary(regressor_opt)
#Remove TO.36
regressor_opt = lm(formula = AllNBA ~ Min + P.36 + TS. + A.36 + Raptor.WAR + PIE. + AWS. + USG. + AST. + TOV. + ORB. + DRB. + TRB. + FTAr, data = dataset_2010_2019)
summary(regressor_opt)
#Remove TOV.
regressor_opt = lm(formula = AllNBA ~ Min + P.36 + TS. + A.36 + Raptor.WAR + PIE. + AWS. + USG. + AST. + ORB. + DRB. + TRB. + FTAr, data = dataset_2010_2019)
summary(regressor_opt)

#Select only columns deemed significant from multiple linear regression
dataset_2010_2019 <- dataset_2010_2019[,c(5,8,9,10,17,18,19,20,21,23,24,25,35,36)]

#Re-Split data 2010-2019 into test set and training set with new data frame
split = sample.split(dataset_2010_2019$AllNBA, SplitRatio = 0.80)
training_set = subset(dataset_2010_2019, split == TRUE)
test_set = subset(dataset_2010_2019, split == FALSE)

#Fitting Logistic Regression to the Training set
classifier = glm(formula = AllNBA ~ ., 
                 family = binomial,
                 data = training_set)

#Predicting the Test set results
prob_pred = predict(classifier,type='response',newdata = test_set[-14])
y_pred = ifelse(prob_pred > 0.5,1,0)

#Making the Confusion Matrix
cm = table(test_set[,14],y_pred)
cm

#Predicting 2020 results
prob_pred_2020 = predict(classifier,type='response',newdata = dataset_2020[-14])
y_pred_2020 = ifelse(prob_pred_2020 > 0.5,1,0)

#Returns 1 values - projected at over 50% to make all NBA team
y_pred_2020[y_pred_2020==1]
dataset_2020_names[c(115,194,233,250,251,308,531), 1]

#Predicting 2020 results - don't convert to 0 and 1 - top 30 odds to make all NBA
head(sort(prob_pred_2020,decreasing=TRUE),30)
dataset_2020_names[c(308,194,250,233,115,251,531,1,456,290,131,22,288,423,383,457,309,232,550,437,214,196,419,512,382,530,514,349,39,25), 1]
