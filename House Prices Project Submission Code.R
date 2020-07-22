# Import data and libraries -----------------------------------------------
library(ggplot2)
library(plyr)
library(tidyverse)
library(forcats)
library(caTools)
library(Metrics)
library(caret)
library(e1071)
library(rpart)
library(randomForest)

train <- read.csv('train.csv')
test <- read.csv('test.csv')
test$SalePrice <- NA
data <- rbind(train, test)

# Preprocessing -----------------------------------------------------------

data$MSSubClass <- factor(data$MSSubClass, labels = c(1:16), exclude = NULL)

data$MSZoning <- factor(data$MSZoning, labels = c(1:6), exclude = NULL)

data$LotFrontage = replace_na(data$LotFrontage, 0)

data$Street <- factor(data$Street, levels = c('Grvl','Pave'), labels = c(0,1), exclude = NULL)

data$Alley <- factor(data$Alley, levels = c(NA,'Grvl','Pave'), labels = c(0,1,2), exclude = NULL)

data$LotShape <- factor(data$LotShape, levels = c('IR1','IR2','IR3','Reg'), labels = c(1,2,3,4))

data$LandContour <- factor(data$LandContour, levels = c('Lvl','Bnk','HLS','Low'), labels = c(1,2,3,4))

data$Utilities <- factor(data$Utilities, labels = c(1:3), exclude = NULL)

data$LotConfig <- factor(data$LotConfig, labels = c(1:5))

data$LandSlope <- factor(data$LandSlope, levels = c('Gtl','Mod','Sev'), labels = c(1:3))

data$Neighborhood <- factor(data$Neighborhood, labels = c(1:25))

data$Condition1 <- factor(data$Condition1, labels = c(1:9))

data$Condition2 <- factor(data$Condition2, labels = c(1:8))

data$BldgType <- factor(data$BldgType, labels = c(1:5))

data$HouseStyle <- factor(data$HouseStyle, labels = c(1:8))

data$RoofStyle <- factor(data$RoofStyle, labels = c(1:6))

data$RoofMatl <- factor(data$RoofMatl, labels = c(1:8))

data$Exterior1st <- factor(data$Exterior1st, labels = c(1:16), exclude = NULL)

data$Exterior2nd <- factor(data$Exterior2nd, labels = c(1:17), exclude = NULL)

data$MasVnrType <- replace_na(data$MasVnrType, 'None')
data$MasVnrType <- factor(data$MasVnrType, levels = c('None','BrkCmn','BrkFace','Stone'), labels = c(0,1,2,3))

data$MasVnrArea <- replace_na(data$MasVnrArea, 0)

Condition <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
#Now will convert column into factor but since length of Condition and levels in this column are not the same, using 'revalue' function instead
data$ExterQual <- revalue(data$ExterQual, Condition)

data$ExterCond <- revalue(data$ExterCond, Condition)

data$Foundation <- factor(data$Foundation, labels = c(1:6))

data$BsmtQual <- fct_explicit_na(data$BsmtQual, na_level = 'None')
data$BsmtQual <- revalue(data$BsmtQual, Condition)

data$BsmtCond <- fct_explicit_na(data$BsmtCond, na_level = 'None')
data$BsmtCond <- revalue(data$BsmtCond, Condition)

data$BsmtExposure <- fct_explicit_na(data$BsmtExposure, na_level = 'No')
data$BsmtExposure = factor(data$BsmtExposure, levels = c('No','Mn','Av','Gd'), labels = c(0,1,2,3))

data$BsmtFinSF1 <- replace_na(data$BsmtFinSF1, 0)

data$BsmtFinSF2 <- replace_na(data$BsmtFinSF2, 0)

data$BsmtUnfSF <- replace_na(data$BsmtUnfSF, 0)

data$TotalBsmtSF <- replace_na(data$TotalBsmtSF, 0)

data$BsmtFinType1 = factor(data$BsmtFinType1, levels = c(NA,'Unf','LwQ','Rec','BLQ','ALQ','GLQ'), labels = c(0,1,2,3,4,5,6), exclude = NULL)

data$BsmtFinType2 = factor(data$BsmtFinType2, levels = c(NA,'Unf','LwQ','Rec','BLQ','ALQ','GLQ'), labels = c(0,1,2,3,4,5,6), exclude = NULL)

data$Heating <- factor(data$Heating, labels = c(1:6))

data$HeatingQC <- revalue(data$HeatingQC, Condition)

data$CentralAir = factor(data$CentralAir, levels = c('N','Y'), labels = c(0,1))

data$Electrical = factor(data$Electrical, levels = c(NA,'FuseA','FuseF','FuseP','Mix','SBrkr'), labels = c(0,1,2,3,4,5), exclude = NULL)

#Will convert to one data column for total bathroom
data$TotalBath <- data$BsmtFullBath + 0.5*data$BsmtHalfBath + data$FullBath + 0.5*data$HalfBath
#Now will remove the four bathroom columns from data which are no longer needed
data <- data[,c(1:47,52:82)]
#Want to move TotalBath column to same place as before in data frame
data <- data[,c(1:47,78,48:77)]

data$TotalBath <- replace_na(data$TotalBath, 0)

data$KitchenQual <- revalue(data$KitchenQual, Condition)

data$Functional = factor(data$Functional, levels = c('Sev','Maj2','Maj1','Mod','Min2','Min1','Typ'), labels = c(1,2,3,4,5,6,7))

data$FireplaceQu <- fct_explicit_na(data$FireplaceQu, na_level = 'None')
data$FireplaceQu <- revalue(data$FireplaceQu, Condition)

data$GarageType <- fct_explicit_na(data$GarageType, na_level = 'No Garage')
data$GarageType = factor(data$GarageType, levels = c('No Garage','CarPort','Detchd','2Types','Basment','Attchd','BuiltIn'), labels = c(0,1,2,3,4,5,6))

#Remove column not needed
data <- data[,c(1:56,58:78)]

data$GarageFinish <- fct_explicit_na(data$GarageFinish, na_level = 'No Garage')
data$GarageFinish = factor(data$GarageFinish, levels = c('No Garage','Unf','RFn','Fin'), labels = c(0,1,2,3))

#Remove column not needed
data <- data[,c(1:58,60:77)]

data$GarageQual <- fct_explicit_na(data$GarageQual, na_level = 'None')
data$GarageQual <- revalue(data$GarageQual, Condition)

data$GarageCond <- fct_explicit_na(data$GarageCond, na_level = 'None')
data$GarageCond <- revalue(data$GarageCond, Condition)

data$PavedDrive = factor(data$PavedDrive, levels = c('N','P','Y'), labels = c(1,2,3))

data$PoolQC <- fct_explicit_na(data$PoolQC, na_level = 'None')
data$PoolQC <- revalue(data$PoolQC, Condition)

data$Fence = factor(data$Fence, levels = c(NA, 'MnWw', 'GdWo','MnPrv','GdPrv'), labels = c(0,1,2,3,4), exclude = NULL)

data$MiscFeature = factor(data$MiscFeature, levels = c(NA,'Gar2','Othr','Shed','TenC'), labels = c(0,1,2,3,4), exclude = NULL)

data$SaleType = factor(data$SaleType, labels = c(1:9))
data$SaleCondition = factor(data$SaleCondition, labels = c(1:6))


# Rearrange data into test and train sets ---------------------------------------------------------------

train <- data[1:1460,]
test <- data[1461:2919,]
test <- test[,1:75]

# Run Model on Training set -----------------------------------------------

svrregressor = svm(formula = SalePrice ~ ., 
                   data = train,
                   type = 'eps-regression')

# Generate Predicted Sale Prices -----------------------------------------

test_pred = predict(svrregressor, newdata = test)
test_pred

Id <- data[1461:2919,1]
submission <- cbind(test_pred,Id)
submission <- submission[,c(2,1)]
colnames(submission) <- c('Id','SalePrice')
write.csv(submission, file = 'submission.csv', row.names = F)


