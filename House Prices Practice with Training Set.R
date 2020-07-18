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

data <- read.csv('train.csv')

# Data preprocessing ------------------------------------------------------

#Data by column breakdown
str(data)
#Summary of missing values per column
colSums(is.na(data))

# -- Data Exploration (Do not need to run section of code each time) --------------------------------------------------------
#Explore data column by column

#Column 1 - ID

#Column 2 - MSSubClass
head(data[,c(1,2)],10)
str(data[,2])
typeof(data[,2])
unique(data[,2])
data %>%
  group_by(MSSubClass) %>%
  dplyr::summarise(n = n())
ggplot(data,aes(x = MSSubClass, y = SalePrice, colour = MSSubClass)) + geom_boxplot()
#MSSubClass column is in integer form, but values are simply categorizing types of dwelling  
#Has 15 unique values
#Will be changed to factors

#Column 3 - MSZoning
head(data[,c(1,3)],10)
str(data[,3])
typeof(data[,3])
unique(data[,3])
data %>%
  group_by(MSZoning) %>%
  dplyr::summarise(n = n())
#MSZoning with 5 unique values in no particular order
#Will be changed to factors

#Column 4 - LotFrontage
head(data[,c(1,4)],10)
str(data[,4])
typeof(data[,4])
unique(data[,4])
data %>%
  group_by(LotFrontage) %>%
  dplyr::summarise(n = n())
sum(is.na(data$LotFrontage))
#In integer form based on linear feet
#Includes 259 NA values which are equivalent to 0 linear feet
#NAs will be changes to zeros

#Column 5 - Lot Area
head(data[,c(1,5)],10)
str(data[,5])
typeof(data[,5])
unique(data[,5])
data %>%
  group_by(LotArea) %>%
  dplyr::summarise(n = n())
sum(is.na(data$LotArea))
#Data is already in integer form - representing lot size in square feet - no missing values
#Nothing to be done to this column

#Column 6 - Street
head(data[,c(1,6)],10)
str(data[,6])
summary(data[,6])
typeof(data[,6])
unique(data[,6])
data %>%
  group_by(Street) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Street))
#Data is split into either Grvl or Pave - just the two unique variables
#To be converted into factors

#Column 7 - Alley
head(data[,c(1,7)],10)
str(data[,7])
summary(data[,7])
typeof(data[,7])
unique(data[,7])
data %>%
  group_by(Alley) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Alley))
#Contains two variables in Grvl and Pave along with 1369 NA values for when there is no Alley
#Converted into factors with NA taking a 0 value

#Column 8 - LotShape
head(data[,c(1,8)],10)
str(data[,8])
summary(data[,8])
typeof(data[,8])
unique(data[,8])
data %>%
  group_by(LotShape) %>%
  dplyr::summarise(n = n())
sum(is.na(data$LotShape))
#Column in four levels - no particular order - no missing values
#To be converted into factors

#Column 9 - LandContour
head(data[,c(1,9)],10)
str(data[,9])
summary(data[,9])
typeof(data[,9])
unique(data[,9])
data %>%
  group_by(LandContour) %>%
  dplyr::summarise(n = n())
sum(is.na(data$LandContour))
#Column in four levels - no particular order - no missing values
#To be convered into factors

#Column 10 - Utilities
head(data[,c(1,10)],10)
str(data[,10])
summary(data[,10])
typeof(data[,10])
unique(data[,10])
data %>%
  group_by(Utilities) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Utilities))
#Column in two levels - AllPub and NoSeWa in no particular order - no missing values
#To be converted into factors

#Column 11 - LotConfig
head(data[,c(1,11)],10)
str(data[,11])
summary(data[,11])
typeof(data[,11])
unique(data[,11])
data %>%
  group_by(LotConfig) %>%
  dplyr::summarise(n = n())
sum(is.na(data$LotConfig))
#Column in five levels - no particular order - no missing values
#To be converted into factors

#Column 12 - LandSlope
head(data[,c(1,12)],10)
str(data[,12])
summary(data[,12])
typeof(data[,12])
unique(data[,12])
data %>%
  group_by(LandSlope) %>%
  dplyr::summarise(n = n())
sum(is.na(data$LandSlope))
#Column is in three levels - Gtl -> Mod -> Sev - no missing values

#Column 13 - Neighborhood 
head(data[,c(1,13)],10)
str(data[,13])
summary(data[,13])
typeof(data[,13])
unique(data[,13])
data %>%
  group_by(Neighborhood) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Neighborhood))
#Column with twenty-five levels - no particular order - no missing values
#To be converted into factors

#Column 14 - Condition1
head(data[,c(1,14)],10)
str(data[,14])
summary(data[,14])
typeof(data[,14])
unique(data[,14])
data %>%
  group_by(Condition1) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Condition1))
#Column with nine levels - no particular order - no missing values
#To be converted into factors

#Column 15 - Condition 2
head(data[,c(1,15)],10)
str(data[,15])
summary(data[,15])
typeof(data[,15])
unique(data[,15])
data %>%
  group_by(Condition2) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Condition2))
#Column with eight levels - no particular order - no missing values
#To be converted into factors

#Column 16 - BldgType
head(data[,c(1,16)],10)
str(data[,16])
summary(data[,16])
typeof(data[,16])
unique(data[,16])
data %>%
  group_by(BldgType) %>%
  dplyr::summarise(n = n())
sum(is.na(data$BldgType))
#Column in five levels - no particular order - no missing values
#To be converted into factors

#Column 17 - HouseStyle
head(data[,c(1,17)],10)
str(data[,17])
summary(data[,17])
typeof(data[,17])
unique(data[,17])
data %>%
  group_by(HouseStyle) %>%
  dplyr::summarise(n = n())
sum(is.na(data$HouseStyle))
ggplot(data,aes(x = HouseStyle, y = SalePrice, colour = HouseStyle)) + geom_boxplot()
#Column in eight levels - no particular order - no missing values
#To be converted into factors

#Column 18 - OverallQual
head(data[,c(1,18)],10)
str(data[,18])
summary(data[,18])
typeof(data[,18])
unique(data[,18])
data %>%
  group_by(OverallQual) %>%
  dplyr::summarise(n = n())
sum(is.na(data$OverallQual))
ggplot(data,aes(x = OverallQual, y = SalePrice, group = OverallQual, colour = OverallQual)) + geom_boxplot()
#Column qualifying quality from 1 to 10 - obvious correlation with price
#No missing values - no changes needed to column

#Column 19 - OverallCond
head(data[,c(1,19)],10)
str(data[,19])
summary(data[,19])
typeof(data[,19])
unique(data[,19])
data %>%
  group_by(OverallCond) %>%
  dplyr::summarise(n = n())
sum(is.na(data$OverallCond))
ggplot(data,aes(x = OverallCond, y = SalePrice, group = OverallCond, colour = OverallCond)) + geom_boxplot()
#Column qualifying condition from 1 to 10 - no values of 10 though - correlation with price
#No missing values - no changes needed to column

#Column 20 - YearBuilt
head(data[,c(1,20)],10)
str(data[,20])
summary(data[,20])
typeof(data[,20])
unique(data[,20])
data %>%
  group_by(YearBuilt) %>%
  dplyr::summarise(n = n())
sum(is.na(data$YearBuilt))
ggplot(data,aes(x = YearBuilt, y = SalePrice)) + geom_point(colour = 'blue') + geom_smooth(method = 'lm', colour = 'black')
#Column with year that property was built between 1872 and 2010 - seeing mostly correlation with price for newer properties
#No missing values - not changing column for now..

#Column 21 - YearRemodAdd
head(data[,c(1,21)],10)
str(data[,21])
summary(data[,21])
typeof(data[,21])
unique(data[,21])
data %>%
  group_by(YearRemodAdd) %>%
  dplyr::summarise(n = n())
sum(is.na(data$YearRemodAdd))
ggplot(data,aes(x = YearRemodAdd, y = SalePrice)) + geom_point(colour = 'blue') + geom_smooth(method = 'lm', colour = 'black')
#Column with remodel date - same as construction date if no remodel - 
#Debating converting to 0/1 to remodel/no remodel ... not sure yet..

#Column 22 - RoofStyle
head(data[,c(1,22)],10)
str(data[,22])
summary(data[,22])
typeof(data[,22])
unique(data[,22])
data %>%
  group_by(RoofStyle) %>%
  dplyr::summarise(n = n())
sum(is.na(data$RoofStyle))
ggplot(data,aes(x = RoofStyle, y = SalePrice, colour = RoofStyle)) + geom_boxplot()
#Column with six levels - in no particular order - no missing values
#To be converted into factors

#Column 23 - RoofMatl
head(data[,c(1,23)],10)
str(data[,23])
summary(data[,23])
typeof(data[,23])
unique(data[,23])
data %>%
  group_by(RoofMatl) %>%
  dplyr::summarise(n = n())
sum(is.na(data$RoofMatl))
ggplot(data,aes(x = RoofMatl, y = SalePrice, colour = RoofMatl)) + geom_boxplot()
#Column with eight levels - in no particular order - no missing values
#To be converted into factors

#Column 24 - Exterior1st
head(data[,c(1,24)],10)
str(data[,24])
summary(data[,24])
typeof(data[,24])
unique(data[,24])
data %>%
  group_by(Exterior1st) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Exterior1st))
ggplot(data,aes(x = Exterior1st, y = SalePrice, colour = Exterior1st)) + geom_boxplot()
#Column with fifteen levels - in no particular order - no missing values
#To be converted into factors

#Column 25 - Exterior2nd
head(data[,c(1,25)],10)
str(data[,25])
summary(data[,25])
typeof(data[,25])
unique(data[,25])
data %>%
  group_by(Exterior2nd) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Exterior2nd))
#Column with sixteen levels - in no particular order - no missing values
#To be converted into factors

#Column 26 - MasVnrType
head(data[,c(1,26)],10)
str(data[,26])
summary(data[,26])
typeof(data[,26])
unique(data[,26])
data %>%
  group_by(MasVnrType) %>%
  dplyr::summarise(n = n())
sum(is.na(data$MasVnrType))
ggplot(data,aes(x = MasVnrType, y = SalePrice, colour = MasVnrType)) + geom_boxplot()
#Column with four levels including 'None' - in no particular order - also 8 missing values
#Want to change NAs to 'None' - then convert to factors

#Column 27 - MasVnrArea
head(data[,c(1,27)],10)
str(data[,27])
summary(data[,27])
typeof(data[,27])
unique(data[,27])
data %>%
  group_by(MasVnrArea) %>%
  dplyr::summarise(n = n())
sum(is.na(data$MasVnrArea))
ggplot(data,aes(x = MasVnrArea, y = SalePrice, group = MasVnrType, colour = MasVnrArea)) + geom_boxplot()
#Column in integer form representing area - 8 missing values just as above which will be converted to '0'

#Column 28 - ExterQual
head(data[,c(1,28)],10)
str(data[,28])
summary(data[,28])
typeof(data[,28])
unique(data[,28])
data %>%
  group_by(ExterQual) %>%
  dplyr::summarise(n = n())
sum(is.na(data$ExterQual))
ggplot(data,aes(x = ExterQual, y = SalePrice, colour = ExterQual)) + geom_boxplot()
#Column is in four levels - in order from Fa(Fair) -> TA(Avg.) -> Gd(Good) -> Ex(Excellent) 
#Po(Poor) also part of scale but not included in this particular column. Will make into a variable since use is frequent
#No missing values

#Column 29 - ExterCond
head(data[,c(1,29)],10)
str(data[,29])
summary(data[,29])
typeof(data[,29])
unique(data[,29])
data %>%
  group_by(ExterCond) %>%
  dplyr::summarise(n = n())
sum(is.na(data$ExterCond))
#Column is in five levels using the Condition variable - no missing values
#Will convert using revalue

#Column 30 - Foundation
head(data[,c(1,30)],10)
str(data[,30])
summary(data[,30])
typeof(data[,30])
unique(data[,30])
data %>%
  group_by(Foundation) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Foundation))
ggplot(data,aes(x = Foundation, y = SalePrice, colour = Foundation)) + geom_boxplot()
#Column is in six levels - no particular order - no missing values
#Will convert into factor

#Column 31 - BsmtQual
head(data[,c(1,31)],10)
str(data[,31])
summary(data[,31])
typeof(data[,31])
unique(data[,31])
data %>%
  group_by(BsmtQual) %>%
  dplyr::summarise(n = n())
sum(is.na(data$BsmtQual))
ggplot(data,aes(x = BsmtQual, y = SalePrice, colour = BsmtQual)) + geom_boxplot()
#Column in four levels following Condition variable - also 37 missing values
#Convert NAs to 'None' then will convert using revalue

#Column 32 - BsmtCond
head(data[,c(1,32)],10)
str(data[,32])
summary(data[,32])
typeof(data[,32])
unique(data[,32])
data %>%
  group_by(BsmtCond) %>%
  dplyr::summarise(n = n())
sum(is.na(data$BsmtCond))
ggplot(data,aes(x = BsmtCond, y = SalePrice, colour = BsmtCond)) + geom_boxplot()
#Column in four levels following Condition variable - also 37 missing values
#Convert NAs to 'None' then will convert using revalue

#Column 33 - BsmtExposure
head(data[,c(1,33)],10)
str(data[,33])
summary(data[,33])
typeof(data[,33])
unique(data[,33])
data %>%
  group_by(BsmtExposure) %>%
  dplyr::summarise(n = n())
sum(is.na(data$BsmtExposure))
#Column in four levels - including 'No' - Also 38 missing variables
#Will convert NAs to 'No' then column into a factor

#Column 34 - BsmtFinType1
head(data[,c(1,34)],10)
str(data[,34])
summary(data[,34])
typeof(data[,34])
unique(data[,34])
data %>%
  group_by(BsmtFinType1) %>%
  dplyr::summarise(n = n())
sum(is.na(data$BsmtFinType1))
#Column with six levels - with an order - also with 37 missing values where there is no basement
#Can factor directly with an order

#Column 35 - BsmtFinSF1
head(data[,c(1,35)],10)
str(data[,35])
summary(data[,35])
typeof(data[,35])
unique(data[,35])
data %>%
  group_by(BsmtFinSF1) %>%
  dplyr::summarise(n = n())
sum(is.na(data$BsmtFinSF1))
#Column in integer form - square footage of basement type 1 - no null values
#Nothing to change here

#Column 36 - BsmtFinType2
head(data[,c(1,36)],10)
str(data[,36])
summary(data[,36])
typeof(data[,36])
unique(data[,36])
data %>%
  group_by(BsmtFinType2) %>%
  dplyr::summarise(n = n())
sum(is.na(data$BsmtFinType2))
#Column with six levels - with an order - also with 38 missing values where there is no basement
#Can factor directly with an order

#Column 37 - BsmtFinSF2
head(data[,c(1,37)],10)
str(data[,37])
summary(data[,37])
typeof(data[,37])
unique(data[,37])
data %>%
  group_by(BsmtFinSF2) %>%
  dplyr::summarise(n = n())
sum(is.na(data$BsmtFinSF2))
#Column in integer form - square footage of basement type 2 - no null values
#Nothing to change here

#Column 38 - BsmtUnfSf
head(data[,c(1,38)],10)
str(data[,38])
summary(data[,38])
typeof(data[,38])
unique(data[,38])
data %>%
  group_by(BsmtUnfSF) %>%
  dplyr::summarise(n = n())
sum(is.na(data$BsmtUnfSF))
#Column in integer form - square footage of unfinished basement- no null values
#Nothing to change here

#Column 39 - TotalBsmtSF
head(data[,c(1,39)],10)
str(data[,39])
summary(data[,39])
typeof(data[,39])
unique(data[,39])
data %>%
  group_by(TotalBsmtSF) %>%
  dplyr::summarise(n = n())
sum(is.na(data$TotalBsmtSF))
#Column in integer form - total square footage basement- no null values
#Nothing to change here

#Column 40 - Heating
head(data[,c(1,40)],10)
str(data[,40])
summary(data[,40])
typeof(data[,40])
unique(data[,40])
data %>%
  group_by(Heating) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Heating))
ggplot(data,aes(x = Heating, y = SalePrice, colour = Heating)) + geom_boxplot()
#Column in six levels - no particular order - no missing values
#Will convert into factor

#Column 41 - HeatingQc
head(data[,c(1,41)],10)
str(data[,41])
summary(data[,41])
typeof(data[,41])
unique(data[,41])
data %>%
  group_by(HeatingQC) %>%
  dplyr::summarise(n = n())
sum(is.na(data$HeatingQC))
#Column with 5 levels by the Condition variable - no missing values
#Convert using revalue

#Column 42 - CentralAir
head(data[,c(1,42)],10)
str(data[,42])
summary(data[,42])
typeof(data[,42])
unique(data[,42])
data %>%
  group_by(CentralAir) %>%
  dplyr::summarise(n = n())
sum(is.na(data$CentralAir))
#Column in two levels - Y and N - no missing values
#Will convert into factor

#Column 43 - Electrical
head(data[,c(1,43)],10)
str(data[,43])
summary(data[,43])
typeof(data[,43])
unique(data[,43])
data %>%
  group_by(Electrical) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Electrical))
#Column in five levels - no particular order - 1 missing value
#Will convert into factor directly with missing value taking a 0

#Column 44 - X1stFlrSF
head(data[,c(1,44)],10)
str(data[,44])
summary(data[,44])
typeof(data[,44])
unique(data[,44])
data %>%
  group_by(X1stFlrSF) %>%
  dplyr::summarise(n = n())
sum(is.na(data$X1stFlrSF))
#Column is in integer form - first floor square feet - no missing values
#Nothing to be done to column

#Column 45 - X2ndFlrSF
head(data[,c(1,45)],10)
str(data[,45])
summary(data[,45])
typeof(data[,45])
unique(data[,45])
data %>%
  group_by(X2ndFlrSF) %>%
  dplyr::summarise(n = n())
sum(is.na(data$X2ndFlrSF))
#Column is in integer form - second floor square feet - no missing values
#Nothing to be done to column

#Column 46 - LowQualFinSF
head(data[,c(1,46)],10)
str(data[,46])
summary(data[,46])
typeof(data[,46])
unique(data[,46])
data %>%
  group_by(LowQualFinSF) %>%
  dplyr::summarise(n = n())
sum(is.na(data$LowQualFinSF))
#Column is in integer form - low qual finished square feet - no missing values
#Nothing to be done to column

#Column 47 - GrLivArea
head(data[,c(1,47)],10)
str(data[,47])
summary(data[,47])
typeof(data[,47])
unique(data[,47])
data %>%
  group_by(GrLivArea) %>%
  dplyr::summarise(n = n())
sum(is.na(data$GrLivArea))
#Column is in integer form - above grade living area square feet - no missing values
#Nothing to be done to column

#Column 48 to 51 - Bathrooms 
head(data[,c(1,48:51)],10)
str(data[,48:51])
summary(data[,48:51])
typeof(data[,48:51])
unique(data[,48:51])
sum(is.na(data[,48:51]))
#All four columns pertain to number of bathrooms in the home - no missing values

#Column 49 - BedroomAbvGr
head(data[,c(1,49)],10)
str(data[,49])
summary(data[,49])
typeof(data[,49])
unique(data[,49])
data %>%
  group_by(BedroomAbvGr) %>%
  dplyr::summarise(n = n())
sum(is.na(data$BedroomAbvGr))
#Column is in integer form - number of bedrooms above ground - from 0 to 8 with no missing values
#Nothing to be done to column

#Column 50 - KitchenAbvGr
head(data[,c(1,50)],10)
str(data[,50])
summary(data[,50])
typeof(data[,50])
unique(data[,50])
data %>%
  group_by(KitchenAbvGr) %>%
  dplyr::summarise(n = n())
sum(is.na(data$KitchenAbvGr))
#Column is in integer form - number of kitchens above ground - from 0 to 3 with no missing values
#Nothing to be done to column

#Column 51 - KitchenQual
head(data[,c(1,51)],10)
str(data[,51])
summary(data[,51])
typeof(data[,51])
unique(data[,51])
data %>%
  group_by(KitchenQual) %>%
  dplyr::summarise(n = n())
sum(is.na(data$KitchenQual))
#Column is in four levels following Condition variable - No missing values
#Will use revalue on column data

#Column 52 - TotRmsAbvGrd
head(data[,c(1,52)],10)
str(data[,52])
summary(data[,52])
typeof(data[,52])
unique(data[,52])
data %>%
  group_by(TotRmsAbvGrd) %>%
  dplyr::summarise(n = n())
sum(is.na(data$TotRmsAbvGrd))
#Column is in integer form - number of rooms above ground - from 2 to 14 with no missing values
#Nothing to be done to column

#Column 53 - Functional
head(data[,c(1,53)],10)
str(data[,53])
summary(data[,53])
typeof(data[,53])
unique(data[,53])
data %>%
  group_by(Functional) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Functional))
ggplot(data,aes(x = Functional, y = SalePrice, colour = Functional)) + geom_boxplot()
#Column is in seven levels - ranked by functionality of home - no missing values
#Will convert into factor in order of functionality

#Column 54 - Fireplaces
head(data[,c(1,54)],10)
str(data[,54])
summary(data[,54])
typeof(data[,54])
unique(data[,54])
data %>%
  group_by(Fireplaces) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Fireplaces))
#Column is in integer form counting number of fireplaces from 0 to 3 - no missing values
#Nothing to be done to column

#Column 55 - FireplaceQu
head(data[,c(1,55)],10)
str(data[,55])
summary(data[,55])
typeof(data[,55])
unique(data[,55])
data %>%
  group_by(FireplaceQu) %>%
  dplyr::summarise(n = n())
sum(is.na(data$FireplaceQu))
#Column is in five levels following Condition variable for quality of fireplace. 
#When no fireplace - NA value - 690 missing values
#Will convert NA to 'None' and then revalue using Condition variable

#Column 56 - GarageType
head(data[,c(1,56)],10)
str(data[,56])
summary(data[,56])
typeof(data[,56])
unique(data[,56])
data %>%
  group_by(GarageType) %>%
  dplyr::summarise(n = n())
sum(is.na(data$GarageType))
ggplot(data,aes(x = GarageType, y = SalePrice, colour = GarageType)) + geom_boxplot()
#Column with six levels - not including NA. No particular order but appears to be some correlation between certain types and sale price
#81 missing values when there is no garage.
#Will convert NA to 'No Garage' then convert column into factor

#Column 57 - GarageYrBlt
head(data[,c(1,57)],10)
str(data[,57])
summary(data[,57])
typeof(data[,57])
unique(data[,57])
data %>%
  group_by(GarageYrBlt) %>%
  dplyr::summarise(n = n())
sum(is.na(data$GarageYrBlt))
ggplot(data,aes(x = GarageYrBlt, y = SalePrice, group = GarageYrBlt)) + geom_point(color = 'red')
#Column is in integer with the year garage was built between 1900 and 2010. 81 missing values when no garage
#Removing this column as NAs are difficult to handle and there are many garage variables. Also likely to have high correlation with Garage Qual and Cond

#Column 57 - GarageFinish
head(data[,c(1,57)],10)
str(data[,57])
summary(data[,57])
typeof(data[,57])
unique(data[,57])
data %>%
  group_by(GarageFinish) %>%
  dplyr::summarise(n = n())
sum(is.na(data$GarageFinish))
#Column is in three levels from unfinished to finished garage. Also 81 missing values when there is no garage
#Will convert NA to 'No Garage' and then convert into factor by order

#Column 58 - GarageCars
head(data[,c(1,58)],10)
str(data[,58])
summary(data[,58])
typeof(data[,58])
unique(data[,58])
data %>%
  group_by(GarageCars) %>%
  dplyr::summarise(n = n())
sum(is.na(data$GarageCars))
#Column is in integer form - number of garage cars from 0 to 4
#No change needed to the column

#Column 59 - GarageArea
head(data[,c(1,59)],10)
str(data[,59])
summary(data[,59])
typeof(data[,59])
unique(data[,59])
data %>%
  group_by(GarageArea) %>%
  dplyr::summarise(n = n())
sum(is.na(data$GarageArea))
#Column is in integer form - represents garage area
#No change needed to the column
cor(data$GarageArea,data$GarageCars)
#Obvious high correlation between garage area and garage cars 
#Will drop variable with lowest correlaion with sale price
cor(data$SalePrice,data$GarageArea)
cor(data$SalePrice,data$GarageCars)
#Garage area has slightly lower correlation so we will drop that variable

#Column 59 (*removed earlier column) - GarageQual
head(data[,c(1,59)],10)
str(data[,59])
summary(data[,59])
typeof(data[,59])
unique(data[,59])
data %>%
  group_by(GarageQual) %>%
  dplyr::summarise(n = n())
sum(is.na(data$GarageQual))
#Column has five levels in order based on Condition variable. 81 missing values where there is no garage.
#Will convert NA to 'None' and then use revalue on column

#Column 60 - GarageCond
head(data[,c(1,60)],10)
str(data[,60])
summary(data[,60])
typeof(data[,60])
unique(data[,60])
data %>%
  group_by(GarageCond) %>%
  dplyr::summarise(n = n())
sum(is.na(data$GarageCond))
#Column has five levels in order based on Condition variable. 81 missing values where there is no garage.
#Will convert NA to 'None' and then use revalue on column
#Previous two columns seem to same - will test for correlation
cor(as.numeric(data$GarageCond),as.numeric(data$GarageQual))
#Correlation is 0.6 - will keep both variables for now

#Column 61 - PavedDrive
head(data[,c(1,61)],10)
str(data[,61])
summary(data[,61])
typeof(data[,61])
unique(data[,61])
data %>%
  group_by(PavedDrive) %>%
  dplyr::summarise(n = n())
sum(is.na(data$PavedDrive))
#Column is in three levels from N -> P -> Y. No missing values
#Will convert into factor

#Column 62 - WoodDeckSf
head(data[,c(1,62)],10)
str(data[,62])
summary(data[,62])
typeof(data[,62])
unique(data[,62])
data %>%
  group_by(WoodDeckSF) %>%
  dplyr::summarise(n = n())
sum(is.na(data$WoodDeckSF))
#Column is in integer form - square feet of wood deck area. No missing values
#No change needed to column

#Column 63 - OpenPorchSF
head(data[,c(1,63)],10)
str(data[,63])
summary(data[,63])
typeof(data[,63])
unique(data[,63])
data %>%
  group_by(OpenPorchSF) %>%
  dplyr::summarise(n = n())
sum(is.na(data$OpenPorchSF))
#Column is in integer form - square feet of open porch area. No missing values
#No change needed to column

#Column 64 - EnclosedPorch
head(data[,c(1,64)],10)
str(data[,64])
summary(data[,64])
typeof(data[,64])
unique(data[,64])
data %>%
  group_by(EnclosedPorch) %>%
  dplyr::summarise(n = n())
sum(is.na(data$EnclosedPorch))
#Column is in integer form - square feet of enclosed porch area. No missing values
#No change needed to column

#Column 65 - X3SsnPorch
head(data[,c(1,65)],10)
str(data[,65])
summary(data[,65])
typeof(data[,65])
unique(data[,65])
data %>%
  group_by(X3SsnPorch) %>%
  dplyr::summarise(n = n())
sum(is.na(data$X3SsnPorch))
#Column is in integer form - square feet of three season porch area. No missing values
#No change needed to column

#Column 66 - ScreenPorch
head(data[,c(1,66)],10)
str(data[,66])
summary(data[,66])
typeof(data[,66])
unique(data[,66])
data %>%
  group_by(ScreenPorch) %>%
  dplyr::summarise(n = n())
sum(is.na(data$ScreenPorch))
#Column is in integer form - square feet of screen porch area. No missing values
#No change needed to column

#Column 67 - PoolArea
head(data[,c(1,67)],10)
str(data[,67])
summary(data[,67])
typeof(data[,67])
unique(data[,67])
data %>%
  group_by(PoolArea) %>%
  dplyr::summarise(n = n())
sum(is.na(data$PoolArea))
#Column is in integer form - square feet of pool area. No missing values
#No change needed to column

#Column 68 - PoolQC
head(data[,c(1,68)],10)
str(data[,68])
summary(data[,68])
typeof(data[,68])
unique(data[,68])
data %>%
  group_by(PoolQC) %>%
  dplyr::summarise(n = n())
sum(is.na(data$PoolQC))
#Column is in three levels following Condition variable. 1453 missing values for when there is no pool
#Will convert NA to 'None' and then convert column using revalue

#Column 69 - Fence
head(data[,c(1,69)],10)
str(data[,69])
summary(data[,69])
typeof(data[,69])
unique(data[,69])
data %>%
  group_by(Fence) %>%
  dplyr::summarise(n = n())
sum(is.na(data$Fence))
ggplot(data,aes(x = Fence, y = SalePrice, color = Fence)) + geom_boxplot()
#Column is in four levels for quality of fence. 1179 missing values
#Can convert directly into factor with NA taking '0' value

#Column 70 - MiscFeature
head(data[,c(1,70)],10)
str(data[,70])
summary(data[,70])
typeof(data[,70])
unique(data[,70])
data %>%
  group_by(MiscFeature) %>%
  dplyr::summarise(n = n())
sum(is.na(data$MiscFeature))
ggplot(data,aes(x = MiscFeature, y = SalePrice, color = MiscFeature)) + geom_boxplot()
#Column is in four levels based on feature. 1406 missing values
#Can convert directly into factor with NA taking '0' value

#Column 71 - MiscVal
head(data[,c(1,71)],10)
str(data[,71])
summary(data[,71])
typeof(data[,71])
unique(data[,71])
data %>%
  group_by(MiscVal) %>%
  dplyr::summarise(n = n())
sum(is.na(data$MiscVal))
ggplot(data,aes(x = MiscVal, y = SalePrice)) + geom_point() + geom_jitter()
#Column is in integer form with value of misc feature. 
#No change needed to this column

#Column 72 to 76 - MoSold, YrSold, SaleType, SaleCondition, SalePrice
head(data[,c(1,72:76)],10)
str(data[,72:76])
summary(data[,72:76])
typeof(data[,72:76])
unique(data[,72:76])
sum(is.na(data[0,72:76]))
#All columns have to do with sale of home
#No missing values
#Columns 72,73,76 already in integer form
#Columns 74 and 75 need to be converted into factors

# -- Encoding variables/Handling missing values ------------------------------

#Explanation in Data Exploration section - below is code needed to execute to clean data
#Convert columns into factors, handle missing data, remove columns, create columns. *Where needed

data$MSSubClass <- factor(data$MSSubClass, labels = c(1:15), exclude = NULL)

data$MSZoning <- factor(data$MSZoning, labels = c(1:5), exclude = NULL)

data$LotFrontage = replace_na(data$LotFrontage, 0)

data$Street <- factor(data$Street, levels = c('Grvl','Pave'), labels = c(0,1), exclude = NULL)

data$Alley <- factor(data$Alley, levels = c(NA,'Grvl','Pave'), labels = c(0,1,2), exclude = NULL)

data$LotShape <- factor(data$LotShape, levels = c('IR1','IR2','IR3','Reg'), labels = c(1,2,3,4))

data$LandContour <- factor(data$LandContour, levels = c('Lvl','Bnk','HLS','Low'), labels = c(1,2,3,4))

data$Utilities <- factor(data$Utilities, labels = c(1,2))

data$LotConfig <- factor(data$LotConfig, labels = c(1:5))

data$LandSlope <- factor(data$LandSlope, levels = c('Gtl','Mod','Sev'), labels = c(1:3))

data$Neighborhood <- factor(data$Neighborhood, labels = c(1:25))

data$Condition1 <- factor(data$Condition1, labels = c(1:9))

data$Condition2 <- factor(data$Condition2, labels = c(1:8))

data$BldgType <- factor(data$BldgType, labels = c(1:5))

data$HouseStyle <- factor(data$HouseStyle, labels = c(1:8))

data$RoofStyle <- factor(data$RoofStyle, labels = c(1:6))

data$RoofMatl <- factor(data$RoofMatl, labels = c(1:8))

data$Exterior1st <- factor(data$Exterior1st, labels = c(1:15))

data$Exterior2nd <- factor(data$Exterior2nd, labels = c(1:16))

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

# Split into train and test set -------------------------------------------

set.seed(1234)
split = sample.split(data$SalePrice, SplitRatio = 0.8)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)

# Create model #1: Multiple Linear Regression with Backward Elimination ------------------------------------------------------------

#Run backward elimination 
backwardElimination <- function(x, sl) { 
  numVars = length(x)
  for (i in c(1:numVars) ){
    mlrregressor = lm(formula = SalePrice ~ ., data = x )
    maxVar = max(coef(summary(mlrregressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(mlrregressor))[c(2:numVars),"Pr(>|t|)"]==maxVar) 
      x = x[,-j]
    }
    numVars = numVars - 1
  }
  return(summary(mlrregressor)) 
}
SL = 0.05
backwardElimination(training_set, SL)

#Using only variables remaining following backwards elimination
mlrregressor = lm(formula = SalePrice ~ YearRemodAdd, data = training_set)
summary(mlrregressor)

# Create model #2: Multiple Linear Regression with Selected Variables --------------------------------------------------------

#Select variables we believe will impact Sale Price the most 
mlrregressor2 = lm(formula = SalePrice ~ LotArea + BldgType + OverallCond + TotalBath + BedroomAbvGr, data = training_set)
summary(mlrregressor2)

#Remove OverallCond and BedroomAbvGr has they are not significant variables 
mlrregressor2 = lm(formula = SalePrice ~ LotArea + BldgType + TotalBath, data = training_set)
summary(mlrregressor2)

# Create model #3: Support Vector Regression (SVR) --------------------------------

svrregressor = svm(formula = SalePrice ~ ., 
                data = training_set,
                type = 'eps-regression')

# Create model #4: Decision Tree -------------------------------------------------------

dtregressor = rpart(formula = SalePrice ~ ., 
                  data=training_set)

# Create model #5: Random Forest ------------------------------------------

set.seed(1234)
rfregressor = randomForest(x=training_set[1:75],
                         y=training_set$SalePrice,
                         ntree = 500)

# Use models to predict sale prices of test set ---------------------------------------------------

#Model #1: Multiple Linear Regression with Backward Elimination
mlr_pred = predict(mlrregressor, newdata = test_set)
mlr_pred

#Model #2: Multiple Linear Regression with Selected Variables
mlr2_pred = predict(mlrregressor2, newdata = test_set)
mlr2_pred

#Model #3: SVR
svr_pred = predict(svrregressor, newdata = test_set)
svr_pred

#Model #4: Decision Tree
dt_pred = predict(dtregressor, newdata = test_set)
dt_pred

#Model #5: Random Forest
rf_pred = predict(rfregressor, newdata = test_set)
rf_pred

# Comparison and Evaluation of predicted prices with actual prices -----------------------------

### Visualisation

#Create new data frame with only columns needed
trial <- data.frame(test_set$SalePrice,mlr_pred,mlr2_pred,svr_pred,dt_pred,rf_pred)
#Order by actual sale price
trial <- trial[order(trial$test_set.SalePrice),]
#Add dummy count variable for charting purposes
trial$count <- c(1:202)
#Chart actual vs. predicted prices
ggplot(trial, aes(x = trial$count)) + 
  geom_point(aes(y = trial$test_set.SalePrice, colour = 'Actual Sale Price')) + 
  geom_smooth(aes(y = trial$test_set.SalePrice), colour = 'black', se = FALSE) +
  geom_point((aes(y = trial$mlr_pred, colour = 'MLR Predicted Sale Price')), alpha = 0.3) +
  geom_smooth(aes(y = trial$mlr_pred), colour = 'blue', se = FALSE) +
  geom_point((aes(y = trial$mlr2_pred, colour = 'MLR 2 Predicted Sale Price')), alpha = 0.3) + 
  geom_smooth(aes(y = trial$mlr2_pred), colour = 'green', se = FALSE) +
  geom_point((aes(y = trial$svr_pred, colour = 'SVR Predicted Sale Price')), alpha = 0.3) + 
  geom_smooth(aes(y = trial$svr_pred), colour = 'red', se = FALSE) +
  geom_point((aes(y = trial$dt_pred, colour = 'Decision Tree Predicted Sale Price')), alpha = 0.3) + 
  geom_smooth(aes(y = trial$dt_pred), colour = 'gold', se = FALSE) +
  geom_point((aes(y = trial$rf_pred, colour = 'Random Forest Predicted Sale Price')), alpha = 0.3) + 
  geom_smooth(aes(y = trial$rf_pred), colour = 'purple', se = FALSE) +
  xlab('Homes') + 
  ggtitle('Comparison of actual home sale prices to predicted sale prices') +
  scale_color_discrete('Model Type') + 
  scale_color_manual(labels = c('Actual Sale Price', 'MLR Predicted Sale Price', 'MLR 2 Predicted Sale Price', 'SVR Predicted Sale Price', 'Decision Tree Predicted Sale Price', 'Random Forest Predicted Sale Price'), 
                     values = c('black', 'blue', 'green', 'red', 'gold', 'purple')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = 'Sale Price ($)', breaks = c(100000,200000,300000,400000), labels = c('100,000','200,000','300,000','400,000'))

### Evaluation of models using RMSE between log or predicted and actual values

rmse(log(test_set$SalePrice),log(mlr_pred))
#Model #1 Result -> 0.3140821

rmse(log(test_set$SalePrice),log(mlr2_pred))
#Model #2 Result -> 0.2559082

rmse(log(test_set$SalePrice),log(svr_pred))
#Model #3 Result -> 0.1083919

rmse(log(test_set$SalePrice),log(dt_pred))
#Model #4 Result -> 0.2066748

rmse(log(test_set$SalePrice),log(rf_pred))
#Model #5 Result -> 0.1262592


