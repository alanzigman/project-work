#Titanic Project Submission Code

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

training_set = pd.read_csv('train.csv')
test_set = pd.read_csv('test.csv')

#Combine training and test set for EDA and data preprocessing
data = pd.concat([training_set, test_set])

#Save PassengerId data for submission file at the end
PassengerId = data['PassengerId'].iloc[891:1309,]

#----- Handling Missing Values
#Age: 263 - Will use the median age to replace missing values. Though this is an important variable, may come back with something more robust.
data['Age'] = data['Age'].fillna(data['Age'].median())

#Embarked: 2 - This is categorical. Will replace 2 missing values with most common value 'S'
data['Embarked'] = data['Embarked'].fillna('S')

#Fare: 1 - This is a numeric variable and we have only 1 missing value. Will replace it with the column mean
data['Fare'] = data['Fare'].fillna(data['Fare'].mean())

#Cabin: 1014 - Due to volume of missing values, will remove this column
data = data.drop('Cabin', axis = 1)

#Check for any remaining missing values
data.isnull().sum()

#Can remove rest of columns we don't need here as well
#Name, PassengerId, Ticket
data = data.drop('Name', axis = 1)
data = data.drop('PassengerId', axis = 1)
data = data.drop('Ticket', axis = 1)

#----- Convert Embarked and Sex to numerical categorical variables

data['Sex'] = data['Sex'].map({'female': 1, 'male': 2}).astype(int)
data['Embarked'] = data['Embarked'].map({'S': 1, 'C': 2, 'Q': 3}).astype(int)

#----- Split 'data' set back into 'train' and 'test' sets

training_set = data.iloc[0:891,]
test_set = data.iloc[891:1309,]

#Split X and y variables
X_train = training_set.iloc[:, 0:7].values
y_train = training_set.iloc[:, 7].values
X_test = test_set.iloc[:, 0:7].values
y_test = test_set.iloc[:, 7].values

#----- Feature scaling

from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
X_train = sc_X.fit_transform(X_train)
X_test = sc_X.transform(X_test)

#----- Fitting Kernel SVM to the training set
from sklearn.svm import SVC
classifier = SVC(kernel = 'rbf', random_state = 0)
classifier.fit(X_train,y_train)

#----- Predicting the Test set results
y_pred = classifier.predict(X_test)

#----- Create DataFrame with PassengerId and Survived
submission = pd.DataFrame({
        'PassengerId': PassengerId,
        'Survived': y_pred})

#----- Save csv file
submission.to_csv('ksvm_submission.csv', index=False)