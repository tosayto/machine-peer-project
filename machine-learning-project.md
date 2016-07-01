# Machine-Peer-Project
Z. Ozcan  
26 June 2016  
###Synposis
Our goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants to predict the manner in which they did the exercise.

###Data Load and Transformation
First, we load our data and libraries that are nessesary.


```r
library(caret)
library(dplyr, quietly=TRUE,verbose=FALSE,warn.conflicts = FALSE)
dat<-read.csv("pml-training.csv", header = TRUE, na.strings = c(""))
dattest<-read.csv("pml-testing.csv", header=TRUE, na.strings = c(""))
```

####Data Cleaning
We also remove some calculated variables to make easy for us.


```r
#Because this are all calculated variables this will be some how correlated with
#the other variables and we don't want that.
dat<-dat[,-c(grep("^max", names(dat)))]
dat<-dat[,-c(grep("^min", names(dat)))]
dat<-dat[,-c(grep("^amplitude", names(dat)))]
dat<-dat[,-c(grep("^kurtosis", names(dat)))]
dat<-dat[,-c(grep("^skewness", names(dat)))]
dat<-dat[,-c(grep("^var", names(dat)))]
dat<-dat[,-c(grep("^total", names(dat)))]
dat<-dat[,-c(grep("^avg", names(dat)))]
dat<-dat[,-c(grep("^stddev", names(dat)))]

dattest<-dattest[,-c(grep("^max", names(dattest)))]
dattest<-dattest[,-c(grep("^min", names(dattest)))]
dattest<-dattest[,-c(grep("^amplitude", names(dattest)))]
dattest<-dattest[,-c(grep("^kurtosis", names(dattest)))]
dattest<-dattest[,-c(grep("^skewness", names(dattest)))]
dattest<-dattest[,-c(grep("^var", names(dattest)))]
dattest<-dattest[,-c(grep("^total", names(dattest)))]
dattest<-dattest[,-c(grep("^avg", names(dattest)))]
dattest<-dattest[,-c(grep("^stddev", names(dattest)))]
```

Here, we see some variables from training data.


```
## 'data.frame':	19622 obs. of  56 variables:
##  $ X                   : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ user_name           : Factor w/ 6 levels "adelmo","carlitos",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ raw_timestamp_part_1: int  1323084231 1323084231 1323084231 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 ...
##  $ raw_timestamp_part_2: int  788290 808298 820366 120339 196328 304277 368296 440390 484323 484434 ...
##  $ cvtd_timestamp      : Factor w/ 20 levels "02/12/2011 13:32",..: 9 9 9 9 9 9 9 9 9 9 ...
##  $ new_window          : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
##  $ num_window          : int  11 11 11 12 12 12 12 12 12 12 ...
##  $ roll_belt           : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
##  $ pitch_belt          : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
##  $ yaw_belt            : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
##   [list output truncated]
```

###Model Creation
We use 3 models from the classification models because our output variable is a discrete valued output. To see that we are on the right direction we have selected the FDA (Flexible Discriminant Analysis), the SVM(Support Vector Machine) and the RF(Random Forest) as a model for our training data and we sub-divided this data in the training and test data. The model with the most accuracy will be used for the final test. We will use the caret package to do all that work.

####Data preperation


```r
#ALL
All<-dat
set.seed(1072)

#We use 70% of training data as traing and 30% as test data.
datAllIndex<- createDataPartition(dat$classe, p=.7, list = FALSE)
datAllTraining<-All[datAllIndex,]
datAllTest<-All[-datAllIndex,]

#removing some unnesesary varibles because they have nothing to do with
#the prediction of the activities (I just guess) and we are removing rows with NA's
cleanAll<-na.omit(datAllTraining)
cleanAll<-cleanAll[,-c(1,2,3,4,5,6,7)]
cleanAllTest<-na.omit(datAllTest)
cleanAllTest<-cleanAllTest[,-c(1,2,3,4,5,6,7)]
```

####Model 1 (FDA)


```r
#MODEL 1
set.seed(1072)

#We train here our model with train function of the caret package
FDAAll<-train(classe~.,data = cleanAll, method="fda")

#Getting predictions
pFDAAll<-predict(FDAAll,cleanAllTest)

#Confusion matrix to get some parameters
confusionMatrix(pFDAAll, cleanAllTest$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1367  152   11    8   49
##          B   83  625   73   99  204
##          C  151  211  837  213  102
##          D   66  151  105  627  180
##          E    7    0    0   17  547
## 
## Overall Statistics
##                                           
##                Accuracy : 0.6802          
##                  95% CI : (0.6681, 0.6921)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.597           
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8166   0.5487   0.8158   0.6504  0.50555
## Specificity            0.9478   0.9033   0.8607   0.8980  0.99500
## Pos Pred Value         0.8614   0.5766   0.5528   0.5554  0.95797
## Neg Pred Value         0.9286   0.8929   0.9568   0.9291  0.89932
## Prevalence             0.2845   0.1935   0.1743   0.1638  0.18386
## Detection Rate         0.2323   0.1062   0.1422   0.1065  0.09295
## Detection Prevalence   0.2697   0.1842   0.2573   0.1918  0.09703
## Balanced Accuracy      0.8822   0.7260   0.8382   0.7742  0.75027
```

####Model 2 (SVM)


```r
#MODEL 2
set.seed(1072)
#Turn off warning messages
options(warn=-1)#use 0 to turn on
#we train our model
SVMAll<-train(classe~.,data = cleanAll, method="svmRadial")

#Getting predictions
pSVMAll<-predict(SVMAll,cleanAllTest)

#Confusion matrix to get some parameters
confusionMatrix(pSVMAll, cleanAllTest$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1661  111    5    3    2
##          B    4  985   42    4   13
##          C    7   38  960  112   49
##          D    0    2   19  844   34
##          E    2    3    0    1  984
## 
## Overall Statistics
##                                         
##                Accuracy : 0.9234        
##                  95% CI : (0.9163, 0.93)
##     No Information Rate : 0.2845        
##     P-Value [Acc > NIR] : < 2.2e-16     
##                                         
##                   Kappa : 0.9029        
##  Mcnemar's Test P-Value : < 2.2e-16     
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9922   0.8648   0.9357   0.8755   0.9094
## Specificity            0.9713   0.9867   0.9576   0.9888   0.9988
## Pos Pred Value         0.9321   0.9399   0.8233   0.9388   0.9939
## Neg Pred Value         0.9968   0.9682   0.9860   0.9759   0.9800
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2822   0.1674   0.1631   0.1434   0.1672
## Detection Prevalence   0.3028   0.1781   0.1981   0.1528   0.1682
## Balanced Accuracy      0.9817   0.9258   0.9466   0.9322   0.9541
```

####Model 3 (RF)


```r
#MODEL 3
set.seed(1072)

#we train our model with random forest algorithme
RFAll<-train(classe~.,data = cleanAll, method="rf")
```

```
## Loading required package: randomForest
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```r
#Getting predictions
pRFAll<-predict(RFAll,cleanAllTest)

#Confusion matrix to get some parameters
confusionMatrix(pRFAll, cleanAllTest$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    9    0    0    0
##          B    0 1129   10    0    0
##          C    0    1 1010   17    0
##          D    0    0    6  947    3
##          E    0    0    0    0 1079
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9922          
##                  95% CI : (0.9896, 0.9943)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9901          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9912   0.9844   0.9824   0.9972
## Specificity            0.9979   0.9979   0.9963   0.9982   1.0000
## Pos Pred Value         0.9947   0.9912   0.9825   0.9906   1.0000
## Neg Pred Value         1.0000   0.9979   0.9967   0.9966   0.9994
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2845   0.1918   0.1716   0.1609   0.1833
## Detection Prevalence   0.2860   0.1935   0.1747   0.1624   0.1833
## Balanced Accuracy      0.9989   0.9946   0.9904   0.9903   0.9986
```

###Out Of Sample Error

Our RF model did good job. The RF model gave an accuracy of 99.22% on our traning dataset, which is much more better than the other models that we have tried. The expected out-of-sample error is 100-99.22 = 0.78%, therefore the RF model was selected to make the final predictions. 

###Final Prediction


```r
#FINAL PREDICTION
set.seed(1072)

#removing some unnesseray variables
dattest<-dattest[,-c(1,2,3,4,5,6,7)]

#Getting predictions
pRFTest<-predict(RFAll,dattest)

#Print predictions
pRFTest
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

We have to verify this predictions now with the last quiz questions to see we did a good work.
