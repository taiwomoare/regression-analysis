> ## REGRESSION ANALYSIS MODEL
> ## BY:
> ## OMOARE, GILBERT TAIWO       &
> ## AKINSOLA, AHMED ABAYOMI
> ## INSTRUCTOR:
> ## DR. MATTHEW JONES
> 
> ##Check our working directory
> getwd()
[1] "/Users/gilbertomoare/Desktop/RA-5120/Final Project/spring2023data"
> ##Load our dataset into R
> diabetes_data <- read.csv("diabetes.csv")
> #Let's explore the dataset
> head(diabetes_data)
  Pregnancies Glucose BloodPressure SkinThickness Insulin  BMI DiabetesPedigreeFunction Age Outcome
1           6     148            72            35       0 33.6                    0.627  50       1
2           1      85            66            29       0 26.6                    0.351  31       0
3           8     183            64             0       0 23.3                    0.672  32       1
4           1      89            66            23      94 28.1                    0.167  21       0
5           0     137            40            35     168 43.1                    2.288  33       1
6           5     116            74             0       0 25.6                    0.201  30       0
> dim(diabetes_data)
[1] 768   9

> # histograms of some of the predictor variables against the response
> ggplot(diabetes_data, aes(x = Glucose, fill = factor(Outcome))) + geom_histogram(alpha = 0.5, bins = 30) + facet_wrap(~Outcome)
> ggplot(diabetes_data, aes(x = BloodPressure, fill = factor(Outcome))) + geom_histogram(alpha = 0.5, bins = 30) + facet_wrap(~Outcome)
> ggplot(diabetes_data, aes(x = BMI, fill = factor(Outcome))) + geom_histogram(alpha = 0.5, bins = 30) + facet_wrap(~Outcome)
> ggplot(diabetes_data, aes(x = Pregnancies, fill = factor(Outcome))) + geom_histogram(alpha = 0.5, bins = 30) + facet_wrap(~Outcome)
> ggplot(diabetes_data, aes(x = DiabetesPedigreeFunction, fill = factor(Outcome))) + geom_histogram(alpha = 0.5, bins = 30) + facet_wrap(~Outcome)

> ##Let's get a summary of the dataset
> summary(diabetes_data)
  Pregnancies        Glucose      BloodPressure    SkinThickness      Insulin           BMI       
 Min.   : 0.000   Min.   :  0.0   Min.   :  0.00   Min.   : 0.00   Min.   :  0.0   Min.   : 0.00  
 1st Qu.: 1.000   1st Qu.: 99.0   1st Qu.: 62.00   1st Qu.: 0.00   1st Qu.:  0.0   1st Qu.:27.30  
 Median : 3.000   Median :117.0   Median : 72.00   Median :23.00   Median : 30.5   Median :32.00  
 Mean   : 3.845   Mean   :120.9   Mean   : 69.11   Mean   :20.54   Mean   : 79.8   Mean   :31.99  
 3rd Qu.: 6.000   3rd Qu.:140.2   3rd Qu.: 80.00   3rd Qu.:32.00   3rd Qu.:127.2   3rd Qu.:36.60  
 Max.   :17.000   Max.   :199.0   Max.   :122.00   Max.   :99.00   Max.   :846.0   Max.   :67.10  
 DiabetesPedigreeFunction      Age           Outcome     
 Min.   :0.0780           Min.   :21.00   Min.   :0.000  
 1st Qu.:0.2437           1st Qu.:24.00   1st Qu.:0.000  
 Median :0.3725           Median :29.00   Median :0.000  
 Mean   :0.4719           Mean   :33.24   Mean   :0.349  
 3rd Qu.:0.6262           3rd Qu.:41.00   3rd Qu.:1.000  
 Max.   :2.4200           Max.   :81.00   Max.   :1.000  
>
> ##Check the structure of the dataset
> str(diabetes_data)
'data.frame':	768 obs. of  9 variables:
 $ Pregnancies             : int  6 1 8 1 0 5 3 10 2 8 ...
 $ Glucose                 : int  148 85 183 89 137 116 78 115 197 125 ...
 $ BloodPressure           : int  72 66 64 66 40 74 50 0 70 96 ...
 $ SkinThickness           : int  35 29 0 23 35 0 32 0 45 0 ...
 $ Insulin                 : int  0 0 0 94 168 0 88 0 543 0 ...
 $ BMI                     : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
 $ DiabetesPedigreeFunction: num  0.627 0.351 0.672 0.167 2.288 ...
 $ Age                     : int  50 31 32 21 33 30 26 29 53 54 ...
 $ Outcome                 : int  1 0 1 0 1 0 1 0 1 1 ...

> ##Check for missing values
> sum(is.na(diabetes_data))
[1] 0

> ##Let's create a logistic regression model using all 
> ##variables as predictors
> full_model <- glm(Outcome ~ ., data = diabetes_data, family = "binomial")
> sumary(full_model)
                            Estimate  Std. Error  z value  Pr(>|z|)
(Intercept)              -8.40469637  0.71663588 -11.7280 < 2.2e-16
Pregnancies               0.12318230  0.03207755   3.8401  0.000123
Glucose                   0.03516371  0.00370871   9.4814 < 2.2e-16
BloodPressure            -0.01329555  0.00523361  -2.5404  0.011072
SkinThickness             0.00061896  0.00689938   0.0897  0.928515
Insulin                  -0.00119170  0.00090123  -1.3223  0.186065
BMI                       0.08970097  0.01508763   5.9453 2.759e-09
DiabetesPedigreeFunction  0.94517974  0.29914746   3.1596  0.001580
Age                       0.01486900  0.00933479   1.5929  0.111192

n = 768 p = 9
Deviance = 723.44538 Null Deviance = 993.48391 (Difference = 270.03853) 
>

> # fit models with all possible subsets of predictors
> library(leaps)
> best.subsets <- regsubsets(Outcome ~ ., data=diabetes_data, nvmax=8)
> # print summary of best models based on adjusted R-squared
> summary(best.subsets)
Subset selection object
Call: regsubsets.formula(Outcome ~ ., data = diabetes_data, nvmax = 8)
8 Variables  (and intercept)
                         Forced in Forced out
Pregnancies                  FALSE      FALSE
Glucose                      FALSE      FALSE
BloodPressure                FALSE      FALSE
SkinThickness                FALSE      FALSE
Insulin                      FALSE      FALSE
BMI                          FALSE      FALSE
DiabetesPedigreeFunction     FALSE      FALSE
Age                          FALSE      FALSE
1 subsets of each size up to 8
Selection Algorithm: exhaustive
         Pregnancies Glucose BloodPressure SkinThickness Insulin BMI DiabetesPedigreeFunction Age
1  ( 1 ) " "         "*"     " "           " "           " "     " " " "                      " "
2  ( 1 ) " "         "*"     " "           " "           " "     "*" " "                      " "
3  ( 1 ) "*"         "*"     " "           " "           " "     "*" " "                      " "
4  ( 1 ) "*"         "*"     " "           " "           " "     "*" "*"                      " "
5  ( 1 ) "*"         "*"     "*"           " "           " "     "*" "*"                      " "
6  ( 1 ) "*"         "*"     "*"           " "           " "     "*" "*"                      "*"
7  ( 1 ) "*"         "*"     "*"           " "           "*"     "*" "*"                      "*"
8  ( 1 ) "*"         "*"     "*"           "*"           "*"     "*" "*"                      "*"

> # plot the model selection criteria
> par(mfrow=c(1,1))
> plot(best.subsets, scale="adjr2")
> cor_mat <- cor(diabetes_data[,1:8])
> cor_mat
                         Pregnancies    Glucose BloodPressure SkinThickness     Insulin        BMI
Pregnancies               1.00000000 0.12945867    0.14128198   -0.08167177 -0.07353461 0.01768309
Glucose                   0.12945867 1.00000000    0.15258959    0.05732789  0.33135711 0.22107107
BloodPressure             0.14128198 0.15258959    1.00000000    0.20737054  0.08893338 0.28180529
SkinThickness            -0.08167177 0.05732789    0.20737054    1.00000000  0.43678257 0.39257320
Insulin                  -0.07353461 0.33135711    0.08893338    0.43678257  1.00000000 0.19785906
BMI                       0.01768309 0.22107107    0.28180529    0.39257320  0.19785906 1.00000000
DiabetesPedigreeFunction -0.03352267 0.13733730    0.04126495    0.18392757  0.18507093 0.14064695
Age                       0.54434123 0.26351432    0.23952795   -0.11397026 -0.04216295 0.03624187
                         DiabetesPedigreeFunction         Age
Pregnancies                           -0.03352267  0.54434123
Glucose                                0.13733730  0.26351432
BloodPressure                          0.04126495  0.23952795
SkinThickness                          0.18392757 -0.11397026
Insulin                                0.18507093 -0.04216295
BMI                                    0.14064695  0.03624187
DiabetesPedigreeFunction               1.00000000  0.03356131
Age                                    0.03356131  1.00000000
> pairs(diabetes_data)

> #Let's plot the data and check for collinearity:
> library(corrplot)
> corrplot(cor_mat, type = "upper", method = "color", order = "hclust")
> ggcorrplot(cor_mat, type = "upper", lab = TRUE)
> #Using heatmap
> heatmap(cor_mat, symm = TRUE, scale = "none", Rowv = NA, Colv = NA, 
+         col = colorRampPalette(c("red", "white", "blue"))(50))
> pairs(diabetes_data)


> #create a logistic regression model using all variables as predictors
> #Model 1: Using all independent variables
> model1 <- glm(Outcome ~ ., data = train, family = "binomial")
> summary(model1)

Call:
glm(formula = Outcome ~ ., family = "binomial", data = train)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.7124  -0.6772  -0.3820   0.6426   2.5654  

Coefficients:
                          Estimate Std. Error z value Pr(>|z|)    
(Intercept)              -9.256065   0.906286 -10.213  < 2e-16 ***
Pregnancies               0.102536   0.040193   2.551   0.0107 *  
Glucose                   0.044270   0.004978   8.892  < 2e-16 ***
BloodPressure            -0.014853   0.007046  -2.108   0.0350 *  
SkinThickness             0.007975   0.008803   0.906   0.3649    
Insulin                  -0.003353   0.001256  -2.669   0.0076 ** 
BMI                       0.089144   0.018969   4.699 2.61e-06 ***
DiabetesPedigreeFunction  0.832683   0.385883   2.158   0.0309 *  
Age                       0.016337   0.011838   1.380   0.1675    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 696.28  on 537  degrees of freedom
Residual deviance: 477.81  on 529  degrees of freedom
AIC: 495.81

Number of Fisher Scoring iterations: 5

> resid <- residuals(model1, type = "response")
> qqnorm(resid)
> qqline(resid)

> # Predict outcomes on the test data using the logistic regression model
> library(caTools)
> set.seed(123) # to ensure reproducibility
> split <- sample.split(diabetes$Outcome, SplitRatio = 0.7) # split 70% of the data into the training set
> train <- subset(diabetes, split == TRUE)
> test <- subset(diabetes, split == FALSE)
> model <- glm(Outcome ~ ., data = train, family = "binomial")
> model1 <- glm(Outcome ~ ., data = train, family = "binomial")
> summary(model1)

Call:
glm(formula = Outcome ~ ., family = "binomial", data = train)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.7124  -0.6772  -0.3820   0.6426   2.5654  

Coefficients:
                          Estimate Std. Error z value Pr(>|z|)    
(Intercept)              -9.256065   0.906286 -10.213  < 2e-16 ***
Pregnancies               0.102536   0.040193   2.551   0.0107 *  
Glucose                   0.044270   0.004978   8.892  < 2e-16 ***
BloodPressure            -0.014853   0.007046  -2.108   0.0350 *  
SkinThickness             0.007975   0.008803   0.906   0.3649    
Insulin                  -0.003353   0.001256  -2.669   0.0076 ** 
BMI                       0.089144   0.018969   4.699 2.61e-06 ***
DiabetesPedigreeFunction  0.832683   0.385883   2.158   0.0309 *  
Age                       0.016337   0.011838   1.380   0.1675    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 696.28  on 537  degrees of freedom
Residual deviance: 477.81  on 529  degrees of freedom
AIC: 495.81

Number of Fisher Scoring iterations: 5

> model2 <- glm(Outcome ~ Pregnancies + Glucose + BMI + Age, data = train, family = "binomial")
> summary(model2)

Call:
glm(formula = Outcome ~ Pregnancies + Glucose + BMI + Age, family = "binomial", 
    data = train)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3334  -0.6894  -0.3894   0.6744   2.4932  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -9.01768    0.83802 -10.761  < 2e-16 ***
Pregnancies  0.10190    0.03894   2.617  0.00887 ** 
Glucose      0.03883    0.00429   9.049  < 2e-16 ***
BMI          0.07962    0.01689   4.714 2.43e-06 ***
Age          0.01571    0.01103   1.425  0.15423    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 696.28  on 537  degrees of freedom
Residual deviance: 492.88  on 533  degrees of freedom
AIC: 502.88

Number of Fisher Scoring iterations: 5

> model3 <- glm(Outcome ~ Pregnancies + Glucose + BMI + Pregnancies:BMI, data = train, family = binomial)
> summary(model3)

Call:
glm(formula = Outcome ~ Pregnancies + Glucose + BMI + Pregnancies:BMI, 
    family = binomial, data = train)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.2914  -0.6935  -0.3833   0.6832   2.2685  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -9.685679   1.043363  -9.283  < 2e-16 ***
Pregnancies      0.387545   0.157936   2.454   0.0141 *  
Glucose          0.040095   0.004268   9.394  < 2e-16 ***
BMI              0.106884   0.025257   4.232 2.32e-05 ***
Pregnancies:BMI -0.007736   0.004630  -1.671   0.0948 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 696.28  on 537  degrees of freedom
Residual deviance: 492.22  on 533  degrees of freedom
AIC: 502.22

Number of Fisher Scoring iterations: 5

> # Predict outcomes on test data using model 1
> pred1 <- predict(model1, newdata = test, type = "response")
> pred1 <- ifelse(pred1 > 0.5, 1, 0)
> # Predict outcomes on test data using model 2
> pred2 <- predict(model2, newdata = test, type = "response")
> pred2 <- ifelse(pred2 > 0.5, 1, 0)
> # Predict outcomes on test data using model 3
> pred3 <- predict(model3, newdata = test, type = "response")
> pred3 <- ifelse(pred3 > 0.5, 1, 0)
> # Calculate accuracy of each model on test data
> accuracy1 <- mean(pred1 == test$Outcome)
> accuracy2 <- mean(pred2 == test$Outcome)
> accuracy3 <- mean(pred3 == test$Outcome)
> # Print accuracy of each model
> cat("Accuracy of Model 1: ", accuracy1, "\n")
Accuracy of Model 1:  0.7391304 
> cat("Accuracy of Model 2: ", accuracy2, "\n")
Accuracy of Model 2:  0.7478261 
> cat("Accuracy of Model 3: ", accuracy3, "\n")
Accuracy of Model 3:  0.7521739
