# set working directory
setwd("C:/Users/jaosi/Desktop/DS-Projects/graduate-project/prostate-cancer")

# load dataset
APPENC05 <- read.csv("./data/processed/APPENC05.txt")
mydata <- APPENC05
summary(mydata)
View(mydata)
names(mydata)

# load packages
library(caTools)
library(ROCR)
#library(pscl)

# declare SeminalVesicleInvasion a categorical variable (SeminalVesicleInvasion = [0, 1])
mydata$SeminalVesicleInvasion <- factor(mydata$SeminalVesicleInvasion)

# create training and testing subsets
myseed <- 123
set.seed(myseed)
split <- sample.split(mydata, SplitRatio=0.8)
train <- subset(mydata, split=="TRUE")
test <- subset(mydata, split=="FALSE")

View(train)
View(test)

# write train & test datasets to CSV files
write.csv(train, "./data/processed/train.txt")
write.csv(test, "./data/processed/test.txt")



#####################
### Model Fitting ###
#####################

### second-order polynomial logistic models ###
# fit full second-order logistic model
logit_poly <- glm(Y_HighGradeCancer ~ poly(PSALevel, 2) + poly(CancerVol, 2) + 
                  poly(Weight, 2) + poly(Age, 2) + poly(BenignProstaticHyperplasia, 2) + 
                  SeminalVesicleInvasion + poly(CapsularPenetration, 2), 
                  data=training, family="binomial")
summary(logit_poly)
step(logit_poly)
#pR2(logitpoly)

# use second-order reduced model setup for quick analysis of adding/removing predictors
logit_poly_red <- glm(Y_HighGradeCancer ~ 
                      poly(PSALevel, 2)
                  #  + poly(CancerVol, 2) +
                  # + Weight^2 +
                    + poly(Age, 2)
                  # + poly(BenignProstaticHyperplasia, 2)
                  # + poly(SeminalVesicleInvasion, 2) 
                   + poly(CapsularPenetration, 2)
                  , family="binomial", data=training)
summary(logit_poly_red)
step(logit_poly_red, direction="backward")


### first-order logistic models ###
# fit full first-order logistic model
logit_full <- glm(Y_HighGradeCancer ~ PSALevel + CancerVol + Weight + 
                  Age + BenignProstaticHyperplasia +
                  SeminalVesicleInvasion + CapsularPenetration, data=training, 
                  family="binomial")
summary(logit_full)
step(logit_full, direction="backward")
#pR2(logit_full)

# use reduced model setup for quick analysis of adding/removing predictors
logit_red <- glm(Y_HighGradeCancer ~ 
                 PSALevel
               + CancerVol
              # + Weight 
              # + Age
              # + BenignProstaticHyperplasia
              # + SeminalVesicleInvasion
              # + CapsularPenetration
              , data=training, family="binomial")
summary(logit_red)
#pR2(logit_red)


#####################################
### Model Checking and Validation ###
#####################################

### accuracy model comparisons
freq <- function(data) {
  # function required input parameter: data.
  # this function will display the table of Y_HighGradeCancer counts (frequency table); i.e. the counts of 0's and 1's in the input data.
  # the function will then display the proportion of 0 to 1 (I already know via previous analysis that the counts of 0's greatly outweigh the count of 1's).
  # we can consider this proportion to be a "base accuracy" for model comparison; i.e. if the model just predicted 0's (most frequent classification), for all cases.
  
  name <- deparse(substitute(data))
  
  if (name=='train') {
    print('TRAINING DATA')
  }
  else {
    print('TESTING DATA')
  }
  
  freq_tab <- table(data$Y_HighGradeCancer)
  most_freq_prop <- sum(freq_tab[1])/sum(freq_tab)
  
  # print out both the table, and calculated base accuracy 
  print('Frequency Table:')
  print(freq_tab)
  print('The proportion of 0 to 1 is:')
  print(most_freq_prop)
}


accuracy <- function(model, data, val=0.5) {
  # function required input parameters: model, data, and decision value boundry (optional); defualt 50%.
  # this function will first apply the fitted model and create classifications, then compare to real values (which we know).
  # the confusion matrix and accuracy score will output to the terminal.
  # idealy we want the accuracy score to be greater than the base score calculated previously (this indicates the logistic model is a better fit).
  # decision boundry value may require analysis and adjustments/optimizations afterwards.
  
  name <- deparse(substitute(data))
  
  if (name=='train') {
    print('TRAINING DATA')
  }
  else {
    print('TESTING DATA')
  }
  
  res <- predict(model, data, type="response")
  tab <- table(ActualValue=data$Y_HighGradeCancer, PredictedValue=res>=val)
  acc <- sum(diag(tab))/sum(tab)
  
  # print out confusion matrix, and calculated accuracy
  print('Confusion Matrix:')
  print(tab)
  print('The calculated accuracy is:')
  print(acc)
}


### Training Data ###
# invoke functions
freq(train)
accuracy(logit_red, train, 0.5)

### training vizualizations
# Residuals vs. Fitted
# Normal Q-Q
# scale-location (Predicted Values vs. sqrt[Std. Pearson Residuals])
# Residuals vs. Leverage
plot(logit_red)

# build and plot ROC curve
pred <- predict(logit_red, train, type="response")
ROCRPred <- prediction(pred, train$Y_HighGradeCancer)
ROCRPref <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPref, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))


####################
### Testing Data ###
####################

# invoke functions
freq(test)
accuracy(logit_red, test, 0.2)






















