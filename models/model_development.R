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
# library(pscl)

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
# pR2(logitpoly)

# use for quick analysis of adding/removing predictors
logit_poly2 <- glm(Y_HighGradeCancer ~ 
                      poly(PSALevel, 2)
                  #  + poly(CancerVol, 2) +
                  # + Weight^2 +
                    + poly(Age, 2)
                  # + poly(BenignProstaticHyperplasia, 2)
                  # + poly(SeminalVesicleInvasion, 2) 
                   + poly(CapsularPenetration, 2)
                  , family="binomial", data=training)
summary(logit_poly2)
step(logit_poly2)


### first-order logistic models ###
# fit full first-order logistic model
logit_full <- glm(Y_HighGradeCancer ~ PSALevel + CancerVol + Weight + 
                  Age + BenignProstaticHyperplasia +
                  SeminalVesicleInvasion + CapsularPenetration, data=training, 
                  family="binomial")
summary(logit_full)
step(logit_full, direction="backward")
# pR2(logit_full)

# use for quick analysis of adding/removing predictors
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
# pR2(logit_red)



#####################################
### Model Checking and Validation ###
#####################################

### accuracy model comparisons
freq_acc <- function(data) {
  freq_tab <- table(data$Y_HighGradeCancer)
  most_freq_acc <- freq_tab[1]/sum(freq_tab)
  
  # print out both the table, and calculated base accuracy 
  print(freq_tab)
  print(most_freq_acc)
}


accuracy <- function(model, data, val=0.5) {
  res <- predict(model, data, type="response")
  tab <- table(ActualValue=data$Y_HighGradeCancer, PredictedValue=res>=val)
  acc <- sum(diag(tab))/sum(tab)
  
  # print out confusion matrix, and calculated accuracy
  print(tab)
  print(acc)
}

freq_acc(training)
accuracy(logit_red, training, 0.5)




######## bonus analysis / plots
plot(logit_red)

pred <- predict(logit_red, training, type="response")
ROCRPred <- prediction(pred, training$Y_HighGradeCancer)
ROCRPref <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPref, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.2))

















