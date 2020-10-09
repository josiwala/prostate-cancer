# load dataset
APPENC05 <- read.csv("C:/Users/jaosi/Desktop/DS-Projects/graduate-project/prostate-cancer/data/processed/APPENC05.txt")
mydata <- APPENC05
summary(mydata)
View(mydata)
names(mydata)

# load packages
library(caTools)
library(pscl)

# declare SeminalVesicleInvasion a categorical variable (SeminalVesicleInvasion = [0, 1])
mydata$SeminalVesicleInvasion <- factor(mydata$SeminalVesicleInvasion)

# create training and testing subsets
split <- sample.split(mydata, SplitRatio=0.8)
split
training <- subset(mydata, split=="TRUE")
testing <- subset(mydata, split=="FALSE")


#####################
### Model Fitting ###
#####################

### second-order polynomial logistic models ###
# fit full second-order logistic model
logit_poly <- glm(Y_HighGradeCancer ~ poly(PSALevel, 2) + poly(CancerVol, 2) + poly(Weight, 2) +
                           poly(Age, 2) + poly(BenignProstaticHyperplasia, 2) + SeminalVesicleInvasion +
                           poly(CapsularPenetration, 2), data=training, family="binomial")
summary(logit_poly)
step(logit_poly)
# pR2(logitpoly)

# use for quick analysis of adding/removing predictors
logit_poly2 <- glm(Y_HighGradeCancer ~ 
                      poly(PSALevel, 2)
                    + poly(CancerVol, 2) +
                  # + Weight^2 +
                    + poly(Age, 2)
                  # + poly(BenignProstaticHyperplasia, 2)
                  # + poly(SeminalVesicleInvasion, 2) 
                  # + poly(CapsularPenetration, 2)
                  , family="binomial", data=training)
summary(logit_poly2)
step(logit_poly2)


### first-order logistic models ###
# fit full first-order logistic model
logit_full <- glm(Y_HighGradeCancer ~ PSALevel + CancerVol + Weight + Age + BenignProstaticHyperplasia +
                  SeminalVesicleInvasion + CapsularPenetration, data=training, family="binomial")
summary(logit_full)
step(logit_full)
# pR2(logit_full)

# use for quick analysis of adding/removing predictors
logit2 <- glm(Y_HighGradeCancer ~ 
                PSALevel
             # + CancerVol
              #+ Weight
              #+ Age
              #+ BenignProstaticHyperplasia
              + SeminalVesicleInvasion
              + CapsularPenetration
              , data=training, family="binomial")
summary(logit2)


#####################################
### Model Checking and Validation ###
#####################################

# create a confusion matrix, and calculate model accuracy
res <- predict(logit2, testing, type="response")
t <- table(ActualValue=testing$Y_HighGradeCancer, PredictedValue=res>0.5)
t
accuracy <- (t[1] + t[4]) / (t[1] + t[2] +t[3] + t[4])
accuracy















