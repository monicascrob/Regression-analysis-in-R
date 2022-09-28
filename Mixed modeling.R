# Mixed modeling homework homework for the People Analytics Certificate:
## Logistic Regressions to explain the probability distribution of Bonus Potential against the explanatory Tenure
## Creating confusion matrices
## Adding a new variable in the mix and re-run it
## Break potential multicollinearity by creating a new variable
## Run a new model with the newly created variable and compare any changes seen in the inferential statistics
## Crafting K-means cluster sets


# Part 1
# Import the dataset "hw3_dataset.csv" into your R environment and set a seed equal to 56789 and perform a 60% train / test split of your data
install.packages("Rtools")
install.packages("esquisse", repos = "https://cran.rstudio.com/")
library("esquisse")
esquisse::esquisser(assig4)
library("ggplot2")

data_filepath <- "C:/Users/monic/OneDrive/Documents/People Analytics/R Course/Assig3"
datadoc <- paste0(data_filepath,
                  "/",
                  "hw3_dataset.csv")

assig4 <- read.csv(datadoc,
                   header = TRUE)

set.seed(56789)
split <- sample(1:nrow(assig4),0.6*nrow(assig4))
train <- assig4[split,]
test <- assig4[-split,]

# Part 2 - Run a simple Logistic Regression where you are trying to estimate the probability distribution of "Bonus Potential" against the explanatory "Tenure", and call it logit_1:
names(assig4)
logit_1 <- glm(formula = bonus_potential ~ tenure
               ,family = "binomial"
               ,data = train)
summary_logit_1 <- summary(logit_1)
summary_logit_1

# What's the AIC and the Pseudo R2? 
# AIC: 1293.5
# Pseudo R2: 0.1072847

null_logit_1 <- glm(formula = bonus_potential ~ 1
                    ,family = "binomial"
                    ,data = train)
pseudo_r2_logit_1 <- 1-( logLik(logit_1) / logLik(null_logit_1) )
pseudo_r2_logit_1

predict_logit_1 <- predict(logit_1
                           ,data = train
                           ,newdata = test
                           ,type = "response")

## just a note to myself about how to calculate the percentage in likelihood that the bonus potential increases 
summary_logit_1$coefficients
100*(exp(summary(logit_1)$coef[2])-1)

# What's the training set's confusion matrix accuracy, as defined by total correct over total rows, and with a 50% cut-off? (15pts)
##                     predicted_bonus_potential
## true_bonus_potential   0   1
##                     0 735 116
##                     1 225 123
accuracy_subset_1 <- data.frame(bonus_potential = train$bonus_potential
                              ,predicted_probability = logit_1$fitted.values
                              ,prediction_binary = as.integer(logit_1$fitted.values>0.5))
confusion_matrix_1 <- table(true_bonus_potential = accuracy_subset_1$bonus_potential
                          ,predicted_bonus_potential = accuracy_subset_1$prediction_binary)
confusion_matrix_1

# What's the training set's AUC? 
## 72% accuracy for our training model
accuracy_1 <- sum(confusion_matrix_1[1,1],confusion_matrix_1[2,2])/nrow(train)
round(accuracy_1,2)

## AUC for our first model is 0.72
library("pROC")
roc_train_1 <- pROC::roc(accuracy_subset_1$bonus_potential ~ accuracy_subset_1$predicted_probability
                       ,plot = TRUE
                       ,print.auc = TRUE)
roc_train_1$auc

# Part 3 - Re-run all of the part 2 statistics, but for a slightly more complex model1 called logit_2: bonus_potential ~ tenure + experience + education
names(assig4)
logit_2 <- glm(formula = bonus_potential ~ tenure + experience + education
               ,family = "binomial"
               ,data = train)
summary(logit_2)

## AIC 490.03

null_logit_2 <- glm(formula = bonus_potential ~ tenure + experience + factor(education)
                    ,family = "binomial"
                    ,data = train)
pseudo_r2_logit_2 <- 1 - (logLik(logit_2) / logLik(null_logit_2))
pseudo_r2_logit_2

## pseudo R^2 -0.004653

predict_logit_2 <- predict(logit_2
                           ,data = train
                           ,newdata = test
                           ,type = "response")

accuracy_subset_2<- data.frame(bonus_potential = train$bonus_potential
                              ,predicted_probability = logit_2$fitted.values
                              ,prediction_binary = as.integer(logit_2$fitted.values>0.5))
confusion_matrix_2 <- table(true_bonus_potential = accuracy_subset_2$bonus_potential
                          ,predicted_bonus_potential = accuracy_subset_2$prediction_binary)
confusion_matrix_2

## accuracy for our second training model is 89%
accuracy_2 <- sum(confusion_matrix_2[1,1],confusion_matrix_2[2,2])/nrow(train)
round(accuracy_2,2)

## AUC for our second model is 0.96
roc_train_2 <- pROC::roc(accuracy_subset_2$bonus_potential ~ accuracy_subset_2$predicted_probability
                       ,plot = TRUE
                       ,print.auc = TRUE)
roc_train_2$auc

# Part 4 

# 1. Run a quick Pearson's correlation between Tenure and Experience
pearson_1 = cor(train$tenure, train$experience, method =c("pearson"))
# What's the correlation coefficient?
pearson_1
## 0.7143173

# Does that seem high to you? Why or why not?
## There's a strong positive correlation between tenure and experience that makes perfect sense.
## In many cases, person who stays a long time with a company, has more experience than someone who stays less.
## Of course, that isn't always the case, because some new employees can also have more experience from previous companies.
## In our case, it looks like a large number of employees stayed with the company for a long time.

# 2. 
ggplot(assig4) +
  aes(x = tenure, y = experience, colour = education) +
  geom_point(shape = "circle", size = 3L) +
  scale_color_distiller(palette = "OrRd", direction = 1) +
  labs (
    x = "Tenure (yrs)",
    y = "Experience (yrs",
    title = "Tenure v Experience c Education",
    subtitle = "Broken-up by Bonus Potential",
    color = "Education Level"
  ) +
  theme_dark() +
  theme(legend.position = "bottom") +
  facet_wrap(vars(bonus_potential)) +
  geom_smooth(method = "loess"
              , colour = "yellow")

# Does this plot reinforce your answer in part 1 or does it change your answer to part 1? Why or why not? 
## The plot and the trend line mostly show a clear positive correlation between the 2 variables.
## There is a small curb for employees with 6-7 years experience within the company, that explains why the correlation isn't even stronger that 0.7

# Part 5 - Run a Pearson's correlation check on train$tenure and train$extra_exp. Did we manage to break potential multicollinearity?

train$extra_exp <- train$experience - train$tenure
test$extra_exp <- test$experience - test$tenure
pearson_2 = cor(train$tenure, train$extra_exp, method =c("pearson"))

pearson_2
## With a pearson correlation of 0.002, we managed to break potential multicollinearity

# Part 6 -  Re-run the logit_2 model BUT replace the experience variable with your newly-created extra_exp variable. Call this model logit_3.

logit_3 <- glm(formula = bonus_potential ~ tenure + extra_exp + education
               ,family = "binomial"
               ,data = train)
summary(logit_3)

accuracy_subset_3<- data.frame(bonus_potential = train$bonus_potential
                               ,predicted_probability = logit_3$fitted.values
                               ,prediction_binary = as.integer(logit_3$fitted.values>0.5))
confusion_matrix_3 <- table(true_bonus_potential = accuracy_subset_3$bonus_potential
                            ,predicted_bonus_potential = accuracy_subset_3$prediction_binary)
confusion_matrix_3

# Which of the inferential statistics change from logit_2 to logit_3?
## the p-value for Tenure changed from 3.82e-13 to <2e-16 
## the coefficient estimate for Tenure doubled from 0.43 to 0.86
## std error changed from 0.05999 to 0.06779
## the z value changed from 7.262 to 12.754

# Why do you think this was the case?
## We obtained a better model with a higher confidence level, by being more specific about experience from/outside the company

# Part 7 - Create a subset called "train_subset" that only includes the variables "rate_hrly",
# "absent_days_yr", "tenure", "extra_exp" and "avg_wkly_hrs" from the train subset.

train_subset <- subset(train, select = c(rate_hrly, absent_days_yr, tenure, extra_exp, avg_wkly_hrs))
head(train_subset)

# Craft a K-means cluster set from this subset using NbClust::NbClust at a min of 2 and a max of 15.
set.seed(12345)

install.packages("factoextra")
install.packages("NbClust")
library("factoextra")
library("NbClust")

numerical_subset <- train_subset[,c("rate_hrly", "absent_days_yr", "tenure", "extra_exp", "avg_wkly_hrs")]

kmeans <- NbClust::NbClust(data = numerical_subset
                           ,min.nc = 2
                           ,max.nc = 10
                           ,method = "kmeans")

kmeans

# How many clusters were considered optimal by the model?
# 8 

# Add this new cluster to your training set as a new variable called "kmeans".
train$kmeans <- kmeans$Best.partition

# Part 8 - Add this new kmeans variable to your logit_3 model as a factor(kmeans) and call this last model logit_4

sapply(train, class)

logit_4 <- glm(formula = bonus_potential ~ tenure + extra_exp + education + factor(kmeans)
               ,family = "binomial"
               ,data = train)
summary(logit_4)

# What happened to the confusion matrix accuracy measure? 
accuracy_subset_4 <- data.frame(bonus_potential = train$bonus_potential
                                ,predicted_probability = logit_4$fitted.values
                                ,prediction_binary = as.integer(logit_4$fitted.values>0.5))
confusion_matrix_4 <- table(true_bonus_potential = accuracy_subset_4$bonus_potential
                            ,predicted_bonus_potential = accuracy_subset_4$prediction_binary)
confusion_matrix_4
## This logit_4 model gives us the least number of False Negatives and False Positives which I belive makes it the best model out of the 4 we tried.