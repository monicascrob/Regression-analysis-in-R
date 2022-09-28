# Regression Modelling homework :
## Train/test split
## Linear regressions
## R2 values
## RMSE
## Model comparison

# Part 1 - Import the dataset

data_filepath <- "C:/Users/monic/OneDrive/Documents/People Analytics/R Course/Assig3"
datadoc <- paste0(data_filepath,
                  "/",
                  "hw3_dataset.csv")

assig3 <- read.csv(datadoc,
                     header = TRUE)

# Part 2 - Set seed equal to 1234 and perform a 60% train/test split of the data

set.seed(12345)
split <- sample(1:nrow(assig3),0.6*nrow(assig3))
train <- assig3[split,]
test <- assig3[-split,]

# Part 3 - Using the "train" subset calculate the Pearson and Spearman correlation coefficients for Hourly Rate vs Tenure. Then, using the formula from the previous HW, calculate the % difference across the two measures.
## From these results, does it seem as though a model would benefit from the added "complexity" of non-linearity? Why or why not?

pearson = cor(train$tenure, train$rate_hrly, method =c("pearson"))
spearman = cor(train$tenure, train$rate_hrly, method =c("spearman"))
percentagedif = ((spearman/pearson)-1)*100

# From running the Spearman and Pearson correlations, the relationship between tenure and hourly rate, seems to be highly significant.
## In this case, I don't consider we need to add more complexity to our model.

# Part 4 - Using your "train" subset, run a linear regression that only tests Y = Hourly Rate against X = Tenure. Call this model object "linear_1":

names(assig3)
linear_1 <- lm(formula = rate_hrly ~ tenure
               ,data = train)
summary(linear_1)
hist(linear_1$residuals)

# What is your coefficient for Tenure?
## 19.2225

# What % of variation capture is determined by the model's R2 value?
## 70.23%

test$prediction_1 <- predict(linear_1
                        ,data = train
                        ,newdata = test)

# Is Tenure statistically significant at the 95% level? 
## Yes, the p value is < 2.2e-16
# How about at the 99% level? Why or why not?
## There is less than a 0.1% chance that the coefficient might be equal to 0 - "***", so yes Tenure is statistically significant at the 99% level as well. 

# Part 5 - Calculate the test RMSE of this model.

rmse_1 = sqrt(
  mean(
    (test$rate_hrly - test$prediction_1)^2
  )
)
rmse_1

# Part 6 - Using your "train" subset, let's run a more complex model, called "linear_2", with Y = Hourly Rate and your Xs = Tenure, Education, Experience and Resident Type.

names(assig3)
linear_2 <- lm(formula = rate_hrly ~ tenure + factor(education) + experience + factor(resident_type)
               ,data = train)
summary(linear_2)
hist(linear_2$residuals)

# Part 7 - A. Model Comparison
# How does the R2 of this model compare to that of the first model?
## The second model does a better job with a R^2 of 0.89 vs the previous one - 0.70

# Are there any variables that fail to meet the 95% confidence level?
## No - even do education 2 and factor 1 have a slightly higher p, but still not over 0.1. 

# How does the test RMSE of this model compare to that of the first model?
## The RMSE for the second  model is lower - 21 than the one for our first model -36

test$prediction_2 <- predict(linear_2
                        ,data = train
                        ,newdata = test)

rmse_2 = sqrt(
  mean(
    (test$rate_hrly - test$prediction_2)^2
  )
)
rmse_2

# Part 7 - B. Decomposing Factor Variables

# Given this concept and example, and looking at your results from linear_2, what is the strongest factor in each of your categorical variables?
## The strongest factors seem to be Education 4 (post-grad) and Resident_type 2 (city) because of the higher coefficient and the pr<2e-16. 

# Regarding the education coefficients, is there a bigger jump in the coefficients from 1 to 2, 2 to 3 or 3 to 4? And, does viewing the coefficients in this way (i.e. as "jumps") make you think differently about "strongest factor"? Why or why not?
## There's a bigger jump from "some college" factor (2) to "college" (3) which made me second guess my previous decision.
## Considering that the coefficients identify the weight of the variable to another, factor 3 is stronger in comparison to factor 2 than factor 4 is in comparison to factor 3.

# Part 8
# Create a new 60% train / test split (call them "train2" and "test2"), but with seed of 6789.
set.seed(6789)
split <- sample(1:nrow(assig3),0.6*nrow(assig3))
train2 <- assig3[split,]
test2 <- assig3[-split,]

# Re-run the model from Part 6 and call this model linear_3
linear_3 <- lm(formula = rate_hrly ~ tenure + factor(education) + experience + factor(resident_type)
               ,data = train2)
summary(linear_3)
hist(linear_3$residuals)

# Then calculate the RMSE for test2.
test2$prediction_3 <- predict(linear_3
                        ,data = train2
                        ,newdata = test2)

rmse_3 = sqrt(
  mean(
    (test2$rate_hrly - test2$prediction_3)^2
  )
)
rmse_3

# How does the R2 compare to that of linear_2?
## The R^2 is a bit higher for our 3rd model - 0.90 vs 0.89 for our 2nd model

# How does the RMSE compare to that of linear_2?
## RMSE 3 is a bit higher than RMSE 2 - 21.69 vs 21.14. 
## Given that we have a higher RMSE and just a bit higher R^2, the 3rd model isn't performing better than our 2nd.

# Were there any changes in the significance levels of the variables?
## Yes, but a really small change, to the factors that were already not statistically significant at the 95% level.

# Given the performance of the same model across two train / test splits, do you feel as though this is a consistent model and something worthy of sharing with others? Why or why not?
## This looks like a consistent model, the data generated looks similar.

# Part 9 - Using your "train" subset, run an additional model (or as many as you wish) that incorporates your own creativity (adding-in new variables, taking-out variables, adding-in polynomial adjustments, etc.).

names(assig3)
linear_4 <- lm(formula = wfh ~ children + resident_type, data=train)
summary(linear_4)

test$prediction_4 <- predict(linear_4
                              ,data = train
                              ,newdata = test)

rmse_4 = sqrt(
  mean(
    (test$wfh - test$prediction_4)^2
  )
)
rmse_4

# What was your thinking process going into this model?
## My hypothesis was that wfh is positively associated with having children and living in the suburbs. 
## R^2 is -0.00132, p-values is 0.8 which means there's a weak association between variables. 
## I am not sure I ended up with the wrong conclusion just because I used 2 Boolean variables? 

# Is it an improvement over the Part 6 model? Why or why not?
## The only improvement is a smaller RMSE, but this is not relevant considering the R^2 and p-value.