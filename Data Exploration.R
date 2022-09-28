# Data exploration homework for the People Analytics Certificate:
## mean, median, max, min, standard deviation, variance, skew and kurtosis
## subsets
## histograms 
## scatterplots 
## trend line
## Pearson and Spearman correlations

# Part1 - import your attrition.csv dataset into your R environment.

library("readxl")

data_folderpath <- "C:/Users/monic/OneDrive/Documents/People Analytics/R Course/Assig2"
attrition <- paste0(data_folderpath,
                    "/",
                    "attrition.csv")


attrition <- data.frame(read.csv(attrition))
dim(attrition)

colnames(attrition)[1] <- "Age" 
colnames(attrition)

# Part2 - Create a new data subset that contains all of the rows but only includes columns: "DailyRate",
#"PerformanceRating", "TotalWorkingYears".

subset_attrition <- subset(attrition,
                       select = c(DailyRate, PerformanceRating, TotalWorkingYears))
subset_attrition

# Part3 - calculate the mean, median, max, min, standard
# deviation, variance, skew and kurtosis of your subset.

summary(subset_attrition)
install.packages("pastecs")
library("pastecs")
pastecs_stats <- data.frame(
  round(
    pastecs::stat.desc(subset_attrition),2
  )
)
# The Mean and the Median are not very far apart, but the Mean is a bit higher than the Median -> Right skew distribution

# Skew and kurtosis

install.packages("moments")
library("moments")
skewness(subset_attrition)
# There's a negative skew for DailyRate that doesn't confirm a right skew distribution for that variable.
# PerformanceRating and TotalWorkingYeats have a positive skew that confirms a right skew distribution
kurtosis(subset_attrition)
# Kurtosis for DailyRate is playkurtic and leptokurtic for PerformanceRating and TotalWorkingYears


# Another way of doing it

install.packages("psych")
library("psych")
psych::describe(subset_attrition)

# Part4 - plot a histogram of DailyRate using the default breaks; 
# re-run the plot with breaks = 30. And, re-run, again, but with breaks = 60.

# I kept getting this error: Error in plot.new() : figure margins too large - and fixed it with something I found on google 
par("mar")
par(mar=c(1,1,1,1))

library("dplyr")
hist(subset_attrition$DailyRate)
hist(subset_attrition$DailyRate, breaks = 30)
hist(subset_attrition$DailyRate, breaks = 60)

# Are you learning anything new about the distribution when you increase the breaks? Or was the default sufficient?
# When using more breaks, it is more clear where are more people situated in terms of daily rate. From the first histogram the difference in daily rate isn't even visible enough.   

# Part5
# Make a scatter plot of TotalWorkingYears for the x-axis and DailyRate for the y-axis with the following additional specifications:
#  i. An appropriate plot title and axis titles
# ii. Navy colored dots

plot(x = subset_attrition$TotalWorkingYears, y = subset_attrition$DailyRate,
     main = "Daily Rate and Years Worked relation",
     xlab = "Total Years Worked",
     ylab = "Daily Rate",
     col = "navy",
     pch = 18)

abline(lm(subset_attrition$DailyRate ~ subset_attrition$TotalWorkingYears),
       col = "red",
       lty = 2)


# Part6
# Calculate both the Pearson and Spearman correlations for "TotalWorkingYears" vs "DailyRate" and calculate how much larger the Spearman correlation is to the Pearson correlation.

  cor(subset_attrition$TotalWorkingYears, subset_attrition$DailyRate, method =c("pearson"))
  
  cor(subset_attrition$TotalWorkingYears, subset_attrition$DailyRate, method =c("spearman"))

# Is it likely that the relationship between the two is linear or non-linear?
# By only looking at the Pearson and Spearman coefficients, the relationship between Total Working Years and Saily rate, doesn't seem to be statistically significant.


  
  
