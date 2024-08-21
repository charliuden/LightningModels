
#Random Forest
#Charlotte Uden
#August 20th 2024

library(randomForest)
library(dplyr)

df <- read.csv("/raid/cuden/data/era5_vaisalaLightning_monthlySummaries_2005-2010_NEclip.csv")
head(df)

#calculate temperature difference
df <- mutate(df, tdif = tmax_monthly_mean - tmin_monthly_mean)
plot(df$cape_monthly_mean, df$tdif)
plot(df$cxp_monthly_mean, df$tdif)
plot(df$tdif, df$mean_strike_rate)
plot(df$tdif, df$mean_strike_rate)
plot(df)

#air temp and short wave radiation correlate - remove one so variable importance measures don't 
#have weird results

df <- df[c("mean_strike_rate", "mtpr_monthly_mean", "t2m_monthly_mean", "i10fg_monthly_mean", "msdwswrf_monthly_mean", "sp_monthly_mean", "rh_monthly_mean", "tdif")]

#check for correlation between variables
#draw random sample of rows to plot 
df_sample <- df %>% sample_frac(0.1)
plot(df_sample)

#apply random forest:
rf <-randomForest(mean_strike_rate~.,data=df, ntree=500) 
print(rf)

#mtry: Number of random variables used in each tree
# - reducing mtry reduces correlation and strength. 
# - the oob error rate tells you what value of mtry is optimal

mtry <- tuneRF(df[-1],df$mean_strike_rate, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
#looking for mtry for the lowest oob error rate 
print(mtry)
print(best.m) 

#apply best mtry to model
rf <-randomForest(mean_strike_rate~.,data=df, mtry=best.m, importance=TRUE,ntree=500)
print(rf)

#Evaluate variable importance
importance(rf)
varImpPlot(rf)



# Mean Decrease Accuracy (%IncMSE): 
# - remove variables from the model and asses which have the greatest impact on model accuracy.
# - Higher the value of mean decrease accuracy or mean decrease gini score , 
#       higher the importance of the variable in the model. 
# - tells you effect of randomly shuffling values of a variable on increasing mse of model predictions
# - a higher number indicates that the variable is more important

# Mean Decrease Gini or Increase in Node Purity (IncNodePurity):
# - expresses the change in the homogeneity of the of the groups created by the trees 
#     (using the Gini coefficient as a measure). 
# - What is expressed is the decrease in said purity if a particular variable 
#      has no information. If a variable has no information to begin with, 
#      the decrase would be zero. (from: https://www.reagro.org/methods/statistical/randomforest.html)
