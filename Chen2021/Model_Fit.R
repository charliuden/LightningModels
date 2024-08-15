#August 14 2024
#Chen 2021. Model Fit
#Models from Chen 2021 have been applied to the Northeast. This script calculates the fit of each model.extract

library(ggplot2)
library(dplyr)
library(Metrics)

df <- read.csv("/raid/cuden/data/era5_vaisalaLightning_montlySummaries_NEclip.csv")
pred <- read.csv("/raid/cuden/data/Chen2021_NEpredictions.csv")[,2:6]

df <- cbind(df, pred)
head(df)

df <- df[,c("lon", "lat", "year", "mean_strike_rate", "cxp", "pl","pl_op","sc","li","li2")]


actual <- df$mean_strike_rate
predicted_pl <- df$pl
predicted_pl_op <- df$pl_op
predicted_sc <- df$sc
predicted_li <- df$li
predicted_li2 <- df$li2

#Root Mean Squared Error - lower values are better
rmse_pl <- rmse(actual, predicted_pl)
rmse_pl_op <- rmse(actual, predicted_pl_op)
rmse_sc <- rmse(actual, predicted_sc)
rmse_li <- rmse(actual, predicted_li)
rmse_li2 <- rmse(actual, predicted_li2)













