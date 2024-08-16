library(ggplot2)
library(dplyr)
library(Metrics)
library(ramify)
library(cowplot)
library(gridExtra)

df <- read.csv("/raid/cuden/data/era5_vaisalaLightning_montlySummaries_NEclip.csv")
head(df)

y <- df$mean_strike_rate
temp <- df$t2m_monthly_mean

log_y <- log(y)
log_temp <- log(temp)

plot(temp, y)
plot(log_temp, log_y)

hist(y)

ggplot(data=df, aes(x=mean_strike_rate, y=after_stat(density))) +
  geom_density(color="#6DBCC3", linetype = "dashed") +
  geom_histogram(aes(y = ..density..), color = "white", fill = alpha("#6DBCC3", .2)) + 
  theme_minimal() +
  guides(y="none", y.sec=guide_axis(title = expression(Lightning~flash~rate~(number~per~km^2~per~month)))) +
  scale_x_continuous(breaks=seq(0, 2, by=1)) +
  xlab("")

ggplot(data=df, aes(x=t2m_monthly_mean, y=after_stat(density))) +
  geom_density(color="#6DBCC3", linetype = "dashed") +
  geom_histogram(aes(y = ..density..), color = "white", fill = alpha("#6DBCC3", .2)) + 
  theme_minimal() +
  guides(y="none", y.sec=guide_axis(title = expression(Lightning~flash~rate~(number~per~km^2~per~month)))) +
  scale_x_continuous(breaks=seq(0, 2, by=1)) +
  xlab("")
