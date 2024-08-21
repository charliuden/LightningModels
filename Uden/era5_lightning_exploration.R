library(ggplot2)
library(dplyr)
library(Metrics)
library(ramify)
library(cowplot)
library(gridExtra)

df <- read.csv("/raid/cuden/data/era5_vaisalaLightning_monthlySummaries_2005-2010_NEclip.csv")
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

ggplot() + geom_point(data=df, aes(x=t2m_monthly_mean, y=mean_strike_rate, alpha=0.1)) + 
  xlab(expression(2~meter~temperatuer~(C))) +
  ylab(expression(Lightning~flash~rate~(number~per~km^2~per~month))) + 
  ggtitle("May-August 2005-2010 n=3246") +
  guides(alpha="none")

x <- df$t2m_monthly_mean
y <- df$mean_strike_rate

model_1 <- lm(y ~ x)
summary(model_1)

a <- summary(model_1)$coefficients[1,1]
b <- summary(model_1)$coefficients[2,1]

y_pred <- b*x+a

plot(x, y_pred)

d <- cbind(x, y, y_pred)

ggplot() + geom_point(d, aes(x, y)) + geom_line(aes(y=y_pred))


