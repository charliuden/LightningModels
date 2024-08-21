#August 14 2024
#Parameterizing Chen 2021. CAPExPrecip models for the Northeastern United States

#Applying models from Chen 2021 to the Northeast. This script parameterizes four models from this paper:

#-- Power law: power law regression: FR = a(CAPE × Precip)^b, with the parameters a and b derived from the 
#optimization at the log-log scale (that is, a linear regression between logarithmic values of flash 
#rate and CAPE × Precip were performed in practice). Similar to Romps et al.36, this model assumed that 
#the lightning flash rate depends on the product of CAPE and Precip. The coefficient b allows nonlinear 
#impacts to be represented in this form. 
#-- Power law (linear opt): is a variation of the power law model, but with the least squares optimization 
#obtained without a logarithmic transformation. 
#-- scale: the original scaling approach proposed by Romps et al.36: FR = a(CAPE × Precip). 
#-- linear: we slightly modified the scaling approach by allowing a negative intercept: FR = a(CAPE × Precip) + b 
#-that is, positive flash rate was simulated only when CAPE × Precip was greater than a threshold. 
#-- a non-parametric: a non-parametric regression model that includes a lookup table of flash rate as 
#a function of CAPE × Precip within the range of contemporary observations. The modelled flash 
#rate value in each bin was derived using the arithmetic means of the observed flash rate in all 
#grid cells associated with the corresponding ranges of CAPE × Precip. A linear extrapolation was used to 
#extend the model to cover CAPE × Precip values outside of the observational range.

library(ggplot2)
library(dplyr)
library(Metrics)
library(ramify)
library(cowplot)
library(gridExtra)

df <- read.csv("/raid/cuden/data/era5_vaisalaLightning_monthlySummaries_2005-2010_NEclip.csv")[,2:16]
df <- df[,c("lon", "lat", "year", "mean_strike_rate", "cape_monthly_mean", "mtpr_monthly_mean", "cxp_monthly_mean")]

x <- df$cxp_monthly_mean
y <- df$mean_strike_rate

plot(x, y)

#-- Power Law: FR = a(CAPE × Precip)^b -- 

# Transform data
log_x <- log(x)
log_y <- log(y)

plot(log_x, log_y)

#fit the model
pl <- nls(log_y ~ a*log_x^b, 
          start = list(a = -1, 
                       b = -1))
summary(pl)

curve.nlslrc = nlsLM(photolrc ~ Am*(1-((1-(Rd/Am))^(1-(PARlrc/LCP)))),
                     start=list(Am=(max(photolrc)-min(photolrc)),
                                Rd=-min(photolrc),
                                LCP= (max(photolrc)-1)),
                     data = curvelrc)

#parameter values
a_pl <- summary(pl)$coefficients[1,1]
b_pl <- summary(pl)$coefficients[2,1]

#predict flash rate
y_pred_pl <- a_pl*log_x^b_pl

plot(x, y_pred_pl)

#-- Power law (linear opt) --

pl_op <- nls(y ~ a*x^b, start = list(a = 1, b = 1))
summary(pl_op)

a_pl_op <- summary(pl_op)$coefficients[1,1]
b_pl_op <- summary(pl_op)$coefficients[2,1]

y_pred_pl_op <- a_pl_op*x^b_pl_op

plot(x, y_pred_pl_op)

#lm(y ~ a*x^b)
#lm(log(y) ~ x)

#log(y) = 150.4*x -1.9

#-- scale: FR = a(CAPE × Precip) --

sc <- lm(y ~ x)
summary(sc)

a_sc <- summary(sc)$coefficients[2,1]
b_sc <- summary(sc)$coefficients[1,1]

y_pred_sc <- a_sc*x

plot(x, y_pred_sc)

#-- linear: FR = a(CAPE × Precip) + b -- 

li <- lm(y ~ x)
summary(li)

a_li <- summary(li)$coefficients[2,1]
b_li <- summary(li)$coefficients[1,1]

y_pred_li <- clip(a_li*x+b_li, 0, NA)

plot(x, y_pred_li)


#-- Non-parametric --
#modelled flash rate value in each bin was derived using the arithmetic means of the observed flash rate in all 
#grid cells associated with the corresponding ranges of CAPE × Precip.
#based on the script Chen shared, there are 50 bins. 
hist(df$cxp)
max <- max(df$cxp)
min <- min(df$cxp)
nbins <- 50
range <- max-min
bin_size = range / nbins

cxp_bin_min <- seq(min, max, by=bin_size)

strike_bin_means <- rep(0, length(bins))

for (i in seq(1: length(cxp_bin_min))){
  bin <- cxp_bin_min[i]
  
  cxp <- df %>% 
    filter(between(cxp, bin, bin+bin_size))
  
  mean <- mean(cxp$mean_strike_rate)
  strike_bin_means[i] <- mean
}

np_table <- as.data.frame(cbind(cxp_bin_min, strike_bin_means))
np_table <- mutate(np_table, cxp_bin_max = cxp_bin_min + bin_size)
head(np_table)

#apply the model
y_pred_np <- rep(0, length(x))

for (i in seq(1:length(x))){
  cxp = x[i]
  
  bin <- np_table %>% filter(cxp_bin_min <= cxp, 
                      cxp_bin_max > cxp)
  
  y_pred_i <- bin$strike_bin_means
  y_pred_np[i] <- y_pred_i
}

plot(x, y_pred_np)


#Root Mean Squared Error - lower values are better
rmse_pl_op <- rmse(y, y_pred_pl_op) #best score
rmse_sc <- rmse(y, y_pred_sc)
rmse_li <- rmse(y, y_pred_li)
rmse_np <- rmse(y, y_pred_np)

cbind(rmse_pl_op, rmse_sc, rmse_li, rmse_np)

#plot results:

df <- cbind(df, y_pred_pl_op, y_pred_sc, y_pred_li, y_pred_np)
df <- mutate(df, mean = (y_pred_pl_op+y_pred_sc+y_pred_li+y_pred_np)/4)
head(df)

p1 <- ggplot(data = df, mapping = aes(x = cxp_monthly_mean, y = mean_strike_rate)) +
  geom_pointdensity(adjust = .001, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(CAPE~x~Precip~(W~m^-2))) +
  ylab(expression(Lightning~flash~rate~(number~per~km^2~per~month))) + 
  ggtitle("May-August 2005-2010 n=3246")+
  guides(alpha="none", color="none") + 
  geom_line(data=df, aes(y=y_pred_pl_op), color='orange', linetype = "dashed",linewidth=0.7) + 
  geom_line(data=df, aes(y=y_pred_sc), color='yellowgreen', linetype = "dashed",linewidth=0.7) + 
  geom_line(data=df, aes(y=y_pred_li), color='lightblue', linetype = "dashed",linewidth=0.7) + 
  geom_line(data=df, aes(y=y_pred_np), color='darkblue', linetype = "dashed",linewidth=0.7) + 
  geom_line(data=df, aes(y=mean), color='black',size=0.7) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

legend_colors <- c("Power law (linear opt.)" = "orange", "Scale" = "yellowgreen", "Linear" = "lightblue", "Non-parametric" ="darkblue", "Mean"= "black")

legend_plot <- ggplot(df, aes(x = cxp_monthly_mean)) +
  geom_line(aes(y = y_pred_pl_op, color = 'Power law (linear opt.)'), linewidth = 0.7, linetype = "dashed") +
  geom_line(aes(y = y_pred_sc, color = 'Scale'), linewidth = 0.7, linetype = "dashed") +
  geom_line(aes(y = y_pred_li, color = 'Linear'), linewidth = 0.7, linetype = "dashed") +
  geom_line(aes(y = y_pred_np, color = 'Non-parametric'), linewidth = 0.7, linetype = "dashed") +
  geom_line(aes(y = mean, color = 'Mean'), linewidth = 0.7, linetype = "dashed") +
  labs(color = "Models") +
  scale_color_manual(values = legend_colors) +
  theme_minimal() 

cowplot::get_legend(legend_plot)
legend <- get_legend(legend_plot)
grid.draw(legend)  



p2 <- ggplot(data=df, aes(x=mean_strike_rate, y=after_stat(density))) +
  coord_flip() + 
  scale_y_reverse() +
  geom_density(color="#6DBCC3", linetype = "dashed") +
  geom_histogram(aes(y = ..density..), color = "white", fill = alpha("#6DBCC3", .2)) + 
  theme_minimal() +
  guides(y="none", y.sec=guide_axis(title = expression(Lightning~flash~rate~(number~per~km^2~per~month)))) +
  scale_x_continuous(breaks=seq(0, 2, by=1)) +
  xlab("")

p3 <- ggplot(data=df, aes(x=cxp_monthly_mean, y=after_stat(density))) +
  geom_density(color="#6DBCC3", linetype = "dashed") +
  geom_histogram(aes(y = ..density..), color = "white", fill = alpha("#6DBCC3", .2)) + 
  theme_minimal() +
  xlab(expression(CAPE~x~Precip~(W~m^-2))) +
  scale_x_continuous(breaks=seq(0, 0.015, by=0.005)) 

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )

grid.arrange(p1, p2, p3, cowplot::get_legend(legend_plot), 
             ncol=2, nrow=2, widths=c(4, 1), heights=c(4, 1))












