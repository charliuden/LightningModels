
library(ggplot2)
library(dplyr)
library(viridis)
library(ggpointdensity)
library(gridExtra)
library(cowplot)

#script to recreate fig 1 of chen 2021 using era5 and vaisala lightning data for the Northeast, 2005 - 2010
#August 13 2024

df <- read.csv("/raid/cuden/data/era5_vaisalaLightning_montlySummaries_NEclip.csv")
pred <- read.csv("/raid/cuden/data/Chen2021_NEpredictions.csv")[,2:6]
df <- cbind(df, pred)
head(df)

df <- mutate(df, mean=(pl+pl_op+sc+li+li2)/5)

p1 <- ggplot(data = df, mapping = aes(x = cxp, y = mean_strike_rate)) +
  geom_pointdensity(adjust = .001, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(CAPE~x~Precip~(W~m^-2))) +
  ylab(expression(Lightning~flash~rate~(number~per~km^2~per~month))) + 
  ggtitle("May-August 2005-2010 n=3246")+
  guides(alpha="none", color="none") + 
  geom_line(data=df, aes(y=pl), color='purple', linetype = "dashed", size=0.7) + 
  geom_line(data=df, aes(y=pl_op), color='orange', linetype = "dashed",size=0.7) + 
  geom_line(data=df, aes(y=sc), color='yellowgreen', linetype = "dashed",size=0.7) + 
  geom_line(data=df, aes(y=li), color='lightblue', linetype = "dashed",size=0.7) + 
  geom_line(data=df, aes(y=li2), color='darkgreen', linetype = "dashed",size=0.7) + 
  geom_line(data=df, aes(y=mean), color='black',size=0.7) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

legend_colors <- c("Power law" = "purple", "Power law (linear opt.)" = "orange", "Scale" = "yellowgreen", "Linear" = "lightblue", "Linear 2" = "darkgreen", "Mean"= "black")
colors <- c("purple", "orange", "yellowgreen", "lightblue", "darkgreen", "black")
models = c('Power law', 'Power law (linear opt.)', 'Scale', 'Linear', 'Linear 2', 'mean')

legend_plot <- ggplot(df, aes(x = cxp)) +
  geom_line(aes(y = pl, color = 'Power law'), size = 0.7, linetype = "dashed") +
  geom_line(aes(y = pl_op, color = 'Power law (linear opt.)'), size = 0.7, linetype = "dashed") +
  geom_line(aes(y = sc, color = 'Scale'), size = 0.7, linetype = "dashed") +
  geom_line(aes(y = li, color = 'Linear'), size = 0.7, linetype = "dashed") +
  geom_line(aes(y = li2, color = 'Linear 2'), size = 0.7, linetype = "dashed") +
  geom_line(aes(y = mean, color = 'Mean'), size = 0.7, linetype = "dashed") +
  labs(color = "") +
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

p3 <- ggplot(data=df, aes(x=cxp, y=after_stat(density))) +
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

