#September 10th 2024
#Plots for lightning paper

library(ggplot2)
library(dplyr)
library(ggpointdensity)
library(viridis)

#monthly averages
#data <- read.csv("/raid/cuden/data/era5_vaisalaLightning_monthlySummaries_2005-2010_NEclip.csv")[,2:16]

#Figure 1: Raw data over time

#daily data with lat lon and date
df <- read.csv("/raid/cuden/data/era5DailySummaries_vaisalaLightningDailyCounts_2005-2010_NEclip.csv")
df <- df[,2:16]

df$date <- as.Date(df$date)
df$month <- as.numeric(format(df$date,'%m'))
df$year <- as.numeric(format(df$date,'%Y'))
df$day <- as.numeric(format(df$date,'%d'))

str(df)

ggplot(data=df, aes(x=date, y=strikes, alpha=0.1)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab("Year") +
  ylab(expression(Cloud~to~ground~lightning~count)) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  scale_x_date(date_breaks="month", date_labels="%b-%Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

#calculate mean strike rate for each month and day (so, average across years and lat lon)
cols <- c("month", "day")
summary_strikes <- df %>% 
  group_by(across(all_of(cols))) %>% 
  summarize(strikes = mean(strikes), .groups = 'drop')

library(lubridate)
# Convert 'month' and 'day' into 'day_of_year' (1 to 365)
summary_strikes <- summary_strikes %>%
  mutate(
    date = as.Date(paste(2024, month, day, sep = "-"), format = "%Y-%m-%d"),
    day_of_year = yday(date),  # 'yday' gives the day of the year (1 to 365)
    month_name = factor(month.abb[month], levels = month.abb)  # Get month names
  )

# Plot using ggplot
ggplot(summary_strikes, aes(x = day_of_year, y = strikes)) +
  #geom_line() +  # Line plot
  geom_point() +  # Optional: Add points
  scale_x_continuous(
    breaks = cumsum(c(0, days_in_month(1:12)))[-13],  # Approximate month start days
    labels = month.abb  # Label x-axis with month abbreviations
  ) +
  labs(x = "Month", y = "Average daily lightning strike count") +
  theme_minimal()


ggplot(data=df, aes(x=as.factor(month), y=strikes)) +
  geom_boxplot() +
  xlab("Month") +
  ylab(expression(Cloud~to~ground~lightning~count)) + 
  guides(alpha="none", color="none") + 
  theme_minimal() 

#normalize the data
df <- df %>%
  mutate(strikes = (strikes-min(strikes))/(max(strikes)-min(strikes))) %>% #normalized 
  mutate(cape = (cape-min(cape))/(max(cape)-min(cape))) %>%
  mutate(precip = (mtpr-min(mtpr))/(max(mtpr)-min(mtpr))) %>%
  mutate(cxp = (cxp-min(cxp))/(max(cxp)-min(cxp))) %>%
  mutate(tair = (d2m-min(d2m))/(max(d2m)-min(d2m))) %>%
  mutate(wind = (i10fg-min(i10fg))/(max(i10fg)-min(i10fg))) %>%
  mutate(swr = (msdwswrf-min(msdwswrf))/(max(msdwswrf)-min(msdwswrf))) %>%
  mutate(sp = (sp-min(sp))/(max(sp)-min(sp))) %>%
  mutate(rh = (rh-min(rh))/(max(rh)-min(rh))) 

ggplot() + geom_point(data=df, aes(x=date, y=strikes, alpha=0.1)) +
  geom_pointdensity() +
  scale_color_viridis() + 
  xlab("Year") +
  ylab(expression(Normalized~cloud~to~ground~lightning~count)) + 
  guides(alpha="none", color="none") + 
  theme_minimal()


ggplot(data=df, aes(x=strikes, y=after_stat(density))) +
  geom_density(color="#6DBCC3", linetype = "dashed")+ 
  geom_histogram(aes(y = ..density..), color = "white", fill = alpha("#6DBCC3", .2)) 

+
  geom_histogram(aes(y = ..density..), color = "white", fill = alpha("#6DBCC3", .2)) + 
  theme_minimal() 
p2
+
  guides(y="none", y.sec=guide_axis(title = expression(Cloud~to~ground~lightning~count))) +
  xlab("")
p2


#Figure 2: map of the region

#monthly averages, no lat lon or date
data <- read.csv("/raid/cuden/data/era5_vaisalaLightning_monthlySummaries_2005-2010_NEclip.csv")[,2:16]

#Normalize the data
data <- data %>%
  mutate(strikes = (mean_strike_rate-min(mean_strike_rate))/(max(mean_strike_rate)-min(mean_strike_rate))) %>% #normalized 
  mutate(cape = (cape_monthly_mean-min(cape_monthly_mean))/(max(cape_monthly_mean)-min(cape_monthly_mean))) %>%
  mutate(precip = (mtpr_monthly_mean-min(mtpr_monthly_mean))/(max(mtpr_monthly_mean)-min(mtpr_monthly_mean))) %>%
  mutate(cxp = (cxp_monthly_mean-min(cxp_monthly_mean))/(max(cxp_monthly_mean)-min(cxp_monthly_mean))) %>%
  mutate(tair = (d2m_monthly_mean-min(d2m_monthly_mean))/(max(d2m_monthly_mean)-min(d2m_monthly_mean))) %>%
  mutate(wind = (i10fg_monthly_mean-min(i10fg_monthly_mean))/(max(i10fg_monthly_mean)-min(i10fg_monthly_mean))) %>%
  mutate(swr = (msdwswrf_monthly_mean-min(msdwswrf_monthly_mean))/(max(msdwswrf_monthly_mean)-min(msdwswrf_monthly_mean))) %>%
  mutate(sp = (sp_monthly_mean-min(sp_monthly_mean))/(max(sp_monthly_mean)-min(sp_monthly_mean))) %>%
  mutate(rh = (rh_monthly_mean-min(rh_monthly_mean))/(max(rh_monthly_mean)-min(rh_monthly_mean))) 

#calculate mean values across years
cols <- c("lon", "lat")

#Lightning Flash Rate
summary_strike <- data %>% 
  group_by(across(all_of(cols))) %>% 
  summarize(strikes = mean(strikes), .groups = 'drop')

library(terra)
r <- rast(summary_strike[,c("lon", "lat", "strikes")])
plot(r)
crs(r) <- "+init=epsg:4326"

p_lightning <- ggplot() + geom_raster(data=summary_strike, aes(x=lon, y=lat, fill=strikes)) + 
  scale_fill_gradient(low="lightyellow",high="orange") +
  guides(fill=guide_legend(title=expression(Flashes/km^2/month))) + 
  theme_minimal()  + 
  theme(
    legend.text = element_text(size=12),
    legend.title.align = 0,
    legend.position = "bottom", 
    legend.justification = "left",
    legend.direction = "horizontal")

p_lightning

#CAPE x Precipitation
summary_cxp <- data %>% 
  group_by(across(all_of(cols))) %>% 
  summarize(cxp = mean(cxp), .groups = 'drop')

r <- rast(summary_cxp[,c("lon", "lat", "cxp")])
plot(r)
crs(r) <- "+init=epsg:4326"

p_cxp <- ggplot() + geom_raster(data=summary_cxp, aes(x=lon, y=lat, fill=cxp)) + 
  scale_fill_gradient(low="lightyellow",high="orange") +
  guides(fill=guide_legend(title=expression(CAPE~x~Precip~(W~m^-2)))) + 
  theme_minimal()  + 
  theme(
    legend.text = element_text(size=12),
    legend.title.align = 0,
    legend.position = "bottom", 
    legend.justification = "left",
    legend.direction = "horizontal")

p_cxp

#Temperature
summary_temp <- data %>% 
  group_by(across(all_of(cols))) %>% 
  summarize(tair = mean(tair), .groups = 'drop')

p_temp <- ggplot() + geom_raster(data=summary_temp, aes(x=lon, y=lat, fill=tair)) + 
  scale_fill_gradient(low="lightyellow",high="orange") +
  guides(fill=guide_legend(title=expression(Temperature (Celcius)))) + 
  theme_minimal()  + 
  theme(
    legend.text = element_text(size=12),
    legend.title.align = 0,
    legend.position = "bottom", 
    legend.justification = "left",
    legend.direction = "horizontal")
p_temp

#Wind
summary_wind <- data %>% 
  group_by(across(all_of(cols))) %>% 
  summarize(wind = mean(wind), .groups = 'drop')

p_wind <- ggplot() + geom_raster(data=summary_wind, aes(x=lon, y=lat, fill=wind)) + 
  scale_fill_gradient(low="lightyellow",high="orange") +
  guides(fill=guide_legend(title=expression(Wind~(m/s)))) + 
  theme_minimal()  + 
  theme(
    legend.text = element_text(size=12),
    legend.title.align = 0,
    legend.position = "bottom", 
    legend.justification = "left",
    legend.direction = "horizontal")
p_wind

#Short-wave radiation
summary_radiation <- data %>% 
  group_by(across(all_of(cols))) %>% 
  summarize(swr = mean(swr), .groups = 'drop')

p_radiation <- ggplot() + geom_raster(data=summary_radiation, aes(x=lon, y=lat, fill=swr)) + 
  scale_fill_gradient(low="lightyellow",high="orange") +
  guides(fill=guide_legend(title=expression(Short-wave~radiation~(W/m^2)))) + 
  theme_minimal()  + 
  theme(
    legend.text = element_text(size=12),
    legend.title.align = 0,
    legend.position = "bottom", 
    legend.justification = "left",
    legend.direction = "horizontal")
p_radiation

#Surface Pressure
summary_sp <- data %>% 
  group_by(across(all_of(cols))) %>% 
  summarize(sp = mean(sp), .groups = 'drop')

p_sp <- ggplot() + geom_raster(data=summary_sp, aes(x=lon, y=lat, fill=sp)) + 
  scale_fill_gradient(low="lightyellow",high="orange") +
  guides(fill=guide_legend(title=expression(Surface~pressure~(Pa)))) + 
  theme_minimal()  + 
  theme(
    legend.text = element_text(size=12),
    legend.title.align = 0,
    legend.position = "bottom", 
    legend.justification = "left",
    legend.direction = "horizontal")
p_sp

#Relative humidity
summary_rh <- data %>% 
  group_by(across(all_of(cols))) %>% 
  summarize(rh = mean(rh), .groups = 'drop')

p_rh <- ggplot() + geom_raster(data=summary_rh, aes(x=lon, y=lat, fill=rh)) + 
  scale_fill_gradient(low="lightyellow",high="orange") +
  guides(fill=guide_legend(title=expression(Relative~humidity~(percent)))) + 
  theme_minimal()  + 
  theme(
    legend.text = element_text(size=12),
    legend.title.align = 0,
    legend.position = "bottom", 
    legend.justification = "left",
    legend.direction = "horizontal")
p_rh

#Precipitation
summary_precip <- data %>% 
  group_by(across(all_of(cols))) %>% 
  summarize(precip = mean(precip), .groups = 'drop')

p_precip <- ggplot() + geom_raster(data=summary_precip, aes(x=lon, y=lat, fill=precip)) + 
  scale_fill_gradient(low="lightyellow",high="orange") +
  guides(fill=guide_legend(title=expression(Precipitation~(kg/m^2/s)))) + 
  theme_minimal()  + 
  theme(
    legend.text = element_text(size=12),
    legend.title.align = 0,
    legend.position = "bottom", 
    legend.justification = "left",
    legend.direction = "horizontal")
p_precip

#CAPE
summary_cape <- data %>% 
  group_by(across(all_of(cols))) %>% 
  summarize(cape = mean(cape), .groups = 'drop')

p_cape <- ggplot() + geom_raster(data=summary_cape, aes(x=lon, y=lat, fill=cape)) + 
  scale_fill_gradient(low="lightyellow",high="orange") +
  guides(fill=guide_legend(title=expression(CAPE~(J/kg)))) + 
  theme_minimal()  + 
  theme(
    legend.text = element_text(size=12),
    legend.title.align = 0,
    legend.position = "bottom", 
    legend.justification = "left",
    legend.direction = "horizontal")
p_cape

#library(gridExtra)
grid.arrange(p_lightning, p_cape, p_precip, p_temp, p_wind, p_sp, p_rh, p_radiation, 
             ncol=4, nrow=2)

#Figure 3: scatter plots with r squared

#Monthly averages
data <- read.csv("/raid/cuden/data/era5_vaisalaLightning_monthlySummaries_2005-2010_NEclip.csv")[,2:16]

#Normalized 
data <- data %>%
  mutate(strikes = (mean_strike_rate-min(mean_strike_rate))/(max(mean_strike_rate)-min(mean_strike_rate))) %>% #normalized 
  mutate(cape = (cape_monthly_mean-min(cape_monthly_mean))/(max(cape_monthly_mean)-min(cape_monthly_mean))) %>%
  mutate(precip = (mtpr_monthly_mean-min(mtpr_monthly_mean))/(max(mtpr_monthly_mean)-min(mtpr_monthly_mean))) %>%
  mutate(cxp = (cxp_monthly_mean-min(cxp_monthly_mean))/(max(cxp_monthly_mean)-min(cxp_monthly_mean))) %>%
  mutate(tair = (d2m_monthly_mean-min(d2m_monthly_mean))/(max(d2m_monthly_mean)-min(d2m_monthly_mean))) %>%
  mutate(wind = (i10fg_monthly_mean-min(i10fg_monthly_mean))/(max(i10fg_monthly_mean)-min(i10fg_monthly_mean))) %>%
  mutate(swr = (msdwswrf_monthly_mean-min(msdwswrf_monthly_mean))/(max(msdwswrf_monthly_mean)-min(msdwswrf_monthly_mean))) %>%
  mutate(sp = (sp_monthly_mean-min(sp_monthly_mean))/(max(sp_monthly_mean)-min(sp_monthly_mean))) %>%
  mutate(rh = (rh_monthly_mean-min(rh_monthly_mean))/(max(rh_monthly_mean)-min(rh_monthly_mean))) 

df <- data[,c("strikes", "cape", "precip", "cxp", "tair", "wind", "swr", "sp", "rh")]

#function that performs a regression using input variable (var) and lighting strike rate, and 
#returns the r squared value
rsq <- function(data, var){
  #var="cape"
  m <- lm(strikes ~ get(var, data), data)
  pred <- predict(m)
  rsq <- 1 - (sum((data$strikes - pred)^2) / sum((data$strikes - mean(data$strikes))^2))
  return(rsq)
}


rsq(df, "cxp")

p1 <- ggplot(data = df, mapping = aes(x = cxp, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Normalized~CAPE~x~Precip~(W~m^-2))) +
  ylab(expression(Normalized~Lightning~flash~rate~(number~per~km^2~per~month))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 0.85, y = 1, label = expression(R^2==0.18)) 
p1

rsq(df, "cape")

p2 <- ggplot(data = df, mapping = aes(x = cape, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Normalized~CAPE~(J/kg))) +
  ylab(expression(Normalized~Lightning~flash~rate~(number~per~km^2~per~month))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 0.85, y = 1, label = expression(R^2==0.20)) 
p2

rsq(df, "precip")

p3 <- ggplot(data = df, mapping = aes(x = precip, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Normalized~precipitation~(J/kg))) +
  ylab(expression(Normalized~Lightning~flash~rate~(kg/m^2/s))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 0.85, y = 1, label = expression(R^2==0.01)) 
p3

rsq(df, "tair")

p4 <- ggplot(data = df, mapping = aes(x = tair, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Normalized~temperature~(C))) +
  ylab(expression(Normalized~Lightning~flash~rate~(kg/m^2/s))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 0.85, y = 1, label = expression(R^2==0.03)) 
p4

rsq(df, "wind")

p5 <- ggplot(data = df, mapping = aes(x = wind, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Normalized~wind~speed~(m/s))) +
  ylab(expression(Normalized~Lightning~flash~rate~(kg/m^2/s))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 0.85, y = 1, label = expression(R^2==0.07))
p5

rsq(df, "swr")

p6 <- ggplot(data = df, mapping = aes(x = swr, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Normalized~short-wave~radiation~(W/m^2))) +
  ylab(expression(Normalized~Lightning~flash~rate~(kg/m^2/s))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 0.85, y = 1, label = expression(R^2==0.16))
p6

rsq(df, "sp")

p7 <- ggplot(data = df, mapping = aes(x = sp, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Normalized~surface~pressure~(Pa))) +
  ylab(expression(Normalized~Lightning~flash~rate~(kg/m^2/s))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 0.85, y = 1, label = expression(R^2==0.01))
p7

rsq(df, "rh")

p8 <- ggplot(data = df, mapping = aes(x = rh, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Normalized~relative~humidity~(percent))) +
  ylab(expression(Normalized~Lightning~flash~rate~(kg/m^2/s))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 0.85, y = 1, label = expression(R^2==0.11))
p8

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, 
             ncol=4, nrow=2)

#not normalized 

rsq(data, "cxp_monthly_mean")

p1 <- ggplot(data = data, mapping = aes(x = cxp_monthly_mean, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(CAPE~x~Precip~(W~m^-2))) +
  ylab(expression(Lightning~flash~rate~(number~per~km^2~per~month))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x =0.025, y = 1, label = expression(R^2==0.18)) 
p1

rsq(data, "cape_monthly_mean")

p2 <- ggplot(data = data, mapping = aes(x = cape_monthly_mean, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(CAPE~(J/kg))) +
  ylab(expression(Lightning~flash~rate~(number~per~km^2~per~month))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 250, y = 1, label = expression(R^2==0.20)) 
p2

rsq(data, "mtpr_monthly_mean")

p3 <- ggplot(data = data, mapping = aes(x = t2m_monthly_mean, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Precipitation~(J/kg))) +
  ylab(expression(Lightning~flash~rate~(kg/m^2/s))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 20, y = 1, label = expression(R^2==0.01)) 
p3

rsq(data, "t2m_monthly_mean")

p4 <- ggplot(data = data, mapping = aes(x = t2m_monthly_mean, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Temperature~(C))) +
  ylab(expression(Lightning~flash~rate~(kg/m^2/s))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 20, y = 1, label = expression(R^2==0.12)) 
p4

rsq(data, "i10fg_monthly_mean")

p5 <- ggplot(data = data, mapping = aes(x = i10fg_monthly_mean, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Wind~speed~(m/s))) +
  ylab(expression(Lightning~flash~rate~(kg/m^2/s))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 8, y = 1, label = expression(R^2==0.07))
p5

rsq(data, "msdwswrf_monthly_mean")

p6 <- ggplot(data = data, mapping = aes(x = msdwswrf_monthly_mean, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Short-wave~radiation~(W/m^2))) +
  ylab(expression(Lightning~flash~rate~(kg/m^2/s))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 260, y = 1, label = expression(R^2==0.16))
p6

rsq(data, "sp_monthly_mean")

p7 <- ggplot(data = data, mapping = aes(x = sp_monthly_mean, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Surface~pressure~(Pa))) +
  ylab(expression(Lightning~flash~rate~(kg/m^2/s))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 100000, y = 1, label = expression(R^2==0.01))
p7

rsq(data, "rh_monthly_mean")

p8 <- ggplot(data = data, mapping = aes(x = rh_monthly_mean, y = strikes)) +
  geom_pointdensity(adjust = .05, alpha=0.5) +
  scale_color_viridis() + 
  xlab(expression(Relative~humidity~(percent))) +
  ylab(expression(Normalized~Lightning~flash~rate~(kg/m^2/s))) + 
  guides(alpha="none", color="none") + 
  theme_minimal() +
  geom_text(x = 85, y = 1, label = expression(R^2==0.11))
p8

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, 
             ncol=4, nrow=2)

