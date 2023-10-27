### Main Code for Data Task
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(prismatic)
library(rcartocolor)

## import Data

library(readr)
data <- read_csv("Desktop/Task-Zwick/R-Codes/data.csv", 
                 col_types = cols(education = col_character()))
View(data)

data_b <- dplyr::filter(data, race=="black")
data_w <- dplyr::filter(data, race=="white")
data_bw <- dplyr::filter(data, race=="white"|race=="black")
data$wealth <- data$asset_total - data$debt_total
data_b$wealth <- data_b$asset_total - data_b$debt_total
data_w$wealth <- data_w$asset_total - data_w$debt_total

pfw <- ggplot(data_w, aes(x = year, y = wealth)) 
pfb <- ggplot(data_b, aes(x = year, y = wealth)) 
ggarrange(pfw,pfb)


# ggerrorplot(data, x = "year", y = "wealth", color="education", desc_stat = "median") 

# ggerrorplot(data, x = "year", y = "wealth", desc_stat = "mean")

# ggerrorplot(data_no_outlier, x = "year", y = "wealth", color="education", desc_stat = "median", add = "violin") 

theme_set(theme_bw())

### education

ggplot(data, aes(x=year, y=wealth, color = education, group = education)) + stat_summary(fun=median, geom="line", lwd = 1, linetype ='longdash') + labs(x = "Year", y = "Wealth") + scale_y_continuous(label = function(x) {return(paste(x/1000, "TD"))}) + scale_color_viridis_d()





### race

ggplot(data, aes(x=year, y=wealth, color = race, group = race)) + stat_summary(fun=median, geom="line", lwd = 1, linetype ='longdash') + labs(x = "Year", y = "Wealth") + scale_y_continuous(label = function(x) {return(paste(x/1000, "TD"))}) + scale_color_viridis_d()





### race bw & housing
data_bw$wealth_h <- data_bw$asset_housing - data_bw$debt_housing
ggplot(data_bw, aes(x=year, y=wealth_h, color = race, group = race)) + stat_summary(fun=median, geom="line", lwd = 1, linetype ='longdash') + labs(x = "Year", y = "Housing Wealth") + scale_y_continuous(label = function(x) {return(paste(x/1000, "TD"))}) + scale_color_viridis_d()

ggplot(data_bw, aes(x=year, y=wealth_h, color = education, group = education)) + stat_summary(fun=median, geom="line", lwd = 1, linetype ='longdash') + labs(x = "Year", y = "Housing Wealth") + scale_y_continuous(label = function(x) {return(paste(x/1000, "TD"))}) + scale_color_viridis_d()




#### house owner age>25

data_hog <- dplyr::filter(data_bw, asset_housing!=0 & age>=25)

data_hog$wealth_h <- data_hog$asset_housing - data_hog$debt_housing
data_hog$wealth_nh <- data_hog$asset_total - data_hog$debt_total - (data_hog$asset_housing - data_hog$debt_housing)

ggplot(data_hog, aes(x=year, y=wealth_h, color = race, group = race)) + stat_summary(fun=median, geom="line", lwd = 1, linetype ='longdash') + labs(x = "Year", y = "Housing Wealth") + scale_y_continuous(label = function(x) {return(paste(x/1000, "TD"))}) + scale_color_viridis_d()
ggplot(data_hog, aes(x=year, y=wealth_nh, color = race, group = race)) + stat_summary(fun=median, geom="line", lwd = 1, linetype ='longdash') + labs(x = "Year", y = "Non Housing Wealth") + scale_y_continuous(label = function(x) {return(paste(x/1000, "TD"))}) + scale_color_viridis_d()


data_prop <- data_hog %>% group_by(year) %>% summarize(datap = median())
data_prop[data_prop$year %in% c(2007, 2010, 2013, 2016)] <- data_prop[data_prop$year %in% c(2007, 2010, 2013, 2016)]







