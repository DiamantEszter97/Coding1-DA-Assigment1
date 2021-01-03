rm(list=ls())

library(tidyverse)
library(scales)
library(lspline)
library(estimatr)
library(texreg)
library(ggthemes)
library(moments)
library(readxl)
library(rgl)
library(xtable)
library(huxtable)

# aim of code:
#     find regression between the price of a car and its usage measured by its odometer reading
#      and age of the car, we try to  find the causality behind it

# Note: the original data table from Kaggle (https://www.kaggle.com/austinreese/craigslist-carstrucks-data) 
# was reduced due to size restriction on github.


# load dataset from github account
df <- read.csv("https://raw.githubusercontent.com/DiamantEszter97/Coding1-DA2-Assigments/master/Used_cars/data/clean/cars.csv")

# because the loading of the data set is long, the table is saved into another table:
cars_df <- df

# check some visuals:
cars_df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()

ggplot(df, aes(x = odometer, y = price)) +
  geom_point()

######################
# create summary statistics

# calculate car ages:
cars_df <- cars_df %>% mutate(age = (2020-year))

# drop missing values of age:
cars_df <- cars_df %>% drop_na(age)

# drop minus values for age:
cars_df <- cars_df %>%  filter(age >= 0)


######################
# create summary statistics

# summary statistics of price
pr <- cars_df %>% summarise(min = min(price),
                            max = max(price),
                            mean = mean(price),
                            median = median(price),
                            sd = sd(price),
                            skew = skewness(price),
                            number_obs = nrow(cars_df))

# summary statistics of odometer:
od <- cars_df %>% summarise(min = min(odometer),
                            max = max(odometer),
                            mean = mean(odometer),
                            median = median(odometer),
                            sd = sd(odometer),
                            skew = skewness(odometer),
                            number_obs = nrow(cars_df))

# summary statistics of year:
ag <- cars_df %>% summarise(min = min(age),
                            max = max(age),
                            mean = mean(age),
                            median = median(age),
                            sd = sd(age),
                            skew = skewness(age),
                            number_obs = nrow(cars_df))
# create full summary table:
cars_summary <- rbind(od, pr, ag)

# naming the rows:
rownames(cars_summary) <- c("Odometer", "Price", "Age")

# remove table pr and od:
remove(od, pr, ag)

xtb <- xtable(cars_summary, type = latex, caption = "Summary statistics for used cars based on their odometer readings and year of creation")
print(xtb, comment = FALSE, include.rownames= TRUE)


####################
# check for extreme values for lower values:
extr_low_pr_df <- cars_df %>% filter(price <= 500)
extr_low_od_df <- cars_df %>% filter(odometer <= 70000)

# check for extremes values on higher values:
extr_high_pr_df <- cars_df %>% filter(price >= 60000)
extr_high_od_df <- cars_df %>% filter(odometer >= 1000000)


# conclusion: there are cases when prices and odometer measures equal to 1
#             there are cases where prices are lower than 500 and their odometer 
#               not high to explain, also they are no more than 10 years old
#             there are cases where the usage is not more than 70000. 
#               They are considered as new cars
#             there are cases where the prices are too high, even exceeds the market price of the car
#             there are cases where the odometer is too high with high prices

cars_df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()

# drop observations where price is lower than 500 and odometer reading is lower than 400 and which are made in:
cars_df <- cars_df %>% filter(price >= 500, price <= 60000, odometer >= 70000, odometer <= 1000000, year < 2020)

# drop where age is zero:
cars_df <- cars_df %>% filter(age > 0)

# delete
# remove tables ext_low_pr_df, ext_low_od_df, extr_high_pr_df, and extr_high_od_df
rm(extr_low_pr_df, extr_low_od_df, extr_high_pr_df, extr_high_od_df)


######################
# check for basic scatterplots for identify the best model fit for price and odometer reading:
# price = ß + L*odometer

# level - level:
ggplot(cars_df, aes(x = odometer, y = price)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(breaks = c(1, 70000, 100000, 300000, 500000, 800000, 1000000)) +
  labs(x = "Odometer Reading", y = " Price of Car") +
  ggtitle("Level Price - Level Odometer") 

# level - log:
ggplot(cars_df, aes(x = odometer, y = price)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(trans = log_trans(), breaks = c(1, 70000, 100000, 300000, 500000, 800000, 1000000))+
  labs(x = "Odometer Reading", y = " Price of Car")+
  ggtitle("Level Price - Log Odometer") 

# log - level:
ggplot(cars_df, aes(x = odometer, y = price)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(trans = log_trans()) +
  scale_x_continuous(breaks = c(1, 70000, 100000, 300000, 500000, 800000, 1000000))+
  labs(x = "Odometer Reading", y = " Price of Car")+
  ggtitle("Log Price - Level Odometer") 

# log -log:
ggplot(cars_df, aes(x = odometer, y = price)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(trans = log_trans(), breaks = c(1, 70000, 100000, 300000, 500000, 800000, 1000000)) +
  scale_y_continuous(trans = log_trans())+
  labs(x = "Odometer Reading", y = " Price of Car")+
  ggtitle("Log Price - Log Odometer") 


# conclusion:
#   none of the models fit in a line but the log-level forms the most linear one with linear splines


# model check for age and price:
# # price = ß + L*age

# level - level:
ggplot(cars_df, aes(x = age, y = price)) +
  geom_point() +
  geom_smooth()+
  labs(x = "Age of car", y = " Price of Car")+
  ggtitle("Level Price - Level Age") 

# log - level:
ggplot(cars_df, aes(x = age, y = price)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(trans = log_trans())+
  labs(x = "Age of car", y = " Price of Car")+
  ggtitle("Log Price - Level Age")


# level - log:
ggplot(cars_df, aes(x = age, y = price)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(trans = log_trans())+
  labs(x = "Age of car", y = " Price of Car")+
  ggtitle("Level Price - Log Age")

# log -log:
ggplot(cars_df, aes(x = age, y = price)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(trans = log_trans()) +
  scale_y_continuous(trans = log_trans())+
  labs(x = "Age of car", y = " Price of Car") +
  ggtitle("Log Price - Log Age")

# conclusion:
#   none of the models fit in a line but the log-level forms
#   the most linear one with linear splines for models (25 for age, 250000 for odometer)

# create models
# price - odometer:
#     reg1: log_pr = L + ß * odometer
#     reg2: log_pr = L + ß1 * odometer * 1(odometer < 250000) + ß2 * odometer * 1(odometer >= 250000)
# price - age:
#     reg3: log_pr = L + ß * age
#     reg4: log_pr = L + ß1 * age * 1(age < 25) + ß2 * age * 1(age >= 25)
# price - odometer - age:
#     reg5: log-pr = L + ß1 * odometer + ß2 * age
# 3) extra: weighted-ols:
#     reg6: log_pr = L + ß1 * odometer  (weight: age)
# 4) quadratics:
#     reg7: log_pr = L + ß1 * odometer + ß2 * odometer^2
#     reg8: log_pr = L + ß1 * age + ß2 * age^2


# create log transformed price:
cars_df <- cars_df %>% mutate(log_pr = log(price),
                              od_sq = odometer^2,
                              age_sq = age^2)

# create regressions:
# price - odometer
# reg1:
reg1 <- lm_robust(log_pr ~ odometer, data = cars_df, se_type = "HC2")
reg1

# summary statistics and visualization:
summary(reg1)
ggplot(cars_df, aes(x = odometer, y = log_pr)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, color = "purple") +
  labs(x = "Odoemeter reading in miles",  y = "Log price of car")+
  ggtitle("Log Price - Age: Linear")


# reg2:
cutoff <- 250000

# create regression: 
reg2 <- lm_robust(log_pr ~ lspline( odometer , cutoff ), data = cars_df )

# summary statistics and visualization:
summary( reg2 )
ggplot( data = cars_df, aes( x = odometer, y = log_pr ) ) + 
  geom_point( color="black") +
  geom_smooth( formula = y ~ lspline(x,cutoff) , method = lm , color = "purple" ) +
  labs(x = "Odoemeter reading in miles",  y = "Log price of car") +
  ggtitle("Log Price - Age: Piecewise Spline Linear")


# price - age:
# reg3:
reg3 <- lm_robust(log_pr ~ age, data = cars_df, se_type = "HC2")
reg3

# summary statistics and visualization:
summary(reg3)
ggplot(cars_df, aes(x = age, y = log_pr)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, color = "purple") +
  labs(x = "Age of car",  y = "Log price of car") +
  ggtitle("Log Price - Odometer: Linear")

# reg4:
cutoff2 <- 25

# create regression: 
reg4 <- lm_robust(log_pr ~ lspline( age , cutoff2 ), data = cars_df )

# summary statistics and visualization:
summary( reg4 )
ggplot( data = cars_df, aes( x = age, y = log_pr ) ) + 
  geom_point( color="black") +
  geom_smooth( formula = y ~ lspline(x,cutoff) , method = lm , color = "purple" ) +
  labs(x = "Age of car",  y = "Log price of car") +
  ggtitle("Log Price - Odometer: Spiecewise Spline Linear")


# price - odometer - age:
# reg5:
reg5 <- lm_robust(log_pr ~ odometer + age, data = cars_df, se_type = "HC2")
reg5

# summary statistics and visualization:
x <- cbind(cars_df$age)
y <- cbind(cars_df$odometer)
z <- cbind(cars_df$price)
plot3d(x, z, y, pch = 18, cex = 2, 
       theta = 20, phi = 20, ticktype = "detailed",
       xlab = "Age of car", ylab = "Odometer reading", zlab = "Price of car") +
  ggtitle("Log Price - Odometer - Age: Mutiple")

# Weighted:
# reg6:
reg6 <- lm_robust(log_pr ~ odometer, data = cars_df, weight = age)
# summary statistics and visualization:
summary(reg6)
ggplot(cars_df, aes(x = odometer, y = log_pr)) +
  geom_point(data = cars_df, aes(size = age), color = "black", shape = 16, alpha = 0.6, show.legend = F) +
  geom_smooth(aes(weight = age), method = lm, color = "purple") +
  ggtitle("Log Price - Odometer: Weighted on Age")

# Quadratics:
# reg7:
reg7 <- lm_robust(log_pr ~ odometer + od_sq, data = cars_df, se_type = "HC2")
reg7

# summary statistics and visualization:
summary(reg7)
ggplot(cars_df, aes(x = od_sq, y = log_pr)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, color = "purple") +
  labs(x = "Age of car",  y = "Log price of car")+
  ggtitle("Log Price - Odometer: Quadratic")


# reg8:
reg8 <- lm_robust(log_pr ~ age + age_sq, data = cars_df, se_type = "HC2")
reg8

# summary statistics and visualization:
summary(reg8)
ggplot(cars_df, aes(x = age_sq, y = log_pr)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, color = "purple") +
  labs(x = "Age of car",  y = "Log price of car")+
  ggtitle("Log Price - Age: Quadratic")


# save it in html:
# creating the model summary
out_path <- "C:/Users/diama/Documents/CEU-BA-Assignments/Coding1_DataAnalysis2/Used_cars/out/"
htmlreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
        type = "html",
        custom.model.names = c("Log Price/odometer - linear", "Log Price/odometer - split",
                               "Log Price/age - linear", "Log Price/age - split",
                               "Log Price/odometer/age - linear", "Log Price/odometer - Weighter linear",
                               "Log Price/odometer - quadratic", "Log Price/age - quadratic"),
        caption = "Modelling prices changes in used cars based on odometer reading and age",
        file = paste0(out_path, 'model_comparison_usedcars.html'), include.ci = F)




######################################################
# Robustness check:
# make a sampole of 1/5 of full cars_df
sample_df <- sample_n(cars_df, 44694)

# testing:
reg1 <- lm_robust(log_pr ~ odometer, data = sample_df, se_type = "HC2")
reg1

# summary statistics and visualization:
summary(reg1)
ggplot(sample_df, aes(x = odometer, y = log_pr)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, color = "purple") +
  labs(x = "Odoemeter reading in miles",  y = "Log price of car")


# reg2:
cutoff <- 250000

# create regression: 
reg2 <- lm_robust(log_pr ~ lspline( odometer , cutoff ), data = sample_df )

# summary statistics and visualization:
summary( reg2 )
ggplot( data = sample_df, aes( x = odometer, y = log_pr ) ) + 
  geom_point( color="black") +
  geom_smooth( formula = y ~ lspline(x,cutoff) , method = lm , color = "purple" ) +
  labs(x = "Odoemeter reading in miles",  y = "Log price of car")


# price - age:
# reg3:
reg3 <- lm_robust(log_pr ~ age, data = sample_df, se_type = "HC2")
reg3

# summary statistics and visualization:
summary(reg3)
ggplot(sample_df, aes(x = age, y = log_pr)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, color = "purple") +
  labs(x = "Age of car",  y = "Log price of car")

# reg4:
cutoff2 <- 25

# create regression: 
reg4 <- lm_robust(log_pr ~ lspline( age , cutoff2 ), data = sample_df )

# summary statistics and visualization:
summary( reg4 )
ggplot( data = sample_df, aes( x = age, y = log_pr ) ) + 
  geom_point( color="black") +
  geom_smooth( formula = y ~ lspline(x,cutoff) , method = lm , color = "purple" ) +
  labs(x = "Age of car",  y = "Log price of car")


# price - odometer - age:
# reg5:
reg5 <- lm_robust(log_pr ~ odometer + age, data = sample_df, se_type = "HC2")
reg5

# summary statistics and visualization:
x <- cbind(sample_df$age)
y <- cbind(sample_df$odometer)
z <- cbind(sample_df$price)
plot3d(x, z, y, pch = 18, cex = 2, 
       theta = 20, phi = 20, ticktype = "detailed",
       xlab = "Age of car", ylab = "Odometer reading", zlab = "Price of car")

# Weighted:
# reg6:
reg6 <- lm_robust(log_pr ~ odometer, data = sample_df, weight = age)
# summary statistics and visualization:
summary(reg6)
ggplot(sample_df, aes(x = odometer, y = log_pr)) +
  geom_point(data = sample_df, aes(size = age), color = "black", shape = 16, alpha = 0.6, show.legend = F) +
  geom_smooth(aes(weight = age), method = lm, color = "purple")

# Quadratics:
# reg7:
reg7 <- lm_robust(log_pr ~ odometer + od_sq, data = sample_df, se_type = "HC2")
reg7

# summary statistics and visualization:
summary(reg7)
ggplot(sample_df, aes(x = od_sq, y = log_pr)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, color = "purple") +
  labs(x = "Age of car",  y = "Log price of car")


# reg8:
reg8 <- lm_robust(log_pr ~ age + age_sq, data = sample_df, se_type = "HC2")
reg8

# summary statistics and visualization:
summary(reg8)
ggplot(sample_df, aes(x = age_sq, y = log_pr)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, color = "purple") +
  labs(x = "Age of car",  y = "Log price of car")


# save it in html:
# creating the model summary
out_path <- "C:/Users/diama/Documents/CEU-BA-Assignments/Coding1_DataAnalysis2/Used_cars/out/"
htmlreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
        type = "html",
        custom.model.names = c("Log Price/odometer - linear", "Log Price/odometer - split",
                               "Log Price/age - linear", "Log Price/age - split",
                               "Log Price/odometer/age - linear", "Log Price/odometer - Weighter linear",
                               "Log Price/odometer - quadratic", "Log Price/age - quadratic"),
        caption = "Modelling prices changes in used cars based on odometer reading and age",
        file = paste0(out_path, 'model_comparison_usedcars_test.html'), include.ci = F)

# Hypothesis test:
# create the tables:
ford <- cars_df %>% filter(manufacturer == "ford")
nissan <- cars_df %>% filter(manufacturer == "nissan")


# do hypothesis test:
test <- t.test(ford$price, nissan$price, var.equal = F)

# create hypothesis table:
table <- data.frame(test = "Budapest Prices",
                            t = test$statistic[[1]],
                            p_value = "< 0.01",
                            CI_95_lower = test$conf.int[1],
                            CI_95_upper = test$conf.int[2])
# make it work in rmd
xtb3 <- xtable(test,type = "latex", caption = "Hypothesis test that different car models have different prices even if there are used")

