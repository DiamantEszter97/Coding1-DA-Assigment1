rm(list=ls())

library(tidyverse)
library(scales)
library(lspline)
library(estimatr)
library(texreg)
library(ggthemes)
library(moments)
library(readxl)


# load the data:
my_path <- "C:/Users/diama/Documents/CEU-BA-Assignments/Coding1_DataAnalysis2/Covid19-Regression/data/"
df <- read.csv(paste0(my_path, "clean/covid_pop_11_02_2020_clean.csv"))
rm(my_path)

# View(df)

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()

ggplot(df, aes(x = confirmed, y = death)) +
  geom_point() +
  ylim(0,10) +
  xlim(0,250)

# Basic descriptive summary::
df_conf_sum <- df %>% summarise(min = min(confirmed),
                           max = max(confirmed),
                           mean = mean(confirmed),
                           median = median(confirmed),
                           sd = sd(confirmed),
                           skew = skewness(confirmed),
                           iq_range = IQR(confirmed))

df_death_sum <- df %>% summarise(min = min(death),
                                 max = max(death),
                                 mean = mean(death),
                                 median = median(death),
                                 sd = sd(death),
                                 skew = skewness(death),
                                 iq_range = IQR(death))

# check for extreme values and drop them: 
extreme_df <- df %>% mutate(country,
                            death_to_conf = death / confirmed)

extreme_df <- extreme_df %>%  filter(death_to_conf < 0.002)
# View(extreme_df)

# Qatar and Singapore are extreme values. Both country has  good health care
# system therefore it might be better to drop them from the analysis 
# because not all of the other countries can report the same

# country_name <- c("Singapore", "Qatar")
# df <- df %>%  filter( !grepl( paste( country_name , collapse="|"), df$country ) )

# remove cases where there is no death because it would result in some problems in log analysis
df <- df %>% filter(death != 0)

rm(extreme_df, country_name)

#################################################
# Check the basic scatter-plots:
#   death = alpha + beta * confirmed


#####
# 1) level - level
ggplot(df, aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "confimred number of COVID-19 infecton", y = "number of death due to COVID-19")

# 2) level - log with scaling
ggplot(df, aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "confimred number of COVID-19 infecton, ln scale", y = "number of death due to COVID-19") +
  scale_x_continuous(trans = log_trans(), breaks = c(1, 100, 200, 500, 1000, 2000, 5000, 10000, 50000, 100000, 500000, 1000000))

# 3) log - level with scaling:
ggplot(df, aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "confimred number of COVID-19 infecton", y = "number of death due to COVID-19, ln scale") +
  scale_y_continuous(trans = log_trans(), breaks = c(1, 100, 200, 500, 1000, 2000, 5000, 10000, 50000, 100000, 500000, 1000000))

# 4) log -log with scales:
ggplot(df, aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "confimred number of COVID-19 infecton, ln scale", y = "number of death due to COVID-19, ln scale") +
  scale_x_continuous(trans = log_trans(), breaks = c(1, 100, 500, 2000, 5000, 20000, 50000, 150000, 750000, 2500000, 9000000)) +
  scale_y_continuous(trans = log_trans(), breaks = c(1, 20, 100, 500, 4000, 20000, 100000, 750000))


# Both level-level and log-log model show linear pattern
# But, the two model will be compared to decide which one works better

############
# take log of number of deaths and number of confirmed cases

df <- df %>% mutate(log_conf = log(confirmed),
                    log_death = log(death))

# Make the two models:
# 1) level-level:
#          reg1: death = alpha + beta * confirmed
#          reg2: death = alpha + beta_1 * confirmed + beta_2 * confirmed^2
#          reg3: death = alpha + beta_1 * confirmed + beta_2 * confirmed^2 + beta_3 * confirmed^3

# 2) log -log:
#          reg4: log_death = alpha + beta * log_conf
#          reg5: log_death = alpha + beta_1 * log_conf + beta_2 * log_conf^2
#          reg6: log_death = alpha + beta_1 * log_conf + beta_2 * log_conf^2 + beta_3 * log_conf^3
#          reg7: log_death = alpha + beta_1 * log_conf * 1(log_conf < 7) + beta_2 * log_conf * 1(log_conf >= 7)

# 3) extra: weighted-ols:
#          reg8: log_death = alpha + beta* log_conf, weights: population


df <- df %>%  mutate(conf_sq = confirmed^2,
                     conf_cb = confirmed^3,
                     log_conf_sq = log_conf^2,
                     log_conf_cb = log_conf^3)

# Built regression:
## level - level
# reg1:
reg1 <- lm_robust(death ~ confirmed, data = df, se_type = "HC2")
reg1
# summary statistics and visualization:
summary(reg1)
ggplot(df, aes(x = confirmed, y = death)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, color = "purple") +
  labs(x = "Number of confimred COVID-19 cases",  y = "Number of death due to COVID-19")

# reg2:
reg2 <- lm_robust(death ~ confirmed + conf_sq, data = df, se_type = "HC2")
reg2
# summary statistics and visualization:
summary(reg2)
ggplot(df, aes(x=conf_sq, y = death)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, color = "purple")

# reg3:
reg3 <- lm_robust(death ~ confirmed + conf_sq + conf_cb, data = df, se_type = "HC2")
reg3
# summary statistics and visualization:
summary(reg3)
ggplot(df, aes( x = conf_cb, y = death)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, color = "purple")



## log - log:
# reg 4:
reg4 <- lm_robust(log_death ~ log_conf, data = df, se_type = "HC2")
reg4
# summary statistics and visualization:
summary(reg4)
ggplot(df, aes(x = log_conf, y = log_death)) +
  geom_point(color = "black") +
  geom_smooth( method = lm, color = "orange")

# reg5
reg5 <- lm_robust(log_death ~ log_conf + log_conf_sq, data = df, se_type = "HC2")
reg5
# summary statistics and visualization:
summary(reg5)
ggplot(df, aes(x = log_conf_sq, y = log_death)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, color = "orange")

# reg6:
reg6 <- lm_robust(log_death ~ log_conf + log_conf_sq + log_conf_cb, data = df, se_type = "HC2")
reg6
# summary statistics and visualization:
summary(reg6)
ggplot(df, aes(x = log_conf_cb, y = log_death)) +
  geom_point(color= "black") +
  geom_smooth(method = lm, color = "orange")

# reg7:
cutoff <- 6
# log transformation -> cutoff needs to be transformed as well
cutoff_ln <- log( cutoff )
# create regression: 
reg7 <- lm_robust(log_death ~ lspline( log_conf , cutoff_ln ), data = df )

# summary statistics and visualization:
summary( reg7 )
ggplot( data = df, aes( x = log_conf, y = log_death ) ) + 
  geom_point( color="black") +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = "orange" )



# reg8:
reg8 <- lm_robust(log_death ~ log_conf, data = df, weight = population)
# summary statistics and visualization:
summary(reg8)
ggplot(df, aes(x = log_conf, y = log_death)) +
  geom_point(data = df, aes(size = population), color = "orange", shape = 16, alpha = 0.6, show.legend = F) +
  geom_smooth(aes(weight = population), method = lm, color = "red")



########################
# creating the model summary
out_path <- "C:/Users/diama/Documents/CEU-BA-Assignments/Coding1_DataAnalysis2/Covid19-Regression/out/"
htmlreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
  type = "html",
  custom.model.names = c("Confirmed cases - linear", "Confirmed cases - quadratic",
                         "Confirmed cases - cubic", "log confirmed cases - linear",
                         "log confirmed cases - quadratic", "log confirmed cases- cubic",
                         "log confirmed cases - PLS",
                         "log confirmed cases -weighted linear"),
  caption = "Modelling confirmed COVID-19 cases and death resulted among them",
  file = paste0(out_path, 'model_comparison_covid.html'), include.ci = F)

######
# Based on model comparison our chosen model is reg4 - log_death ~ log_conf
#   Substantive: - log-log transformation works better considering the high skewness
#                - considering the
#   Statistical: - the R^2 of reg4-6 exceeds the R^2 of the reg1-3 -> capture the variables well
#                - it has the lowest p value compared to the other log regressions that indicates the lowest chance of false positive


########################
## Testing hypothesis ##
########################

##
# Coefficient is equal to 0:
library(car)
# Let test: H0: log_conf = 0
#           HA: log_conf != 0
linearHypothesis( reg4 , "log_conf = 0")




######
# Residual analysis.


# Get the predicted y values from the model
df$reg4_y_pred <- reg4$fitted.values
# Calculate the errors of the model
df$reg4_res <- df$log_death - df$reg4_y_pred 

# Find countries with largest negative errors
df %>% top_n( -5 , reg4_res ) %>% 
  select( country , log_death , reg4_y_pred , reg4_res )

# Find countries with largest positive errors
df %>% top_n( 5 , reg4_res ) %>% 
  select( country , log_death , reg4_y_pred , reg4_res )



#################################
## Prediction uncertainty
#

# CI of predicted value/regression line is implemented in ggplot
ggplot( data = df, aes( x = log_conf, y = log_death ) ) + 
  geom_point( color='black') +
  geom_smooth( method = lm , color = 'orange' , se = T)


##
# Now we change to get the prediction intervals!
#
pred4_PI <- predict( reg4, newdata = df , interval ="prediction" , alpha = 0.05 )

# Hand made Prediction Interval for regression line
# 1) Add to dataset (You can use the SE's as well if you wish...
#                        then alpha does not have any meaning)
df <- df %>% mutate( PI_reg4_lower = pred4_PI$fit[,2],
                     PI_reg4_upper = pred4_PI$fit[,3] )
# 2) Plot
ggplot(  ) + 
  geom_point( data = df, aes( x = log_conf, y = log_death ) , color='blue') +
  geom_line( data = df, aes( x = log_conf, y = reg4_y_pred ) , color = 'red' , size = 1 ) +
  geom_line( data = df, aes( x = log_conf, y = PI_reg4_lower ) , color = 'green' ,
             size = 1 , linetype = "dotted" ) +
  geom_line( data = df, aes( x = log_conf, y = PI_reg4_upper ) , color = 'black' ,
             size = 1 , linetype = "dotted" ) +
  labs(x = "log(confirmed COVID-19 cases)",y = "log(number of death due to COVID-19)") 

# top 5 countries that relatively lost the most people and top 5 that saved the most:

top_countries_df <- df %>% select(country, log_conf, log_death, PI_reg4_lower, PI_reg4_upper)




# View(df)

