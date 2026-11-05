### Economic Forecasting
###
### Problem Set 2

# Libraries

library(dplyr)
library(forecast)
library(leaps)


# Problem 1

# (a)

# Read data

data <- readxl::read_excel("Goyal2025.xlsx", sheet = "Quarterly") %>%
  setNames(make.names(names(.))) %>% #make sure column names are clean for R use
  mutate(yt = ret - Rfree) %>% #variable of interest is risk premium (return - risk free rate)
  filter(yyyyq >= 19471) #use data from Q1 1947 onward
  

# (b)

#Fit an OLS model on y_t+1 and predictors

predictors <- c("d.p", "d.y", "e.p", "d.e", "b.m", # valuation ratios
                "tbl", "lty", "ltr", "dfy", "dfr", # interest rates, spreads
                "infl", "ntis", "svar", # inflation, issuing, volatility
                "lzrt", "wtexas", "skvw", "tail", # proposed after 2008
                "dtoy", "dtoat", "avgcor", "rdsp") # proposed after 2008


data_model <- data %>% #create dataframe for model
  mutate(yt_plus1 = lead(yt)) %>% #create variable for y_t+1
  filter(yyyyq < 19701) %>% #use data up to Q4 1969
  mutate(across(all_of(predictors), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))


model <- lm(reformulate(predictors, response = "yt_plus1"), data = data_model)

summary(model)

# Five variables are significant at 5% level
# Question: is there a way to filter our results to include only significant ones?

# (c)
sapply(data_model, anyNA)

# Question: how do we handle NAs here? 
model_bss <- regsubsets(data_model[,(names(data_model) %in% predictors)], data_model$yt_plus1, nvmax = 15)
model_bss_leaps <- leaps(data_model[,(names(data_model) %in% predictors)], data_model$yt_plus1)

summary(model_bss)
summary(model_bss_leaps)


