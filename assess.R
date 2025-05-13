# necessary packages
library(tidyverse)
library(caret)
library(ggplot2)
library(readr)
library(dplyr)

# Import relevant data -----------------------------------------------------
pred.original <- read_csv('/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/physician race/data/output/final-npi-race.csv')
pred.ranger <- read_csv('/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/physician race/data/output/final-npi-combined.csv') %>% 
  mutate(common_pred=ifelse(common_pred=="None","Other", common_pred),
         common_pred=tolower(common_pred))
full.ranger <- pred.ranger %>% filter(rf_pred_model==1) 
name.ranger <- pred.ranger %>% filter(rf_pred_model %in% c(2,3,4)) 


# confusion matrix -------------------------------------------------------------
conf.full <- confusionMatrix(factor(full.ranger$rf_pred_race), factor(full.ranger$true_race))  
conf.name <- confusionMatrix(factor(name.ranger$rf_pred_race), factor(name.ranger$true_race)) 
conf.max <- confusionMatrix(factor(pred.ranger$common_pred), factor(pred.ranger$true_race))
conf.np <- confusionMatrix(factor(pred.ranger$np_mdppas), factor(pred.ranger$true_race))
conf.wru <- confusionMatrix(factor(pred.ranger$wru_mdppas), factor(pred.ranger$true_race))
conf.zocdoc <- confusionMatrix(factor(pred.ranger$zocdoc), factor(pred.ranger$true_race))

print(conf.full)
print(conf.name)
print(conf.max)
print(conf.np)
print(conf.wru)
print(conf.zocdoc)

#count number of rows
nrow(pred.original)
nrow(pred.ranger)
nrow(full.ranger)
nrow(name.ranger)

