library(xtable)
library(knitr)
# Convert to LaTeX

# confusion matrix for full (name and face)
xtable(conf.full$table, caption = "Confusion Matrix for Name and Facial Recognition Method")

#confusion matrix for name only 
xtable(conf.name$table, caption = "Confusion Matrix for Name Based Method")

# table of counts and total accuracy by method
ranger_counts <- data.frame(
  Method = c("Name and Facial", "Facial", "Name based", "Name Prism", "WRU", "Max"),
  Count = c(nrow(full.ranger), nrow(full.ranger), nrow(name.ranger), nrow(name.ranger), nrow(name.ranger), nrow(pred.ranger)),
  Accuracy = c(conf.full$overall[1], conf.zocdoc$overall[1], conf.name$overall[1], conf.np$overall[1], conf.wru$overall[1], conf.max$overall[1])
)
ranger_counts$Accuracy <- round(ranger_counts$Accuracy, 3)

print(ranger_counts)
xtable(ranger_counts, format = "latex", caption = "Statistics by Method")

# table of accuracy by race by method 
# Named list of confusion matrices
conf_matrices <- list(
  "Name and Facial" = conf.full,
  "Facial" = conf.zocdoc,
  "Name based" = conf.name,
  "Name Prism" = conf.np,
  "WRU" = conf.wru,
  "Max" = conf.max
)

# Extract PPV and reshape to wide format
ranger_class <- map_dfr(conf_matrices, ~ {
  as.data.frame(.x$byClass) %>%
    mutate(Race = rownames(.), Value = `Pos Pred Value`) %>%
    select(Race, Value)
}, .id = "Method") %>%
  pivot_wider(names_from = Race, values_from = Value)

print(ranger_class)
xtable(ranger_class, format = "latex", caption = "Positive Predictive Value by Method")
