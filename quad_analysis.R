# Meta --------------------------------------------------------------------
## Title:         Analysis of Discordant and Concordant Pairs
## Author:        Jillian Wilkins
## Date Created:  8/11/2025
## Date Edited:   8/11/2025


colnames(df_logit_twfe)
colnames(df_full_referrals)
str(df_full_referrals)

# -------------------------------------------------------# 
# get a long list of all providers in the estimation
df_in <- df_logit_twfe %>%
  pivot_longer(
    cols = c(doc1, spec1, doc2, spec2),
    names_to = "role",
    values_to = "npi"
  ) %>%
  mutate(include = 1) %>%
  select(role, npi, include)

#change roles from "doc#" to doctor, "spec#" to specialist, and distinct npi 
df_in <- df_in %>%
  mutate(
    role = case_when(
      grepl("^doc", role)  ~ "doctor",
      grepl("^spec", role) ~ "specialist",
      TRUE ~ role)) %>%
  distinct(npi, role, .keep_all = TRUE)

# doctors only 
df_doctor_wide <- df_full_referrals %>%
  select(doctor, starts_with("doc_"), total_pcp_patients, Year) %>%  # include doctor totals
  rename_with(~ gsub("^doc_", "", .x), .cols = starts_with("doc_")) %>%
  rename(npi = doctor,total_patients = total_pcp_patients) %>% 
  distinct(npi, .keep_all = TRUE)

# specialists only 
df_spec_wide <- df_full_referrals %>%
  select(specialist, starts_with("spec_"), total_spec_patients, Year) %>%  # include specialist totals
  rename_with(~ gsub("^spec_", "", .x), .cols = starts_with("spec_")) %>%
  rename(npi = specialist, total_patients = total_spec_patients) %>% 
  distinct(npi, .keep_all = TRUE)

#NOTE: what is spec?? I think that its the doctor specialty. 
# Ensure spec column is character in both
df_doctor_wide <- df_doctor_wide %>%
  mutate(spec = as.character(spec))

df_spec_wide <- df_spec_wide %>%
  mutate(spec = as.character(spec))

# Add missing column 'qual' to doctors
if(!"qual" %in% names(df_doctor_wide)) {
  df_doctor_wide <- df_doctor_wide %>%
    mutate(qual = NA)}

#bind together 
df_providers <- bind_rows(
  df_doctor_wide %>% mutate(role = "doctor"),
  df_spec_wide   %>% mutate(role = "specialist"))

# add df_in "include" when that NPI is in the estimation. 
df_providers <- df_providers %>%
  left_join(df_in %>% select(npi, include), by = "npi") %>%
  mutate(include = ifelse(is.na(include), 0, include))

df_providers <- df_providers %>%
  mutate(include = as.numeric(unlist(include)))

df_providers %>% 
  summarise(total_obs = n())

df_in %>%
  summarize(toal_in = n())

df_providers %>% 
  distinct(sex) %>% 
  arrange(sex)

#---------------------------------------------# 
# df_providers has all npis and if they are included or not in the estimation 
# Summarize by treatment group
summary_table <- df_providers  %>%
  group_by(include) %>%
  summarize(
    obs = sum(n = n(), na.rm = TRUE),
    mean_age = mean(Year - birth, na.rm = TRUE),
    #sd_age = sd( Year - birth, na.rm = TRUE),
    mean_exp = mean(Year - grad_year, na.rm = TRUE), 
    mean_qual = mean(qual, na.rm = TRUE), 
    #sd_qual = sd(qual, na.rm = TRUE),
)

# role percentages 
role_pct <- df_providers %>%
  group_by(include, role) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(include) %>%
  mutate(pct = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = role, values_from = pct, values_fill = 0)

# Race percentages
race_pct <- df_providers %>%
  group_by(include, race) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(include) %>%
  mutate(pct = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = race, values_from = pct, values_fill = 0)

# gender pct 
sex_pct <- df_providers %>%
  group_by(include, sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(include) %>%
  mutate(pct = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = sex, values_from = pct, values_fill = 0) %>%
  rename(pct_male = M, pct_female = F)

summary_table <- summary_table %>%
  left_join(race_pct, by = "include") %>%
  left_join(sex_pct, by = "include") %>%
  left_join(role_pct, by = "include")

View(summary_table)

# make summary table for la tex 
# variables into rows
summary_table_long <- summary_table %>%
  pivot_longer(
    cols = -include,             # keep 'include' separate
    names_to = "variable",
    values_to = "value"
  )

# make included the columns
summary_table_wide <- summary_table_long %>%
  pivot_wider(
    names_from = include,
    values_from = value,
    names_prefix = "included_"
  )

# variable labels
var_labels <- c(
  mean_age = "Mean Age",
  mean_exp = "Mean Years of Experience",
  mean_qual = "Mean Specialist Quality",
  pct_male = "Percent Male",
  white = "Percent White",
  black = "Percent Black",
  asian = "Percent Asian",
  hispanic = "Percent Hispanic",
  obs = "Observations"
)

# rename column title 
summary_table_wide <- summary_table_wide %>%
  rename(
    `Not Included` = included_0,
    `Included`     = included_1
  )

# filter columns we dont want 
summary_table_wide <- summary_table_wide %>%
  filter(!variable %in% c("other", "NA.x", "NA.y", "NA", "pct_female", "doctor", "specialist")) %>%
  filter(!is.na(variable))

# order rows and apply labels 
summary_table_wide <- summary_table_wide %>%
  mutate(variable = recode(variable, !!!var_labels)) %>%
  mutate(variable = factor(variable, levels = var_labels)) %>%
  arrange(variable)

summary_table_wide %>%
  kable(format = "latex", booktabs = TRUE, digits = 2) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

