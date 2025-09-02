# Meta --------------------------------------------------------------------
## Title:         Analysis of Discordant and Concordant Pairs
## Author:        Jillian Wilkins
## Date Created:  8/11/2025
## Date Edited:   8/20/2025


colnames(df_logit_twfe)
colnames(df_full_referrals)
colnames(df_initial_referrals)
str(df_full_referrals)
library(dplyr)

doctor_ref_counts <- df_full_referrals %>%
  group_by(npi = doctor) %>%
  summarise(deg = n_distinct(specialist), .groups = "drop")



View(doctor_ref_counts)

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
df_doctor_wide <- df_initial_referrals %>%
  select(doctor, starts_with("doc_"), total_pcp_patients, Year, num_ref) %>%  
  rename_with(~ gsub("^doc_", "", .x), .cols = starts_with("doc_")) %>%
  rename(npi = doctor,total_patients = total_pcp_patients) %>% 
  distinct(npi, .keep_all = TRUE)

# specialists only 
df_spec_wide <- df_initial_referrals %>%
  select(specialist, starts_with("spec_"), total_spec_patients, Year) %>% 
  rename_with(~ gsub("^spec_", "", .x), .cols = starts_with("spec_")) %>%
  rename(npi = specialist, total_patients = total_spec_patients) %>% 
  distinct(npi, .keep_all = TRUE)

colnames(df_spec_wide)
colnames(df_doctor_wide)
#NOTE: what is spec?? I think that its the doctor specialty. 

# Add missing column 'qual' to doctors
if(!"qual" %in% names(df_doctor_wide)) {
  df_doctor_wide <- df_doctor_wide %>%
    mutate(qual = NA)}

# Ensure spec column is character in both
df_doctor_wide <- df_doctor_wide %>%
  mutate(spec = as.character(spec))

df_spec_wide <- df_spec_wide %>%
  mutate(spec = as.character(spec))

#bind together 
df_providers <- bind_rows(
  df_doctor_wide %>% mutate(role = "doctor"),
  df_spec_wide   %>% mutate(role = "specialist"))

# add df_in "include" when that NPI is in the estimation. 
df_providers <- df_providers %>%
  left_join(df_in %>% select(npi, include), by = "npi") %>%
  mutate(include = ifelse(is.na(include), 0, include)) %>%
  left_join(doctor_ref_counts, by = "npi")

df_providers <- df_providers %>%
  mutate(include = as.numeric(unlist(include)))

#---------------------------------------------# 
# df_providers has all npis and if they are included or not in the estimation 
# Summarize by treatment group

make_summary <- function(df) {
  df %>%
    group_by(include, role) %>%
    summarise(
      "Mean Age" = mean(Year - birth, na.rm = TRUE),
      "Mean Years of Experience" = mean(Year - grad_year, na.rm = TRUE),
      "Mean Specialist Quality" = mean(qual, na.rm = TRUE),
      "Percent Male"  = mean(sex == "M", na.rm = TRUE),
      "Percent White" = mean(race == "white", na.rm = TRUE),
      "Percent Black" = mean(race == "black", na.rm = TRUE),
      "Percent Hispanic" = mean(race == "hispanic", na.rm = TRUE),
      "Percent Asian" = mean(race == "asian", na.rm = TRUE),
      "Total Patients" = mean(total_patients, na.rm = TRUE),
      "Unique Referrals Made" =mean(num_ref, na.rm = TRUE),
      "Observations" = n_distinct(npi),
      .groups = "drop"
    ) %>% 
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA_real_, .))) %>%
    pivot_longer( # nolint
      cols = -c(include, role),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(colname = case_when(
      include == 1 & role == "doctor"     ~ "Included PCP",
      include == 0 & role == "doctor"     ~ "Not Included PCP",
      include == 1 & role == "specialist" ~ "Included Specialist",
      include == 0 & role == "specialist" ~ "Not Included Specialist"
    )) %>%
    select(-include, -role) %>%
    pivot_wider(
      names_from = colname,
      values_from = value
    ) 
}

# la tex output 
sum_table <- make_summary(df_providers)
sum_table <- sum_table %>%
  select(
    variable,
    `Included PCP`,
    `Not Included PCP`,
    `Included Specialist`,
    `Not Included Specialist`
  )

sum_table %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 2,  
    na = ""      # NA will appear as blank
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

