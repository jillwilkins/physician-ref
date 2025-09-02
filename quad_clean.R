# Meta --------------------------------------------------------------------
## Title:         Analysis of Included and Not Included Pairs
## Author:        Jillian Wilkins
## Date Created:  8/11/2025
## Date Edited:   9/1/2025


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


# count number of unique referrals 
referral_counts <- df_initial_referrals %>%
  group_by(npi = doctor) %>%
  summarise(deg = n_distinct(specialist), .groups = "drop")

# doctors only 
df_doctor_wide <- df_initial_referrals %>%
  select(doctor, starts_with("doc_"), total_pcp_patients, Year, num_ref, spec_qual) %>%  
  rename_with(~ gsub("^doc_", "", .x), .cols = starts_with("doc_")) %>%
  rename(npi = doctor,total_patients = total_pcp_patients, qual = spec_qual) %>% 
  distinct(npi, .keep_all = TRUE)

# specialists only 
df_spec_wide <- df_initial_referrals %>%
  select(specialist, starts_with("spec_"), total_spec_patients, Year) %>% 
  rename_with(~ gsub("^spec_", "", .x), .cols = starts_with("spec_")) %>%
  rename(npi = specialist, total_patients = total_spec_patients) %>% 
  distinct(npi, .keep_all = TRUE)


#bind together 
df_providers <- bind_rows(
  df_doctor_wide %>% mutate(role = "doctor", qual = NA, spec = as.character(spec)),
  df_spec_wide   %>% mutate(role = "specialist", spec = as.character(spec)))


# add df_in "include" when that NPI is in the estimation and doctor_ref_counts
df_providers <- df_providers %>%
  left_join(df_in %>% select(npi, include), by = "npi") %>% 
  mutate(include = ifelse(is.na(include), 0, include)) %>%
  left_join(referral_counts, by = "npi")

#---------------------------------------------#  
# Summarize by inclusion and role 
options(knitr.kable.NA = '')

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
      "Unique Referrals Made" =mean(deg, na.rm = TRUE),
      "Observations" = n_distinct(npi),
      .groups = "drop"
    ) %>% 
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.) | is.infinite(.), NA_real_, .))) %>%
    pivot_longer( # nolint
      cols = -c(include, role),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(colname = case_when(
      include == 1 & role == "doctor"     ~ "Included PCPs",
      include == 0 & role == "doctor"     ~ "Not Included PCPs",
      include == 1 & role == "specialist" ~ "Included Specialists",
      include == 0 & role == "specialist" ~ "Not Included Specialists"
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
  select(variable,`Included PCPs`,`Not Included PCPs`,`Included Specialists`,
    `Not Included Specialists`)

sum_table %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 2,  
    na = "" ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

