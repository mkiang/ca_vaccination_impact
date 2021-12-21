## utils.R ----
## 
## M Kiang utility functions for California excess mortality project.
## 
## NOTE: This file is used across many different projects so I have commented
## out many functions that are not relevant to this particular project. 

## Helpers for identifying cause of death ----
## Notes from CDC website:
## https://www.cdc.gov/nchs/nvss/vsrr/covid19/excess_deaths.htm
## 
## COVID is U07.1 for underlying or multiple cause
## 
## Main cause of death categories for CDC are respiratory diseases,
## circulatory diseases, malignant neoplasms, alzheimer disease and 
## dementia, and other select causes of death. We are further interested
## in natural causes, poisonings (by drug type), and injuries. 
## 
## 
## Respiratory diseases
##   Influenza and pneumonia (J09–J18)
##   Chronic lower respiratory diseases (J40–J47)
##   Other diseases of the respiratory system (J00–J06, J20–J39, 
##      J60–J70, J80–J86, J90–J96, J97–J99, R09.2, U04)
## 
## Circulatory diseases
##   Hypertensive diseases (I10–I15)
##   Ischemic heart disease (I20–I25)
##   Heart failure (I50)
##   Cerebrovascular diseases (I60–I69)
##   Other disease of the circulatory system (I00–I09, I26–I49, 
##      I51, I52, I70–I99)
## 
## Malignant neoplasms (C00–C97)
## 
## Alzheimer disease and dementia (G30, G31, F01, F03)
#
## Other select causes of death
##   Diabetes (E10–E14)
##   Renal failure (N17–N19)
##   Sepsis (A40–A41)
## 
## Natural causes A00–A39, A42–B99, D00–E07, E15–E68, 
##  E70–E90, F00, F02, F04–G26, G31–H95, K00–K93, 
##  L00–M99, N00–N16, N20–N98, O00–O99, P00–P96, Q00–Q99
library(tidyverse)
library(narcan)

# k_autoarima <- function(k, p = 365.25 / 7) {
#     ARIMA(n_deaths ~ fourier(K =  k, period = p) + PDQ(0, 0, 0),  
#           stepwise = FALSE, 
#           approximation = TRUE)
# }

## Get population denominators
## Note that because we decided to do race*education as one of the analyses,
## we cannot use a pre-made ACS table and need to use the microdata. This
## needs to be pulled manually. See ./data_raw/acs_microdata_pulls/README.md
## for details on how we pulled it. 
# read_acs_microdata <- function(f_name) {
#     x <- read_csv(
#         f_name,
#         skip = 7,
#         col_names = c("target_pop", "total", "hispanic", "nonhispanic")
#     )
#     
#     x$year <- as.numeric(substr(basename(f_name),
#                                 nchar(basename(f_name)) - 7,
#                                 nchar(basename(f_name)) - 4))
#     x$educ <- c("all",
#                 rep("under25", 31),
#                 "over25",
#                 rep(c(
#                     "over25", ">bs", "< hs", "hs", "< bs", "bs"
#                 ), 5))
#     
#     x$race <- c("all",
#                 "all",
#                 rep(c(
#                     "white", "black", "aian", "asian", "other"
#                 ), each = 6),
#                 "all",
#                 rep(c(
#                     "white", "black", "aian", "asian", "other"
#                 ), each = 6))
#     
#     x[c(1:3, 9, 15, 21, 27, 33:63), - 1]
# }


# get_acs_race_pops <- function(year_x) {
#     state_pops_by_race <- bind_rows(
#         tidycensus::get_acs(
#             geography = "state",
#             table = "B01001A",
#             year = year_x,
#             cache_table = TRUE,
#             survey = "acs1"
#         ) %>%
#             mutate(race_eth = "white"),
#         tidycensus::get_acs(
#             geography = "state",
#             table = "B01001B",
#             year = year_x,
#             cache_table = TRUE,
#             survey = "acs1"
#         ) %>%
#             mutate(race_eth = "black"),
#         tidycensus::get_acs(
#             geography = "state",
#             table = "B01001C",
#             year = year_x,
#             cache_table = TRUE,
#             survey = "acs1"
#         ) %>%
#             mutate(race_eth = "aian"),
#         tidycensus::get_acs(
#             geography = "state",
#             table = "B01001D",
#             year = year_x,
#             cache_table = TRUE,
#             survey = "acs1"
#         ) %>%
#             mutate(race_eth = "asian"),
#         tidycensus::get_acs(
#             geography = "state",
#             table = "B01001H",
#             year = year_x,
#             cache_table = TRUE,
#             survey = "acs1"
#         ) %>%
#             mutate(race_eth = "non_hisp_white"),
#         tidycensus::get_acs(
#             geography = "state",
#             table = "B01001I",
#             year = year_x,
#             cache_table = TRUE,
#             survey = "acs1"
#         ) %>%
#             mutate(race_eth = "latino")
#     )
# 
#     all_races_state <- tidycensus::get_acs(
#         geography = "state",
#         table = "B01001",
#         year = year_x,
#         cache_table = TRUE,
#         survey = "acs1"
#     ) %>%
#         mutate(race_eth = "all")
# 
#     bind_rows(all_races_state, state_pops_by_race) %>%
#         left_join(
#             load_variables(year_x, "acs1") %>%
#                 select(
#                     variable = name,
#                     var_label = label,
#                     var_concept = concept
#                 )
#         ) %>%
#         mutate(year = year_x)
# }

# get_acs_educ_pops <- function(year_x) {
#     state_pops_by_educ <- tidycensus::get_acs(
#         geography = "state",
#         table = "B15003",
#         year = year_x,
#         cache_table = TRUE,
#         survey = "acs1"
#     ) %>%
#         left_join(
#             tidycensus::load_variables(year_x, "acs1") %>%
#                 select(
#                     variable = name,
#                     var_label = label,
#                     var_concept = concept
#                 )
#         ) %>%
#         mutate(year = year_x)
# }

# get_acs_educ_non_hisp_pops <- function(year_x) {
#     state_pops_by_educ_hisp <- tidycensus::get_acs(
#         geography = "state",
#         table = "B15002I",
#         year = year_x,
#         cache_table = TRUE,
#         survey = "acs1"
#     ) %>%
#         left_join(
#             tidycensus::load_variables(year_x, "acs1") %>%
#                 select(
#                     variable = name,
#                     var_label = label,
#                     var_concept = concept
#                 )
#         ) %>%
#         mutate(year = year_x) %>% 
#         mutate(race = "hispanic")
#     
#     state_pops_by_educ_total <- tidycensus::get_acs(
#         geography = "state",
#         table = "B15002",
#         year = year_x,
#         cache_table = TRUE,
#         survey = "acs1"
#     ) %>%
#         left_join(
#             tidycensus::load_variables(year_x, "acs1") %>%
#                 select(
#                     variable = name,
#                     var_label = label,
#                     var_concept = concept
#                 )
#         ) %>%
#         mutate(year = year_x) %>% 
#         mutate(race = "total")
#     
#     bind_rows(state_pops_by_educ_hisp,
#               state_pops_by_educ_total)
# }

categorize_race <- function(df) {
    df %>%
        mutate(
            race_cat = factor(
                race,
                levels = c(
                    "all",
                    "white",
                    "black",
                    "hispanic",
                    "asian",
                    "other",
                    "all_nonhisp"
                ),
                labels = c(
                    "Total",
                    "Non-Hispanic White",
                    "Non-Hispanic Black",
                    "Hispanic",
                    "Asian",
                    "Other",
                    "All non-Hispanic"
                ),
                ordered = TRUE
            ),
            race_cat_rev = factor(
                race,
                levels = rev(
                    c(
                        "all",
                        "white",
                        "black",
                        "hispanic",
                        "asian",
                        "other",
                        "all_nonhisp"
                    )
                ),
                labels = rev(
                    c(
                        "Total",
                        "Non-Hispanic White",
                        "Non-Hispanic Black",
                        "Hispanic",
                        "Asian",
                        "Other",
                        "All non-Hispanic"
                    )
                ),
                ordered = TRUE
            )
        )
}

categorize_educ <- function(df) {
    df %>%
        mutate(
            educ_cat = factor(
                educ,
                levels = c("all",
                           "over25",
                           "under25",
                           "< hs",
                           "hs",
                           "< bs",
                           "bs",
                           ">bs"),
                labels = c(
                    "All levels",
                    "All 25+ y/o",
                    "All <25 y/o",
                    "Less than HS",
                    "HS / GED",
                    "Less than BA/BS",
                    "BA/BS",
                    "Graduate"
                ),
                ordered = TRUE
            ),
            educ_cat_rev = factor(
                educ,
                levels = rev(
                    c("all",
                      "over25",
                      "under25",
                      "< hs",
                      "hs",
                      "< bs",
                      "bs",
                      ">bs")
                ),
                labels = rev(
                    c(
                        "All levels",
                        "All 25+ y/o",
                        "All <25 y/o",
                        "Less than HS",
                        "HS / GED",
                        "Less than BA/BS",
                        "BA/BS",
                        "Graduate"
                    )
                ),
                ordered = TRUE
            )
        )
}

# categorize_death <- function(df) {
#     df %>%
#         mutate(
#             death_cat_rev = factor(
#                 death_type,
#                 levels = rev(
#                     c(
#                         "all_deaths",
#                         "non_covid",
#                         "any_poisoning",
#                         "drug_poisoning",
#                         "nondrug_poisoning",
#                         "alcohol",
#                         "benzos",
#                         "cocaine",
#                         "meth",
#                         "opioids",
#                         "heroin",
#                         "natural_opioid",
#                         "synthetic_opioid",
#                         "other_opioid",
#                         "intent_drug_poisoning",
#                         "assault_drug_poisoning",
#                         "unintent_drug_poisoning",
#                         "unknown_drug_poisoning",
#                         "intent_nondrug_poisoning",
#                         "assault_nondrug_poisoning",
#                         "unintent_nondrug_poisoning",
#                         "unknown_nondrug_poisoning"
#                     )
#                 ),
#                 labels = rev(
#                     c(
#                         "All causes",
#                         "Non-COVID",
#                         "Any poisoning",
#                         "Drug poisoning",
#                         "Non-drug poisoning",
#                         "Alcohol",
#                         "Benzodiazepines",
#                         "Cocaine",
#                         "Methamphetamine",
#                         "Opioids",
#                         "Heroin",
#                         "Natural opioids",
#                         "Synthetic opioids",
#                         "Other opioids",
#                         "Intentional drug poisoning",
#                         "Assault drug poisoning",
#                         "Unintentional drug poisoning",
#                         "Unknown intent drug poisoning",
#                         "Intentional nondrug poisoning",
#                         "Assault nondrug poisoning",
#                         "Unintentional nondrug poisoning",
#                         "Unknown intent nondrug poisoning"
#                     )
#                 ),
#                 ordered = TRUE
#             ),
#             death_cat = factor(
#                 death_type,
#                 levels = c(
#                     "all_deaths",
#                     "non_covid",
#                     "any_poisoning",
#                     "drug_poisoning",
#                     "nondrug_poisoning",
#                     "alcohol",
#                     "benzos",
#                     "cocaine",
#                     "meth",
#                     "opioids",
#                     "heroin",
#                     "natural_opioid",
#                     "synthetic_opioid",
#                     "other_opioid",
#                     "intent_drug_poisoning",
#                     "assault_drug_poisoning",
#                     "unintent_drug_poisoning",
#                     "unknown_drug_poisoning",
#                     "intent_nondrug_poisoning",
#                     "assault_nondrug_poisoning",
#                     "unintent_nondrug_poisoning",
#                     "unknown_nondrug_poisoning"
#                 ),
#                 labels = c(
#                     "All causes",
#                     "Non-COVID",
#                     "Any poisoning",
#                     "Drug poisoning",
#                     "Non-drug poisoning",
#                     "Alcohol",
#                     "Benzodiazepines",
#                     "Cocaine",
#                     "Methamphetamine",
#                     "Opioids",
#                     "Heroin",
#                     "Natural opioids",
#                     "Synthetic opioids",
#                     "Other opioids",
#                     "Intentional drug poisoning",
#                     "Assault drug poisoning",
#                     "Unintentional drug poisoning",
#                     "Unknown intent drug poisoning",
#                     "Intentional nondrug poisoning",
#                     "Assault nondrug poisoning",
#                     "Unintentional nondrug poisoning",
#                     "Unknown intent nondrug poisoning"
#                 ),
#                 ordered = TRUE
#             )
#         )
# }

calculate_age_std_rates <- function(age_spec_rates) {
    bind_rows(
        age_spec_rates %>%
            narcan::calc_stdrate_var(
                asrate_col = covid_rate,
                asvar_col = covid_var,
                race,
                date,
                date_start
            ) %>%
            mutate(death_type = "covid") %>%
            rename(rate = covid_rate,
                   var = covid_var),
        age_spec_rates %>%
            narcan::calc_stdrate_var(
                asrate_col = non_covid_rate,
                asvar_col = non_covid_var,
                race,
                date,
                date_start
            ) %>%
            mutate(death_type = "non_covid") %>%
            rename(rate = non_covid_rate,
                   var = non_covid_var),
        age_spec_rates %>%
            narcan::calc_stdrate_var(
                asrate_col = all_deaths_rate,
                asvar_col = all_deaths_var,
                race,
                date,
                date_start
            ) %>%
            mutate(death_type = "all_deaths") %>%
            rename(rate = all_deaths_rate,
                   var = all_deaths_var)
    ) %>%
        ungroup()
}

# calculate_excess_deaths <- function(death_df,
#                                     race_x,
#                                     educ_x, 
#                                     death_x,
#                                     best_model_df,
#                                     forecast_start = FORECAST_START,
#                                     last_week = LAST_WEEK,
#                                     n_reps = N_BOOT) {
#     ## Number of weeks to forecast
#     h_x <- round(as.numeric(difftime(last_week, forecast_start, units = "weeks")))
#     
#     ## Pull out observed
#     observed_total_deaths <- death_df %>%
#         filter(between(date, as.Date(forecast_start), as.Date(last_week)),
#                death_type == death_x,
#                race == race_x,
#                educ == educ_x) %>%
#         pull(n_deaths) %>%
#         sum()
#     
#     ## Pull out the model used for forecasting
#     target_model <- best_model_df %>%
#         filter(death_type == death_x, 
#                race == race_x, 
#                educ == educ_x) %>%
#         pull(model)
#     target_model <- target_model[[1]]
#     
#     ## Get bootstrap samples of the expected total deaths
#     expected_total_deaths <- replicate(expr = {
#         forecast(
#             target_model,
#             simulate = TRUE,
#             times = 1,
#             h = h_x
#         ) %>%
#             pull(.mean) %>%
#             sum()
#     },
#     n = n_reps)
#     
#     excess_deaths <- observed_total_deaths - expected_total_deaths
#     
#     tibble(
#         race = race_x,
#         educ = educ_x,
#         death_type = death_x,
#         start_interval = as.Date(forecast_start),
#         end_interval = as.Date(last_week),
#         n = n_reps,
#         mean = mean(excess_deaths),
#         p025 = quantile(excess_deaths, .025),
#         p250 = quantile(excess_deaths, .25),
#         p500 = quantile(excess_deaths, .5),
#         p750 = quantile(excess_deaths, .75),
#         p975 = quantile(excess_deaths, .975),
#         min = min(excess_deaths),
#         max = max(excess_deaths),
#         sd = sd(excess_deaths)
#     )
# }

## Give it the path of a joinpoint result file (f_path). Returns a tibble.
import_jp <- function(f_path, ctypes = NULL) {
    readr::read_delim(f_path, delim = ";", col_types = ctypes) %>%
        janitor::clean_names(.)
}

flag_covid_death <- function(cleaned_df, ucod_only = FALSE) {
    ## cleaned_df is a dataframe with ucod and record_all columns
    if (ucod_only) {
        cleaned_df %>%
            mutate(covid = grepl("\\<U071", ucod) + 0)
    } else {
        cleaned_df %>%
            mutate(covid = grepl("\\<U071", record_all) + 0)
    }
}

# flag_unintent_drug_poisoning <- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(unintent_drug_poisoning = grepl("\\<X4[0-4]{1}", ucod) + 0)
# }
# 
# flag_unintent_nondrug_poisoning <- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(unintent_nondrug_poisoning = grepl("\\<X4[5-9]{1}", ucod) + 0)
# }
# 
# flag_intent_drug_poisoning <- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(intent_drug_poisoning = grepl("\\<X6[0-4]{1}", ucod) + 0)
# }
# 
# flag_intent_nondrug_poisoning <- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(intent_nondrug_poisoning = grepl("\\<X6[5-9]{1}", ucod) + 0)
# }
# 
# flag_assault_drug_poisoning <- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(assault_drug_poisoning = grepl("\\<X85", ucod) + 0)
# }
# 
# flag_assault_nondrug_poisoning <- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(assault_nondrug_poisoning = grepl("\\<X8[6-9]{1}|\\<X90", ucod) + 0)
# }
# 
# flag_unknown_drug_poisoning <- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(unknown_drug_poisoning = grepl("\\<Y1[0-4]{1}", ucod) + 0)
# }
# 
# flag_unknown_nondrug_poisoning <- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(unknown_nondrug_poisoning = grepl("\\<Y1[5-9]{1}", ucod) + 0)
# }
# 
# flag_any_poisoning_death <- function(cleaned_df) {
#     cleaned_df %>%
#         flag_unintent_drug_poisoning() %>%
#         flag_unintent_nondrug_poisoning() %>%
#         flag_intent_drug_poisoning() %>%
#         flag_intent_nondrug_poisoning() %>%
#         flag_assault_drug_poisoning() %>%
#         flag_assault_nondrug_poisoning() %>%
#         flag_unknown_drug_poisoning() %>%
#         flag_unknown_nondrug_poisoning() %>%
#         mutate(any_poisoning = 0 + ((
#             unintent_drug_poisoning +
#                 intent_drug_poisoning +
#                 assault_drug_poisoning +
#                 unknown_drug_poisoning +
#                 unintent_nondrug_poisoning +
#                 intent_nondrug_poisoning +
#                 assault_nondrug_poisoning +
#                 unknown_nondrug_poisoning
#         ) > 0
#         ))
# }
# 
# flag_drug_poisoning <- function(cleaned_df) {
#     cleaned_df %>%
#         flag_unintent_drug_poisoning() %>%
#         flag_intent_drug_poisoning() %>%
#         flag_assault_drug_poisoning() %>%
#         flag_unknown_drug_poisoning() %>%
#         mutate(drug_poisoning = 0 + ((
#             unintent_drug_poisoning +
#                 intent_drug_poisoning +
#                 assault_drug_poisoning +
#                 unknown_drug_poisoning
#         ) > 0
#         ))
# }
# 
# flag_nondrug_poisoning <- function(cleaned_df) {
#     cleaned_df %>%
#         flag_unintent_nondrug_poisoning() %>%
#         flag_intent_nondrug_poisoning() %>%
#         flag_assault_nondrug_poisoning() %>%
#         flag_unknown_nondrug_poisoning() %>%
#         mutate(nondrug_poisoning = 0 + ((
#                 unintent_nondrug_poisoning +
#                 intent_nondrug_poisoning +
#                 assault_nondrug_poisoning +
#                 unknown_nondrug_poisoning
#         ) > 0
#         ))
# }
# 
# flag_nonopioid_analgesics <- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(nonopioid_analgesics = grepl("\\<T39", record_all) + 0)
# }
# 
# flag_psychotropics <- function(cleaned_df) {
#     cleaned_df %>% 
#         flag_drug_poisoning() %>% 
#         mutate(psychotropics = (drug_poisoning == 1 & 
#                                     grepl("\\<T4[23]{1}", record_all)) + 0)
# }
# 
# flag_benzos <- function(cleaned_df) {
#     cleaned_df %>% 
#         flag_drug_poisoning() %>% 
#         mutate(benzos = (drug_poisoning == 1 & 
#                              grepl("\\<T424", record_all)) + 0)
# }
# 
# flag_meth <- function(cleaned_df) {
#     cleaned_df %>% 
#         flag_drug_poisoning() %>% 
#         mutate(meth = (drug_poisoning == 1 & 
#                              grepl("\\<T436", record_all)) + 0)
# }
# 
# flag_opioids <- function(cleaned_df) {
#     cleaned_df %>% 
#         flag_drug_poisoning() %>% 
#         mutate(opioids = (drug_poisoning == 1 & 
#                               grepl("\\<T40[012346]{1}", record_all)) + 0,
#                heroin = (drug_poisoning == 1 & 
#                              grepl("\\<T401", record_all)) + 0,
#                synthetic_opioid = (drug_poisoning == 1 & 
#                                        grepl("\\<T404", record_all)) + 0,
#                natural_opioid = (drug_poisoning == 1 & 
#                                      grepl("\\<T402", record_all)) + 0,
#                other_opioid = (drug_poisoning == 1 & 
#                                    grepl("\\<T406", record_all)) + 0)
# }
# 
# flag_cocaine <- function(cleaned_df) {
#     cleaned_df %>% 
#         flag_drug_poisoning() %>% 
#         mutate(cocaine = (drug_poisoning == 1 & 
#                               grepl("\\<T405", record_all)) + 0)
# }
# 
# flag_other_drug <- function(cleaned_df) {
#     cleaned_df %>% 
#         flag_drug_poisoning() %>% 
#         mutate(other_drug = (drug_poisoning == 1 & 
#                                  grepl("\\<T509", record_all)) + 0)
# }
# 
# flag_alcohol<- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(alcohol = grepl("\\<T51", record_all) + 0)
# }
# 
# flag_solvents <- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(solvents = grepl("\\<T5[23]{1}", record_all) + 0)
# }
# 
# flag_gasses <- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(gasses = grepl("\\<T5[89]{1}", record_all) + 0)
# }
# 
# flag_other_nondrug <- function(cleaned_df) {
#     cleaned_df %>% 
#         mutate(cocaine = grepl("\\<T5[4-7]{1}|\\<T60|\\<T65", record_all) + 0)
# }

## Dataframe of state abbreviation, name, fips, and division mapping ----
## Early years of MCOD files use NCHS-specific state codes.
nchs_state_codes <-
    list(
        "AL" = "01",
        "AK" = "02",
        "AZ" = "03",
        "AR" = "04",
        "CA" = "05",
        "CO" = "06",
        "CT" = "07",
        "DE" = "08",
        "DC" = "09",
        "FL" = "10",
        "GA" = "11",
        "HI" = "12",
        "ID" = "13",
        "IL" = "14",
        "IN" = "15",
        "IA" = "16",
        "KS" = "17",
        "KY" = "18",
        "LA" = "19",
        "ME" = "20",
        "MD" = "21",
        "MA" = "22",
        "MI" = "23",
        "MN" = "24",
        "MS" = "25",
        "MO" = "26",
        "MT" = "27",
        "NE" = "28",
        "NV" = "29",
        "NH" = "30",
        "NJ" = "31",
        "NM" = "32",
        "NY" = "33",
        "NC" = "34",
        "ND" = "35",
        "OH" = "36",
        "OK" = "37",
        "OR" = "38",
        "PA" = "39",
        "RI" = "40",
        "SC" = "41",
        "SD" = "42",
        "TN" = "43",
        "TX" = "44",
        "UT" = "45",
        "VT" = "46",
        "VA" = "47",
        "WA" = "48",
        "WV" = "49",
        "WI" = "50",
        "WY" = "51"
    )

st_info <- dplyr::tibble(
    abbrev   = datasets::state.abb,
    division = as.character(datasets::state.division),
    st_lat   = datasets::state.center$y,
    st_lon   = datasets::state.center$x
) %>%
    ## We have to add DC because it's not a state
    dplyr::add_row(
        abbrev = "DC",
        division = "South Atlantic",
        st_lat = 38.9072,
        st_lon = -77.0369
    ) %>%
    dplyr::left_join(narcan::st_fips_map) %>%
    dplyr::rename(st_fips = fips) %>%
    dplyr::arrange(name) %>%
    dplyr::left_join(dplyr::tibble(
        abbrev = names(nchs_state_codes),
        nchs_fips = unlist(nchs_state_codes)
    ),
    by = "abbrev")
# rm(nchs_state_codes)

return_st_info <- function(subset_states = FALSE) {
    st_info <- dplyr::tibble(
        abbrev   = datasets::state.abb,
        division = as.character(datasets::state.division),
        st_lat   = datasets::state.center$y,
        st_lon   = datasets::state.center$x
    ) %>%
        ## We have to add DC because it's not a state
        dplyr::add_row(
            abbrev = "DC",
            division = "South Atlantic",
            st_lat = 38.9072,
            st_lon = -77.0369
        ) %>%
        dplyr::left_join(narcan::st_fips_map, by = "abbrev") %>%
        # ## Add in whole US and NA
        # dplyr::add_row(
        #     abbrev = "US",
        #     name = "zzWhole US",
        #     division = "Whole US",
        #     st_lat = 0,
        #     st_lon = 200
        # ) %>%
        # dplyr::add_row(
        #     abbrev = NA,
        #     name = "zzzUnknown State",
        #     division = "Unknown",
        #     st_lat = 0,
        #     st_lon = 199
        # ) %>%
        dplyr::rename(st_fips = fips) %>%
        dplyr::arrange(st_lon) %>%
        dplyr::mutate(
            lon_rank = dplyr::dense_rank(st_lon),
            alpha_rank = dplyr::dense_rank(name)
        ) %>%
        dplyr::mutate(name = gsub("zz|zzz", "", name))
    
    st_info <- st_info %>%
        dplyr::mutate(
            st_cat = factor(
                abbrev,
                levels = st_info %>%
                    dplyr::arrange(lon_rank) %>%
                    dplyr::pull(abbrev),
                ordered = TRUE
            ),
            name_cat = factor(
                name,
                levels = st_info %>%
                    dplyr::arrange(name) %>%
                    dplyr::pull(name),
                ordered = TRUE
            ),
            name_cat_alpha = factor(
                name,
                levels = st_info %>%
                    dplyr::arrange(alpha_rank) %>%
                    dplyr::pull(name),
                ordered = TRUE
            )
        )
    
    if (subset_states) {
        st_info %>%
            dplyr::arrange(name_cat) %>%
            dplyr::filter(
                abbrev %in% c(
                    "AL",
                    "AR",
                    "AZ",
                    "CA",
                    "CO",
                    "CT",
                    "DC",
                    "DE",
                    "FL",
                    "GA",
                    "IL",
                    "IN",
                    "KS",
                    "KY",
                    "LA",
                    "MA",
                    "MD",
                    "MI",
                    "MN",
                    "MO",
                    "MS",
                    "NC",
                    "NJ",
                    "NV",
                    "NY",
                    "OH",
                    "OK",
                    "PA",
                    "SC",
                    "TN",
                    "TX",
                    "VA",
                    "WA",
                    "WI"
                )
            )
    } else {
        st_info
    }
}
