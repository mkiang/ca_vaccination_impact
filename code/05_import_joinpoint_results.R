## 05_import_joinpoint_results.R ----
##
## After we calculated age standardized rates (04 file), we run a joinpoint
## regression (using an external progra -- NCI's Joinpoint Regression Program).
## This file collects all the results from the joinpoint regression program.

## Imports ----
library(here)
library(fs)
library(tidyverse)
source(here::here("code", "utils.R"))

## Data ----
wpc_df <-
    readr::read_csv(here::here("joinpoint", "age_standardized_rates.csv")) %>%
    dplyr::left_join(
        import_jp(here::here(
            "joinpoint", "age_standardized_rates.data.txt"
        )) %>%
            dplyr::select(-rate_trunc,
                          -joinpoints, 
                          -final_selected_model, 
                          -flag) %>%
            dplyr::rename(modeled_rate = model, 
                          model_se = standard_error)
    ) %>%
    dplyr::left_join(
        import_jp(here::here(
            "joinpoint", "age_standardized_rates.wpc.txt"
        )) %>%
            dplyr::select(
                time_period,
                age_group,
                death_type,
                race,
                segment,
                week_from_start = segment_start,
                wpc,
                wpc_lower = wpc_95_percent_lcl,
                wpc_upper = wpc_95_percent_ucl,
                wpc_pval = p_value
            )
    ) %>%
    dplyr::left_join(
        import_jp(here::here(
            "joinpoint", "age_standardized_rates.modelestimates.txt"
        )) %>%
            dplyr::transmute(
                time_period,
                age_group,
                death_type,
                race,
                segment,
                n_obs = number_obs,
                n_params = number_param,
                model_df = df,
                model_sse = sse,
                model_mse = mse,
                model_corr = auto_corr,
                joinpoint = joinpoint,
                jp_lower = joinpoint_95_percent_lcl,
                jp_upper = joinpoint_95_percent_ucl,
                slope_estimate,
                slope_se = slope_std_error,
                slope_pval = slope_p_value,
                slope_change = slope_chg_estimate,
                slope_change_se = slope_chg_std_error,
                slope_change_pval = slope_chg_p_value
            )
    ) %>%
    dplyr::left_join(
        import_jp(here::here(
            "joinpoint", "age_standardized_rates.awpc.txt"
        )) %>%
            dplyr::filter(awpc_index == "Full Range") %>%
            dplyr::select(
                time_period,
                age_group,
                death_type,
                race,
                week_from_start = start_obs,
                average_wpc = awpc,
                average_wpc_lower = awpc_c_i_low,
                average_wpc_upper = awpc_c_i_high,
                average_wpc_pval = p_value
            )
    ) %>%
    zoo::na.locf(na.rm = FALSE) %>%
    dplyr::mutate(
        race_cat = factor(
            race,
            levels = c("all", "white", "black", "hispanic", "asian"),
            labels = c("All", "White", "Black", "Latino", "Asian"),
            ordered = TRUE
        ),
        death_cat = factor(
            death_type,
            levels = c("all_deaths", "covid", "non_covid"),
            labels = c("Any cause", "COVID-19", "Non-COVID-19"),
            ordered = TRUE
        ),
        age_cat = factor(
            age_group,
            levels = c("all_ages", "65_and_up", "40_to_64", "under_40"),
            labels = c("All ages", "65 and over", "40 to 64", "Under 40"),
            ordered = TRUE
        ),
        time_cat = factor(
            time_period,
            levels = c("full", "vaccine"),
            labels = c("Full time period", "After Jan 13, 2021"),
            ordered = TRUE
        )
    )
saveRDS(wpc_df, here::here("data_private", "joinpoint_results.RDS"))
