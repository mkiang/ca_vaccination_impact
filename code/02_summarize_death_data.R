## 02_summarize_death_data.R ----
##
## Here, we read in the (now cleaned) decedent-level data, removing non-CA
## residents, and summarizing COVID-19 deaths by week.

## Imports ----
# library(lubridate)
library(here)
library(fs)
library(tidyverse)
source(here::here("code", "utils.R"))

## Constants ----
SAVE_DIR <- config::get("private_save_folder")
FIRST_WEEK <- config::get("first_week")

## Data ----
death_df <- readRDS(paste0(SAVE_DIR, "cleaned_line_data.RDS")) %>%
    tibble::as_tibble() %>%
    dplyr::filter(dod >= FIRST_WEEK)

## Subset to CA residents ----
death_df <- death_df %>%
    dplyr::filter(state_name == "CA") %>%
    dplyr::select(-state_name)

## Flag the death data ----
flagged_df <- death_df %>%
    flag_covid_death() %>%
    dplyr::mutate(
        non_covid = 1 - covid,
        all_deaths = 1
    )

## Collapse AIAN into Other ----
## Let's collapse AINA and Other
flagged_df <- flagged_df %>%
    dplyr::mutate(
        race = dplyr::case_when(
            race == "White" ~ "white",
            race == "Black" ~ "black",
            race == "Asian / Pacific Islander" ~ "asian",
            race == "Hispanic" ~ "hispanic",
            race == "Other" ~ "other",
            race == "American Indian / Native American" ~ "other"
        )
    )

## Double the data frame for the "all" categories ----
flagged_df <- dplyr::bind_rows(
    flagged_df,
    flagged_df %>%
        dplyr::mutate(race = "all")
)
flagged_df <- dplyr::bind_rows(
    flagged_df,
    flagged_df %>%
        dplyr::mutate(educ = "all")
)

## Now do it again for the over 25 ----
## "under25" is already done
flagged_df <- dplyr::bind_rows(
    flagged_df,
    flagged_df %>%
        dplyr::filter(
            age >= 25,
            educ != "all"
        ) %>%
        dplyr::mutate(educ = "over25")
)

## Bin into 5-year age groups ----
flagged_df <- flagged_df %>%
    dplyr::mutate(age_old = age) %>%
    dplyr::mutate(age = (cut(age_old,
        breaks = c(0, seq(5, 85, 5), 150),
        right = FALSE,
        include.lowest = TRUE,
        labels = FALSE
    ) - 1) * 5)

## Create weekly indicators
flagged_df$week_from_start <- floor(as.numeric(flagged_df$dod -
    min(flagged_df$dod)) / 7) + 1

## Summarize into weeks and 5-year age bins
weekly_df <- flagged_df %>%
    dplyr::group_by(week_from_start) %>%
    dplyr::mutate(
        date = max(dod),
        date_start = min(dod)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(race, educ, age, date_start) %>%
    dplyr::select(
        -id,
        -dod,
        -age_old,
        -sex,
        -county_name,
        -county_fip
    ) %>%
    dplyr::group_by(race, educ, age, date, date_start, week_from_start)

weekly_summarized_df <- weekly_df %>%
    dplyr::select(-ucod, -record_all) %>%
    dplyr::summarize_all(sum, na.rm = TRUE)

## Save ----
saveRDS(weekly_summarized_df, 
        here::here("data_private", "summarized_weekly_deaths.RDS"))
