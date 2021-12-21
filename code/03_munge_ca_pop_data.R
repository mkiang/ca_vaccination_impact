## 03_munge_ca_pop_data.R ----
##
## Here, we just take the USCB population estimates and clean it up to match
## the death data.

## Imports ----
library(tidyverse)
library(here)

## Read raw data ----
## Subset to total population (SEX == 0) and California
raw_df <- readr::read_csv(here::here("data_raw", "SC-EST2020-ALLDATA6.csv")) %>%
    dplyr::filter(
        STATE == "06",
        SEX == 0
    ) %>%
    dplyr::select(
        -SEX,
        -STATE,
        -SUMLEV,
        -REGION,
        -DIVISION
    )

## Make matching race/ethnicity categories ----
raw_df <- raw_df %>%
    dplyr::mutate(race = dplyr::case_when(
        RACE == 1 ~ "white",
        RACE == 2 ~ "black",
        RACE == 4 ~ "asian",
        TRUE ~ "other"
    ))

pop_df <- dplyr::bind_rows(
    raw_df %>%
        dplyr::filter(ORIGIN == 1),
    raw_df %>%
        dplyr::filter(ORIGIN == 2) %>%
        dplyr::mutate(race = "hispanic")
) %>%
    dplyr::select(
        -ORIGIN,
        -RACE
    )

## Double up race to make an "all" category ----
pop_df <- dplyr::bind_rows(
    pop_df,
    pop_df %>%
        dplyr::mutate(race = "all")
)

## Collapse into 5-year age bins ----
pop_df <- pop_df %>%
    dplyr::mutate(age = (cut(AGE,
        breaks = c(0, seq(5, 85, 5), 150),
        right = FALSE,
        include.lowest = TRUE,
        labels = FALSE
    ) - 1) * 5) %>%
    dplyr::select(-AGE) %>%
    dplyr::group_by(NAME, age, race) %>%
    dplyr::summarize_all(sum)

## Pivot to long format ----
pop_df_long <- pop_df %>%
    dplyr::select(state = NAME, age, race, dplyr::starts_with("POPESTIMATE")) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
        cols = dplyr::starts_with("POPESTIMATE"),
        names_to = "year"
    ) %>%
    dplyr::mutate(year = as.integer(substr(year, 12, 15))) %>%
    dplyr::filter(year > 2000) %>%
    dplyr::arrange(race, year, age)

## Save ----
saveRDS(pop_df_long, here::here("data", "ca_pops_by_race.RDS"))
