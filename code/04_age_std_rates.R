## 04_age_std_rates.R ----
##
## From the death data (01 and 02 files) and the population data (03 file),
## we can now calculate the age-standardized death rates across different
## groups and age ranges.

## Imports ----
library(here)
library(fs)
library(tidyverse)
source(here::here("code", "utils.R"))

## Data ----
death_df <-
    readRDS(here::here("data_private", "summarized_weekly_deaths.RDS")) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
        educ == "all",
        !is.na(age),
        race != "other",
        !is.na(week_from_start),
        date <= as.Date("2021-08-01")
    ) %>%
    dplyr::select(-educ)
pop_df <- readRDS(here::here("data", "ca_pops_by_race.RDS")) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year >= 2019) %>%
    dplyr::rename(pop = value)

## Extrapolate out to 2021 ----
pop_df <- dplyr::bind_rows(
    pop_df,
    pop_df %>%
        dplyr::group_by(state, age, race) %>%
        dplyr::mutate(pop_change = pop[year == 2020] / pop[year == 2019]) %>%
        dplyr::arrange(state, race, year, age) %>%
        dplyr::filter(year %in% 2020) %>%
        dplyr::mutate(pop_2021 = pop *
            pop_change^(as.numeric(
                difftime(
                    max(death_df$date),
                    as.Date("2020-07-01")
                )
            ) / 365)) %>%
        dplyr::mutate(
            year = 2021,
            pop = pop_2021
        ) %>%
        dplyr::select(
            -pop_2021,
            -pop_change
        )
) %>%
    dplyr::arrange(state, race, year, age)

## Make a weekly, linearly interpolated population column to test sensitivity ----
pop_combos <- pop_df %>%
    dplyr::ungroup() %>%
    dplyr::select(-pop, -year) %>%
    dplyr::distinct()

interpolated_data <- vector("list", NROW(pop_combos))
for (i in 1:NROW(pop_combos)) {
    state_x <- pop_combos$state[i]
    race_x <- pop_combos$race[i]
    age_x <- pop_combos$age[i]

    holder <- NULL
    for (y in 2019:2020) {
        start_date <- as.Date(sprintf("%s-07-01", y))
        end_date <- as.Date(ifelse((y + 1) == 2021,
            as.character(max(death_df$date) + 1),
            sprintf("%s-07-01", y + 1)
        ))
        seq_date <- seq(start_date, end_date - 1, by = 1)

        start_pop <- pop_df %>%
            dplyr::filter(
                state == state_x,
                race == race_x,
                age == age_x,
                year == y
            ) %>%
            dplyr::pull(pop)

        end_pop <- pop_df %>%
            dplyr::filter(
                state == state_x,
                race == race_x,
                age == age_x,
                year == y + 1
            ) %>%
            dplyr::pull(pop)

        seq_pop <-
            round(seq(start_pop, end_pop, along.with = seq_date))

        holder <- dplyr::bind_rows(
            holder,
            dplyr::tibble(
                state = state_x,
                race = race_x,
                age = age_x,
                date = seq_date,
                pop = seq_pop
            )
        )
    }

    interpolated_data[[i]] <- holder
}
interpolated_data <- dplyr::bind_rows(interpolated_data) %>%
    dplyr::left_join(death_df %>% dplyr::select(date, date_start, week_from_start) %>% dplyr::distinct()) %>%
    dplyr::filter(
        date >= min(death_df$date),
        date <= max(death_df$date),
        !is.na(week_from_start)
    )

## Analytic data ----
analytic_df <- dplyr::left_join(
    interpolated_data,
    death_df,
    by = c("race", "age", "date", "date_start", "week_from_start")
) %>%
    dplyr::mutate_at(
        .vars = dplyr::vars(covid:all_deaths),
        .funs = function(x) {
              ifelse(is.na(x), 0, x)
          }
    ) %>%
    narcan::add_std_pop()

## Age-specific rates
age_spec_rates <- analytic_df %>%
    narcan::calc_asrate_var(covid, covid) %>%
    dplyr::left_join(analytic_df %>%
        narcan::calc_asrate_var(non_covid, non_covid)) %>%
    dplyr::left_join(analytic_df %>%
        narcan::calc_asrate_var(all_deaths, all_deaths)) %>%
    dplyr::arrange(state, race, date, age)

## Age-standardized rates ----
age_std_rates <- dplyr::bind_rows(
    calculate_age_std_rates(age_spec_rates) %>%
        dplyr::mutate(age_group = "all_ages"),
    calculate_age_std_rates(age_spec_rates %>%
        dplyr::filter(dplyr::between(age, 0, 39))) %>%
        dplyr::mutate(age_group = "under_40"),
    calculate_age_std_rates(age_spec_rates %>%
        dplyr::filter(dplyr::between(age, 40, 64))) %>%
        dplyr::mutate(age_group = "40_to_64"),
    calculate_age_std_rates(age_spec_rates %>%
        dplyr::filter(dplyr::between(age, 65, 100))) %>%
        dplyr::mutate(age_group = "65_and_up")
)

## Add number of deaths back in ----
n_deaths <- dplyr::bind_rows(
    death_df %>%
        dplyr::group_by(race, date, week_from_start) %>%
        dplyr::select(
            -age,
            -date_start
        ) %>%
        dplyr::summarize_all(sum) %>%
        tidyr::pivot_longer(covid:all_deaths,
            names_to = "death_type",
            values_to = "n_deaths"
        ) %>%
        dplyr::mutate(age_group = "all_ages"),
    death_df %>%
        dplyr::filter(dplyr::between(age, 0, 39)) %>%
        dplyr::group_by(race, date, week_from_start) %>%
        dplyr::select(
            -age,
            -date_start
        ) %>%
        dplyr::summarize_all(sum) %>%
        tidyr::pivot_longer(covid:all_deaths,
            names_to = "death_type",
            values_to = "n_deaths"
        ) %>%
        dplyr::mutate(age_group = "under_40"),
    death_df %>%
        dplyr::filter(dplyr::between(age, 40, 64)) %>%
        dplyr::group_by(race, date, week_from_start) %>%
        dplyr::select(
            -age,
            -date_start
        ) %>%
        dplyr::summarize_all(sum) %>%
        tidyr::pivot_longer(covid:all_deaths,
            names_to = "death_type",
            values_to = "n_deaths"
        ) %>%
        dplyr::mutate(age_group = "40_to_64"),
    death_df %>%
        dplyr::filter(dplyr::between(age, 65, 100)) %>%
        dplyr::group_by(race, date, week_from_start) %>%
        dplyr::select(
            -age,
            -date_start
        ) %>%
        dplyr::summarize_all(sum) %>%
        tidyr::pivot_longer(covid:all_deaths,
            names_to = "death_type",
            values_to = "n_deaths"
        ) %>%
        dplyr::mutate(age_group = "65_and_up"),
)

age_std_rates <- age_std_rates %>%
    dplyr::left_join(n_deaths)

## Make a version for joinpoint regression with 0's replaced by small numbers ----
age_std_rates <- age_std_rates %>%
    dplyr::mutate(
        rate_trunc = ifelse(rate == 0, .01, rate),
        var_trunc = ifelse(var == 0, .05, var)
    ) %>%
    dplyr::arrange(age_group, race, death_type, date)

## Save ----
saveRDS(age_std_rates, here::here("data_private", "age_standardized_rates.RDS"))
saveRDS(age_spec_rates, here::here("data_private", "age_specific_rates.RDS"))
readr::write_csv(
    dplyr::bind_rows(
        age_std_rates %>%
            dplyr::mutate(time_period = "full"),
        age_std_rates %>%
            dplyr::filter(date_start >= as.Date("2021-01-13")) %>%
            dplyr::mutate(time_period = "vaccine")
    ) %>%
        dplyr::arrange(
            time_period,
            dplyr::desc(age_group),
            death_type,
            race,
            date
        ) %>%
        dplyr::filter(
            race != "other",
            death_type == "covid"
        ),
    here::here("joinpoint", "age_standardized_rates.csv")
)
