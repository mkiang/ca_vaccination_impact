## 06_fit_polynomial_regressions.R ----
##
## Here, we fit a flexible line to the proportion vaccinated by race/ethnicity
## over time. These model predictions will be used for Figure 2.

## Imports ----
library(here)
library(tidyverse)

## Data ----
age_spec_rates <-
    readRDS(here::here("data_private", "age_specific_rates.RDS")) %>%
    dplyr::filter(
        date_start >= as.Date("2020-03-01"),
        race != "other",
        !is.na(week_from_start)
    ) %>%
    dplyr::select(race,
        age,
        date,
        date_start,
        age,
        covid_deaths = covid,
        pop
    )

## Double up so we can do all ages ----
age_spec_rates <- dplyr::bind_rows(
    age_spec_rates,
    age_spec_rates %>%
        dplyr::mutate(age = 999)
)

## Clean up and collapse down ----
age_spec_rates <- age_spec_rates %>%
    dplyr::mutate(age_bin = dplyr::case_when(
        dplyr::between(age, 0, 39) ~ "under40",
        dplyr::between(age, 40, 64) ~ "40-64",
        dplyr::between(age, 65, 110) ~ "65+",
        age == 999 ~ "all_ages"
    )) %>%
    dplyr::mutate(
        age_cat = factor(
            age_bin,
            levels = c("all_ages", "65+", "40-64", "under40"),
            labels = c("All", "65 and over", "40 to 64", "Under 40"),
            ordered = TRUE
        ),
        race_cat = factor(
            race,
            levels = c("all", "white", "black", "hispanic", "asian"),
            labels = c("All", "White", "Black", "Latino", "Asian"),
            ordered = TRUE
        )
    )

summarized_df <- age_spec_rates %>%
    dplyr::group_by(race_cat, race, age_bin, age_cat, date, date_start) %>%
    dplyr::summarize(
        age = min(age),
        covid_deaths = sum(covid_deaths),
        pop = sum(pop)
    ) %>%
    dplyr::ungroup()

## Calculate proportions ----
prop_df <- dplyr::left_join(
    summarized_df %>%
        dplyr::filter(race != "all"),
    summarized_df %>%
        dplyr::filter(race == "all") %>%
        dplyr::select(age_bin, date, all_covid = covid_deaths, all_pop = pop)
) %>%
    dplyr::mutate(
        covid_prop = covid_deaths / all_covid,
        pop_prop = pop / all_pop
    ) %>%
    dplyr::group_by(race, age_bin) %>%
    dplyr::arrange(race, age_bin, date) %>%
    dplyr::mutate(
        week_from_start = as.integer((date - min(date))) / 7 + 1,
        covid_prop = ifelse(is.nan(covid_prop), 0, covid_prop)
    )

## Add polynomial regression predictions ----
combos <- prop_df %>%
    dplyr::ungroup() %>%
    dplyr::select(
        race_cat,
        age_cat
    ) %>%
    dplyr::distinct()

results_holder <- vector("list", NROW(combos))
for (i in 1:NROW(combos)) {
    sub_df <- prop_df %>%
        dplyr::filter(
            race_cat == combos$race_cat[i],
            age_cat == combos$age_cat[i]
        ) %>%
        dplyr::ungroup()

    lm_fit <- stats::lm(
        covid_prop ~ 1 + week_from_start +
            I(week_from_start^2) +
            I(week_from_start^3) +
            I(week_from_start^4),
        data = sub_df,
        weight = all_covid
    )
    lm_pred <- stats::predict(lm_fit,
        se.fit = TRUE
    )

    results_holder[[i]] <- sub_df %>%
        dplyr::mutate(
            lm_mean = lm_pred$fit,
            lm_se = lm_pred$se.fit
        ) %>%
        dplyr::mutate(
            lm_lower = lm_mean - 1.96 * lm_se,
            lm_upper = lm_mean + 1.96 * lm_se
        )
}

## Save ----
prop_df <- dplyr::bind_rows(results_holder)
saveRDS(prop_df, here::here("data_private", "proportion_vaccinated.RDS"))
