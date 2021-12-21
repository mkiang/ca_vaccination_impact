## Imports ----
library(tidyverse)
library(here)
library(ggrepel)
source(here("code", "mk_nytimes.R"))

## Data ----
age_std_rates <-
    readRDS(here("data_private", "joinpoint_results.RDS")) %>%
    filter(time_period == "vaccine",
           age_group != "under_40")

## Plotting ----
f1 <- ggplot(
    age_std_rates,
    aes(
        x = date_start,
        y = modeled_rate,
        ymin = modeled_rate - 1.96 * (model_se),
        ymax = modeled_rate + 1.96 * (model_se),
        color = race_cat,
        fill = race_cat
    )
) +
    geom_ribbon(color = NA,
                alpha = .3) +
    geom_point(
        data = age_std_rates %>%
            group_by(race, death_type, age_group, segment) %>%
            filter(week_from_start == min(week_from_start),
                   segment > 0),
        aes(alpha = slope_change_pval < .05)
    ) +
    geom_line(alpha = .9) +
    facet_grid(age_cat ~ race_cat, scales = "free_y") +
    scale_y_continuous("Smoothed age-standardized COVID-19 mortality rate (per 100,000)",
                       expand = c(0, .5)) +
    scale_x_date(
        NULL,
        breaks = c(
            as.Date("2021-01-13"),
            as.Date("2021-03-01"),
            as.Date("2021-05-01"),
            as.Date("2021-07-01")
        ),
        labels = c("Jan 13\n2021",
                   "Mar 1",
                   "May 1",
                   "Jul 1"),
        limits = c(as.Date("2021-01-13"), as.Date("2021-08-01"))
    ) +
    scale_color_brewer("Race/Ethnicity", palette = "Dark2", guide = "none") +
    scale_fill_brewer("Race/Ethnicity", palette = "Dark2", guide = "none") +
    scale_alpha_manual(
        "Change in slope",
        values = c(.25, 1),
        labels = c("Not statistical sig.",
                   "Statistically sig.")
    ) +
    mk_nytimes(legend.position = "bottom", 
               axis.text.x = element_text(hjust = c(.3, 0, 0, 0)))

## Save ----
ggsave(
    filename = here("plots", "fig01_age_groups_std_rates.eps"),
    plot = f1,
    width = 9.5,
    height = 6,
    scale = 1,
    device = cairo_ps
)
ggsave(
    filename = here("plots", "fig01_age_groups_std_rates.jpg"),
    plot = f1,
    width = 9.5,
    height = 6,
    scale = 1,
    dpi = 1200
)

## Save actual data ----
save_df <- age_std_rates %>%
    select(
        death_cat,
        time_cat,
        race_cat,
        age_cat,
        date_start,
        date,
        week_from_start,
        modeled_rate,
        model_se,
        segment,
        wpc,
        wpc_lower,
        wpc_upper,
        wpc_pval,
        n_obs,
        n_params,
        joinpoint,
        jp_lower,
        jp_upper,
        slope_change,
        slope_change_se,
        slope_change_pval
    )
write_csv(save_df, here("output", "fig1_data.csv"))
saveRDS(save_df, here("output", "fig1_data.RDS"))
