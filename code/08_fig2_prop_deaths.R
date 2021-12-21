## Imports ----
library(here)
library(tidyverse)
source(here("code", "mk_nytimes.R"))

## Data ----
prop_df <- readRDS(here("data_private", "proportion_vaccinated.RDS"))

## Plot ----
f2 <- ggplot(prop_df, 
       aes(x = date_start, 
           y = covid_prop,
           size = covid_deaths,
           color = race_cat,
           fill = race_cat
           )) + 
    geom_hline(data = prop_df %>% 
                   filter(date == min(date)),
               aes(yintercept = pop_prop),
               linetype = "dashed",
               color = "black",
               alpha = .7) + 
    geom_point(alpha = .15) + 
    geom_ribbon(aes(ymin = lm_lower,
                    ymax = lm_upper),
                size = 1,
                color = NA, 
                alpha = .35) + 
    geom_line(aes(y = lm_mean),
              size = 1, 
              alpha = .9) + 
    facet_grid(age_cat ~ race_cat) + 
    scale_size_area("Number of COVID-19 deaths",
                    breaks = c(10, 100, 500, 1000, 1500),
                    labels = c("<10", "100", "500", "1000", "1500")) +
    scale_y_continuous("Proportion of deaths (weekly)",
                       expand = c(0, .02),
                       breaks = seq(0, 1, .25),
                       labels = c("0.0", "", "0.5", "", "1.0")) + 
    scale_x_date(
        NULL,
        breaks = scales::date_breaks(width = "4 months"),
        labels = scales::label_date_short(),
        limits = c(as.Date("2020-03-01"), as.Date("2021-08-01"))
    ) +
    scale_color_brewer("Race/Ethnicity", palette = "Dark2", guide = "none") + 
    scale_fill_brewer("Race/Ethnicity", palette = "Dark2", guide = "none") + 
    mk_nytimes(legend.position = "bottom",
               axis.text.y = element_text(vjust = c(0, .5, .5, .5, 1))) + 
    geom_vline(xintercept = as.Date("2021-01-13"), 
               alpha = .5,
               linetype = "dotted")

## Save ----
ggsave(
    filename = here("plots", "fig02_prop_of_deaths.eps"),
    plot = f2,
    width = 9.5,
    height = 6,
    scale = 1,
    device = cairo_ps
)
ggsave(
    filename = here("plots", "fig02_prop_of_deaths.jpg"),
    plot = f2,
    width = 9.5,
    height = 6,
    scale = 1,
    dpi = 1200
)

## Save the data ----
write_csv(prop_df %>% 
              select(-covid_deaths), 
          here("output", "fig2_data.csv"))
saveRDS(prop_df %>% 
            select(-covid_deaths), 
        here("output", "fig2_data.RDS"))
