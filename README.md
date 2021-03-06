
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Recent Shifts in Racial/Ethnic Disparities in COVID-19 Mortality in the Vaccination Period in California

<img src="./plots/fig01_age_groups_std_rates.jpg" width="700px" style="display: block; margin: auto;" />

This is reproducible code for our recent *Journal of General Internal Medicine* paper, [“Recent Shifts in
Racial/Ethnic Disparities in COVID-19 Mortality in the Vaccination
Period in California”](https://link.springer.com/article/10.1007/s11606-021-07380-6), which uses **restricted-access**,
decedent-level death certificate data from the California Department of
Public Health (CDPH) to systematically examine racial/ethnic disparities
in COVID-19 deaths during the vaccination period. The full citation is:

> Riley AR†, Kiang MV†, Chen Y-H, Bibbins-Domingo K, & Glymour MM, Recent shifts in racial/ethnic disparities in COVID-19 mortality in the vaccination period in California, Journal of General Internal Medicine (February 2022), doi: 10.1007/s11606-021-07380-6. †First authors contributed equally.

## Issues

Please submit issues [via
Github](https://github.com/mkiang/ca_vaccination_impact/issues) or via
email.

## Important note about reproducibility

In accordance with our data use agreement with the CDPH Vital
Statistics, we cannot share individual level data. When possible, we
provide aggregated data in cases where there are more than 10
observations. This restriction means this pipeline is not fully
reproducible without the restricted-access data.

## Software

All analyses are conducted using `R`, which can be [downloaded via
CRAN](https://cran.r-project.org/), and the Joinpoint Regression
Program, which can be [downloaded from the National Cancer
Institute](https://surveillance.cancer.gov/joinpoint/).

We also recommend the use of
[RStudio](https://www.rstudio.com/products/rstudio/download/) when
running `R`, which will allow users to take advantage of
[`renv`](https://rstudio.github.io/renv/index.html) for dependency
management.

# Analysis pipeline

The analysis pipeline is divided into three discrete steps.

In Step (1), we clean, subset, munge, and calculate mortality rates
using the raw (restricted-access) data. This results in a working
dataframe that contains the data necessary for the Joinpoint Regression
Program to fit our models of interest. These are held in the `01` to
`04` code files.

In Step (2), the joinpoint regressions are fit in an external program
([NCI Joinpoint Regression
Program](https://surveillance.cancer.gov/joinpoint/)) and the results
are exported. The `./joinpoint_analyses/age_std_rates.jps` file contains
our session information to reproduce our analysis and requires the
`./joinpoint_analyses/age_std_rates_long.csv` file generated from the
step above (potentially with noise added to small cells).

In Step (3), the resulting (exported) joinpoint files are combined into
a single file for plotting and to create tables. These are code files
`05` to `08`.

All files should be run sequentially.

# Authors

-   [Kirsten
    Bibbins-Domingo](https://profiles.ucsf.edu/kirsten.bibbins-domingo)
    (![Twitter](http://i.imgur.com/wWzX9uB.png):
    [@KBibbinsDomingo](https://twitter.com/KBibbinsDomingo))
-   [Yea-Hung Chen](https://yea-hung.rbind.io)
    (![Github](http://i.imgur.com/9I6NRUm.png):
    [yea-hung](https://github.com/yea-hung) \|
    ![Twitter](http://i.imgur.com/wWzX9uB.png):
    [@ch272n](https://twitter.com/ch272n))
-   [Maria Glymour](https://profiles.ucsf.edu/maria.glymour)
    (![Twitter](http://i.imgur.com/wWzX9uB.png):
    [@MariaGlymour](https://twitter.com/MariaGlymour))
-   [Mathew Kiang](https://mathewkiang.com)
    (![Github](http://i.imgur.com/9I6NRUm.png):
    [mkiang](https://github.com/mkiang) \|
    ![Twitter](http://i.imgur.com/wWzX9uB.png):
    [@mathewkiang](https://twitter.com/mathewkiang))
-   [Alicia
    Riley](https://sociology.ucsc.edu/about/directory-faculty.php?uid=ariley3)
    (![Twitter](http://i.imgur.com/wWzX9uB.png):
    [@aliciacita](https://twitter.com/aliciacita))
