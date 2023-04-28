<!-- README.md is generated from README.Rmd. Please edit that file -->

# mumhquarterly

<!-- badges: start -->
<!-- badges: end -->

This code is published as part of the NHSBSA Official Statistics team's commitment to open code and transparency in how we produce our publications. The mumhquarterly package is owned and maintained by the Official Statistics team.

# Introduction

The goal of mumhquarterly is to bring together all of the functions required for production of the [Medicines Used in Mental Health quarterly Official Statistics publication](https://www.nhsbsa.nhs.uk/statistical-collections/medicines-used-mental-health-england) by the NHSBSA with accompanying documentation in line with reproducible analytical pipeline (RAP) best practice. These functions are designed to be used as part of the [MUMH quarterly RAP](https://github.com/nhsbsa-data-analytics/mumh-quarterly-rap), also available on GitHub.

This package is a work in progress and will be replaced by several new packages designed to hold functions used in the creation of our Official Statistics publications. The current mumhquarterly package does not contain unit testing, and this will not be added due to the move to the new packages.

## Installation

You can install the development version of mumhquarterly from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nhsbsa-data-analytics/mumhquarterly")
```
You can also view the [source code for the mumhquarterly package](https://github.com/nhsbsa-data-analytics/mumhquarterly) on GitHub.

# 

## Functions guide

All of the functions can be found in the [`R` folder](https://github.com/nhsbsa-data-analytics/mumhquarterly/tree/main/R). 

These functions cover several different task areas. Each has an example within the documentation at the top of each function's `.R` file. Below is a guide to what the functions are used for:

1. Functions for getting and analysing the required data, including `available_data()`, `get_dispensing_days()`, `ons_national_pop()`, `create()`, `import_data()`

2. Functions for saving and formatting outputs such as charts and excel summary tables, including `covid_chart_hc()`, `create_metadata()`, `create_wb()`, `format_data()`, `format_number()`, `group_chart_hc()`, `infoBox_border()`, `infoBox_no_border()`, `save_data()`, `write_sheet()`

3. Miscellaneous functions, including `mumh_options()`, `notin()`, `apply_sdc()`, `utils()`

# Contributing

Contributions are not currently being accepted for the mumhquarterly package. If this changes, a contributing guide will be made available.

# License

The mumhquarterly package repository, including associated documentation, is released under the MIT license. Details can be found in the `LICENSE` file.
