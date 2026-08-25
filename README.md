# Codes and processed data for the study on Excess Deaths during Philippine War on Drugs

## R Codes
1. Selection of knots for drug war period and degrees of freedom for seasonality per province based on lowest qAIC
2. Classical ITS modeling using quasi Poisson regression and calculation of excess deaths
3. Selecting weeks with significantly positive firearm deaths for conservative estimation
4. Creating table of excess deaths
5. Creating figures 5 & 6

## Data (no raw data included)
1. Dates and ISO weeks table ("drugwar_isoweeks.csv")
2. All qAIC values for selection of knots and degrees of freedom ("qaic_allspecs_prov.rds)
3. Best models based on lowest qAIC ("qaic_lowest_specs_prov.rds")
4. Weeks with higher-than-expected firearm deaths ("fira_signifweeks.csv")
5. Weeks of natural-cause deaths that co-incide with higher-than-expected firearm deaths weeks ("others_signifweeks.csv")
6. Table of excess deaths ("summary_kills_prov-type.csv")
7. Other datasets are too large to upload. Can be requested via email: paulcarloschua@gmail.com
