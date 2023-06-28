# Geographic targeting of COVID-19 testing to maximize detection in Los Angeles County 

This repository includes code and data accompanying the paper “Jia KM, Kahn R, Fisher R, Balter S, Lipsitch M. (2023). Geographic targeting of COVID-19 testing to maximize detection in Los Angeles County.”

## Data
Testing and vaccination data are from L.A. County Department of Public Health. 

| Field	        | Description   |
| ------------- | ------------- |
| geo_merge  | Identifier of the city/community |
| epi_week	 | Indicator of the week (Date) |
| time	| Indicator of the week (Integer) |
| total_tests	| Total number of tests during the week |
| new_pos_t	| Total number of positive tests during the week |
| cum_pos_tests |	Cumulative number of positive tests |
| pct_cum_doses	| Cumulative proportion of residents (aged 12 years and older) who were fully vaccinated | 
| population	 | Population estimate |

Note that in this publicly available dataset, cells containing a value of 1 to 11 were suppressed.

## Codes
To reproduce figures and outputs, R codes for data processing, model development and evaluation are available in the `R` folder.

## Acknowledgements

Funding: Surveillance efforts were supported by a cooperative agreement awarded by the Department of Health and Human Services (HHS) with funds made available under the Coronavirus Preparedness and Response Supplemental Appropriations Act, 2020 (P.L. 116-123); the Coronavirus Aid, Relief, and Economic Security Act, 2020 (the “CARES Act”) (P.L. 116-136); the Paycheck Protection Program and Health Care Enhancement Act (P.L. 116-139); the Consolidated Appropriations Act and the Coronavirus Response and Relief Supplement Appropriations Act, 2021 (P.L. 116-260) and/or the American Rescue Plan of 2021 (P.L. 117-2). 

Acknowledgements: We thank Phoebe Danza, Remy Landon, Chelsea Foo, the ACDC Morning Data Team (Dr. Katie Chun, Harry Persaud, Tuff Witarama, Mark Johnson, Laureen Masai, Zoe Thompson, Dr. Ndifreke Etim), and Dr. Paul Simon from the L.A. County Department of Public Health for providing data and valuable support on this work.
