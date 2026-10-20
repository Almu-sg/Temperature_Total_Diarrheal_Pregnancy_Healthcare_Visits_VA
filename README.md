# Impacts of Temperature on Total, Diarrheal, and Pregnancy-Related Healthcare Visits in Virginia

Evaluation of changes in total, pregnancy-related, and diarrheal-related emergency department and urgent care visits with temperature and precipitation in Virginia, USA between 2015 and 2023.

Data and scripts for analyses presented in:

_Almudena Sanz Gutierrez, Meredith Jagger, Ira Mehta, Alasdair Cohen, Ben Brewer, Bianca Corpuz, Samarth Swarup, Benjamin F. Zaitchik, Julia Gohlke. Impacts of Temperature and Precipitation on Total, Diarrhea-, and Pregnancy-Related Healthcare Visits in Virginia, 2015-2023._


## Repository content
DATA folder:
- _2020_zcta_census_: shapefile of US zctas.
- _13042026_lookup match_Meredith.xlsx_: manually found updated zipcodes.
- _new Virginia weather data_ folder: home-based exposure data from Department of Energy’s Daymet (Daymet).
- _Synthetic Population data_ folder: activity-based exposure data from the High Resolution Rapid Refresh (HRRR)
- **Note** that healthcare visit data cannot be shared per Data Use Agreement with Virginia Department of Health.

Scripts:
- _01_Data_Cleaning_: processing of data sources into comprehensive dataset for analysis.
- _02_Data_Analyses_: all main analyses presented in the manuscript.

_findmin.R_: pre-specified function to calculate MMT from dlnm.
