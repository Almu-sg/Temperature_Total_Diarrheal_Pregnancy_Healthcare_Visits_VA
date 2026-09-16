# Impacts of Temperature on Total, Diarrheal, and Pregnancy-Related Healthcare Visits in Virginia

Evaluation of changes in total, pregnancy-related, and diarrheal-related emergency department and urgent care visits with temperature and precipitation in Virginia, USA between 2015 and 2023.

CITATION OF PAPER WITH LINK HERE

## Repository content
DATA folder:
- _2020_zcta_census_: shapefile of US zctas.
- _13042026_lookup match_Meredith.xlsx_: manually found updated zipcodes.
- _new Virginia weather data_ folder: home-based exposure data from Department of Energy’s Daymet (Daymet). Refer ti DataDictionary inside folder.
- _Synthetic Population data_ folder: activity-based exposure data from the High Resolution Rapid Refresh (HRRR)
- **Note** that healthcare visit data cannot be shared per our Data Use Agreement with Virginia Department of Health.

Scripts:
- _01_Data_Cleaning_: processing of data sources into comprehensive dataset for analysis.
- _02_Data_Analyses_: all main analyses presented in the manuscript.

_findmin.R_: pre-specified function to calculate MMT from dlnm.
