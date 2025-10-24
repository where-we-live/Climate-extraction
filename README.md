# Climate-extraction<br>
This repository is for extracting climate data, combining with survey data, generating PDI, as well as outputting dimensionality reduction and clustering results.<br>
<br>
The following folders contain:<br>
/src - all code and functions<br>
/data - all data that is written out<br>
/geodata - the geographic shape files needed to extract data<br>
/plots - all plots/graphs generated from the code<br>
<br>
## For point data extractions:<br>
To extract data, run the following script:<br>
extract_climate_point_data_meta<br>
<br>
Which will extract all climate data for daily, monthly, and yearly time points from 1979-present, for the stipulated locations as stated in:<br>
Climate-extraction/data/city_coordinates.csv<br>
<br>
The following functions are used:<br>
extract_climate_point_data_daily.R<br>
extract_climate_point_data_monthly.R<br>
extract_climate_point_data_yearly.R<br>
<br>
## For area data extractions and analysis:<br>
extract_terraclimate_terr
