# EggCase_Distribution

Here is a series of scripts to extract enviromental variables, select depth layers from 4D netCDFs, fit BRT models, predict bycatch accurrence and abundance and define Potential Spawning Areas (PSAs).
Cite using the following doi:
[![DOI](https://zenodo.org/...)](https://zenodo.org/doi/...)

### Scripts available
*1_prep_df*: Data organising.

*2_enviro*: Enables downloading data from CMEMS; extracting the enviromental data to the surveyed points; select depth layers in 4D enviro rasters; and generating stacks.

*3_Subsetting_Checking_data*: Conduct pre-fitting checks on data.

*4_Fitting_brt*: Enables fitting a brt models to predict egg case bycatch ocurrence and abundance; use bootstrap methods to deal with stochasticity; make spatial predictions; determine PSAs

*5_calcPSAs*:  Analyse the overlap of PSAs with fishing effort, marine protected areas (MPAs), trawling exclusion zones (TEZs), and important shark and ray areas (ISRAs); determine significance of seasonal changes in predicted bycatch CPUE and PSAs. 

*PSA_polygon*: resulting overall PSA polygon as a shapefile.

*fig*: Create data checking plots; study area map; predicted bycatch CPUE weighted by occurrence maps; egg case density maps; plotting overlap between PSAs and MPAs, TEZs or ISRAs; and maps of enviromental predictors.

*fun*: Generate a new function to extract enviromental data from 4D rasters (lon, lat, depth, time) or 3D (lon, lat, depth) to a certain depth value.
