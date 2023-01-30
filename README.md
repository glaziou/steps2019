# steps2019
2019 STEPS survey in French Polynesia


## Folders
- csv: datasets with timestamp included in file names 
- input: raw data and dictionaries
- output: various output tables and figures
- data: latest version datasets in R binary format
- html: report pages
- R: R and R markdown scripts


## Report generation
In an R console, type:

source('R/repMaster.R°)

This script runs the following sequence:
- preprocess data
- generate an html page describing the sample (in folder html)
- generate 3 html pages for each of Step 1, 2 and 3
