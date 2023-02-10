# steps2019
2019 STEPS survey in French Polynesia


## Folders
- csv: datasets with timestamp included in file names 
- data: latest version datasets in R binary format
- doc: documentation including survey questionnaire
- input: raw data and dictionaries
- html: report pages
- output: various output tables and figures
- R: R and R markdown scripts


## Report web pages (in folder html)
- samplingDescription.html Description of the sample characteristics
- step1.html Results from questionnaire (Step 1)
- step2.html Results from physical and biological measures, adjusted for sampling design (Steps 2 and 3)


## Report generation
In an R console, type:

source('R/repMaster.R°)

This script runs the following sequence:
- preprocess raw data
- creates and loads utility functions and cleaned report data
- generate an html page describing the sample (in folder html)
- generate 3 html pages for each of Step 1, 2 and 3
