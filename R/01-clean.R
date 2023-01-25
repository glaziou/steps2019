#' ---
#' title: Pre-process STEPS
#' author: Philippe Glaziou
#' date: 2023/01/25
#' output:
#'    html_document:
#'      mode: selfcontained
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      number_sections: true
#'      theme: flatly
#'      highlight: zenburn
#'      df_print: paged
#'      code_folding: hide
#' ---

#' Last updated: `r Sys.Date()`
#'
library(data.table)
library(here)


#' load data
#' 
steps <- fread(here('input/PF_STEPS2019_V3_2_1_results.csv')) # STEP 1
loc <- fread(here('input/loc.csv'))  # geo codes


#' clean-up locations
#' 
steps[, table(`geo_location:I1`)]
steps[, table(`geo_location:I2`)]
steps[, table(`geo_location:I2`, `geo_location:I1`)]
steps[`geo_location:I1`=='6']  # Tubuai
steps[`geo_location:I2`=='Va'] # Rangiroa

steps[, geocode := `geo_location:I1`]

steps[geocode==6, `geo_location:I2`]
steps[geocode==51, `geo_location:I2`]
steps[geocode==6, geocode := 53]  # assume I2 is correct
steps[geocode==51, geocode := 53] # assume I2 is correct

steps1 <- merge(steps, loc, by.x = 'geocode', by.y = 'code', all.x = TRUE)
dim(steps)[1]==dim(steps1)[1]
steps1[`geo_location:I2`=='Va', .(geocode, commune)]

sum(is.na(steps1$commune))==0

steps1[, table(archipel)]


#' clean-up age groups
#' 
steps1[, table(`step1:demographics:age`)]
steps1[, table(`step1:demographics:agerange`)]
steps1[, table(`step1:demographics:agerange`, `step1:demographics:age`)] # looks OK
# 9 participants aged 70 (not eligible)

steps1[, age := `step1:demographics:age`]
steps1[, agegroup := `step1:demographics:agerange`]


#' Recruitment by stratum
#' 









