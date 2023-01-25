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

steps[, codeloc := `geo_location:I1`]

steps[codeloc==6, `geo_location:I2`]
steps[codeloc==51, `geo_location:I2`]
steps[codeloc==6, codeloc := 53]  # assume I2 is correct
steps[codeloc==51, codeloc := 53] # assume I2 is correct

steps <- merge(steps, loc, by.x = 'codeloc', by.y = 'code', all.x = TRUE)
steps[`geo_location:I2`=='Va', .(codeloc, commune)]

sum(is.na(steps$commune))==0

steps[, table(archipel)]


#' clean-up age groups
#' 
steps[, table(`step1:demographics:age`)]
steps[, table(`step1:demographics:agerange`)]
steps[, table(`step1:demographics:agerange`, `step1:demographics:age`)] # looks OK
steps[, age := `step1:demographics:age`]
steps[, agegroup := `step1:demographics:agerange`]

# 9 participants aged 70 (not eligible)
steps[age==70, .(codeloc, commune, age, agegroup)]
steps[, eligible := TRUE]
steps[age==70, eligible := FALSE]


# 382 participants with no age/agegroup info: no consent
steps[agegroup=='', .(codeloc, commune, age, agegroup, 
                       `step1:demographics:DOB:C2a`,
                       `step1:demographics:DOB:C2b`,
                       `step1:demographics:DOB:C2c`,
                       `step1:demographics:C2`,
                       `step1:demographics:C3`,
                       `consent_language_name:I5`,
                       `consent_language_name:I6`,
                       `consent_language_name:I8`,
                       `consent_language_name:I9`,
                       `consent_language_name:I10`)]
steps[agegroup=='', eligible := FALSE]


#' Recruitment by stratum
#' 
steps[, .N]
steps[, .N, by=eligible] # age documented and in (18-69)
steps[eligible==F, table(archipel)]
steps[eligible==T, table(agegroup, archipel)]








