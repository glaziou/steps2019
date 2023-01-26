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
library(haven)


#' load data
#' 
steps <- fread(here('input/PF_STEPS2019_V3_2_1_results.csv')) # STEP 1
loc <- fread(here('input/loc.csv'))  # geo codes
pop <- as.data.table(read_dta(here('input/ponderation_draft.dta')))



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
steps[, agegr := agegroup]
steps[agegr %in% c('45-59', '60-69'), agegr := '45-69']

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
steps[eligible==T, table(agegr, archipel)]



#' Include pop and calculate weights
#' 
s <- copy(steps)
steps[agegr=='18-29', agecat := 1]
steps[agegr=='30-44', agecat := 2]
steps[agegr=='45-69', agecat := 3]
steps[, sex := `step1:demographics:C1`]
steps[eligible==TRUE, table(sex)]
steps[eligible==TRUE, table(sex, agecat)]

pop[archipelago %in% c('Australes','Marquises'), archipel := archipelago]
pop[archipelago=='Société (Îles-du-vent)', archipel := 'IDV']
pop[archipelago=='Société (Îles-sous-le-vent)', archipel := 'ISLV']
pop[archipelago=='Tuamotu-Gambier', archipel := 'Tuamotu']
pop[, sex := gender]

steps <- merge(steps[eligible==TRUE], pop[,.(archipel, sex, agecat=age_cat, pop=N)], by=c('archipel','sex','agecat'), all.x=TRUE)
steps[, .(archipel, sex, agecat, pop)]


# 3 geo strata
steps[, gstratum := archipel]
steps[archipel %in% c('Australes','Marquises','Tuamotu'), gstratum := 'Autres']


# sample size by stratum
(out <- steps[, .N, by=.(archipel, sex, agecat, pop)])
# 3 geo strata
out[, gstratum := archipel]
out[archipel %in% c('Australes','Marquises','Tuamotu'), gstratum := 'Autres']
out <- out[, .(N=sum(N), pop=sum(pop)), by=.(gstratum, sex, agecat)]
out[, f := N/pop]  # sampling fraction
out[, w := 1/f]    # sampling weights
(out)
fwrite(out, file=here('output/sample.csv'))



