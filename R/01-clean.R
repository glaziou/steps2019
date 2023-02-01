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
steps <-
  fread(here('input/PF_STEPS2019_V3_2_1_results.csv')) # STEP 1
step2 <-
  fread(here('input/PF_STEPS2019_V3_2_2_results.csv')) # STEP 2
step3 <-
  fread(here('input/PF_STEPS2019_V3_2_Lab_results.csv')) # STEP 3


loc <- fread(here('input/loc.csv'))  # geo codes
pop <- as.data.table(read_dta(here('input/ponderation_draft.dta')))

#' unique IDs - use the QR code
#' 
steps[, uid := `identification:qr_scanned`]
step2[, uid := `identification:qr_scanned`]
step3[, uid := `qr_code:qr_scanned`]

steps[is.na(uid), uid := `identification:qr_manual`]
step2[, sum(is.na(uid))==0]
step3[, sum(is.na(uid))==0]

length(steps$uid); length(step2$uid); length(step3$uid)
length(intersect(steps$uid, step2$uid))
length(intersect(steps$uid, step3$uid))




#' clean-up locations
#'
steps[, table(`geo_location:I1`)]
steps[, table(`geo_location:I2`)]
steps[, table(`geo_location:I2`, `geo_location:I1`)]
steps[`geo_location:I1` == '6']  # Tubuai
steps[`geo_location:I2` == 'Va'] # Rangiroa

steps[, codeloc := `geo_location:I1`]

steps[codeloc == 6, `geo_location:I2`]
steps[codeloc == 51, `geo_location:I2`]
steps[codeloc == 6, codeloc := 53]  # assume I2 is correct
steps[codeloc == 51, codeloc := 53] # assume I2 is correct

steps <-
  merge(steps,
        loc,
        by.x = 'codeloc',
        by.y = 'code',
        all.x = TRUE)
steps[`geo_location:I2` == 'Va', .(codeloc, commune)]

sum(is.na(steps$commune)) == 0

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

# 9 participants aged 70 (not enroll)
steps[age == 70, .(codeloc, commune, age, agegroup)]
steps[, enroll := 1]
steps[age == 70, enroll := 0]


# 382 participants with no age/agegroup info: no consent
steps[agegroup == '', .(
  codeloc,
  commune,
  age,
  agegroup,
  `step1:demographics:DOB:C2a`,
  `step1:demographics:DOB:C2b`,
  `step1:demographics:DOB:C2c`,
  `step1:demographics:C2`,
  `step1:demographics:C3`,
  `consent_language_name:I5`,
  `consent_language_name:I6`,
  `consent_language_name:I8`,
  `consent_language_name:I9`,
  `consent_language_name:I10`
)]
steps[agegroup == '', enroll := 0]


#' Recruitment by stratum
#'
steps[, .N]
steps[, .N, by = enroll] # age documented and in (18-69)
steps[enroll == 0, table(archipel)]
steps[enroll == 1, table(agegr, archipel)]

# Distribution in IDV
steps[enroll == 1 & archipel == 'IDV', table(agegr, commune)]



#' Include pop and calculate weights
#'
s <- copy(steps)
steps[agegr == '18-29', agecat := 1]
steps[agegr == '30-44', agecat := 2]
steps[agegr == '45-69', agecat := 3]
steps[`step1:demographics:C1` == 1, sex := 'M']
steps[`step1:demographics:C1` == 2, sex := 'F']

steps[enroll == 1, table(sex)]
steps[enroll == 1, table(sex, agecat)]

pop[archipelago %in% c('Australes', 'Marquises'), archipel := archipelago]
pop[archipelago == 'Société (Îles-du-vent)', archipel := 'IDV']
pop[archipelago == 'Société (Îles-sous-le-vent)', archipel := 'ISLV']
pop[archipelago == 'Tuamotu-Gambier', archipel := 'Tuamotu']
pop[gender == 1, sex := 'M']
pop[gender == 2, sex := 'F']

steps <-
  merge(steps,
        pop[, .(archipel, sex, agecat = age_cat, pop = N)],
        by = c('archipel', 'sex', 'agecat'),
        all.x = TRUE)
setkey(steps, enroll)

steps[enroll == 0, .(archipel)]
steps[enroll == 1, .(archipel, sex, agecat, pop)]


# 3 geo strata
steps[, gstratum := archipel]
steps[archipel %in% c('Australes', 'Marquises', 'Tuamotu'), gstratum := 'Autres']


# Step1 weights
out1 <- steps[agegr!='', .N, by=.(gstratum,agecat)]
pop[, gstratum := archipel]
pop[archipel %in% c('Australes', 'Marquises', 'Tuamotu'), gstratum := 'Autres']
out2 <- pop[, .(pop=sum(N)), by=.(gstratum,age_cat)]
out3 <- merge(out1, out2, by.x=c("gstratum", "agecat"), by.y=c("gstratum", "age_cat"))
tmp <-
  merge(steps,
        out3[, .(gstratum, agecat, w1 = pop / N)],
        by = c('gstratum', 'agecat'),
        all.x = TRUE)
dim(steps); dim(tmp)
steps <- copy(tmp)


# consent
sum(is.na(steps$`consent_language_name:I5`)) == 0
steps[, consent := `consent_language_name:I5`]
steps[`consent_language_name:I5` == 2, consent := 0]


# HH size
sum(is.na(steps$"step1:demographics:C9"))
steps[, hh := `step1:demographics:C9`]


# place of birth
sum(is.na(steps$`steps:demographics:X1`)) == 0
steps$place <- factor(steps$`step1:demographics:X1`)
levels(steps$place) <- c('France', 'DOM/COM', 'Polynésie Française')


# education
sum(is.na(steps$`steps:demographics:C5`)) == 0
steps$edu <- factor(steps$`step1:demographics:C5`)
levels(steps$edu) <- c(
  'Aucune',
  'Pré-primaire',
  'Primaire',
  'Collège',
  'Lycée',
  'Université',
  'Post-université',
  '-'
)
steps[edu=='-',edu := NA]


# work
sum(is.na(steps$"step1:demographics:C8"))
steps$work <- factor(steps$`step1:demographics:C8`)
levels(steps$work) <- c(
  'Public',
  'Privé',
  'Indépendant',
  'Bénévole',
  'Etudiant',
  'Au foyer',
  'Retraité',
  'Sans emploi',
  'Invalide',
  '-'
)
steps[work=='-',work := NA]


# Step2
step2[, height := `height_weight:M11`]
step2[, weight := `height_weight:M12`]

step2[height>250, height := NA]
step2[weight>300, weight := NA]
step2[, bmi := weight/(height/100)^2]

step2[bmi<18.5, bmigr := 0]
step2[bmi>=18.5 & bmi<25, bmigr := 1]
step2[bmi>=25 & bmi<30, bmigr := 2]
step2[bmi>=30 & bmi<35, bmigr := 3]
step2[bmi>=35 & bmi<40, bmigr := 4]
step2[bmi>=40, bmigr := 5]
step2[, bmigr := factor(
  bmigr, levels = 0:5,
  labels = c(
    'Maigreur [<18.5]',
    'Norme [18.5 - 24.9]',
    'Surpoids [25 - 29.9]',
    'Obésité modérée [30 - 34.9]',
    'Obésité sévère [35 - 39.9]',
    'Obésité massive [\u226540]'
  ),
  ordered = TRUE
)]
step2[bmi<25, overweight := 0]
step2[bmi>=25, overweight := 1]
step2[bmi<30, obese := 0]
step2[bmi>=30, obese := 1]

tmp2 <-
  merge(steps,
        step2[, .(uid, height, weight, bmi, overweight, obese, bmigr)], by = 'uid', all.x = TRUE)
dim(steps); dim(step2); dim(tmp2)

steps <- copy(tmp2)
rm(tmp, tmp2)


#' save
#'
fwrite(steps, file = here(paste0('csv/steps_', Sys.Date(), '.csv')))
save(steps, file = here('data/steps.Rdata'))
