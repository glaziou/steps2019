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


# Step2 ----------------------
# weight
step2[, height := `height_weight:M11`]
step2[, weight := `height_weight:M12`]

step2[height>250 | height<110, height := NA]
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


# HTA
step2[, diastol := `step2_physical_measurements:average_DBP`]
step2[, systol := `step2_physical_measurements:average_SBP`]
sum(step2$diastol > step2$systol, na.rm=TRUE)==0
step2[!is.na(diastol) & !is.na(systol), hta := 0]
step2[!is.na(hta) & (diastol >= 90 | systol >= 140), hta := 1]


# belly 
step2[, waist := `waist:M14`]
step2[waist==888, waist := NA]

# step 3
step2[ ,mange := `step3_biochem_measurements:B1`]
step2[mange==2, mange := 0]
step2[ ,glycemie := `step3_biochem_measurements:blood_test:B5`]
step2[glycemie==777, glycemie := NA]

# step2[ ,urine := `step3_biochem_measurements:urinary_sodium_creatinine:B10`]



# merge step2 with steps
#
tmp2 <-
  merge(steps,
        step2[, .(uid,
                  height,
                  weight,
                  bmi,
                  overweight,
                  obese,
                  bmigr,
                  diastol,
                  systol,
                  hta,
                  waist,
                  mange,
                  glycemie)], by = 'uid', all.x = TRUE)
dim(steps)
dim(step2)
dim(tmp2)

steps <- copy(tmp2)
rm(tmp, tmp2)


# merge step3 with steps
#
tmp3 <-
  merge(steps,
        step3[, .(uid,
                  sodium=`urinary_sodium_creatinine:B14`,
                  creat=`urinary_sodium_creatinine:B15`)], by = 'uid', all.x = TRUE)
dim(steps)
dim(step3)
dim(tmp3)

steps <- copy(tmp3)
rm(tmp3)


# Step2 weights
out1 <- steps[!is.na(obese) & enroll==1, .N, by=.(gstratum,agecat)]
out3 <- merge(out1, out2, by.x=c("gstratum", "agecat"), by.y=c("gstratum", "age_cat"))
tmp <-
  merge(steps,
        out3[, .(gstratum, agecat, w2 = pop / N)],
        by = c('gstratum', 'agecat'),
        all.x = TRUE)
dim(steps); dim(tmp)
steps <- copy(tmp)


# Step3 weights
out1 <- steps[!is.na(glycemie) & enroll==1, .N, by=.(gstratum,agecat)]
out3 <- merge(out1, out2, by.x=c("gstratum", "agecat"), by.y=c("gstratum", "age_cat"))
tmp <-
  merge(steps,
        out3[, .(gstratum, agecat, w3 = pop / N)],
        by = c('gstratum', 'agecat'),
        all.x = TRUE)
dim(steps); dim(tmp)
steps <- copy(tmp)

# urines done in only one geo stratum, no weighting possible
steps[, sum(!is.na(creat)), by=gstratum]


# add calculated variables
#
steps[!is.na(waist), bigbelly := 0]
steps[waist >= 88 & sex=='F', bigbelly := 1]
steps[waist >= 102 & sex=='M', bigbelly := 1]
steps[!is.na(glycemie), diabete := 0]
steps[glycemie > 110, diabete := 1]
steps[sex == 'M', salt := 23.51 + 0.45*sodium - 3.09*creat + 4.16*bmi + 0.22*age]
steps[sex == 'F', salt := 3.74 + 0.33*sodium - 2.44*creat + 2.42*bmi + 2.34*age - 0.03*age^2]
steps[, salt.g := salt / 17.1]
steps[!is.na(salt.g), abnormal.salt := 1]
steps[salt.g > 2 & salt.g < 5, abnormal.salt := 0]


# simplify varnames
setnames(steps, names(steps), gsub(':','.',names(steps)))
setnames(steps, names(steps), gsub('step1.','',names(steps)))
setnames(steps, names(steps), gsub('_activity','',names(steps)))
setnames(steps, names(steps), gsub('.Quality_of_','',names(steps)))
setnames(steps, names(steps), gsub('Quality_of_','',names(steps)))
setnames(steps, names(steps), gsub('_use','',names(steps)))
setnames(steps, names(steps), gsub('-Consommation_de_','',names(steps)))
setnames(steps, names(steps), gsub('_consumption','',names(steps)))
setnames(steps, names(steps), gsub('_coordinates-geopoint','',names(steps)))
setnames(steps, names(steps), gsub('_history_of_','',names(steps)))
setnames(steps, names(steps), gsub('raised_total_','',names(steps)))
setnames(steps, names(steps), gsub('_disease','',names(steps)))
setnames(steps, names(steps), gsub('lifestyle_advice-','',names(steps)))
setnames(steps, names(steps), gsub('_of','',names(steps)))
setnames(steps, names(steps), gsub('_h20','',names(steps)))
setnames(steps, names(steps), gsub('history','hx',names(steps)))
setnames(steps, names(steps), gsub('cholesterol','chol',names(steps)))
setnames(steps, names(steps), gsub('.screening','',names(steps)))
setnames(steps, names(steps), gsub('.vegetqables_servings','',names(steps)))
setnames(steps, names(steps), gsub('.control','',names(steps)))
setnames(steps, names(steps), gsub('grp','',names(steps)))
setnames(steps, names(steps), gsub('T1.T1','T1',names(steps)))
setnames(steps, names(steps), gsub('Consommation_de_pakalolo','paka',names(steps)))
setnames(steps, names(steps), gsub('smok_paka','paka',names(steps)))
setnames(steps, names(steps), gsub('dietary_salt','salt',names(steps)))


# missing values
steps[tobacco.T5a==77, tobacco.T5a := NA]
steps[tobacco.T5aw==777, tobacco.T5a := NA]
steps[tobacco.T5b==77, tobacco.T5b := NA]
steps[tobacco.T5bw %in% c(77,777), tobacco.T5bw := NA]

steps[paka.X2 %in% c(77, 88), paka.X2 := NA]
steps[paka.X3 %in% c(77, 88), paka.X3 := NA]
steps[paka.X4 %in% c(77, 88), paka.X4 := NA]
steps[paka.X5 %in% c(77, 88), paka.X5 := NA]
steps[paka.X6 %in% c(77, 88, 99), paka.X6 := NA]
steps[paka.X7 %in% c(77, 88), paka.X7 := NA]

steps[alcweek.A10a %in% c(77), alcweek.A10a := NA]
steps[alcweek.A10b %in% c(77), alcweek.A10b := NA]
steps[alcweek.A10c %in% c(77), alcweek.A10c := NA]
steps[alcweek.A10d %in% c(77), alcweek.A10d := NA]
steps[alcweek.A10e %in% c(77), alcweek.A10e := NA]
steps[alcweek.A10f %in% c(77), alcweek.A10f := NA]
steps[alcweek.A10g %in% c(77), alcweek.A10g := NA]


# alcohol grams per day over the past 30 days
steps[A6 < 77 & A7 < 77, alc.gpd := A6 * A7 / 3]
# 3 WHO groups of alcohol consumption (30 days) (1 = yes, 2 = no)
steps[sex=="M" & alc.gpd >= 40, alc.cat1 := 2]
steps[sex=="M" & alc.gpd < 40, alc.cat1 := 1]
steps[sex=="F" & alc.gpd >= 20, alc.cat1 := 2]
steps[sex=="F" & alc.gpd < 20, alc.cat1 := 1]

steps[sex=="M" & (alc.gpd < 40 | alc.gpd >= 60), alc.cat2 := 2]
steps[sex=="M" & (alc.cat1==2 & alc.gpd < 60), alc.cat2 := 1]
steps[sex=="F" & (alc.gpd < 20 | alc.gpd >= 40), alc.cat2 := 2]
steps[sex=="F" & (alc.cat1==2 & alc.gpd < 40), alc.cat2 := 1]

steps[sex=="M" & alc.gpd < 60, alc.cat3 := 2]
steps[sex=="M" & alc.gpd >= 60, alc.cat3 := 1]
steps[sex=="F" & alc.gpd < 40, alc.cat3 := 2]
steps[sex=="F" & alc.gpd >= 40, alc.cat3 := 1]


# alcohol grams per day over the past 7 days (1 = yes, 2 = no)
steps[, alc.gpd7 := (
  alcweek.A10a + alcweek.A10b + alcweek.A10c + alcweek.A10d + alcweek.A10e +
    alcweek.A10f + alcweek.A10g
) / .7] 

# fruits and veggies
steps[diet.fruits_serv.D2 < 77 & diet.vegetables_serv.D4 < 77, 
      fruitveg := diet.fruits_serv.D2 + diet.vegetables_serv.D4]
steps[fruitveg >= 5, fruitveg5 := 1]
steps[fruitveg < 5, fruitveg5 := 0]


# Categories of physical activity (low, med, high)
steps[physical.P2==77, physical.P2 := NA]
steps[physical.P3.P3a==77, physical.P3.P3a := NA]
steps[physical.P3.P3b==77, physical.P3.P3b := NA]
steps[physical.P5==77, physical.P5 := NA]
steps[physical.P6.P6a==77, physical.P6.P6a := NA]
steps[physical.P6.P6b==77, physical.P6.P6b := NA]
steps[physical.P8==77, physical.P8 := NA]
steps[physical.P9.P9a==77, physical.P9.P9a := NA]
steps[physical.P9.P9b==77, physical.P9.P9b := NA]

# steps[(8 * (((physical.P2 / 7) * (physical.P3.P3a * 60 + physical.P3.P3b)) + 
#               ((physical.P11 / 7) * (physical.P12.P12a * 60 + physical.P12.P12b))) >= 1500) | 
#         (4 * ((physical.P5 / 7) * (physical.P6.P6a * 60 + physical.P6.P6b)) +
#            4 * ((physical.P14 / 7) * (physical.P15.P15a * 60 + physical.P15.P15b)) +
#         4 * ((physical.P8 / 7) * (physical.P9.P9a * 60 + physical.P9.P9b)) >= 3000), 
#       met.high := 1]
# steps[enroll==1 & is.na(met.high), met.high := 0]
# 
# steps[met.high == 0 &
#         ((physical.P2 >= 3 &
#             (physical.P3.P3a >= 1 | physical.P3.P3b >= 20)) |
#            (physical.P11 >= 3 &
#               (physical.P12.P12a >= 1 | physical.P12.P12b >= 20)) |
#            (physical.P5 >= 5 &
#               (physical.P6.P6a >= 1 | physical.P6.P6b >= 30)) |
#            (physical.P14 >= 3 &
#               (physical.P15.P15a >= 1 | physical.P15.P15b >= 20)) |
#            (physical.P8 >= 5 &
#               ((physical.P8 / 7) * (physical.P9.P9a * 60 + physical.P9.P9b)
#               ) >= 600)), met.med := 1]
steps[(8 * ((physical.P2 * (physical.P3.P3a * 60 + physical.P3.P3b)) + 
              (physical.P11 * (physical.P12.P12a * 60 + physical.P12.P12b))) >= 1500) | 
        (4 * (physical.P5 * (physical.P6.P6a * 60 + physical.P6.P6b)) +
           4 * (physical.P14 * (physical.P15.P15a * 60 + physical.P15.P15b)) +
           4 * (physical.P8 * (physical.P9.P9a * 60 + physical.P9.P9b)) >= 3000), 
      met.high := 1]
steps[enroll==1 & is.na(met.high), met.high := 0]

steps[met.high == 0 &
        ((physical.P2 >= 3 &
            (physical.P3.P3a >= 1 | physical.P3.P3b >= 20)) |
           (physical.P11 >= 3 &
              (physical.P12.P12a >= 1 | physical.P12.P12b >= 20)) |
           (physical.P5 >= 5 &
              (physical.P6.P6a >= 1 | physical.P6.P6b >= 30)) |
           (physical.P14 >= 3 &
              (physical.P15.P15a >= 1 | physical.P15.P15b >= 20)) |
           (physical.P8 >= 5 &
              (physical.P8 * (physical.P9.P9a * 60 + physical.P9.P9b)
              ) >= 600)), met.med := 1]

steps[enroll==1 & is.na(met.med), met.med := 0]

steps[met.high==0 & met.med==0, met.low := 1]
steps[enroll==1 & is.na(met.low), met.low := 0]



steps[((physical.P2 < 3 &
            (physical.P3.P3a == 0 | physical.P3.P3b < 20)) |
           (physical.P5 < 5 &
              (physical.P6.P6a == 0 | physical.P6.P6b < 30))), met.work.low := 1]
steps[enroll==1 & is.na(met.work.low), met.work.low := 0]

steps[((physical.P11 < 3 &
          (physical.P12.P12a == 0 | physical.P12.P12b < 20)) |
         (physical.P14 < 5 &
            (physical.P15.P15a == 0 | physical.P15.P15b < 30))), met.sport.low := 1]
steps[enroll==1 & is.na(met.sport.low), met.sport.low := 0]

# steps[(physical.P8 >= 5 &
#          ((physical.P8 / 7) * (physical.P9.P9a * 60 + physical.P9.P9b)
#          ) < 600), met.move.low := 1]
steps[(physical.P8 >= 5 &
         (physical.P8 * (physical.P9.P9a * 60 + physical.P9.P9b)
         ) < 600), met.move.low := 1]
steps[enroll==1 & is.na(met.move.low), met.move.low := 0]




# number of risk factors and combined risk >= 3 risk factors
steps[, n.risk := rowSums(cbind(tobacco.T2==1, 
                       fruitveg5==0, 
                       met.low==1,
                       overweight==1,
                       hta==1), na.rm=T)]
steps[n.risk >= 3, c.risk := 1]
steps[n.risk < 3, c.risk := 0]

steps[n.risk == 0, zero.risk := 1]
steps[n.risk > 0, zero.risk := 0]


#' save
#'
fwrite(steps, file = here(paste0('csv/steps_', Sys.Date(), '.csv')))
save(steps, file = here('data/steps.Rdata'))
