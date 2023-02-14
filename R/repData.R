#' ---
#' title: Sample description STEPS
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
library(flextable)

load(here('data/steps.Rdata')) # loads latest pre-processed data


# utility functions
# recode 1=yes 2=no into 1=yes 0=no, with all other values reset to missing
yesno <- function(x){
  x[x==2] <- 0
  x[x>1 | x<0] <- NA
  return(x)
}

# various frequency codes
#
freq3 <- function(x) {
  x <- factor(x,
              levels = 1:3,
              labels = c(
                  "Lors des repas",
                  "Entre les repas",
                  "Lors des repas et entre les repas"
              ),
              ordered = TRUE)
  return(x)
}


freq3b <- function(x) {
  x <- factor(x,
              levels = 1:3,
              labels = c(
                "Oui, beaucoup limité",
                "Oui, un peu limité",
                "Non, pas du tout limité"
              ),
              ordered = TRUE)
  return(x)
}


freq4 <- function(x) {
  x <- factor(
    x,
    levels = 1:4,
    labels = c("Jamais",
               "Parfois",
               "Souvent",
               "Toujours"),
    ordered = TRUE
  )
  return(x)
}

freq5 <- function(x) {
  x <- factor(x,
              levels = 5:1,
              labels = rev(
                c(
                  "5-7 jours par semaine",
                  "2-4 jours par semaine",
                  "1 jour par semaine",
                  "1-3 jours par mois",
                  "Moins d'une fois par mois"
                )
              ),
              ordered = TRUE)
  return(x)
}


freq5b <- function(x) {
  x <- factor(x,
              levels = 5:1,
              labels = rev(
                c(
                  "Plus d'une fois par mois",
                  "Chaque mois",
                  "Plusieurs fois mais moins d'une fois par mois",
                  "1-2 fois",
                  "Non"
                )
              ),
              ordered = TRUE)
  return(x)
}


freq5d <- function(x) {
  x <- factor(x,
              levels = 1:5,
              labels = rev(
                c(
                  "Bien trop peu",
                  "Trop peu",
                  "Juste la quantité nécessaire",
                  "Trop",
                  "Beaucoup trop"
                )
              ),
              ordered = TRUE)
  return(x)
}


freq5e <- function(x) {
  x <- factor(
    x,
    levels = 1:5,
    labels = c("Excellente",
               "Très bonne",
               "Bonne",
               "Médiocre",
               "Mauvaise"),
    ordered = TRUE
  )
  return(x)
}

freq5f <- function(x) {
  x <- factor(x,
              levels = 1:5,
              labels = rev(
                c(
                  "Jamais",
                  "Rarement",
                  "Parfois",
                  "La plupart du temps",
                  "Toujours"
                )
              ),
              ordered = TRUE)
  return(x)
}


freq5g <- function(x) {
  x <- factor(
    x,
    levels = 1:5,
    labels = c(
      "Pas du tout",
      "Un petit peu",
      "Moyennement",
      "Beaucoup",
      "Enormément"
    ),
    ordered = TRUE
  )
  return(x)
}

freq7 <- function(x) {
  x <- factor(x,
              levels = 7:1,
              labels = rev(
                c(
                  "Quotidiennement",
                  "5-6 jours par semaine",
                  "3-4 jours par semaine",
                  "1-2 jours par semaine",
                  "1-3 jours par mois",
                  "Moins d'une fois par mois",
                  "Jamais"
                )
              ),
              ordered = TRUE)
  return(x)
}


# returns Beta shape and scale params using the method of moments
get.beta <- function(ev, sd) {
  #' @param ev expected value.
  #' @param sd standard deviation.
  #' @export
  stopifnot(ev > 0 & ev < 1)
  stopifnot(sd > 0)
  
  S = (ev * (1 - ev) / sd ^ 2) - 1
  if (S < 0)
    stop('Not distributed Beta: sd^2 >= ev*(1-ev)')
  
  a = S * ev
  b = S * (1 - ev)
  return(c(a = a, b = b))
}


# generate low and high bounds assuming Beta distribution
lohi <- function(ev, sd) {
  #' @param ev expected value.
  #' @param sd standard deviation.
  #' @export
  stopifnot(ev > 0 & ev < 1)
  stopifnot(sd > 0)
  
  par <- get.beta(ev, sd)
  lo <- qbeta(0.025, par[1], par[2])
  hi <- qbeta(0.975, par[1], par[2])
  return(c(lo = lo, hi = hi))
}

# simulate quantiles of a difference in proportions
dprop <- function(ev1, sd1, ev2, sd2, nsim = 1e5) {
  par1 <- get.beta(ev1, sd1)
  par2 <- get.beta(ev2, sd2)
  s1 <- rbeta(nsim, par1[[1]], par1[[2]])
  s2 <- rbeta(nsim, par2[[1]], par2[[2]])
  d <- s2 - s1
  out <-
    c(
      mean.diff = mean(d),
      lo = quantile(d, 0.025),
      hi = quantile(d, 0.975)
    )
  return(out)
}



# splits multichoice questions
qcm <- function(x, value=1) {
  x <- gsub(" ","", x)
  y <- grepl(value, x)
  y <- factor(y, labels=c('Non','Oui'))
  y[x==''] <- NA
  return(y)
}





steps[enroll == 1, inclus := 'Inclus']
steps[enroll == 0, inclus := 'Exclus']

# sample size by stratum
(a1 <-
    steps[enroll == 1, .N, by = .(archipel, sex, agecat, agegr, pop)])
# 3 geo strata
a1[archipel %in% c('Australes', 'Marquises', 'Tuamotu'), archipel := 'Autres']
a1 <-
  a1[, .(enrolled = sum(N), pop = sum(pop)), by = .(archipel, age = agegr)]
a1[, fraction := signif(enrolled * 100 / pop, 2)]  # sampling fraction


(a2 <-
    steps[enroll == 1, .N, by = .(archipel, sex, agecat, agegr, pop)])
# 3 geo strata
a2[archipel %in% c('Australes', 'Marquises', 'Tuamotu'), archipel := 'Autres']
a2 <-
  a2[, .(enrolled = sum(N), pop = sum(pop)), by = .(archipel, sex, age =
                                                      agegr)]
a2[, fraction := signif(enrolled * 100 / pop, 2)]  # sampling fraction


# split QCMs into binary vars
for (i in 1:8) {
  steps[, paste0("diet.X8.", i) := qcm(diet.X8, i)]
  steps[, paste0("diet.X9.", i) := qcm(diet.X9, i)]
} 



# update var values
steps[, sexe := sex]
steps[sex=='M', sexe := 'Homme']
steps[sex=='F', sexe := 'Femme']
steps[, sexe := factor(sexe, levels=c('Homme','Femme'))]
steps[, gstratum := factor(gstratum, 
                           levels=c('IDV','ISLV','Autres'),
                           labels=c('Iles du vent','Iles sous le vent','Autres archipels'))]
steps[, grosbide := factor(bigbelly, levels=0:1, labels=c('Non','Oui'))]
steps[, Surpoids := factor(overweight, levels=0:1, labels=c('Non','Oui'))]
steps[, Obese := factor(obese, levels=0:1, labels=c('Non','Oui'))]
steps[, HTA := factor(hta, levels=0:1, labels=c('Non','Oui'))]
steps[, Diabete := factor(diabete, levels=0:1, labels=c('Non','Oui'))]


# # missed individuals
# (a0 <-
#     steps[, .N, by = .(enroll)][, percent := signif(100 * N / sum(N), 2)])
#
#
# # missed individuals by stratum
# (a2 <-
#     steps[, .N, by = .(enroll, commune)][, percent := signif(100 * N / sum(N), 2), by =
#                                            commune][enroll == 0, .(commune, N, percent)])
# a2 <- steps[, .N, by=.(commune, enroll)]
# a2[ ,percent := round(100 * (N / sum(N)), 2)]
# setkey(a2, commune)
# (a2 <- a2[enroll==0, .(commune, missed=N, percent)])

# save_as_html(flextable(a2), path=here('html/missedDesc.html'))

# none were missed in Makemo, Raivavae, Tubuai

# save
save(yesno,
     freq3,
     freq3b,
     freq4,
     freq5,
     freq5b,
     freq5c,
     freq5d,
     freq5e,
     freq5f,
     freq5g,
     freq7,
     get.beta,
     lohi,
     dprop,
     qcm,
     steps,
     a1,
     a2,
     file = here('data/report.Rdata'))


