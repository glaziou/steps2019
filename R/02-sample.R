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


# sample size by stratum
(a1 <- steps[enroll==1, .N, by=.(archipel, sex, agecat, agegr, pop)])
# 3 geo strata
a1[archipel %in% c('Australes','Marquises','Tuamotu'), archipel := 'Autres']
a1 <- a1[, .(enrolled=sum(N), pop=sum(pop)), by=.(archipel, sex, age=agegr)]
a1[, `fraction (%)`:= signif(enrolled*100/pop, 2)]  # sampling fraction
# a1[, weight := signif(pop/enrolled, 2)]    # sampling weights
(a1)


# missed individuals
(a0 <-
    steps[, .N, by = .(enroll)][, percent := signif(100 * N / sum(N), 2)])


# missed individuals by stratum
(a2 <-
    steps[, .N, by = .(enroll, commune)][, percent := signif(100 * N / sum(N), 2), by =
                                           commune][enroll == 0, .(commune, N, percent)])
# a2 <- steps[, .N, by=.(commune, enroll)]
# a2[ ,percent := round(100 * (N / sum(N)), 2)]
# setkey(a2, commune)
# (a2 <- a2[enroll==0, .(commune, missed=N, percent)])

save_as_html(flextable(a2), path=here('html/missedDesc.html'))

# none were missed in Makemo, Raivavae, Tubuai

# save
save(a0, a1, a2, file=here('data/report.Rdata'))

