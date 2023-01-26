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
library(flextable)

load(here('data/steps.Rdata')) # loads latest pre-processed data


# sample size by stratum
(out <- steps[enroll==1, .N, by=.(archipel, sex, agecat, agegr, pop)])
# 3 geo strata
out[archipel %in% c('Australes','Marquises','Tuamotu'), archipel := 'Autres']
out <- out[, .(enrolled=sum(N), pop=sum(pop)), by=.(archipel, sex, age=agegr)]
out[, `fraction (%)`:= round(enrolled*100/pop, 2)]  # sampling fraction
out[, weight := round(pop/enrolled, 2)]    # sampling weights
(out)
save_as_html(flextable(out), path=here('html/sampleDesc.html'))



# missed individuals by stratum
out2 <- steps[, .N, by=.(commune, enroll)]
out2[ ,percent := round(100 * (N / sum(N)), 2)]
setkey(out2, commune)
(out2 <- out2[enroll==0, .(commune, missed=N, percent)])
save_as_html(flextable(out2), path=here('html/missedDesc.html'))

# none were missed in Makemo, Raivavae, Tubuai

fwrite(out, file=here('csv/sample.csv'))

