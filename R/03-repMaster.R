#' ---
#' title: Generate Steps2019 report pages
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

#' STEPS 2019 - Generate html report
#' 

library(here)
library(rmarkdown)


# load datasets and utility functions
load('data/report.Rdata')


# generate html pages for each step
# sample description
system.time(rmarkdown::render(
  here('R/repSample.Rmd'),
  output_dir = here('html'),
  output_file = 'samplingDescription.html'
))

# step 1, questionnaire
system.time(rmarkdown::render(
  here('R/repStep1.Rmd'),
  output_dir = here('html'),
  output_file = 'step1.html'
))

# steps 2-3, physical and biological measures
system.time(rmarkdown::render(
  here('R/repStep2.Rmd'),
  output_dir = here('html'),
  output_file = 'step2.html'
))

