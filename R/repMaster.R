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


# load datasets
source(here('R/repData.R'))


# generate html pages for each step
# sample description
rmarkdown::render(
  here('R/repSample.Rmd'),
  output_dir = here('html'),
  output_file = 'samplingDescription.html'
)

# step 1, questionnaire
rmarkdown::render(
  here('R/repStep1.Rmd'),
  output_dir = here('html'),
  output_file = 'step1.html'
)

# step 2, physical and biological measures
rmarkdown::render(
  here('R/repStep2.Rmd'),
  output_dir = here('html'),
  output_file = 'step2.html'
)

