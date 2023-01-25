#' ---
#' title: Compare STEPS with MATAEA
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
library(readxl)

steps <- fread(here('input/PF_STEPS2019_V3_2_1_results.csv'))
M <- as.data.table(read_excel(here('input/02_MATAEA_final.xlsx')))
