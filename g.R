#!/usr/bin/Rscript
args = commandArgs(trailingOnly=TRUE)
#install.packages("cvequality",repos = "http://cran.us.r-project.org")
suppressMessages(library(dplyr))
suppressMessages(library(jsonlite))

setwd('.')
rmarkdown::render(args[1], 'pdf_document')
