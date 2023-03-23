#'---
#'title: "R Example"
#'author: "maashaw"
#'date: "Started on 2023-03-23; This report compiled `r format(Sys.time(), '%Y-%m-%dT%H:%M:%S%z')`"
#'output: 
#'    html_document:
#'      theme: "flatly"
#'      highlight: "espresso"
#'      number_sections: true
#'      toc: true
#'      toc_depth: 5
#'      toc_float:
#'        collapsed: false
#'      df_print: kable
#'      code_folding: "hide"
#'---

#' # Preface
#' Junior Doctors working in the NHS have seen their pay fall substantially in
#' the last 15 years; when adjusted for inflation, F1s have seen pay cuts of
#' 26%. This has happened because of a deliberate decision on the part of the
#' british government to grant below-inflation pay rises.
#'
#' Because Junior Doctors progress through training grades with with increasing
#' responsibilities and pay, many have not seen their pay fall in real terms,
#' but their pockets are emptier than their colleagues who came before them. At
#' the same time, the costs of living and training have continued to raise, and
#' the scope and clinical complexity of medical care have only increased.
#' 
#' This repo brings together a number of different resources to help people who
#' are interested in these subjects to explore the underlying data.
#' 
#' # Example
#' This example file just imports the datasets. You might find it useful as a
#' starting point for further exploration.
#' 
#' ## RMarkdown
#' This example file includes RMarkdown content, so that you can easily create
#' beautiful reports and documents using Markdown formatting. In RStudio, you 
#' can use File > Compile Report... to generate a document.
#' 
#' ## Github
#' Make something cool? Spotted something dumb? Feel free to send a pull
#' request.

library(ggplot2) #Make nice plots
library(readr)   #Import CSVs

importTab <- function(fname){
# This little helper function makes it easier to import CSV Tables
# It requires a csv with size x, y with row and column titles formatted as so:
# |       x | Title A | Title B | ...
# | Title 1 |       1 |       2 | ...
# | Title 2 |       4 |       5 | ...
#     ...       ...       ...
# And reformats it as an transposed tibble of y-1,x-1 size,
# with row and column names set by the first row and column, e.g.
# |       x | Title 1 | Title 2 | ...
# | Title A |       1 |       4 | ...
# | Title B |       2 |       5 | ...
#     ...       ...       ...
  t1 <- read_csv(fname, col_names = FALSE, show_col_types = FALSE)
  t2 <- data.table::transpose(t1)[-1,-1]
  rownames(t2) <- t1[1,-1]
  colnames(t2) <- data.table::transpose(t1[-1,1])
  return(t2)
}

Pay <- importTab("RData/Pay.csv")
Inflation <- importTab("RData/Inflation.csv")
NROC <- importTab("RData/NROC.csv")
