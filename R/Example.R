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
  t1 <- read_csv(fname, show_col_types = FALSE)
  t2 <- data.table::transpose(t1[,-1])
  colnames(t2) <- unlist(t1[,1])
  rownames(t2) <- colnames(t1)[-1]
  return(t2)
}

getWeekendMulti <- function(weekends){
  # Brief helper function to return the weekend multiplier for a given
  # Weekend frequency; e.g. for 1:4 weekends, weekends = 4, returns 0.06
  weekendMulti <- c(0.15,  0.1, 0.075, 0.06, 0.05, 0.04, 0.03, 0)
  cutoffs      <- c(   2,    3,     4,    5,    6,    7,    8)
  return(weekendMulti[findInterval(weekends, cutoffs, left.open = TRUE) +1])
}

calcPay2002 <- function(base, banding){
  # Calculates pay under the 2002 contract
  # base    (numeric) - base pay
  # banding (numeric) - banding multiplier (e.g. for 50%, 0.50)
  # Unbanded:       <40h/wk                      - 0.0
  # 1A:  >40h but <= 48h/wk,     most antisocial - 0.5
  # 1B:  >40h but <= 48h/wk, moderate antisocial - 0.4
  # 1C:  >40h but <= 48h/wk,    least antisocial - 0.2
  # 2A:  >48h but <= 56h/wk,     most antisocial - 0.8
  # 2B:  >48h but <= 56h/wk,    least antisocial - 0.5
  #  3:  >56h or not achieving rest requirements - 1.0
  # You may have noticed that it is undefined which band a practitioner working
  # exactly 40h/wk should fall into. This appears to be how the contract is
  # worded (pp 15-18, TCS 2002 version 10, March 2013).
  # The exact banding definitions depend on the on-call and weekend frequencies,
  # as well as the proportion of hours.
  # However, anecdotally most posts on the 2002 contract attracted 1A or 2B
  # banding supplements.
  return(base * (1+ banding))
}

calcPay2016 <- function(base, addHours, supHours, weekends, nroc = 0){
  # Calculates pay under the 2016 contact
  # base     (numeric) - base pay
  # addHours (numeric) - additional total weekly hours beyond 40/wk 
  # supHours (numeric) - number of weekly unsociable hours at enhanced rate
  # nroc     (numeric) - NROC supplement, if any, or 0 (usually 0)
  # weekends (numeric) - 1:frequency of weekends (e.g. for 1:4 weekends, 4)
  addHoursMulti  <- 1/40 * addHours           # additional hours fraction
  supHoursMulti  <- 1/40 * 0.37 * supHours    # enhanced rate is 1.37 time
  weekendMulti   <- getWeekendMulti(weekends) # Look up the weekend multiplier
  pay <- base * (1 + addHoursMulti + supHoursMulti + weekendMulti) + nroc
  return(pay)
}

Pay <- importTab("RData/Pay.csv")
Inflation <- importTab("RData/Inflation.csv")
NROC <- importTab("RData/NROC.csv")

calcPay2002(Pay["2008", "FY1"], 0.5)
calcPay2016(Pay["2022", "FY1"], 5, 10, 4)

