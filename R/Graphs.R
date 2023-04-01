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
#' # Graph.R
#' This file creates some graphs to illustrate how doctors' pay has fallen.
#' You might find it useful as a starting point for further exploration.
#' 
#' ## RMarkdown
#' This example file includes RMarkdown content, so that you can easily create
#' beautiful reports and documents using Markdown formatting. In RStudio, you 
#' can use File > Compile Report... to generate a document.
#' 
#' ## Github
#' Make something cool? Spotted something dumb? Feel free to send a pull
#' request.
#' 
#' # Graph

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

calcPay <- function(grade, year, banding, addHours, supHours, weekends, nrocNode = 0){
  # Calculates gross pay for a particular grade in a particular year
  #   - grade is a string matching one of the grades in the table's labels, or an
  #     index into the table (e.g. "FY1" or 1).
  #   - year is an integer in the range 2007:2022
  #   - banding is a banding multiplier and is applied if the post falls under the
  #     2002 contract (e.g. year is <= 2016)
  # The following are considered if the post falls under the 2016 contract
  #   (e.g. the year is > 2016)
  #   - addHours is a number representing the additional hours per week over 40
  #   - supHours is a number representing the number of weekly hours attracting
  #     enhanced rates
  #   - weekends is a number representing the weekend frequency (e.g. 1:4 -> 4)
  #   - nrocNode is a number representing the nodal point for non-resident on-calls
  #     NB this is different from the calcPay2016, which asks for a cash value
  #     whereas this function will look it up for you in the NROC table.
  # Year is an integer in the range 2007:2022
  base <- Pay[as.character(year), grade]
  nrocPay <- 0
  if(year <= 2016){
    return(calcPay2002(base, banding))
  }
  else if (nrocNode > 0){
    nrocPay <- NROC[as.character(year), nrocNode]
  }
  return(calcPay2016(base, addHours, supHours, weekends, nrocPay))
}

inflate <- function(baseYear, cashYear, value, metric){
  # Calculates the inflation-adjusted value of money which is paid in cashYear
  # in the value of baseYear money; e.g. if I'm paid £10 in 2008, how much would
  # that be worth in 2022, using the RPI metric (about £16)
  # baseYear is an integer in the range 2007:2022 - which year's money do you
  #   want to express the equivalent value in?
  # cashYear is an integer in the range 2007:2022 - which year's money is the
  #   input value expressed in?
  # value is the sum of money in the cashYear
  # metric is a string referencing a column label in 'Inflation'
  #  - "RPI" uses the ONS's Retail Price Index measure of inflation
  #  - "CPI" uses the ONS's Consumer Price Index measure of inflation
  #  - "Freddo" uses the value of a freddo over time as a single-item basket of
  #      goods for a measure of inflation. Like RPI, but Principally for 
  #      illustration or humerous effect.
  inflationMulti <- Inflation[as.character(baseYear), metric] / Inflation[as.character(cashYear), metric]
  return(inflationMulti * value)
}

tabApply <- function(tab, f, x = NULL, out = "tab"){
  # Helper function for applying a function to the whole table
  # tab - table to work on
  # f   - function to apply, of the form f(tab, row, column, var)
  # x   - control variable to pass to function
  # out - output mode. Defaults to 'tab' to produce a table of dimensions
  #       matching the input table; if set to 'DF', returns a dataframe with
  #       one row per cell containing the return value from function f.
  tab.x <- lengths(tab)[1]  # e.g. cols; Assumes array's rectangular, not ragged
  tab.y <- length(tab)      # e.g. rows;
  outDF <- data.frame()
  outTab <- tab
  
  # Apologies, this is definitely the C++ way, not the R way
  for(i in 1:tab.y){        # for each column
    for(j in 1:tab.x){      # for each row
      if(out == "tab"){
        outTab[j,i] <- f(tab, j, i, x)
      }
      else if(out == "DF"){
        outDF <- rbind(outDF, f(tab, j, i, x))
      }
    }
  }
  if(out == "tab"){ return(outTab)}
  else if(out == "DF"){return(outDF)}
}

tabCalcPay <- function(tab, j, i, x){
  v <- calcPay( colnames(tab)[i],
                rownames(tab)[j],
                0.5,
                10, 5, 4, 0)
  return(v)
}

tabInflate <- function(tab, j, i, x){
  v <- inflate(x[1], 
               rownames(tab)[j], 
               tab[j,i], 
               x[2])
  return(v)
}

tabDF <- function(tab, j, i, x){
  v <- list(colnames(tab)[i],
          rownames(tab)[j],
          tab[j,i])
  return(v)
}

tabDelta <- function(tab, j, i, x){
  base <- tab[x[1],i]
  current <- tab[j,i]
  v <- (current/base) -1
  if(x[2] == "fraction"){
    return(v)
  }
  else if(x[2] == "multiplier"){
    return(v+1)
  }
  else if(x[2] == "percent")
  return(v*100)
}

Pay <- importTab("RData/Pay.csv")
Inflation <- importTab("RData/Inflation.csv")
NROC <- importTab("RData/NROC.csv")

# this data from House of Commons Library publication 7735, "National Minimum Wage Statistics"
minWage <- data.frame(c(5.52, 5.73, 5.80, 5.93, 6.08, 6.19, 6.31, 6.50, 6.70, 7.20, 7.50, 7.83, 8.21, 8.72, 8.91, 9.50))
rownames(minWage) <- c(2007:2022)
colnames(minWage) <- "minWage"

# this data from the living wage foundation (livingwage.org.uk)
livingWageLondon <- data.frame(c(7.20, 7.45, 7.60, 7.85, 8.30, 8.55, 8.80, 9.15, 9.40, 9.75, 10.20, 10.55, 10.75, 10.85, 11.05, 11.95))
rownames(livingWageLondon) <- c(2007:2022)
colnames(livingWageLondon) <- c("livingWageLondon")

# this data from the living wage foundation (livingwage.org.uk)
livingWageNational <- data.frame(c(NA, NA, NA, NA, 7.20, 7.45, 7.65, 7.85, 8.25, 8.45, 8.75, 9.00, 9.30, 9.50, 9.90, 10.90))
rownames(livingWageNational) <- c(2007:2022)
colnames(livingWageNational) <- c("livingWageLondon")

minWage.2022 <- tabApply(minWage, tabInflate, c(2022, "RPI"))
minWage.2022.DF <- tabApply(minWage.2022, tabDF, out="DF")
colnames(minWage.2022.DF) <- c("group", "year", "minWage")

livingWageLondon.2022 <- tabApply(livingWageLondon, tabInflate, c(2022, "RPI"))
livingWageLondon.2022.DF <- tabApply(livingWageLondon.2022, tabDF, out="DF")
colnames(livingWageLondon.2022.DF) <- c("group", "year", "livingWageLondon")

livingWageNational.2022 <- tabApply(livingWageNational, tabInflate, c(2022, "RPI"))
livingWageNational.2022.DF <- tabApply(livingWageNational.2022, tabDF, out="DF")
colnames(livingWageNational.2022.DF) <- c("group", "year", "livingWageNational")

payGross <- tabApply(Pay, tabCalcPay)
payGross.2022 <- tabApply(payGross, tabInflate, c(2022, "RPI"))
payGross.2022.DF <- tabApply(payGross.2022, tabDF, out="DF")
colnames(payGross.2022.DF) <- c("grade", "year", "pay")
payGross.2022.DF$grade <- factor(payGross.2022.DF$grade, levels = rev(colnames(Pay)))

payGross.2008 <- tabApply(payGross, tabInflate, c(2008, "RPI"))
payGross.2008.DF <- tabApply(payGross.2008, tabDF, out="DF")
colnames(payGross.2008.DF) <- c("grade", "year", "pay")
payGross.2008.DF$grade <- factor(payGross.2008.DF$grade, levels = rev(colnames(Pay)))

annualHours <- 45 * (365.25/7)
baseHours <- 40 * (365.25/7)

payGross.2022.hourly <- payGross.2022 / annualHours
payGross.2022.hourly.DF <- tabApply(payGross.2022.hourly, tabDF, out="DF")
colnames(payGross.2022.hourly.DF) <- c("grade", "year", "pay")
payGross.2022.hourly.DF$grade <- factor(payGross.2022.hourly.DF$grade, levels = rev(colnames(Pay)))

pay.2022 <- tabApply(Pay, tabInflate, c(2022, "RPI"))
pay.2022.hourly <- pay.2022 / baseHours
pay.2022.hourly.DF <- tabApply(pay.2022.hourly, tabDF, out="DF")
colnames(pay.2022.hourly.DF) <- c("grade", "year", "pay")
pay.2022.hourly.DF$grade <- factor(pay.2022.hourly.DF$grade, levels = rev(colnames(Pay)))

payGross.2022.change <- tabApply(payGross.2022, tabDelta, c("2008", "percent"))
payGross.2022.change.DF <- tabApply(payGross.2022.change, tabDF, out="DF")
colnames(payGross.2022.change.DF) <- c("grade", "year", "pay")
payGross.2022.change.DF$grade <- factor(payGross.2022.change.DF$grade, levels = rev(colnames(Pay)))


#' ## Fig.1: Gross (Pre-Tax) Annual Pay, adjusted to 2022£
#+ fig.width = 8, fig.height = 8, dpi = 300

ggplot(data=payGross.2022.DF, aes(x=year, y=pay, group=grade)) +
  geom_line(aes(color=grade)) +
  scale_y_continuous(name = "Gross Annual Pay (Adjusted to 2022£ by RPI Index)", labels = scales::comma) +
  scale_x_discrete(name = "Year") +
  labs(color = "Grade") +
  ggtitle("Doctors' Total Pay 2007 - 2022: in 2022£") +
  annotate(geom = "text", x= '2018', y = -Inf, label = "0.5 banding / 45h weekly with 10h unsociable and 1:4 weekends", size = 3, color='grey', vjust = -0.5) +
  annotate(geom = "text", x = '2017', y = +Inf, label = "New Contract", angle=90, vjust = -0.5, hjust = 1.2 ) +
  annotate(geom = "rect", xmin = '2016', xmax = '2017', ymin = -Inf, ymax = Inf, fill = 'orange', alpha = 0.1) +
  geom_vline(xintercept = '2017', color = 'orange') +
  theme_minimal() 

#' ## Fig.2: Gross (Pre-Tax) Annual Pay, adjusted to 2008£
#+ fig.width = 8, fig.height = 8, dpi = 300

ggplot(data=payGross.2008.DF, aes(x=year, y=pay, group=grade)) +
  geom_line(aes(color=grade)) +
  scale_y_continuous(name = "Gross Annual Pay (Adjusted to 2008£ by RPI Index)", labels = scales::comma) +
  scale_x_discrete(name = "Year") +
  labs(color = "Grade") +
  ggtitle("Doctors' Total Pay 2007 - 2022: in 2008£") +
  annotate(geom = "text", x= '2018', y = -Inf, label = "0.5 banding / 45h weekly with 10h unsociable and 1:4 weekends", size = 3, color='grey', vjust = -0.5) +
  annotate(geom = "text", x = '2017', y = +Inf, label = "New Contract", angle=90, vjust = -0.5, hjust = 1.2 ) +
  annotate(geom = "rect", xmin = '2016', xmax = '2017', ymin = -Inf, ymax = Inf, fill = 'orange', alpha = 0.1) +
  geom_vline(xintercept = '2017', color = 'orange') +
  theme_minimal()

#' ## Fig.3: Gross (Pre-Tax) Mean Hourly Pay, adjusted to 2022£
#+ fig.width = 8, fig.height = 8, dpi = 300

ggplot(data=payGross.2022.hourly.DF, aes(x=year, y=pay)) +
  geom_line(aes(group=grade, color=grade)) +
  scale_y_continuous(name = "Gross Mean Hourly Pay (Adjusted to 2022£ by RPI Index)", labels = scales::comma, limits=c(15,45)) +
  scale_x_discrete(name = "Year") +
  labs(color = "Grade") +
  ggtitle("Doctors' Total Pay 2007 - 2022: in mean hourly 2022£") +
  annotate(geom = "text", x = '2017', y = +Inf, label = "New Contract", angle=90, vjust = -0.5, hjust=1.2 ) +
  annotate(geom = "rect", xmin = '2016', xmax = '2017', ymin = -Inf, ymax = Inf, fill = 'orange', alpha = 0.1) +
  geom_vline(xintercept = '2017', color = 'orange') +
  annotate(geom = "text", x= '2018', y = -Inf, label = "0.5 banding / 45h weekly with 10h unsociable and 1:4 weekends", size = 3, color='grey', vjust = -0.5) +
  theme_minimal()

#' ## Fig.4: Gross (Pre-Tax) Base Hourly Pay, adjusted to 2022£
#' 
#' NB The 2002 contract does not directly tie pay to hours worked. While
#' nominally the basic contract is for 40h/week in sociable hours, in practice 
#' most rotas are issued at around 43-46 hours weekly with additional night and 
#' weekend work. This is compensated for by the allocation of one of several 
#' fixed 'banding' multipliers, chosen based on overall hours and unsociability.
#' 
#' In contrast, the 2016 contract explicitly predicates pay on hours worked, and
#' with the abolition of the banding system, base pay was increased while the
#' supplements supplied for unsociable hours were significantly reduced.
#' The overall effect was that total pay was similar, but base pay higher.
#' 
#+ fig.width = 8, fig.height = 8, dpi = 300

ggplot(data=pay.2022.hourly.DF, aes(x=year, y=pay)) +
  geom_line(aes(group=grade, color=grade)) +
  scale_y_continuous(name = "Gross Hourly Base Pay (Adjusted to 2022£ by RPI Index)", labels = scales::comma, limits=c(0,33)) +
  scale_x_discrete(name = "Year") +
  labs(color = "Grade") +
  ggtitle("Doctors' Base Pay 2007 - 2022: in hourly 2022£") +
  annotate(geom = "text", x = '2017', y = +Inf, label = "New Contract", angle=90, vjust = -0.5, hjust = 1.2 ) +
  annotate(geom = "rect", xmin = '2016', xmax = '2017', ymin = -Inf, ymax = Inf, fill = 'orange', alpha = 0.1) +
  geom_vline(xintercept = '2017', color = 'orange') +
  geom_area(data=minWage.2022.DF, fill = 'red', alpha=0.2, aes(x=year, y=minWage, group=group)) +
  geom_line(data=minWage.2022.DF, color = 'red2',linewidth=0.8, linetype='dashed', aes(x=year, y=minWage, group=group)) +
  annotate(geom = "text", x= +Inf, y = 9.46, label = "National Minimum Wage", size = 3, color='red4', vjust = 1, hjust = 1.2) +
  geom_line(data=livingWageNational.2022.DF, color = 'orangered', linetype='dashed',linewidth=0.5, aes(x=year, y=livingWageNational, group=group)) +
  annotate(geom = "text", x= +Inf, y = 11.04, label = "National Living Wage", size = 3, color='orangered2', vjust = -0.3, hjust = 1.2) +
  geom_line(data=livingWageLondon.2022.DF, color = 'orange', linetype='dashed',linewidth=0.5, aes(x=year, y=livingWageLondon, group=group)) +
  annotate(geom = "text", x= +Inf, y = 12.74, label = "London Living Wage", size = 3, color='orange2', vjust = -0.3, hjust = 1.2) +
  theme_minimal()

#' ## Fig.5: Change in Gross (Pre-Tax) Annual Pay, adjusted to 2022£
#+ fig.width = 8, fig.height = 8, dpi = 300

ggplot(data=payGross.2022.change.DF, aes(x=year, y=pay, group=grade)) +
  geom_line(aes(color=grade)) +
  scale_y_continuous(name = "Change in Gross Annual Pay since 2008 (Inflation Adjusted by RPI Index)", labels = scales::comma, limits=c(-30, 10)) +
  scale_x_discrete(name = "Year") +
  labs(color = "Grade") +
  ggtitle("Change in Doctors' Total Pay 2007 - 2022 (inflation adjusted)") +
  annotate(geom = "text", x= '2018', y = -Inf, label = "0.5 banding / 45h weekly with 10h unsociable and 1:4 weekends", size = 3, color='grey', vjust = -0.5) +
  annotate(geom = "text", x = '2017', y = +Inf, label = "New Contract", angle=90, vjust = -0.5, hjust = 1.2 ) +
  annotate(geom = "rect", xmin = '2016', xmax = '2017', ymin = -Inf, ymax = Inf, fill = 'orange', alpha = 0.1) +
  geom_vline(xintercept = '2017', color = 'orange') +
  geom_hline(yintercept = 0) +
  theme_minimal() 