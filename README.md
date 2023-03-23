# fpr

Since 2008, pay for Junior Doctors - any doctor below the level of consultant, many of whom are senior decision-makers, some of whom have over a decade of experience in medicine, and all of whom are invaluable members of the team - have seen inflation-adjusted pay cuts of 26%.

This repo pulls together a number of data sources to illustrate changes to doctors' pay over the last 15 years, to make this data available for further analysis and exploration.

## Contents
  - ### Data Sources
    - ONS Statistics:
      - CPI data
      - RPI data
    - Pay Circulars:
      - NHS Pay Circulars from 2007 - 2022
  - ### Data Products
    - RData:
      - Pay: Raw base pay values for each grade, year-by-year, from 2007-2022
      - NROC: Non-Resident On-Call supplements for each grade, year-by-year, from 2017-2022
      - Inflation: Inflation measures year-by-year, including absolute measures and multipliers
  - ### Analyses:
    - R:
      - Example: An example script that just imports the data products from RData. Also includes some example RMarkdown for compiling reports.
    - Reports:
      - Example: The compiled report produced from the Example File
      
## Troubleshooting
  - ### Why do I get "File Not Found" when I try to compile the reports:
    Make sure that you set the RMarkdown working directory to mirror the project's working directory. To do this, go into Tools > Global Options > RMarkdown and set "Evaluate chunks in directory:" to "Project"
      