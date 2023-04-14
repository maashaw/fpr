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
    - Living Wage:
      - Information from the Living Wage on historical living wage values. Retrieved from Livingwage.org.uk.
    - House of Commons Library:
      - Information on current and historic values of the national minimum wage.
  - ### Data Products
    - RData:
      - Pay: Raw base pay values for each grade, year-by-year, from 2007-2022
      - NROC: Non-Resident On-Call supplements for each grade, year-by-year, from 2017-2022
      - Inflation: Inflation measures year-by-year, including absolute measures and multipliers
  - ### Analyses:
    - R:
      - Example: An example script that just imports the data products from RData. Also includes some example RMarkdown for compiling reports.
      - Graphs: Generates some graphs illustrating doctors' pay over time
    - Reports:
      - Example: The compiled report produced from the Example File
      - Graphs: The compiled report produced from the Graphs File
      
## Troubleshooting
  - ### Why do I get "File Not Found" when I try to compile the reports:
    Make sure that you set the RMarkdown working directory to mirror the project's working directory. To do this, go into Tools > Global Options > RMarkdown and set "Evaluate chunks in directory:" to "Project"
    
    # Graphs

## Fig.1
![Graph-1](https://user-images.githubusercontent.com/72826751/229305930-5908b2a0-5397-4655-937c-668db2952ed0.png)

## Fig.2
![Graph-2](https://user-images.githubusercontent.com/72826751/229305948-8f780221-fc05-4f27-a181-465ca3b10839.png)

## Fig.3
![Graph-3](https://user-images.githubusercontent.com/72826751/229305951-8b454548-9dba-4cc1-8400-9034b9f66327.png)

## Fig.4

NB The 2002 contract does not directly tie pay to hours worked. While nominally the basic contract is for 40h/week in sociable hours, in practice most rotas are issued at around 43-46 hours weekly with additional night and weekend work. This is compensated for by the allocation of one of several fixed ‘banding’ multipliers, chosen based on overall hours and unsociability.

In contrast, the 2016 contract explicitly predicates pay on hours worked, and with the abolition of the banding system, base pay was increased while the supplements supplied for unsociable hours were significantly reduced. The overall effect was that total pay was similar, but base pay higher.

![Graph-4](https://user-images.githubusercontent.com/72826751/229311541-7f4bae0a-7e74-4d18-8dfc-0baff146d39b.png)

## Fig.5
![Graph-5](https://user-images.githubusercontent.com/72826751/229311576-a64b0644-8fe2-4c15-b175-53f0acce17ae.png)
