# Scotland_Covid
COVID cases, deaths and tests

This code extracts the most recent numbers on COVID cases, deaths and tests performed in Scotland using two separate files provided by NHS Scot.

(Historical) Trend data: https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4
Total cases by age and sex (daily): https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/19646dce-d830-4ee0-a0a9-fcec79b5ac71

It is important to note that the second file provides better detail, since age groups include 0 to 4 and 5 to 14. The trend data does not provide information on individuals 0-14. The first age group is 15 to 19. I created a group, 0 to 14, which captures all cases, deaths, and tests derived from the difference between the daily total and the daily total of all other age groups (15 to 85 +). 
