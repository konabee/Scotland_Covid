library(readxl)
library(tidyverse)
library(janitor)

## Extract daily cases, with 0 to 4 and 5 to 14 age groups ##
sccases<-read_csv('https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/19646dce-d830-4ee0-a0a9-fcec79b5ac71/download/total_cases_agesex_20201114.csv')

## Extract trend data, 0-14 missing ## 
sctrend<-read_csv('https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4/download/trend_agesex_20201114.csv')

## keep only useful columns ## 
save<-c(1,2,3,5,7,9,11)

## arrange date into a form that is recognizable by R, yyyy-mm-dd ## 
sccases$Date<-gsub('(....)(..)(.*)','\\1-\\2-\\3',sccases$Date)

sc<-sccases %>% 
  select(save) %>%
  filter(AgeGroup!='Total')%>%
  mutate(
    Date = format(as.Date(Date),'%d.%m.%Y'),
    Age = recode(AgeGroup,
                 '0 to 4' = 0,
                 '5 to 14' = 5,
                 '15 to 19' = 15,
                 '20 to 24' = 20,
                 '25 to 44' = 25,
                 '45 to 64' = 45,
                 '65 to 74' = 65,
                 '75 to 84' = 74,
                 '85plus' = 85),
    Sex = recode(Sex,'Female' = 'f',
                 'Male' = 'm'),
    AgeInt = case_when(
      Age == 0 ~ 5,
      Age == 5 ~ 10,
      Age == 15 ~ 5,
      Age == 20 ~ 5,
      Age == 25 ~ 20,
      Age == 45 ~ 20,
      Age == 65 ~ 10,
      Age == 74 ~ 10,
      Age == 85 ~ 20),
    Tests = TotalPositive + TotalNegative) %>% 
  rename(Deaths = TotalDeaths,
         Cases = TotalPositive) %>% 
  select(-AgeGroup,-TotalNegative,-Country)

## create rows for both sexes ## 
Date<-rep(sc$Date[1],9)
Sex<-rep('b',9)
Cases<-rep(0,9)
Deaths<-rep(0,9)
Tests<-rep(0,9)
Age<-c(sc$Age[1:9])
AgeInt<-c(sc$AgeInt[1:9])

x<-data.frame(Date,Sex,Cases,Deaths,Tests,Age,AgeInt)
x$Cases<-sc$Cases[1:9]+sc$Cases[10:18]
x$Deaths<-sc$Deaths[1:9]+sc$Deaths[10:18]
x$Tests<-sc$Tests[1:9]+sc$Tests[10:18]

all<-bind_rows(sc,x,.id=NULL)

## wide to long ## 
sclong<-gather(all,Measure,Value,Cases,Deaths,Tests,factor_key=T)

sclong<-sclong %>% mutate(
  Country = 'Scotland',
  Region = 'All',
  Metric = 'Count',
  Code = paste0('GB_SCO_',Date))

## reorder columns ##
colorder<-c('Country','Region','Code','Date','Sex','Age','AgeInt','Metric','Measure','Value')
sclong<-sclong[,colorder]

## now deal with historical trend data, keep useful columns ##
save2<-c(1,3,5,8,11,13)

## arrange date into a form that is recognizable by R, yyyy-mm-dd ## 
sctrend$Date<-gsub('(....)(..)(.*)','\\1-\\2-\\3',sctrend$Date)

## keep record starting March 09, 2020 ## 
sctrend<-sctrend[241:nrow(sctrend),]

sct<-sctrend %>% 
  select(save2) %>%
  mutate(
    Date = as.POSIXct(Date),
    Age = recode(AgeGroup,
                 'Total' = 99,
                 '15 to 19' = 15,
                 '20 to 24' = 20,
                 '25 to 44' = 25,
                 '45 to 64' = 45,
                 '65 to 74' = 65,
                 '75 to 84' = 74,
                 '85plus' = 85),
    Sex = recode(Sex,'Female' = 'f',
                 'Male' = 'm',
                 'Total' = 'b'),
    AgeInt = case_when(
      Age == 0 ~ 15,
      Age == 15 ~ 5,
      Age == 20 ~ 5,
      Age == 25 ~ 20,
      Age == 45 ~ 20,
      Age == 65 ~ 10,
      Age == 74 ~ 10,
      Age == 85 ~ 20),
    Tests = CumulativePositive + CumulativeNegative) %>% 
  rename(Deaths = CumulativeDeaths,
         Cases = CumulativePositive) %>% 
  select(-AgeGroup,-CumulativeNegative)

## create 0-14 number cases deaths and test ## 
a<- sct %>% slice(seq(8, n(), by = 8)) %>% select(Date,Age,Sex,Tests,Deaths,Cases) ## the total 
b<- sct %>% slice(-seq(8,n(), by = 8)) %>% select(Tests,Deaths,Cases) ## all other age groups
ind<-c(rep(7,floor(dim(b)[1]/7)),floor(dim(b)[1]%%7))
ind<-rep(1:length(ind),times=ind)    
c<-apply(b, 2, function(x) tapply(x, ind, sum)) %>% data.frame()%>% bind_cols(a[,1:3]) ## sum of all other age groups 
c$Tests<-a$Tests-c$Tests
c$Deaths<-a$Deaths-c$Deaths
c$Cases<-a$Cases-c$Cases
c$AgeInt<-15
c$Age<-0
colorder<-colnames(sct)
c<-c[,colorder]

## drop Age==99 and add Age==0 ##
sctnew<-sct %>% filter(Age!=99) %>% bind_rows(c) %>% arrange(Date,Age,Sex)

sctlong<-gather(sctnew,Measure,Value,Cases,Deaths,Tests,factor_key=T)
sctlong$Date<-format(as.Date(sctlong$Date),'%d.%m.%Y')
sctlong<-sctlong %>% mutate(
  Country = 'Scotland',
  Region = 'All',
  Metric = 'Count',
  Code = paste0('GB_SCO_',Date))

colorder<-c('Country','Region', 'Code','Date','Sex','Age','AgeInt','Metric','Measure','Value')
sctlong<-sctlong[,colorder]
sctall<-bind_rows(sctlong,sclong) 

## push to googlesheets ## 
googlesheets4::sheet_write(sctall,ss="https://docs.google.com/spreadsheets/d/1PE0Fyp9sp7m97SmZ0eu6wTJCzYfQWk_-0iCr0yhzdkM/edit#gid=1079196673",sheet = "database")


