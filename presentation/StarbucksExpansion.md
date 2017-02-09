---
title: "Where should starbucks go next?"
output: html_document
---
# Intro
Starbucks is one of the best known brands in the world with thousands of locations.  There are still zipcodes in the United States without a way to get your hyper-caffienated-four-dollar fix.  Can we use data to make an informed decision about where Starbucks could expand next?

The train of thought is that Starbucks locations tend to be found in affluent and populous areas.  By using publicly available data, we can overlay income data from zip codes and compare them by set: those having and lacking a Starbucks.

Note: this presentation was given to the University of Georgia in February 2017; it was not put into a an R Markdown presentation so that more code could be demonstrated.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## The Data

1. Starbucks Locaitons in the U.S. (https://opendata.socrata.com/Business/All-Starbucks-Locations-in-the-US-Map/ddym-zvjk)
2. IRS Income Data Statistics by Zip Code for two years.
3. Zipcodes Package (for cleaning up and providing Lat-Lon info)

## The Data Loading Code

```{r echo=TRUE, eval=TRUE, results="hide", message=FALSE}
library(readxl)
library(readr)
library(sqldf)
library(dplyr)
library(tidyr) #for reshaping data
library(zipcode)
library(plotly)
library(tidyr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(plotly)

#clean up all variables before running from the top
rm(list=ls(all=TRUE))

data(zipcode)

#read in the startbucks data
starbuckslocations <- read_csv("~/UGA/Thinking in SQL/TIS_UGA/data/starbuckslocations.csv")
starbuckslocations$Zip = clean.zipcodes(starbuckslocations$Zip)

#note: always, always, always assume that your data is dirty.  Some of the zip codes from the raw file had their
#leading zero pulled off for zips in the northeast (3904-> 03904)

#create a 5 character zip column 
starbuckslocations$zip_five <-substr(starbuckslocations$Zip,1,5)

starbuckslocations<-sqldf("select count(*) as storecount,State,zip_five from starbuckslocations 
                          group by State,zip_five")

#View(sqldf("select * from starbuckslocations"))
#how many starbucks are in each state?
#View(sqldf("select count(*) as storecount,state from starbuckslocations group by state order by count(*) desc"))

#sum up the total income by zip code from the IRS Data
#https://www.irs.gov/uac/soi-tax-stats-individual-income-tax-statistics-2014-zip-code-data-soi
#and https://www.irs.gov/uac/soi-tax-stats-individual-income-tax-statistics-2013-zip-code-data-soi

#agi stub could be very useful because the bigger the number the more people have
#higher incomes.  #lesson : READ THE DATA DICTIONARY.
columnlist<-c("STATE","N1", "zipcode","agi_stub","A02650","YEAR")
if(!exists("Income2013"))
{
Income2013 <- read_csv("~/UGA/Thinking in SQL/TIS_UGA/data/13zpallagi.csv")
Income2013$YEAR<-2013
Income2013<-select(Income2013, one_of(columnlist))
Income2013$zipcode<-clean.zipcodes(Income2013$zipcode)
}

if(!exists("Income2014"))
{
Income2014<-read_csv("~/UGA/Thinking in SQL/TIS_UGA/data/14zpallagi.csv")
Income2014$YEAR<-2014
Income2014<-select(Income2014, one_of(columnlist))
Income2014$zipcode<-clean.zipcodes(Income2014$zipcode)
}

IncomeData<-rbind(Income2013,Income2014)
IncomeData$IncomeState<-IncomeData$STATE
IncomeData <- subset(IncomeData, select = -c(STATE) )
IncomeData$TotalIncome <- IncomeData$A02650
IncomeData <- subset(IncomeData, select = -c(A02650) )
#get rid of any income data with zip code 99999; it means they had less than 100 returns
#this could also be single resident zip codes
#•	ZIP codes with less than 100 returns and those identified as a single 
#building or nonresidential ZIP code were categorized as “other” (99999).
IncomeData<-sqldf("select * from IncomeData where zipcode not in ('99999','00000')")

rm(Income2014)
rm(Income2013)
#join the starbucks dataset to the IRS income dataset
# I need one row per zip code for the starbucks locations.

#first recognize a good problem: there are many things you can learn from this data
#and its easy to go 100 different directions and get NO WHERE
#the data is at different levels, you will need to clean, aggregate, and average
#before you can analyze

IncomeData$zipcode<-as.character(IncomeData$zipcode)

all_data<-full_join(IncomeData,starbuckslocations,by=c("zipcode"="zip_five"),copy=TRUE)

#data quality check: count the numbe of total observations
total_rows<-nrow(IncomeData)
#how many starbucks locations do not have income associated
sb_zip_no_income<-nrow(dplyr::filter(all_data,is.na(`N1`)))
sb_zip_no_income_pct = (sb_zip_no_income/total_rows)*100
#hom many income zip codes do not have starbucks
income_zip_no_sb<-nrow(dplyr::filter(all_data,is.na(`storecount`)))
income_zip_no_sb_pct = (income_zip_no_sb/total_rows)*100

#View(sqldf("select * from all_data where storecount is null"))


#analysis and visualization ideas
#put the number of starbucks on the map by zip code and see how they correlate with a heat map.
#do a linear model predicting the number of starbucks in a zip code by income
set.seed(8265)

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
#analysis and visualization ideas
#put the number of starbucks on the map by zip code and see how they correlate with a heat map.
#Idea: do a linear model predicting the number of starbucks in a zip code by income

all_data$storecount<-na.zero(all_data$storecount)

#filter these records out. income data is a critical factor
#View(sqldf("select * from all_data where IncomeState is null"))
all_data <- filter(all_data, !is.na(IncomeState))


income_data<-tidyr::spread(all_data,agi_stub,TotalIncome)
income_data<-income_data[,1:12]
#income_data<-filter(income_data,!is.na(IncomeState))
return_count<-tidyr::spread(all_data,agi_stub,N1)
return_count<-return_count[,1:12]
#return_count<-filter(return_count,!is.na(IncomeState))
income_data$storecount<-na.zero(income_data$storecount)
income_data$`1`<-na.zero(income_data$`1`)
income_data$`2`<-na.zero(income_data$`2`)
income_data$`3`<-na.zero(income_data$`3`)
income_data$`4`<-na.zero(income_data$`4`)
income_data$`5`<-na.zero(income_data$`5`)
income_data$`6`<-na.zero(income_data$`6`)
return_count$storecount<-na.zero(return_count$storecount)
return_count$`1`<-na.zero(return_count$`1`)
return_count$`2`<-na.zero(return_count$`2`)
return_count$`3`<-na.zero(return_count$`3`)
return_count$`4`<-na.zero(return_count$`4`)
return_count$`5`<-na.zero(return_count$`5`)
return_count$`6`<-na.zero(return_count$`6`)


names(income_data)[7]<-"agi_one_income"
names(income_data)[8]<-"agi_two_income"
names(income_data)[9]<-"agi_three_income"
names(income_data)[10]<-"agi_four_income"
names(income_data)[11]<-"agi_five_income"
names(income_data)[12]<-"agi_six_income"

names(return_count)[7]<-"agi_one_nt"
names(return_count)[8]<-"agi_two_nt"
names(return_count)[9]<-"agi_three_nt"
names(return_count)[10]<-"agi_four_nt"
names(return_count)[11]<-"agi_five_nt"
names(return_count)[12]<-"agi_six_nt"


```

### Data Slices
```{r eval=TRUE}
library(xtable)



df1<-(sqldf("select zipcode,year,incomestate, 
                 sum(agi_one_income)+sum(agi_two_income)+sum(agi_three_income)
                  +sum(agi_four_income)+sum(agi_five_income)+sum(agi_six_income) as total_income, 
                 sum(N1) as total_returns_filed,  sum(storecount) as storecount,
                 sum(agi_one_income) as agi_one,
                 sum(agi_two_income) as agi_two,
                 sum(agi_three_income) as agi_three,
                 sum(agi_four_income) as agi_four,
                 sum(agi_five_income) as agi_five,
                 sum(agi_six_income) as agi_six
                 from income_data 
                 group by zipcode,year,incomestate"))

head(df1,10)
```

```{r}
df2<-(sqldf("select zipcode,year,incomestate, 
                 sum(agi_one_nt)+sum(agi_two_nt)+sum(agi_three_nt)
                  +sum(agi_four_nt)+sum(agi_five_nt)+sum(agi_six_nt) as total_nt, 
                   sum(storecount) as storecount,
                 sum(agi_one_nt) as nt_one,
                 sum(agi_two_nt) as nt_two,
                 sum(agi_three_nt) as nt_three,
                 sum(agi_four_nt) as nt_four,
                 sum(agi_five_nt) as nt_five,
                 sum(agi_six_nt) as nt_six
                 from return_count 
                 group by zipcode,year,incomestate"))
head(df2,10)
```

```{r}
df3<-inner_join(df1,df2,by=c("zipcode","YEAR","IncomeState"))
#tag zipcodes where there is no starbucks
df3$has_starbucks<-df3$storecount.x > 0

head(df3,10)
```

```{r}
#this is just for formatting and creating a legend
# 1 = $1 under $25,000
# 2 = $25,000 under $50,000
# 3 = $50,000 under $75,000
# 4 = $75,000 under $100,000
# 5 = $100,000 under $200,000
# 6 = $200,000 or more
label = c("agi_one","agi_two","agi_three","agi_four","agi_five","agi_six")
bucket = c("1 under 25,000","25,000 under 50,000","50,000 under 75,000","75,000 under 100,000","100,000 under 200,000","200,000 or more")
position = c(1,2,3,4,5,6)
dflegend<-data.frame(label,bucket,position,stringsAsFactors = FALSE)

```

##The Analysis Code
Starbucks tends to target more affluent areas, or households filing in the upper buckets of our data distribution. This analysis will look at agi buckets four through six for 2014.

```{r}
#change the shape of the data before we plot and analyze
dfmeltbase<-melt(df3,measure.vars=7:12)%>%filter(YEAR==2014 & variable %in% c("agi_four", "agi_five","agi_six"))

head(dfmeltbase,10)

```

Let's box plot and compare the two types of zip codes (those having vs. those lacking a Starbucks).
```{r}
dfmelt<-  inner_join(dfmeltbase, dflegend,by=c("variable" = "label"))%>%
arrange(position)%>%
#number of returns in each bucket
#dfmelt<-melt(df3,measure.vars=15:20)%>%filter(YEAR==2014)%>%
select(one_of(c("variable", "value","has_starbucks","bucket","zipcode","IncomeState")))

#boxplot the data to see how the data is distributed among various dimensions
dfmelt$bucketf = factor(dfmelt$bucket,levels=c("75,000 under 100,000","100,000 under 200,000","200,000 or more"))

ggplot(dfmelt,  aes(x=has_starbucks, y=value,fill=bucket))+
  geom_boxplot()+
  geom_point(aes(text=paste("zipcode:",zipcode)))+
  #facet_grid(.~bucket)
  facet_grid(.~bucketf)

```

Now let's take a percentile of zip codes without a starbucks and those that are above the 75th percentile in income could qualify for expansion.
```{r}
dfmedian<-  inner_join(dfmeltbase, dflegend,by=c("variable" = "label"))

#df %>% group_by(a) %>% summarise_each(funs(sum)) the line below was adapted from this line 
##this line is very powerful, what it is doing is grouping by each bucket and then for each one
##finding the median. YOU CANNOT DO THIS NATIVELY IN SQL OR ON LIGHTER SQL PLATFORMS BECAUSE
##FINDING THE MEIDAIN INVOLVES SORTING THE DATA. R does this inherently; median could be replaced
##with any other analytic function.
dfresult<-dfmedian %>% group_by(bucket) %>% summarise(med_val=(quantile(value,.75)))

#join the dfmelt "table" with the dfresult table and get all zip codes without starbucks for each bucket
#that have a gross reported income of at least the median or more

dfnewzips<-inner_join (dfmelt,dfresult,by=c("bucket"))%>%
  filter(value >= med_val & has_starbucks==FALSE)%>%select(one_of(c("variable","bucket", "value","med_val","zipcode","IncomeState")))%>%
  arrange(desc(value))

dfNewLocationList<-melt(df3,measure.vars=7:12)%>%
  filter(YEAR==2014 & variable %in% c("agi_four", "agi_five","agi_six"))%>%
  filter(paste0(variable,zipcode) %in% paste0(dfnewzips$variable,dfnewzips$zipcode))

#data quality check: google results show over 2000 zipcodes in New York; 692 of them could be new starbucks locations
results<-sqldf("select sum(distinct total_income) as total_income,sum(distinct total_returns_filed) as total_returns_filed,
count(distinct zipcode) as num_zip_codes,IncomeState from dfNewLocationList
      group by incomestate order by sum(distinct total_income) desc")

results
```
