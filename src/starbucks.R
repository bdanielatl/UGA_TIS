library(readxl)

library(readr)
library(sqldf)
library(dplyr)
library(tidyr) #for reshaping data
library(zipcode)
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

Income2013 <- read_csv("~/UGA/Thinking in SQL/TIS_UGA/data/13zpallagi.csv")
Income2013$YEAR<-2013
Income2013<-select(Income2013, one_of(columnlist))
Income2013$zipcode<-clean.zipcodes(Income2013$zipcode)


Income2014<-read_csv("~/UGA/Thinking in SQL/TIS_UGA/data/14zpallagi.csv")
Income2014$YEAR<-2014
Income2014<-select(Income2014, one_of(columnlist))
Income2014$zipcode<-clean.zipcodes(Income2014$zipcode)


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

df3<-inner_join(df1,df2,by=c("zipcode","YEAR","IncomeState"))

df3$has_starbucks<-df3$storecount.x > 0




