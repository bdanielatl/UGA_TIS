source('./src/starbucks.R')
##data visualizaiton
library(tidyr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(plotly)
#boxplot the total income
#p<-plot_ly(df3,y=~total_income,x=~has_starbucks,  type="box")
#p

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


##This is the key to putting the data into the right shape. (the melt procedure)
#income
dfmeltbase<-melt(df3,measure.vars=7:12)%>%filter(YEAR==2014 & variable %in% c("agi_four", "agi_five","agi_six"))

dfmelt<-  inner_join(dfmeltbase, dflegend,by=c("variable" = "label"))%>%
arrange(position)%>%
#number of returns in each bucket
#dfmelt<-melt(df3,measure.vars=15:20)%>%filter(YEAR==2014)%>%
select(one_of(c("variable", "value","has_starbucks","bucket","zipcode","IncomeState")))

#boxplot the data to see how the data is distributed among various dimensions
dfmelt$bucketf = factor(dfmelt$bucket,levels=c("75,000 under 100,000","100,000 under 200,000","200,000 or more"))

ggplot(dfmelt,  aes(x=has_starbucks, y=value,fill=bucket))+
  geom_boxplot()+
#  geom_point(aes(text=paste("zipcode:",zipcode)))+
  #facet_grid(.~bucket)
  facet_grid(.~bucketf)
#ggplotly()



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
(sqldf("select sum(distinct total_income) as total_income,sum(distinct total_returns_filed) as total_returns_filed,
count(distinct zipcode) as num_zip_codes,IncomeState from dfNewLocationList
      group by incomestate order by sum(distinct total_income) desc"))


