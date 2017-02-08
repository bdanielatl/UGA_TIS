source('./src/starbucks.R')
##data visualizaiton
library(tidyr)
library(reshape2)
library(dplyr)
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
dfmelt<-melt(df3,measure.vars=7:12)%>%filter(YEAR==2014 & variable %in% c("agi_four", "agi_five","agi_six"))

dfmelt<-  inner_join(dfmelt, dflegend,by=c("variable" = "label"))%>%
arrange(position)%>%
#number of returns in each bucket
#dfmelt<-melt(df3,measure.vars=15:20)%>%filter(YEAR==2014)%>%
select(one_of(c("variable", "value","has_starbucks","bucket")))

dfmelt$bucketf = factor(dfmelt$bucket,levels=c("75,000 under 100,000","100,000 under 200,000","200,000 or more"))

ggplot(dfmelt,  aes(x=has_starbucks, y=value,fill=bucket))+
  geom_boxplot()+
  #facet_grid(.~bucket)
  facet_grid(.~bucketf)
ggplotly()





