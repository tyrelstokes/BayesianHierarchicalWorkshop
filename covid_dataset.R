#############################3
#############################
library(dplyr)
library(lubridate)
covid <- read.csv("confirmed_cases_table1_location.csv")


covid <- covid %>% mutate(month = month(notification_date),
                          year = year(notification_date),
                          week = week(notification_date),
                          case = 1)

debut <- "2020-02-28"

covid <- covid %>% mutate(pandemic_month = (interval(debut,notification_date)%/% months(1)))

covid_area <- covid %>% group_by(lhd_2010_name,year,month) %>% summarise(cases = sum(case))

covid_area <- covid_area %>% filter(lhd_2010_name !=""&(pandemic_month>0))


population_df <- data.frame(lhd_2010_name = c("South Western Sydney","Central Coast",
                                              "Far West","Hunter New England","Illawarra Shoalhaven",
                                              "Mid North Coast","Murrumbidgee",
                                              "Nepean Blue Mountains","Northern NSW",
                                              "Northern Sydney","South Eastern Sydney","Southern NSW",
                                              "Sydney","Western NSW","Western Sydney","Correctional settings"
                                              ), pop = c(996450,309000,28622,
                                                                                920370,
                                                         393204,308372,287073,
                                                         361656,290271,850385,991429,
                                                         200174,700000,270775,1100000,41000))



mn_year <- unique(data.frame(covid_area$year,covid_area$month))


districts <- unique(covid_area2$lhd_2010_name)

library(foreach)

aug_data <- foreach(i=1:(nrow(mn_year)+2), .combine = rbind)%do%{
  
  if(i < 25){
  myear <- mn_year[i,]
  
  dtf <- covid_area2 %>% filter((month == myear$covid_area.month)&(year == myear$covid_area.year))
  
  if(nrow(dtf)< length(districts)){
 data.frame(lhd_2010_name = districts[which(!(districts%in% dtf$lhd_2010_name))], month = myear$covid_area.month, year = myear$covid_area.year, cases = 0) 
  }
  }else{
    
    data.frame(lhd_2010_name = districts, month = i -22,year = 2022,cases = NA)
  }
  
}


covid2 <- rbind(covid_area,aug_data)



covid_area2 <- left_join(covid2,population_df,by = "lhd_2010_name")

covid_area3 <- covid_area2 %>% filter(lhd_2010_name != "Hotel Quarantine")




saveRDS(covid_area3,"covid_aus.Rds")








