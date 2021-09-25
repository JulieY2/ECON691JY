#This is Homework 2
# states: louisiana|texas|arkansas|tennessee|alabama

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(rvest)

#PART 1
# scrape data & create dataframe D_Votes

#Function % change of votes
pct<-function(x,y){
  temp<-x/y
  return(temp)
}

delta<-function(x,y){
  tem<-(y-x)/x
  return(tem)
}

countypres_2000.2020 <- read.csv("~/Downloads/dataverse_files/countypres_2000-2020.csv")
s<-c("ALABAMA", "ARKANSAS", "LOUISIANA" ,"TEXAS", "TENNESSEE")

states<-c("louisiana", "texas", "arkansas", "tennessee","alabama")

count2020<-filter(countypres_2000.2020, year==c("2020"), state %in% s, party!="OTHER")

count2016<-filter(countypres_2000.2020, year=="2016",state %in% s, party!="OTHER")

count_V<-merge(count2016, count2020, by.x=c("state", "county_name"), by.y=c("state", "county_name"))

D_Votes<-count_V %>%
  mutate(pct2016 = pct(count_V$candidatevotes.x, count_V$totalvotes.x),
         pct2020 = pct(count_V$candidatevotes.y, count_V$totalvotes.y),
         pctChange = delta(count_V$candidatevotes.x, count_V$candidatevotes.y))
#PART 2
# merge D_Votes with Census.2 
# map with the change in the vote for the Republic candidate 
# map with the change in the vote for the Democratic candidate

library(ggplot2)
library(sf)

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", "B02001_003","B05001_001","B05001_006","B07001_001", "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081" )

states<-c("alabama","arkansas","louisiana","tennessee","texas") 
fips<-c(01, 05, 22, 47, 48)

#API Command
k<-1

for(i in fips){
  acs<-get_acs(geography="county",
               variables = vars, 
               state = i,
               year  =  2019, 
               geometry  =  TRUE)
  
  temp<-acs %>%
    mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                                 variable=="B01001_002" ~ "Male",
                                 variable=="B02001_001" ~ "TotRace",
                                 variable=="B02001_002" ~ "White",
                                 variable=="B02001_003" ~ "Black",
                                 variable=="B05001_001" ~ "TotCit",
                                 variable=="B05001_006" ~ "NonCit",
                                 variable=="B07001_001" ~ "TotMob",
                                 variable=="B07001_017" ~ "Stay",
                                 variable=="B07001_033" ~ "SameCounty",
                                 variable=="B07001_049" ~ "SameSt",
                                 variable=="B07001_065" ~ "OthState",
                                 variable=="B07001_081" ~ "Abroad",
                                 TRUE ~ "other")) %>%
    select(!c(moe,variable)) %>%
    spread(key=variable2, value=estimate) %>%
    mutate(perMale = Male/TotPop,
           perWhite = White/TotPop,
           perBlack = Black/TotPop,
           perCit = 1-(NonCit/TotCit),
           perStay = Stay/TotMob,
           perSameCounty = SameCounty/TotMob,
           perSameSt = SameSt/TotMob,
           perOthState = OthState/TotMob,
           perAbroad = Abroad/TotMob) %>%
    select("GEOID","NAME",starts_with("per"),"geometry") %>%
    mutate(state = states[k])
  
  #assign(paste0(states[k],"census"),temp)
  ifelse(k==1, census.2<-temp, census.2<-rbind(census.2, temp))
  
  temp$area<-st_area(temp)
  map <- temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  
  #assign(paste0(states[k],"map"),map) 
  ifelse(k==1, MAP<-map, MAP<-rbind(MAP,map))
  k<-k+1
  rm(temp, map)
}

census.2<-census.2 %>%
  mutate(county = as.data.frame(str_split_fixed(NAME, ",", 2))[,1],
         county = trimws(gsub(" County", "", county)),
         county = trimws(gsub(" Parish", "", county)))

census.2$state<-toupper(census.2$state)
census.2$county<-toupper(census.2$county)

census.2$county[which(census.2$county=="LASALLE")]<- "LA SALLE"

Cdata<-merge(census.2, D_Votes, by.x = c("state", "county"), by.y = c("state", "county_name"), all=TRUE)

Republican<-filter(Cdata, party.x == "REPUBLICAN", party.y == "REPUBLICAN")

p1<-ggplot(Republican)+
  geom_sf(aes(fill = pctChange))+
 scale_fill_gradient(low="white",high="blue", limits = c(-1,1), 
                      aes(name="Percent Change"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())+
  geom_sf(
    data = MAP,
    fill=NA, colour="black",
    size=1,
    inherit.aes=FALSE
  )

Democrat<-filter(Cdata, party.x == "DEMOCRAT", party.y == "DEMOCRAT")

p2<-ggplot(Democrat)+
  geom_sf(aes(fill = pctChange))+
  scale_fill_gradient(low="red",high="white", limits = c(-1,1), 
                      aes(name="Percent Change"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())+
  geom_sf(
    data = MAP,
    fill=NA, colour="black",
    size=1,
    inherit.aes=FALSE
  )

plot_grid(p1,p2)

#PART 3
# regression

k<-1

for(i in fips){
  acs<-get_acs(geography="county",
               variables = vars, 
               state = i,
               year  =  2016, 
               geometry  =  TRUE)
  
  temp<-acs %>%
    mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                                 variable=="B01001_002" ~ "Male",
                                 variable=="B02001_001" ~ "TotRace",
                                 variable=="B02001_002" ~ "White",
                                 variable=="B02001_003" ~ "Black",
                                 variable=="B05001_001" ~ "TotCit",
                                 variable=="B05001_006" ~ "NonCit",
                                 variable=="B07001_001" ~ "TotMob",
                                 variable=="B07001_017" ~ "Stay",
                                 variable=="B07001_033" ~ "SameCounty",
                                 variable=="B07001_049" ~ "SameSt",
                                 variable=="B07001_065" ~ "OthState",
                                 variable=="B07001_081" ~ "Abroad",
                                 TRUE ~ "other")) %>%
    select(!c(moe,variable)) %>%
    spread(key=variable2, value=estimate) %>%
    mutate(perMale = Male/TotPop,
           perWhite = White/TotPop,
           perBlack = Black/TotPop,
           perCit = 1-(NonCit/TotCit),
           perStay = Stay/TotMob,
           perSameCounty = SameCounty/TotMob,
           perSameSt = SameSt/TotMob,
           perOthState = OthState/TotMob,
           perAbroad = Abroad/TotMob) %>%
    select("GEOID","NAME",starts_with("per"),"geometry") %>%
    mutate(state = states[k])
  
  #assign(paste0(states[k],"census"),temp)
  ifelse(k==1, census.1<-temp, census.1<-rbind(census.1, temp))
  
  temp$area<-st_area(temp)
  map <- temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  
  #assign(paste0(states[k],"map"),map) 
  ifelse(k==1, MAP.1<-map, MAP.1<-rbind(MAP.1,map))
  k<-k+1
  rm(temp, map)
}

census.3<-cbind(census.1, census.2)

census.3 <- census.3 %>%
  mutate(new_perMale = census.2$perMale - census.1$perMale,
         new_perWhite = census.2$perWhite - census.1$perMale,
         new_perBlack = census.2$perBlack - census.1$perBlack,
         new_perCit = census.2$perCit - census.1$perCit,
         new_perStay = census.2$perStay - census.1$perStay,
         new_perSameCounty = census.2$perSameCounty - census.1$perSameCounty,
         new_perSameSt = census.2$perSameSt - census.1$perSameSt,
         new_perOthState = census.2$perOthState - census.1$perOthState,
         new_perAbroad = census.2$perAbroad - census.1$perAbroad)

census.3<-census.3 %>%
  mutate(county = as.data.frame(str_split_fixed(NAME, ",", 2))[,1],
         county = trimws(gsub(" County", "", county)),
         county = trimws(gsub(" Parish", "", county))) %>%
  select("GEOID", "NAME", starts_with("new"), "geometry", "state", "county")
  
census.3$state<-toupper(census.3$state)
census.3$county<-toupper(census.3$county)

census.3$county[which(census.3$county=="LASALLE")]<- "LA SALLE"

REG<-merge(census.3, D_Votes, by.x=c("state","county"),by.y=c("state","county_name"), all=TRUE)

REG.R<-filter(REG, party.x == "REPUBLICAN", party.y=="REPUBLICAN")

REG.D<-filter(REG, party.x=="DEMOCRAT", party.y=="DEMOCRAT")

mod1<-lm(pctChange~perMale+perWhite, data = Republican)

mod2<-lm(pctChange~perMale+perWhite, data = Democrat)

mod3<-lm(pctChange~new_perWhite+new_perMale, data = REG.R)

mod4<-lm(pctChange~new_perWhite+new_perMale, data = REG.D)

mod5<-lm(pctChange~new_perWhite+new_perMale-1, data = REG.R)

mod6<-lm(pctChange~new_perWhite+new_perMale-1, data = REG.D)

library(cowplot)
library(stargazer)

stargazer(mod1, mod2, mod3, mod4, mod5,mod6, type="latex", out="./Build/Output/table.latex")
stargazer(mod1, mod2, mod3, mod4, mod5,mod6, type="html", out="./Build/Output/table2.html")
stargazer(REG, type = "latex", out="./Build/Output/summ1.latex")
stargazer(REG, type = "html", out="./Build/Output/summ1.html")
stargazer(REG.R, REG.D, type = "latex", out="./Build/Output/summ2.latex")
stargazer(REG.R, REG.D, type = "html", out="./Build/Output/summ2.html")
stargazer(D_Votes, type="html", out="./Build/Output/votes.html")
stargazer(Republican, Democrat, type="html", out="./Build/Output/data.html")

