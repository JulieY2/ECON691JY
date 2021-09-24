#This is Homework 2
# states: louisiana|texas|arkansas|tennessee|alabama

rm(list=ls())

library(tidyverse)
library(rvest)

#PART 1 
# puling information from NY post for 2016 votes

states<-c("louisiana", "texas", "arkansas", "tennessee", "alabama")

for(i in states){
  
  url<-paste0("https://www.nytimes.com/elections/2016/results/", i)
  
  webpage<-read_html(url)
  
  tables<-webpage %>%
    html_nodes("table")
  
  results<-as.data.frame(html_table(tables[2], header=TRUE, fill = TRUE ))
  
  temp<-results %>%
    rename("County"="Vote.by.county") %>%
    mutate("Clinton" = as.numeric(gsub(",","",Clinton)),
           "Trump" = as.numeric(gsub(",","",Trump)),
           "pctClinton" = (Clinton)/(Clinton+Trump),
           "pctTrump" = (Trump)/(Clinton+Trump),
           "State" = i)
  
  assign(i, temp)
}

votes<-rbind(louisiana, texas, arkansas, tennessee, alabama)

library(tidycensus)

# PART 2
# puling information from census from 2016 for 
#louisiana|texas|arkansas|tennessee|alabama
# and combinig inforamation to census.1

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", 
        "B02001_003","B05001_001","B05001_006","B07001_001", 
        "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081")
  
acs.1 <- get_acs(geography = "county",
               variables = vars,
               state = 01,
               year = 2016,
               geometry = TRUE)

al.acs<-acs.1 %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

acs.2 <- get_acs(geography = "county",
                 variables = vars,
                 state = 05,
                 year = 2016,
                 geometry = TRUE)

ar.acs<-acs.2 %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

acs.3 <- get_acs(geography = "county",
                 variables = vars,
                 state = 22,
                 year = 2016,
                 geometry = TRUE)

la.acs<-acs.3 %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

acs.4 <- get_acs(geography = "county",
                 variables = vars,
                 state = 47,
                 year = 2016,
                 geometry = TRUE)

tn.acs<-acs.4 %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

acs.5 <- get_acs(geography = "county",
                 variables = vars,
                 state = 48,
                 year = 2016,
                 geometry = TRUE)

tx.acs<-acs.5 %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

census.1<-rbind(al.acs, ar.acs, la.acs, tn.acs, tx.acs)

# puling census information from 2019 for
# louisiana|texas|arkansas|tennessee|alabama
# and combining it to census.2

acs.1.1 <- get_acs(geography = "county",
                 variables = vars,
                 state = 01,
                 year = 2019,
                 geometry = TRUE)

al.1.acs<-acs.1.1 %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

acs.1.2 <- get_acs(geography = "county",
                 variables = vars,
                 state = 05,
                 year = 2019,
                 geometry = TRUE)

ar.1.acs<-acs.1.2 %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

acs.1.3 <- get_acs(geography = "county",
                 variables = vars,
                 state = 22,
                 year = 2019,
                 geometry = TRUE)

la.1.acs<-acs.1.3 %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

acs.1.4 <- get_acs(geography = "county",
                 variables = vars,
                 state = 47,
                 year = 2019,
                 geometry = TRUE)

tn.1.acs<-acs.1.4 %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

acs.1.5 <- get_acs(geography = "county",
                 variables = vars,
                 state = 48,
                 year = 2019,
                 geometry = TRUE)

tx.1.acs<-acs.1.5 %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

census.2<-rbind(al.1.acs, ar.1.acs, la.1.acs, tn.1.acs, tx.1.acs)

# finding the difference between census.2(2019 data) & census.1(2016 data)
# since all data is in %, so % change is simple subtraction

C_1<-tibble(census.1$perMale, census.1$perWhite,census.1$perBlack, census.1$perCit,
            census.1$perStay, census.1$perSameCounty,census.1$perSameSt, 
            census.1$perOthState, census.1$perAbroad)
C_2<-tibble(census.2$perMale, census.2$perWhite,census.2$perBlack, census.2$perCit,
            census.2$perStay, census.2$perSameCounty,census.2$perSameSt, 
            census.2$perOthState, census.2$perAbroad)
C_2-C_1

# create data frame census.3 with percentage change, names and geometry

census.3<- cbind2(census.1, census.2)

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

select(census.3, c("GEOID", "NAME", starts_with("new"), "geometry"))

# PART 3
# displaying the map

library(ggplot2)
library(cowplot)

#Remove the added text in ACS data
al.acs<-al.acs %>%
  mutate(County = as.data.frame(str_split_fixed(NAME, ",", 2))[,1],
         County = trimws(gsub(" County", "", County)))

#Sort both data so they have the same sorting process
al.acs<-al.acs[order(acs.1$NAME),]
alabama<-alabama[order(alabama$County),]
#Logic test to make sure the names match
al.acs$County==alabama$County

al.acs<-merge(al.acs, alabama ,by="County",all=TRUE)
b<-al.acs[is.na(al.acs$perWhite),]
al.acs<-al.acs[!is.na(al.acs$perWhite),]

map1<-ggplot(al.acs)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

map2<-ggplot(al.acs)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="green",high="white",limits=c(0,1),aes(name="Percent
White"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(map1,map2)



