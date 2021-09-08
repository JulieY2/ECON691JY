#Script for introducing scripts
#created on August 25, 2021

rm(list=ls())

library(tidyverse)

#Function####

delta <-function(x){
  temp<-((x-lag(x))/lag(x))
  return(temp)
}

library(readr)
ILCovid19 <- read_csv("~/Desktop/ECON 691/Data Files/ILCovid19.csv", show_col_types = FALSE)

covidIL<-ILCovid19 %>%
  mutate(pc_test = delta(Tests),
         pc_positives = delta(Positives),
         pc_deaths = delta(Deaths))

covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)]<-NA

covidIL$Date<-as.Date(covidIL$Date, format="%m/%d/%Y")

plot(covidIL$Date, covidIL$pc_positives)

plot(covidIL$Date, covidIL$pc_positives,
     main="Percent cases",
     xlab="",
     ylab="",
     type="l",
     col="blue")


