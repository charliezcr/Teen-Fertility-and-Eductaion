## ----setup, include=FALSE------------------------------------------------
library(dplyr)
library(sandwich)
library(lmtest)
library(ggplot2)
library(AER)
library(foreign)


## ------------------------------------------------------------------------
wdi <- read.csv("wdi.csv")
df <- na.omit(wdi %>% select(countryname,year,sp_ado_tfrt,se_sec_nenr))
df <- rename(df,fertility=sp_ado_tfrt,enrollment=se_sec_nenr)
countries <- unique(df$countryname)
for (i in countries){
  dfsubset <- df %>% filter(countryname==i)
  #remove countries with fewer than 5 entries
  if (nrow(dfsubset)<5){
    countries = countries[countries!=i]
  }
}
df <- subset(df,countryname %in% countries)


## ------------------------------------------------------------------------
set.seed(65)
sample.countries<- sample(countries,40)
#excluding regions, only include countries
sample.countries = sample.countries[-c(5,6,14,17,22,23,32,33,38)]
df2 <- subset(df,countryname %in% sample.countries)


## ------------------------------------------------------------------------
description <- data.frame(country=character(),start_year=integer(),end_year=integer(),average_secondary_school_enrollment=double(),average_teen_fertility_rate=double())
for (i in sample.countries){
  descrdf <- df2 %>% filter(countryname==i)
  descr <- data.frame(country=i,start_year=min(descrdf$year),end_year=max(descrdf$year),average_secondary_school_enrollment=mean(descrdf$enrollment),average_teen_fertility_rate=mean(descrdf$fertility))
  description <- rbind(description,descr)
}
description


## ------------------------------------------------------------------------
Peru <- wdi %>% filter(countryname=="Peru",year>1992,year<2003) %>% select(year,sp_ado_tfrt,se_sec_enrr)
Peru <- rename(Peru,fertility=sp_ado_tfrt,enrollment.gross=se_sec_enrr)
Colombia <- wdi %>% filter(countryname=="Colombia",year>1992,year<2003) %>% select(year,sp_ado_tfrt,se_sec_enrr)
Colombia <- rename(Colombia,fertility=sp_ado_tfrt,enrollment.gross=se_sec_enrr)


## ------------------------------------------------------------------------
for (i in 1:length(Colombia$enrollment.gross)){
  if (is.na(Colombia$enrollment.gross[i])){
    Colombia$enrollment.gross[i]<-(Colombia$enrollment.gross[i-1]+Colombia$enrollment.gross[i+1])/2
  }
}


## ------------------------------------------------------------------------
p1 <- ggplot(Peru,aes(x=year))+
  geom_line(aes(y=fertility,color="teen fertility rate"))+
  geom_line(aes(y=enrollment.gross,color="secondary school enrollment rate"))+
  ggtitle("Peru")+
  xlab("Year")+
  ylab("Percentage")+
  ylim(50,90)+
  scale_x_continuous("year", labels = as.character(Peru$year), breaks = Peru$year)+
  geom_vline(xintercept = 1997)
dfperu <- data.frame(country="Peru",start_year=min(Peru$year),end_year=max(Peru$year),average_secondary_school_enrollment=mean(Peru$enrollment.gross),average_teen_fertility_rate=mean(Peru$fertility))
p2 <- ggplot(Colombia,aes(x=year))+
  geom_line(aes(y=Colombia$fertility,color="teen fertility rate"))+
  geom_line(aes(y=Colombia$enrollment.gross,color="secondary school enrollment rate"))+
  ggtitle("Colombia")+
  xlab("Year")+
  ylab("Percentage")+
  ylim(50,90)+
  xlim(1993,2002)+
  scale_x_continuous("year", labels = as.character(Colombia$year), breaks = Colombia$year)+
  geom_vline(xintercept = 1997)
dfcolombia <- data.frame(country="Colombia",start_year=min(Colombia$year),end_year=max(Colombia$year),average_secondary_school_enrollment=mean(Colombia$enrollment.gross),average_teen_fertility_rate=mean(Colombia$fertility))
p1
p2
rbind(dfperu,dfcolombia)


## ------------------------------------------------------------------------
#countries whose enrollment is statistically significant
olsresult <- data.frame(country=character(),intercept=double(),coef_enrollment=double(),p_value_enrollment=double())
#OLS regression for each country
for (i in sample.countries){
  reg1 <- lm(fertility~enrollment,df2 %>% filter(countryname==i))
  ttest1 <- coeftest(reg1, vcov=vcovHC(reg1, type = "HC1"))
  if (ttest1["enrollment","Estimate"]<0 & ttest1["enrollment","Pr(>|t|)"]<0.05){
    olsresult <- rbind(olsresult,data.frame(country=i,intercept=ttest1["(Intercept)","Estimate"],coef_enrollment=ttest1["enrollment","Estimate"],p_value_enrollment=ttest1["enrollment","Pr(>|t|)"]))
  }
}


## ----include=FALSE-------------------------------------------------------
Colombia$Peru = Peru$fertility - Colombia$fertility
Colombia$After = ifelse(Colombia$year>1997,1,0)
Colombia$PeruAfter = Colombia$Peru*Colombia$After
did <- lm(fertility~Peru+After+PeruAfter,Colombia)


## ------------------------------------------------------------------------
olsresult


## ------------------------------------------------------------------------
summary(did)

