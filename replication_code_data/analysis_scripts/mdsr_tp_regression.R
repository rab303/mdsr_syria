# Term Paper: State Repression in Syria
# Course: Microdynamics of State Repression
# Lecturer: Dr. Alexander De Juan
# submitted by: Johannes Willmann; (01/790064); MSc SEDA

# Code for regression analysis 

# load packages ----
library(ggplot2)
library(gridExtra)
library(doBy)
require(rgdal)
library(maptools)
library(tikzDevice)
library(xtable)
library(xts)
library(lubridate)
library(pscl)
library(MASS)
library(boot)
library(stargazer)
library(Zelig)
library(dyn)        
library(DataCombine)
library(plyr)

# set wd ----
setwd("/Users/johanneswillmann/Documents/UNI/MSc2/microdynamics/term_paper/data_analysis")

# load data ----

# civil <- read.csv("vdc_selective.csv")
civil <- read.csv("vdc_collective.csv")
# civil <- read.csv("vdc_export.csv")
nomale <- read.csv("vdc_export.csv")
fsa <- read.csv("vdc_export.csv")  # opposition combatants
mil <- read.csv("mil_export.csv")

# transfrom Factor to Date ----
civil$Date.of.death <- as.Date(civil$Date.of.death, format = "%Y-%m-%d")
nomale$Date.of.death <- as.Date(nomale$Date.of.death, format = "%Y-%m-%d")
mil$Date.of.death <- as.Date(mil$Date.of.death, format = "%Y-%m-%d")

# exclude non-civilians ----
civil <- civil[(civil$Status == " Civilian"),]
shelling <- civil[(civil$Cause.of.death == " Shelling"  | 
                   civil$Cause.of.death ==  " Warplane shelling"),]

fsa <-  fsa[(fsa$Status != " Civilian"),]
nomale <- nomale[(nomale$Sex != " Adult - Male"),]

# plots -----
ggplot(civil, aes(x = Date.of.death)) + 
  geom_bar(stat = "bin", binwidth = 1) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "", y = "Caualties per day")

ggplot(shelling, aes(x = Date.of.death)) + 
  geom_bar(stat = "bin", binwidth = 1) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "", y = "Caualties per day")

# aggregate data by month and province ----

# build variable that reflects month and year
civil$week <- week(civil$Date.of.death)
civil$month <- month(civil$Date.of.death)
civil$year <- year(civil$Date.of.death)

nomale$week <- week(nomale$Date.of.death)
nomale$month <- month(nomale$Date.of.death)
nomale$year <- year(nomale$Date.of.death)

fsa$week <- week(fsa$Date.of.death)
fsa$month <- month(fsa$Date.of.death)
fsa$year <- year(fsa$Date.of.death)

mil$week <- week(mil$Date.of.death)
mil$month <- month(mil$Date.of.death)
mil$year <- year(mil$Date.of.death)

# aggregate casualties 
civil$freq <- 1
civil_month <- summaryBy(freq~Province + month + year, data=civil, FUN=c(sum))
civil_week <- summaryBy(freq~Province + week + month + year, data=civil, FUN=c(sum))

# rename frequency
names(civil_month)[names(civil_month)=="freq.sum"] <- "civil"
names(civil_week)[names(civil_week)=="freq.sum"] <- "civil"

# aggregate casualties 
nomale$freq <- 1
nomale_month <- summaryBy(freq~Province + month + year, data=nomale, FUN=c(sum))
nomale_week <- summaryBy(freq~Province + week + month + year, data=nomale, FUN=c(sum))

# rename frequency
names(nomale_month)[names(nomale_month)=="freq.sum"] <- "nomale"
names(nomale_week)[names(nomale_week)=="freq.sum"] <- "nomale"

# aggregate casualties
fsa$freq <- 1
fsa_month <- summaryBy(freq~Province + month + year, data=fsa, FUN=c(sum))
fsa_week <- summaryBy(freq~Province + week + month + year, data=fsa, FUN=c(sum))

# rename frequency
names(fsa_month)[names(fsa_month)=="freq.sum"] <- "fsa"
names(fsa_week)[names(fsa_week)=="freq.sum"] <- "fsa"

# aggregate casualties 
mil$freq <- 1
mil_month <- summaryBy(freq~Province + month + year, data=mil, FUN=c(sum))
mil_week <- summaryBy(freq~Province + week + month + year, data=mil, FUN=c(sum))

# rename frequency
names(mil_month)[names(mil_month)=="freq.sum"] <- "milit"
names(mil_week)[names(mil_week)=="freq.sum"] <- "milit"

# merge data sets
merge_month <- merge(civil_month, mil_month, by = c("month", "year", "Province"))
merge_month2 <- merge(merge_month, nomale_month, by = c("month", "year", "Province"))
comb_month <- merge(merge_month2, fsa_month, by = c("month", "year", "Province"))
head(comb_month)
merge_week <- merge(civil_week, mil_week, by = c("week", "month", "year", "Province"))
merge_week2 <- merge(merge_week, nomale_week, by = c("week", "month", "year", "Province"))
comb_week <- merge(merge_week2, fsa_week, by = c("week", "month", "year", "Province"))
head(comb_week)

# include dates with no casualties
province <- levels(comb_month$Province)
months11 <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
months12 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
weeks <- c(1:53)

# for months
m1 <- unique(merge(province, months11, all = T))
m1$year <- 2011

m2 <- unique(merge(province, months12, all = T))
m2$year <- 2012

full_govn <- rbind(m1, m2)

names(full_govn)[names(full_govn) == "x"] <- "Province"
names(full_govn)[names(full_govn) == "y"] <- "month"

# for weeks 
weeks11 <- c(12:53)
weeks12 <- c(1:48)
w11 <- unique(merge(province, weeks11, all = T))
w12 <- unique(merge(province, weeks12, all = T))
w11$year <- 2011
w12$year <- 2012

full_weeks <- rbind(w11, w12)
names(full_weeks)[names(full_weeks) == "x"] <- "Province"
names(full_weeks)[names(full_weeks) == "y"] <- "week"

comb_month <- merge(comb_month, full_govn, all = TRUE)
comb_month[is.na(comb_month)] <- 0

comb_week$month <- NULL
comb_week <- merge(comb_week, full_weeks, all = TRUE)
comb_week[is.na(comb_week)] <- 0

# control variables ----
# population: https://data.humdata.org/dataset/syrian-arab-republic-other-0
pop <- read.csv("pop_syria_2011.csv")
cont_pop_month <- merge(comb_month, pop, by = "Province")
cont_pop_week <- merge(comb_week, pop, by = "Province")

# add duration variable (monthly)
month = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
year = c(2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012)
duration_month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)

dur_month.df <- cbind(month, year, duration_month)

# add duration variable (weekly)

duration_weeks = c(1: (length(weeks11) + length(weeks12)))
full_weeks$Province <- NULL
week_join <- unique(full_weeks)

dur_week.df <- cbind(week_join, duration_weeks)

cont_month <- merge(cont_pop_month, dur_month.df, by = c("month", "year"))
cont_week <- merge(cont_pop_week, dur_week.df, by = c("week", "year"))

# add lagged dependent variable 

# order data frame
cont_month <- cont_month[with(cont_month, order(Province, year, month)), ]

cont_month <- ddply(cont_month, .(Province), transform, civil_lag =
                c(NA, civil[-length(civil)]
                )
)

cont_month <- ddply(cont_month, .(Province), transform, milit_lag =
                      c(NA, milit[-length(milit)]
                      )
)
head(cont_month)


cont_week <- cont_week[with(cont_week, order(Province, year, week)), ]

cont_week <- ddply(cont_week, .(Province), transform, civil_lag =
                      c(NA, civil[-length(civil)]
                      )
)

cont_week <- ddply(cont_week, .(Province), transform, milit_lag =
                     c(NA, milit[-length(milit)]
                     )
)


names(cont_month)[names(cont_month) == "duration_month"] <- "duration"
names(cont_week)[names(cont_week) == "duration_weeks"] <- "duration"

# export data ----
# write.csv(cont_month, "replication_collective_month.csv")
# write.csv(cont_week, "replication_collective_week.csv")
# write.csv(cont_month, "replication_selective_month.csv")
# write.csv(cont_week, "replication_selective_week.csv")
# write.csv(cont_month, "replication_complete_month.csv")
# write.csv(cont_week, "replication_complete_week.csv")

# Description of data ----

# import data 
cont_month <- read.csv("replication_complete_month.csv")
cont_week <- read.csv("replication_complete_week.csv")
cont_month_s <- read.csv("replication_selective_month.csv")
cont_week_s <- read.csv("replication_selective_week.csv")
cont_month_c <- read.csv("replication_collective_month.csv")
cont_week_c <- read.csv("replication_collective_week.csv")


# Histogram of dependent variable 

ggplot(cont_month_c, aes(civil)) +
  geom_histogram(binwidth=4) 

ggplot(cont_week_c, aes(civil)) +
  geom_histogram(binwidth=3) 

stargazer(cont_month_c)
stargazer(cont_week_c)


# OLS regression (However, count data are highly non-normal and are not well estimated by OLS regression.) ----
lm.month.1 <- lm(civil ~ milit, data = cont_month)
lm.week.1 <- lm(civil ~ milit, data = cont_week)
summary(lm.month.1)
summary(lm.week.1)

lm.month.2 <- lm(civil ~ milit + log(population) + duration, data = cont_month)
lm.week.2 <- lm(civil ~ milit + log(population) + duration, data = cont_week)
summary(lm.month.2)
summary(lm.week.2)


# Count Data: Poisson Regression ----

# complete data ----

zelig.month.1 <- zelig(civil ~ milit + log(population) + duration + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_month)

zelig.week.1 <- zelig(civil ~ milit + log(population) + duration + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_week)

zelig.month.2 <- zelig(civil ~ milit +  civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_month)

zelig.week.2 <- zelig(civil ~ milit + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_week)

summary(zelig.month.1)
summary(zelig.week.1)
summary(zelig.month.2)
summary(zelig.week.2)

exp(coefficients(zelig.week.1)["milit"])
exp(coefficients(zelig.month.1)["milit"])
# stargazer(zelig.week.2, zelig.month.2, zelig.week.1, zelig.month.1)

# collective data ----
zelig.month.3 <- zelig(civil ~ milit + log(population) + duration + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_month_c)

zelig.week.3 <- zelig(civil ~ milit + log(population) + duration + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_week_c)

zelig.month.4 <- zelig(civil ~ milit + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_month_c)

zelig.week.4 <- zelig(civil ~ milit +  civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_week_c)

summary(zelig.month.1)
summary(zelig.week.1)
summary(zelig.month.2)
summary(zelig.week.2)

exp(coefficients(zelig.week.3)["milit"])
exp(coefficients(zelig.month.3)["milit"])
# stargazer(zelig.week.4, zelig.month.4, zelig.week.3, zelig.month.3)

# selective data ----
zelig.month.5 <- zelig(civil ~ milit + log(population) + duration + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_month_s)

zelig.week.5 <- zelig(civil ~ milit + log(population) + duration + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_week_s)

zelig.month.6 <- zelig(civil ~ milit + fsa + log(population) + duration + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_month_s)

zelig.week.6 <- zelig(civil ~ milit + fsa + log(population) + duration + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_week_s)

summary(zelig.month.1)
summary(zelig.week.1)
summary(zelig.month.2)
summary(zelig.week.2)

exp(coefficients(zelig.week.1)["milit"])
exp(coefficients(zelig.month.1)["milit"])
# stargazer(zelig.week.5, zelig.month.5, zelig.week.6, zelig.month.6)


# no male adults data ----

zelig.month.7 <- zelig(nomale ~ milit + log(population) + duration + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_month_c)

zelig.week.7 <- zelig(nomale ~ milit + log(population) + duration + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_week_c)

zelig.month.8 <- zelig(nomale ~ milit + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_month_c)

zelig.week.8 <- zelig(nomale ~ milit + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_week_c)

summary(zelig.month.7)
summary(zelig.week.7)
summary(zelig.month.8)
summary(zelig.week.8)

exp(coefficients(zelig.week.1)["milit"])
exp(coefficients(zelig.month.1)["milit"])
# stargazer(zelig.week.8, zelig.month.8, zelig.week.7, zelig.month.7)


# lagged approach ----
zelig.month.9 <- zelig(civil ~ milit_lag + log(population) + duration + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_month)

zelig.week.9 <- zelig(civil ~ milit_lag + log(population) + duration + civil_lag, model="poisson", robust = TRUE, cluster = "Province", data=cont_week)

summary(zelig.month.9)
summary(zelig.week.9)
stargazer(zelig.week.9, zelig.month.9)


# Plot predicted values ----

glm.month1 <- glm(civil ~ milit, family="poisson", data=cont_month)
glm.month2 <- glm(nomale ~ milit, family="poisson", data=cont_month)
cont_month$phat1 <- predict(glm.month1, type="response")
cont_month$phat2 <- predict(glm.month2, type="response")

## create the plot
ggplot(cont_month, aes(x = milit, y = civil)) +
  geom_point(aes(y = civil), alpha=.5,  col = "blue") +
  # geom_point(aes(y = nomale), alpha=.5,  col = "red") +
  geom_line(aes(y=phat1), col = "blue") + 
  # geom_line(aes(y=phat2), col = "red") + 
  theme_minimal() + 
  labs(x = "Military Casualties", y = "Number of Civilian Casualties")




