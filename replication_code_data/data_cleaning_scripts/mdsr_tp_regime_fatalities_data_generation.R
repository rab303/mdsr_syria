# Term Paper: State Repression in Syria
# Course: Microdynamics of State Repression
# Lecturer: Dr. Alexander De Juan
# submitted by: Johannes Willmann; (01/790064); MSc SEDA

# Code cleaning scraped data of regime's casualties

# set wd ----
setwd("/Users/johanneswillmann/Documents/UNI/MSc2/microdynamics/term_paper/data_analysis")

# packages ----
library(foreign)
library(ggplot2)
library(raster)
library(RecordLinkage)
library(plyr)
library(sp)
library(doBy)
library(RSQLite)
library(sqldf)
library(maptools)

# import data ----
mil <- read.csv("python_files/regime_fatalities.csv", sep = ";", header = T)
View(mil)
# clean up data ----

# delete unnecessary columns
mil$Name <- NULL  
mil$X <- NULL

# reformat date and set proper timeframe (begining of conflict until founding of FSA)
mil$Date.of.death <- as.Date(mil$Date.of.death, format = "%Y-%m-%d")

mil <- mil[!(is.na(mil$Date.of.death)),]
mil <- mil[(mil$Date.of.death <= "2013-01-01"),] # founding of FSA
mil <- mil[(mil$Date.of.death > "2011-03-01"),] # founding of FSA
View(mil)



# prepare data for mapping ----

# Rename Provinces to match polygons names 
mil_rename <- mil

mil_rename$Province2 <- revalue(mil_rename$Province, 
                                c(" Daraa"="Dar`a",
                                  " Lattakia"="Lattakia",
                                  " Homs"="Hims",
                                  " Damascus Suburbs"="Rif Dimashq",
                                  " Deir Ezzor"="Dayr Az Zawr",
                                  " Tartous"="Tartus",
                                  " Damascus"="Damascus",
                                  " Hama"="Hamah",
                                  " Idlib"="Idlib",
                                  " Hasakeh"="Al ???asakah",
                                  " Raqqa"="Ar Raqqah",
                                  " Aleppo"="Aleppo",
                                  " Sweida"="As Suwayda'",
                                  " Quneitra"="Quneitra"))

mil <- mil_rename

# write.csv(mil, "mil_export.csv")


mil_mar <- mil[(mil$Date.of.death < "2011-04-01"),] # founding of FSA
mil_apr <- mil[(mil$Date.of.death >= "2011-04-01" & mil$Date.of.death < "2011-05-01"),] 
mil_may <- mil[(mil$Date.of.death >= "2011-05-01" & mil$Date.of.death < "2011-06-01"),] 
mil_jun <- mil[(mil$Date.of.death >= "2011-06-01" & mil$Date.of.death < "2011-07-01"),] 
mil_jul <- mil[(mil$Date.of.death >= "2011-07-01" & mil$Date.of.death < "2011-08-01"),] 



# import provinces in syria 
sy <- getData("GADM",country="Syria",level=1)

# change projection and datum of Spatial Polygon Data Frame
proj4string(sy)
sy <- spTransform(sy, CRS("+proj=longlat +datum=WGS84"))


sy@data$id = rownames(sy@data)

sy.points = fortify(sy, region="id")
sy.df = join(sy.points, sy@data, by="id")

# generate aggregated data
mil_ag <- data.frame ( table ( mil$Province2, mil$Cause.of.death ) )
colnames(mil_ag) <- c("Province", "Cause.of.death", "Frequency")
mil_mar_ag <- data.frame ( table ( mil_mar$Province2, mil_mar$Cause.of.death ) )
colnames(mil_mar_ag) <- c("Province", "Cause.of.death", "Frequency")
mil_apr_ag <- data.frame ( table ( mil_apr$Province2, mil_apr$Cause.of.death ) )
colnames(mil_apr_ag) <- c("Province", "Cause.of.death", "Frequency")
mil_may_ag <- data.frame ( table ( mil_may$Province2, mil_may$Cause.of.death ) )
colnames(mil_may_ag) <- c("Province", "Cause.of.death", "Frequency")
mil_jun_ag <- data.frame ( table ( mil_jun$Province2, mil_jun$Cause.of.death ) )
colnames(mil_jun_ag) <- c("Province", "Cause.of.death", "Frequency")
mil_jul_ag <- data.frame ( table ( mil_jul$Province2, mil_jul$Cause.of.death ) )
colnames(mil_jul_ag) <- c("Province", "Cause.of.death", "Frequency")


mil_ag <- mil_ag[-(1:15),]
mil_mar_ag <- mil_mar_ag[-(1:15),]
mil_apr_ag <- mil_apr_ag[-(1:15),]
mil_may_ag <- mil_may_ag[-(1:15),]
mil_jun_ag <- mil_jun_ag[-(1:15),]
mil_jul_ag <- mil_jul_ag[-(1:15),]

# hard-coded Quneitra into csv files


# export datasets ----

# mil_export <- mil_rename
# write.csv(mil, "rf_export.csv")
# write.csv(mil_ag, "rf_aggregated.csv")
# write.csv(sy.df, "sy_df.csv")
# write.csv(mil_mar_ag, "rf_mar_ag.csv")
# write.csv(mil_apr_ag, "rf_apr_ag.csv")
# write.csv(mil_may_ag, "rf_may_ag.csv")
# write.csv(mil_jun_ag, "rf_jun_ag.csv")
#write.csv(mil_jul_ag, "rf_jul_ag.csv")







