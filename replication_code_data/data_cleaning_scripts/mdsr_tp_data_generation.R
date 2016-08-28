# Term Paper: State Repression in Syria
# Course: Microdynamics of State Repression
# Lecturer: Dr. Alexander De Juan
# submitted by: Johannes Willmann; (01/790064); MSc SEDA

# Code cleaning scraped data of state repression casualties

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
vdc <- read.csv("vdc_all_state_repression.csv", sep = ";", header = T)

# clean up data ----

# delete unnecessary columns
vdc$Name <- NULL  
vdc$X <- NULL

# reformat date and set proper timeframe (begining of conflict until founding of FSA)
vdc$Date.of.death <- as.Date(vdc$Date.of.death, format = "%Y-%m-%d")
vdc <- vdc[!(is.na(vdc$Date.of.death) | vdc$Date.of.death=="1970-01-01"), ]
View(vdc)

# prepare data for mapping ----

# Rename Provinces to match polygons names 
vdc_rename <- vdc

vdc_rename$Province2 <- revalue(vdc_rename$Province, 
                                 c(" Daraa"="Dar`a",
                                   " Lattakia"="Lattakia",
                                   " Homs"="Hims",
                                   " Damascus Suburbs"="Rif Dimashq",
                                   " Deir Ezzor"="Dayr Az Zawr",
                                   " Tartous"="Tartus",
                                   " Damascus"="Damascus",
                                   " Hama"="Hamah",
                                   " Idlib"="Idlib",
                                   " Hasakeh"="Al <U+1E24>asakah",
                                   " Raqqa"="Ar Raqqah",
                                   " Aleppo"="Aleppo",
                                   " Sweida"="As Suwayda'",
                                   " Quneitra"="Quneitra"))

vdc <- vdc_rename

# import provinces in syria 
sy <- getData("GADM",country="Syria",level=1)

# change projection and datum of Spatial Polygon Data Frame
proj4string(sy)
sy <- spTransform(sy, CRS("+proj=longlat +datum=WGS84"))


sy@data$id = rownames(sy@data)

sy.points = fortify(sy, region="id")
sy.df = join(sy.points, sy@data, by="id")

# generate aggregated data
vdc_ag <- data.frame ( table ( vdc$Province2, vdc$Cause.of.death ) )
colnames(vdc_ag) <- c("Province", "Cause.of.death", "Frequency")

vdc_ag <- vdc_ag[-(1:32),]
vdc_ag <- vdc_ag[!(vdc_ag$Province == ""),]
View(vdc_ag)
# export datasets ----

# vdc_export <- vdc_rename
# write.csv(vdc_export, "vdc_export.csv")
# write.csv(vdc_ag, "vdc_aggregated.csv")
# write.csv(sy.df, "sy_df.csv")








