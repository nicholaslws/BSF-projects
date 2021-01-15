library(here)
library(readxl)
here()

#Feed in data
feed_tal<-read_excel(here( "R projects", "BSF-projects","Fermented", "BSF Facility Data Records.xlsx"), sheet="Feed Tally", skip=1)

#Removing unneeded columns (7-26, 33-41)
feed_tal<-feed_tal[, -c(7:26, 33:41)]

#subset out harvested
library(dplyr)
harvest<-filter(feed_tal, tray_status=="Harvested")

#reorder to get dates
harvest_sort<-harvest[order(harvest$tray_date),]

#Only looking at trays from  October onwards.
harvest_octdec<-harvest_sort[-c(1:430),]
#Remove fluff column
harvest_octdec<-harvest_octdec[,-12]

harvest_fin<-harvest_octdec[,c(1:4,6,7,9:11)]
harvest_fin$feedg<-harvest_fin$feed_amt*550*2
harvest_fin$fcr<-harvest_fin$larvae_fluff/harvest_fin$feedg
harvest_fin$frasscon<-harvest_fin$frass/harvest_fin$feedg

#select only rows that we harvested
harvest_fil<-harvest_fin %>% filter(complete.cases(.))

#October shall be non fermented, december 7 onwards for fermented
harvest_oct<-harvest_fil[1:42,]
harvest_dec<-harvest_fil[99:nrow(harvest_fil),]

#Prep the two data frames
harvest_oct$Month<-"oct"
harvest_dec$Month<-"dec"

#rbind them
harvest_comb<-bind_rows(harvest_dec,harvest_oct)

#Making a new column for Line.
library(stringr)
harvest_comb$line<-substr(harvest_comb$tray_ID, 1,1)


