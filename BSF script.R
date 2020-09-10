# Load libraries
library(here)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(multcomp)

library(usethis)
use_github()

#####EGG TALLY###############
#####################################
#Load in data for egg tally
egg_tally<- read_excel(here("Data", 'BSF Facility data records.xlsx'),
                  sheet="Egg Tally")
#Remove columns right of egg mass
egg_tally<-egg_tally[,1:7] 
#Complete cases using tidyr
egg_tally<-drop_na(egg_tally)
#Add in new column to concatenate line+ID
egg_tally<-unite(egg_tally, col=cage_code, 3:4, remove=F, sep="")
#Aggregate eggs
egg_summary<-egg_tally %>%
              group_by(cage_code) %>%
              summarise_at(vars(egg_mass), funs(sum(., na.rm=TRUE)))
# Add in Line to egg summary
egg_summary$line<-substr(egg_summary$cage_code,1,1)


#####     CAGE TALLY###############
#####################################
#Load in data for cage tally
cage_tally<- read_excel(here("Data", 'BSF Facility data records.xlsx'),
                        sheet="Cage Tally")
#Keep first 6 rows
cage_tally<-cage_tally[,c(2:6)]
#Keep complete cases
cage_tally<-drop_na(cage_tally)
#Rename date  and change the date format
colnames(cage_tally)[1]<-'date'
cage_tally$date<-ymd(cage_tally$date)
#subset out Light dates
cage_light<-filter(cage_tally,event=='Light')
#How long have they been in light
cage_light$light_time<-Sys.Date()-cage_light$date



########FEED TALLY###############
#####################################
feed_tally<- read_excel(here("Data", 'BSF Facility data records.xlsx'),
                        sheet="Feed Tally", skip=1)

#Cut out extra rows, columns
feed_tally<-feed_tally[,-33]
feed_tally<-feed_tally[is.na(feed_tally$tray_ID)==F,]

#Subset out the two halves. Old is based on the old feeding records, new on the new. New records the dates which we fed
feed_tally_old<-feed_tally[is.na(feed_tally$Scp1),]
feed_tally_new<-feed_tally[is.na(feed_tally$Scp1)==F,]

#Weird date type (POSIXct). got to change it later, 
str(feed_tally_new)

#make new feed tally wide to long
feed_tally_new_long<-gather(feed_tally_new,key="scoop", value="feed_date", Scp1:Scp20, na.rm=T)
str(feed_tally_new_long)

#Change POSIXct to a readeable date
feed_tally_new_long$feed_date<-as.Date(as.POSIXct(feed_tally_new_long$feed_date,
                                                  tz="Singapore", origin="1970-01-01"))

#Remove scoops columns from feed tally old
feed_tally_old<-feed_tally_old[,-6:-25]




#If scoop date is not needed for feed tally. c= culled, h= harvested
feed_tally_summary<-feed_tally[,c(-6:-25)]
feed_tally_c<- filter(feed_tally_summary, tray_status=="Culled")
feed_tally_h<- filter(feed_tally_summary, tray_status=="Harvested")



##########NURSERIES#############
#import in, skip first 47 rows which are old. insert row name based on row 48 and remove 48.
nursery_data<- read_excel(here("Data", 'BSF Facility data records.xlsx'),
                        sheet="Nursery Quant.", skip=47, col_names=F)
names(nursery_data) <- as.matrix(nursery_data[1, ])
nursery_data<-nursery_data[-1,]

#Data messy until row 82. Remove earlier ones, remove empty rows and note columns
nursery_clean<-nursery_data[-1:-81,]
nursery_clean<-nursery_clean[,-20]
nursery_clean<-nursery_clean[is.na(nursery_clean$`Egg Date`)==F,]
nursery_clean


#### TEST ANALYSIS ON FEED TALLY######
#Trial
feed_harvested_old<-filter(feed_tally_old, tray_status=="Harvested")
feed_harvested_old<-feed_harvested_old[,-6:-25]
feed_harvested_old$line<-str_sub(feed_harvested_old$tray_ID, 1,1)
feed_harvested_old<-drop_na(feed_harvested_old)
str(feed_harvested_old)
feed_harvested_old$line<-as.factor(feed_harvested_old$line)

summary(feed_harvested_old)
anv1<-aov(larvae_fluff~line, data=feed_harvested_old)
summary(anv1)
TukeyHSD(anv1)

#Using multcomp package
summary(glht(anv1, linfct = mcp(line = "Tukey")))
