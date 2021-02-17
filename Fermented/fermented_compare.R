library(here)
library(readxl)
here()

#Feed in data
feed_tal<-read_excel(here("Fermented", "BSF Facility Data Records2.xlsx"), sheet="Feed Tally", skip=1)

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
harvest_oct$Month<-"Oct"
harvest_dec$Month<-"Dec"

#rbind them
harvest_comb<-bind_rows(harvest_dec,harvest_oct)

#Making a new column for Line.
library(stringr)
harvest_comb$line<-substr(harvest_comb$tray_ID, 1,1)

#Lets see some stuff
harvest_comb %>%
  group_by(Month, line) %>%
  summarise(meanfcr = mean(fcr),
            meanfrass=mean(frasscon),
            n = n())

#ggplotslol
#install.packages("ggplot2")
library(ggplot2)


#histograms for fcr, frasscon?
range(harvest_comb$fcr)
range(harvest_comb$frasscon)

#initialise
plotbsf<-ggplot(data=harvest_comb, aes(fill=Month))

#Feed conversion ratio
plotbsf+ 
  geom_histogram(bins=20,aes(fcr))+ 
  facet_grid(vars(Month))+
  scale_x_continuous(name="feed conversion ratio", breaks=seq(0.05,0.25,0.02))

plotbsf+ 
  geom_histogram(bins=20,aes(fcr))+ 
  facet_grid(vars(line, Month))+
  scale_x_continuous(name="feed conversion ratio", breaks=seq(0.05,0.25,0.01))

#Frass conversion
plotbsf+ 
  geom_histogram(bins=20,aes(frasscon))+ 
  facet_grid(vars(Month))+
  scale_x_continuous(name="frass conversion ratio", breaks=seq(0.0,0.1,0.01))

plotbsf+ 
  geom_histogram(bins=20,aes(frasscon))+ 
  facet_grid(vars(line, Month))+
  scale_x_continuous(name="frass conversion ratio", breaks=seq(0.0,0.1,0.01))



#t test lol
t.test(fcr~Month, data=harvest_comb)
t.test(frasscon~Month, data=harvest_comb)

summary(lm(fcr~line+Month+ppp, data =harvest_comb))

#How about tray age?
harvest_comb$tray_date<-as.numeric(harvest_comb$tray_date)
harvest_comb$harvest_date<-as.numeric(harvest_comb$harvest_date)
harvest_comb$age<-harvest_comb$harvest_date - harvest_comb$tray_date
summary(harvest_comb)

#scatter plot for age
plotbsf+
  geom_point(aes(x=harvest_comb$age, y=fcr, colour=Month))
