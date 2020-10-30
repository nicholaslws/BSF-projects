#Load libraries
library(readxl)
library(here)
library(tidyr)
library(ggplot2)
library(dplyr)


egg_weight<-read_excel(here("Egg", "bsf_egg_track.xlsx"), sheet="egg_track")
cage_track<-read_excel(here("Egg", "bsf_egg_track.xlsx"), sheet="Sheet2")
egg_weight<-read.csv(here("Egg","bsf_egg_track.csv"))
str(egg_weight)

egg_weight$trap_date<-as.POSIXct(egg_weight$trap_date, format="%d/%m/%Y")
egg_weight$harvest_date<-as.POSIXct(egg_weight$harvest_date, format="%d/%m/%Y")
str(egg_weight)

#Remove NAs
egg_weight_clean<-egg_weight %>% drop_na()
egg_weight_clean$line_cage_code<-paste(egg_weight_clean$line,egg_weight_clean$line_ID, sep="")

#Merged
df1<-merge(egg_weight_clean,cage_track, by="line_cage_code")
df1$Light_time<-difftime(df1$harvest_date,df1$Light, units = c("days"))
str(df1)

#remove D12, date wrong
df1_r<-df1[!(df1$line_cage_code=="D12"),]

#plotting with points
test_plot<-ggplot(df1_r, aes(x=Light_time, y=egg_mass, group=line_cage_code, fill=line_cage_code))
test_plot+geom_point()+geom_text(aes(label=line_cage_code),hjust=0, vjust=0)

test_plot2<-ggplot(df1_r, aes(x=Light_time, y=egg_mass, group=line, fill=line))
test_plot2+geom_smooth(alpha=0.3)


#subset data
df_a<- df1_r %>% filter(line=='A')
df_b<- df1_r %>% filter(line=='B')
df_c<- df1_r %>% filter(line=='C')
df_d<- df1_r %>% filter(line=='D')
df_e<- df1_r %>% filter(line=='E')

#A plot
plot_a<-ggplot(df_a, aes(x=Light_time, y=egg_mass, group=line_cage_code, fill=line_cage_code))
plot_a+geom_line()+geom_text(aes(label=line_cage_code),hjust=0, vjust=0)

#B plot
plot_b<-ggplot(df_b, aes(x=Light_time, y=egg_mass, group=line_cage_code, fill=line_cage_code))
plot_b+geom_line()+geom_text(aes(label=line_cage_code),hjust=0, vjust=0)

#C plot
plot_c<-ggplot(df_c, aes(x=Light_time, y=egg_mass, group=line_cage_code, fill=line_cage_code))
plot_c+geom_point()+geom_text(aes(label=line_cage_code),hjust=0, vjust=0)

#D plot
plot_d<-ggplot(df_d, aes(x=Light_time, y=egg_mass, group=line_cage_code, fill=line_cage_code))
plot_d+geom_point()+geom_text(aes(label=line_cage_code),hjust=0, vjust=0)

#Plot E
plot_e<-ggplot(df_e, aes(x=Light_time, y=egg_mass, group=line_cage_code, fill=line_cage_code))
plot_e+geom_line()+geom_text(aes(label=line_cage_code),hjust=0, vjust=0)



