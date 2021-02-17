library(here)
library(readxl)
library(ggplot2)
here()

#Feed in data
fst_data<-read_excel(here("FST", "fst_proximate.xlsx"), sheet="Sheet1")

ggplot(data=fst_data, aes(y=Mean, x=Line, group=Line, colour=Line))+
  geom_pointrange(aes(ymin=Mean-SD, ymax=Mean+SD))+
  facet_wrap(~Parameters, scales='free')+
  theme(text = element_text(size=20))+
  labs(y="Mean %")



