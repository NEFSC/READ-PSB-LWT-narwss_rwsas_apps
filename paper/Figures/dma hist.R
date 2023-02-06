library(ggplot2);library(dplyr);library(lubridate)

dmas<-read.csv("./paper/data/DMAINFO_20230113.csv", header = T, stringsAsFactors = T)
#dmas<-read.csv("./data/20220523export.csv", header = T, stringsAsFactors = T)

dmas_year<-dmas%>%
  dplyr::distinct(NAME, TRIGGERDATE, INITOREXT, TRIGGERGROUPSIZE, TRIGGERTYPE, CANCELLED, COMMENTS)%>%
  mutate(YEAR = year(dmy_hms(TRIGGERDATE)),
         Type = case_when(
           TRIGGERTYPE == 'a' ~ 'Acoustic SLOW Zone',
           TRIGGERTYPE == 'v' ~ "DMA from sightings"
         ))%>%
  filter(CANCELLED != toupper("CANCELLED") | CANCELLED != toupper("YES"))%>%
  filter(YEAR < 2023 & YEAR > 2008)


ggplot(dmas_year, aes(x = as.factor(YEAR), fill = Type))+
        geom_histogram(stat = 'count', binwidth = 1, alpha = 0.7)+
        theme_bw(base_size = 15)+
        xlab("Year")+
        ylab("Number declared in the US")+
        theme(legend.position = "bottom")

ggsave('./paper/Fig1.png',dpi = 320, width = 250, height = 100, units = 'mm')  

