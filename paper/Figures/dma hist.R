library(ggplot2);library(dplyr);library(lubridate)

dmas<-read.csv("./paper/data/DMAINFO_26SEP2022.csv", header = T, stringsAsFactors = T)
#dmas<-read.csv("./data/20220523export.csv", header = T, stringsAsFactors = T)

dmas_year<-dmas%>%
  dplyr::distinct(NAME, TRIGGERDATE, INITOREXT, TRIGGERGROUPSIZE, TRIGGERTYPE)%>%
  mutate(YEAR = year(dmy_hms(TRIGGERDATE)),
         Type = case_when(
           TRIGGERTYPE == 'a' ~ 'Acoustic SLOW Zone',
           TRIGGERTYPE == 'v' ~ "DMA from sightings"
         ))%>%
  filter(YEAR > 2008)%>%# & YEAR < 2022)
  #mutate(YEAR_star = YEAR)
  mutate(YEAR_star = case_when(
    YEAR == 2022 ~ '2022*',
    YEAR != 2022 ~ as.character(YEAR)
  ))


ggplot(dmas_year, aes(x = as.factor(YEAR_star), fill = Type))+
        geom_histogram(stat = 'count', binwidth = 1, alpha = 0.7)+
        theme_bw(base_size = 15)+
        xlab("Year")+
        ylab("# declared in the US")+
        theme(legend.position = "bottom")

ggsave('./paper/Fig1.png',dpi = 320, width = 250, height = 100, units = 'mm')  

