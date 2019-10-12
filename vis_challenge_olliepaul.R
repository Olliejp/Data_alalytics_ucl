library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)

#Data Wrangling
access <- read.csv("access_rural.csv", stringsAsFactors = F)

access_tidy <- gather(data = access,key = "year",value = "%_rural_access",2:29)
access_tidy$year <- str_replace_all(access_tidy$year, "X", "")
names(access_tidy) <- c("country", "year", "rural_access")
access_tidy$country <- str_replace_all(access_tidy$country, "South Sudan", "NA")
access_tidy <- drop_na(access_tidy, 3)

access_bot_ten <- filter(access_tidy, country %in% c("Zimbabwe","Zambia","Nigeria"
                                                     ,"Bolivia","Cote d'Ivoire","Sudan"
                                                     ,"Panama","Turks and Caicos Islands"
                                                     ,"Jamaica","Brazil"))

#heatmap plot

ggplot(access_bot_ten, aes(x=year,y=country,fill=rural_access)) + 
  scale_fill_gradient2(low ="red",mid ="blue",
                       high ="green",midpoint = 50,
                       guide ="colourbar",name="")+
  geom_tile(colour="white", size=0.25)+
  scale_x_discrete(expand=c(0,0),breaks=c("1990","1995","2000","2005","2010","2015"))+
  scale_y_discrete(expand=c(0,0),limits=c("Zambia","Zimbabwe","Nigeria"
                            ,"Sudan","Cote d'Ivoire","Bolivia"
                            ,"Panama","Turks and Caicos Islands"
                            ,"Jamaica","Brazil"))+
  
  labs(x="",y="",title="Percentage of rural population with access to electricity between 1990 and 2017\namongst countries with the lowest access levels in 1990 - Data from World Bank")+
  theme(
      panel.background = element_blank(),
      legend.text=element_text(size=7,face="bold"),
      plot.title=element_text(size=14,face="bold"),
      axis.text.x=element_text(size=8,face="bold"),
      axis.text.y=element_text(size=8,face="bold")
      )

ggsave(filename="heat_map_access.png", plot=last_plot(),
       width = 500, height = 333.5, 
       units = "mm",
       dpi = 200)








  
  
