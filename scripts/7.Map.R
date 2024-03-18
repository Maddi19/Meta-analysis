####MAP#####
rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,rtry,rJava, dplyr,metafor,cowplot,orchaRd,ggbeeswarm,tidyr,ggthemes,sp,broom,lemon,MuMIn,glmulti,PerformanceAnalytics,GGally,gt,geodata,
               ggmap,mapproj,glmulti,MuMIn, RColorBrewer,ggsci,tmap,leaflet,ggplot2)
#######
install.packages("rworldmap")
library(rworldmap)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


repr.success <- read.csv("data/3.effectsizes_clean.csv")
####delete data without effect size, there is one without country, this is also removed
###remove also observations on abundance and visitation rates
effect.size.total<-repr.success[!is.na(repr.success$yi), ]
unique(effect.size.total$Pollinator.variable.measure)
effect.size.total<-effect.size.total%>%
  filter(!str_detect(Pollinator.variable.measure, "abundance")) %>%
  filter(!str_detect(Pollinator.variable.measure, "visitation"))
effect.size.total<-effect.size.total[!is.na(effect.size.total$vi), ]
effect.size.total <-effect.size.total%>%
  filter(!is.na(Country))


library(tmaptools)

#create a list of countries and number of studies
effect.size.total$Country <- recode(effect.size.total$Country, "Perú"="Peru",
                               
                               "Spain, Canary Islands"="Canary Islands",
                               "England"="UK")

##for observations:  
obs.country <- effect.size.total%>%
  separate_rows(Country,sep=", ")%>%
  separate_rows(Country,sep=" & ")%>%
  group_by(Country)%>%
  summarise(obs_country=n())%>%
  filter(!is.na(Country))

#try using google maps
library(ggmap)
register_google(key="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

study_locations<-obs.country$Country
study_locations<-obs.country$Country

# run the geocode function from ggmap package
country_ggmap <- geocode(location = study_locations, output = "more", source = "google")

#join to number of studies
country_coord_count<-cbind(country_ggmap,obs.country=obs.country$obs.country)

crop_studies <- effect.size.total%>%
  separate_rows(Country,sep=", ")%>%
  separate_rows(Country,sep=" & ")%>%
  filter(!is.na(Country))


crop_studies<-table(crop_studies$Country,crop_studies$Cultivo)
crop_studies<- as.data.frame(crop_studies)
crop_studies<-crop_studies%>%
  spread(key=Var2, value=Freq)%>%
  separate_rows(Var1,sep=", ")%>%
  separate_rows(Var1,sep=" & ")

crop_studies$Total.studies<-rowSums(crop_studies[c(2,3)])

country_coord_counts<-cbind(country_coord_count,crop_studies)

#make global map
world_map<-map_data("world")

world_map_plot<-ggplot(world_map,aes(long,lat,group=group))+
  geom_polygon(fill="lightgray",colour="darkgrey",linewidth=0.05)+
  theme_void()+
  theme(axis.title = element_text(colour="white"),
        axis.text = element_text(colour="white"),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

install.packages("scatterpie")
library(scatterpie)
help(geom_scatterpie)

str(crop_studies)

country_coord_counts$obs.country<- as.numeric(country_coord_counts$obs.country)
map<-world_map_plot+
  scatterpie::geom_scatterpie(data=country_coord_counts,
                              aes(x=lon,y=lat,group=Var1), cols=colnames(country_coord_counts[,c(12:13)]),
                              alpha=0.9,color=NA, pie_scale=0.9)+ coord_fixed()+
  theme(legend.title = element_blank(),
        legend.position = c(0.9, 0.6),
        legend.text = element_text(size = 15))+
  scale_fill_manual(values = c("#79AF97FF","#6A6599FF"), labels=c("Crop","Wild plants"))+
  scale_color_manual(values = c("#79AF97FF","#6A6599FF"),labels=c("Crop","Wild plants"))+
  geom_text(data=country_coord_counts,aes(x =lon, y =lat,group=Total.studies,label=Total.studies),
            size = 5)+
  annotate("rect", ymin = 35, ymax = 70,
           xmin = -10, xmax = 27, color= "black",fill = NA) # color = "#E64B35B2",
 
map
map.eu<- world_map_plot+
  coord_sf(xlim = c(-10, 27), ylim = c(35, 70), expand = FALSE) +
  geom_scatterpie(data=country_coord_counts,
                              aes(x=lon,y=lat,group=Var1), cols=colnames(country_coord_counts[,c(12:13)]),
                              alpha=0.9,color=NA,pie_scale = 0.31)+
  theme(legend.position = "none")+
  theme_void()+
  scale_fill_manual(values = c("#79AF97FF","#6A6599FF"), labels=c("Crop","Wild plants"))+
  scale_color_manual(values = c("#79AF97FF","#6A6599FF"),labels=c("Crop","Wild plants"))+
  guides (fill=FALSE)+
  geom_text(data=country_coord_counts,aes(x =lon, y =lat,group=Total.studies,label=Total.studies),
            size = 3.75)
map.eu
library(patchwork)
p12 <- map+ inset_element(map.eu+theme(plot.background = element_rect(fill = "white"),
                                       panel.border = element_rect(fill = NA, linewidth = 1)),
                            left = 0.415, bottom = 0, right = 0.915, top = 0.35,
                          )

ggsave("Map.figure.1.png", width = 15, height= 12, plot=p12)



