####Mapa estudios
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) 

rm(list = ls(all.names = TRUE)) #Limpiar objetos ocultos
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #Limpiar paquetes ocultos
pacman::p_load(tidyverse,rJava, dplyr,metafor,cowplot,orchaRd,ggbeeswarm,tidyr,ggthemes,sp,broom,lemon,MuMIn,glmulti,PerformanceAnalytics,GGally,gt,geodata,
               ggmap,mapproj,glmulti,MuMIn, RColorBrewer,ggsci)

install.packages("rworldmap")
library(rworldmap)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


repr.success <- read.csv("data\\3.effectsizes_clean.csv")
##eliminar datos sin effect size (hay uno sin country)
effect.size.total<-repr.success[!is.na(repr.success$yi), ]
unique(effect.size.total$Pollinator.variable.measure)
effect.size.total<-effect.size.total%>%
  filter(!str_detect(Pollinator.variable.measure, "abundance")) %>%
  filter(!str_detect(Pollinator.variable.measure, "visitation"))
effect.size.total<-effect.size.total[!is.na(effect.size.total$vi), ]
effect.size.total <-effect.size.total%>%
  filter(!is.na(Country))


library(tmaptools)

unique(pr$Title)

##obs donde hay datos de todo
#create a list of countries and number of studies
effect.size.total$Country <- recode(effect.size.total$Country, "Perú"="Peru",
                               
                               "Spain, Canary Islands"="Canary Islands",
                               "England"="UK")

##para obs  
obs.pais <- effect.size.total%>%
  separate_rows(Country,sep=", ")%>%
  separate_rows(Country,sep=" & ")%>%
  group_by(Country)%>%
  summarise(estudios.pais=n())%>%
  filter(!is.na(Country))


#try using google maps
library(ggmap)
register_google(key="AIzaSyBMLdr5kn90Q5Au3Q3SLiGXfb2D_W7ZF2s")

study_locations<-obs.pais$Country
study_locations<-obs.pais$Country

# run the geocode function from ggmap package
country_ggmap <- geocode(location = study_locations, output = "more", source = "google")

#join to number of studies
country_coord_count<-cbind(country_ggmap,obs.pais=obs.pais$estudios.pais)

estudios.cultivo.pais <- effect.size.total%>%
  separate_rows(Country,sep=", ")%>%
  separate_rows(Country,sep=" & ")%>%
  filter(!is.na(Country))


estudios.cultivo.pais <-table(estudios.cultivo.pais$Country,estudios.cultivo.pais$Cultivo)
estudios.cultivo<- as.data.frame(estudios.cultivo.pais)
estudios.cultivo<-estudios.cultivo%>%
  spread(key=Var2, value=Freq)%>%
  separate_rows(Var1,sep=", ")%>%
  separate_rows(Var1,sep=" & ")

estudios.cultivo$Total.estudios<-rowSums(estudios.cultivo[c(2,3)])

country_coord_counts<-cbind(country_coord_count,estudios.cultivo)

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

str(estudios.cultivo)

country_coord_counts$obs.pais<- as.numeric(country_coord_counts$obs.pais)
map<-world_map_plot+
  scatterpie::geom_scatterpie(data=country_coord_counts,
                              aes(x=lon,y=lat,group=Var1), cols=colnames(country_coord_counts[,c(12:13)]),
                              alpha=0.9,color=NA, pie_scale=0.9)+ coord_fixed()+
  theme(legend.title = element_blank(),
        legend.position = c(0.9, 0.6),
        legend.text = element_text(size = 15))+
  scale_fill_manual(values = c("#79AF97FF","#6A6599FF"), labels=c("Crop","Wild plants"))+
  scale_color_manual(values = c("#79AF97FF","#6A6599FF"),labels=c("Crop","Wild plants"))+
  geom_text(data=country_coord_counts,aes(x =lon, y =lat,group=Total.estudios,label=Total.estudios),
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
  geom_text(data=country_coord_counts,aes(x =lon, y =lat,group=Total.estudios,label=Total.estudios),
            size = 3.75)
map.eu
library(patchwork)
p12 <- map+ inset_element(map.eu+theme(plot.background = element_rect(fill = "white"),
                                       panel.border = element_rect(fill = NA, linewidth = 1)),
                            left = 0.415, bottom = 0, right = 0.915, top = 0.35,
                          )

figure<- p12+  geom_segment(aes(x=0.2, xend=0.43,y=0.5,yend=0.37), linewidth=3)
figure

ggsave("Map.figure.1.png", width = 15, height= 12, plot=p12)

remotes::install_github("hughjonesd/ggmagnify")
library(ggmagnify)

m<-map + geom_magnify(
  from=c(xmin=-10,xmax=25,ymin=35,ymax=70),
    to=c(xmin=40,xmax=105,ymin=-80,ymax=-15), 
    linewidth = 0.7, shadow = FALSE,
   colour = "black",
  inherit.aes = FALSE)




help("geom_magnify")

print(combined_plot)


                   
ggsave(filename = "RData/mymap.eu.png",
       plot = m,
       width = 12, height = 8, units = "cm")

  
ggsave(filename = "RData/mymap.2.png",
       plot = map,
       width = 12, height = 8, units = "cm")

save(map, file= "RData/mymap.2.RData")
load(file="RData/mymap.2.RData")


ragg::agg_png("ragg_5x5.png", width = 20, height = 16, units = "in", res = 300, scaling = 0.5)
map
dev.off()

ggsave("map.2.3.png", width = 10, height= 8, dpi=600, unit="in",plot=map)
ggsave("map.eu.png", width = 10, height= 8, plot=m)

##info observaciones/tipo habitat
habitat.pais <- effect.size.total%>%
  group_by(Landscape)%>%
  summarise(habitat.pais=n())




