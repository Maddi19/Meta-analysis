########## EFFECT SIZE CALCULATION ######

rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,rJava, dplyr,metafor,esc,cowplot,orchaRd,ggbeeswarm,tidyr,ggthemes,sp,broom,lemon,MuMIn,glmulti,PerformanceAnalytics,GGally,gt,geodata,
               ggmap,mapproj,glmulti,MuMIn)

data <- read.csv("data/2.clean_data_nw.csv")

unique(data$Higher_diversity_guilds)
table(data$Higher_diversity_guilds)
unique(data$Lower_diversity_guild)
unique(data$Year)
data$Country <- recode(data$Country, "Camada"="Canada",
                        "Neederlands"="Netherlands")

###### EFFECT SIZES#####

##convert to numeric all values
data$Value<-as.numeric(data$Value)
data$N<-as.numeric(data$N)
table(data$N)
data$valueC<-as.numeric(data$valueC)
data$valueT<-as.numeric(data$valueT)
data$seC<-as.numeric(data$seC)
data$seT<-as.numeric(data$seT)
data$N.treat<-as.numeric(data$N.treat)


##1. data that compares trat with control:
trat <-data%>%
  filter(!is.na(valueC)& !is.na(valueT))
trat.0 <- trat %>% filter((N.treat == 0))
trat<-trat %>% 
  filter( !(X %in% c(505,516)))
          
unique(trat$Title) #143 studies



##### EFFECT SIZE IN METAFOR AND ESC
#SD calculation
trat$SD_C <- trat$seC*sqrt(trat$N.control)
trat$SD_T <- trat$seT*sqrt(trat$N.treat)


##CV calculation
trat$cv_c <- trat$SD_C/trat$valueC
trat$cv_t <- trat$SD_T/trat$valueT

### median CV calculation for Control and Treatment
trat%>%
  filter(!is.na(cv_c)&!is.na(cv_t))%>%
  summarise(median(cv_c))
##0.391

trat%>%
  filter(!is.na(cv_c)&!is.na(cv_t))%>%
  summarise(median(cv_t))
##0.468

###Now we can calculate sd for those without data
newtrat<- trat%>% mutate( 
  SD_C = ifelse(is.na(SD_C), 0.391*valueC, SD_C),
  SD_T =  ifelse(is.na(SD_T), 0.468*valueT,SD_T),
  N=ifelse(is.na(N), N.control+N.treat, N),
  N.treat=ifelse(is.na(N.treat), N/2, N.treat),
  N.control=ifelse(is.na(N.control), N/2, N.control))



### Control and Trat
trat_es<-escalc(m1i=valueC,m2i=valueT,sd1i=SD_C,sd2i=SD_T,
                n1i=N.control,n2i=N.treat,data = newtrat,measure = "SMD",
                slab=Title)


######FOR OTHER MEASURES that are not treatment versus control######
## t-statistic
data.t <-data%>%
  filter(Measure.used=="t")

effect_sizes.t<- effect_sizes(data.t, t = Value, totaln = N, fun = "esc_t", es.type = c("g"))

effect_sizes.t2<-as.data.frame(effect_sizes.t)

effect_sizes.t2 <- effect_sizes.t2 %>%
  rename("yi"="es",
         "vi"="var")
effect_sizes.t2 <-cbind(data.t, effect_sizes.t2)

effect_sizes.t2<- effect_sizes.t2 %>%
  select(ACC,Author,Title,Year,Biome,Landscape,Lower_diversity_guild,Higher_diversity_guilds,
         Location,Plant.species.family,Plant.species, Reproductive.succes.measure,
         Pollinator.variable.measure,Obs.Exp, Escala.N, yi,vi, Country, Measure.used,N)



##Chi square
data.x2 <-data%>%
  filter( Measure.used=="X2",
          !is.na(Measure.used)&!is.na(N))

effect.size_x2<- effect_sizes(data.x2, chisq =Value, totaln = N, fun = "esc_chisq", es.type = "g")

effect_sizes.x2<-as.data.frame(effect.size_x2)
effect_sizes.x2 <- effect_sizes.x2 %>%
  rename("yi"="es",
         "vi"="var")

data.x2 <- data.x2%>%
  filter(!(row_number() %in% c(21, 54)))

effect_sizes.x2 <-cbind(data.x2,effect_sizes.x2)

effect_sizes.x2<- effect_sizes.x2 %>%
  select(ACC,Author,Title,Year,Biome,Landscape,Lower_diversity_guild,Higher_diversity_guilds,
         Location,Plant.species.family,Plant.species, Reproductive.succes.measure,
         Pollinator.variable.measure,Obs.Exp, Escala.N, yi,vi, Country, Measure.used,N)

effect.size.total <- bind_rows(effect_sizes.t2,effect_sizes.x2)

##correlations:
data.r <-data%>%
  filter( Measure.used=="r")


effect.size_r<- effect_sizes(data.r, r =Value, totaln = N, fun = "esc_rpb", es.type = "g")

effect_sizes.r<-as.data.frame(effect.size_r)
effect_sizes.r <- effect_sizes.r%>%
  rename("yi"="es",
         "vi"="var")
effect_sizes.r <-cbind(data.r, effect_sizes.r)

effect_sizes.r<- effect_sizes.r %>%
  select(ACC,Author,Title,Year,Biome,Landscape,Lower_diversity_guild,Higher_diversity_guilds,
         Location,Plant.species.family,Plant.species, Reproductive.succes.measure,
         Pollinator.variable.measure,Obs.Exp, Escala.N, yi,vi, Country, Measure.used,N)

effect.size.total <- bind_rows(effect_sizes.t2,effect_sizes.x2, effect_sizes.r)


## Fisher´s F
data.f <-data%>%
  filter(Measure.used=="F")


effect.size_f<- effect_sizes(data.f, f =Value, totaln = N, fun = "esc_f", es.type = "g")
effect_sizes.f<-as.data.frame(effect.size_f)
effect_sizes.f <- effect_sizes.f%>%
  rename("yi"="es",
         "vi"="var")
effect_sizes.f <-cbind(data.f, effect_sizes.f)

effect_sizes.f<- effect_sizes.f %>%
  select(ACC,Author,Title,Year,Biome,Landscape,Lower_diversity_guild,Higher_diversity_guilds,
         Location,Plant.species.family,Plant.species, Reproductive.succes.measure,
         Pollinator.variable.measure,Obs.Exp, Escala.N, yi,vi, Country, Measure.used,N)

effect.size.total <- bind_rows(effect_sizes.t2,effect_sizes.x2, effect_sizes.f, effect_sizes.r)


##z value
data.z <-data%>%
  filter(Measure.used=="z")

data.z$yi <-sqrt((data.z$Value*sqrt(data.z$N))/ (1- sqrt((data.z$Value^2)*(1/data.z$N))))
effect_sizes.z<-as.data.frame(data.z)


effect_sizes.z<- effect_sizes.z %>%
  select(ACC,Author,Title,Year,Biome,Landscape,Lower_diversity_guild,Higher_diversity_guilds,
         Location,Plant.species.family,Plant.species, Reproductive.succes.measure,
         Pollinator.variable.measure,Obs.Exp, Escala.N, yi, Country, Measure.used,N)


effect.size.total <- bind_rows( effect_sizes.t2,effect_sizes.x2, effect_sizes.f, effect_sizes.r, effect_sizes.z)

##add C and T
effect.size.total <- bind_rows(trat_es, effect_sizes.t2,effect_sizes.x2, effect_sizes.f, effect_sizes.r, effect_sizes.z)
repr.success<- effect.size.total

####CLEAN DATA, CREATE NEW COLUMNS
repr.success$Lower_diversity_guild <- repr.success$Lower_diversity_guild %>% replace_na("cor.lineal")

repr.success<- repr.success %>%  
  mutate("Guild_sp" = case_when(grepl("cor.lineal", Lower_diversity_guild) ~ "Species",
                                            grepl("open.allvisitors", Lower_diversity_guild) ~"Species",
                                            grepl("all",Lower_diversity_guild)~"Species",
                                            grepl("sp", Higher_diversity_guilds)~ "Species",
                                           grepl( "sp", Lower_diversity_guild) | grepl( "open.allvisitors", Higher_diversity_guilds) ~"Guild",
                                           if_any(all_of(c("Lower_diversity_guild", "Higher_diversity_guilds")), ~grepl("sp", .))~"Species", 
                                            grepl("exclusion", Lower_diversity_guild) ~"Guild",
                                            grepl("excluded", Lower_diversity_guild) ~"Guild",
                                            grepl("blow flies", Lower_diversity_guild) ~"Guild",
                                            grepl("25mm mesh", Lower_diversity_guild) ~"Guild",
                                            grepl("bees", Lower_diversity_guild) ~"Guild",
                                            TRUE~"NA"))
table(repr.success$Guild_sp)  


repr.success<- repr.success %>%  
  mutate("Exp_no.exp" = case_when(grepl("open.allvisitors", Lower_diversity_guild) ~ "TRUE",
                                            grepl("sp", Lower_diversity_guild) ~"TRUE",
                                            TRUE~"FALSE"))
table(repr.success$`Exp_no.exp`)  

###About what has been excluded:
##managed or wild pollinator
repr.success<- repr.success %>%  
  mutate("Managed_Wild" = case_when(grepl("HB", Lower_diversity_guild) ~ "Managed",
                             grepl("Honey", Lower_diversity_guild) ~"Managed",
                             grepl("Managed", Lower_diversity_guild)~"Managed",
                             grepl("HB", Higher_diversity_guilds)~"Managed",
                             grepl("Bumble bees", Higher_diversity_guilds) ~"Managed",
                             TRUE ~"Wild"))
table(repr.success$`Managed_Wild`)

#vertebrate or invertebrate
repr.success<- repr.success %>%  
  mutate("Vert_Invert" = case_when(grepl("Vertebrate exclusion", Lower_diversity_guild) ~ "Vertebrate",
                                   grepl("Vertebrate", Lower_diversity_guild) ~ "Vertebrate",
                                  grepl("Lepidoptera excluded", Lower_diversity_guild)~"Invertebrate",
                                  grepl("Invertebrate", Lower_diversity_guild)~"Invertebrate",
                                  grepl("vertebrate",Lower_diversity_guild)~"Vertebrate",
                                  grepl("all.sp",Lower_diversity_guild)~"Invertebrate",
                                  grepl("sp",Lower_diversity_guild)~"Invertebrate",
                                  grepl("sp",Higher_diversity_guilds)~"Invertebrate",
                                  grepl("open.allvisitors", Lower_diversity_guild) ~"Invertebrate",
                                  grepl("all",Lower_diversity_guild)~"Invertebrate",
                                  grepl("all insect visitors",Higher_diversity_guilds)~"Invertebrate",
                                  grepl("honeybees",Higher_diversity_guilds)~"Invertebrate",
                                  grepl("solitary bees, bumblebees, honeybees",Higher_diversity_guilds)~"Invertebrate",
                                  grepl("Bees,flies and ants",Higher_diversity_guilds)~"Invertebrate",
                                  grepl("non-apis bees",Higher_diversity_guilds)~"Invertebrate",
                                  grepl("bees, hoverflies and butterflies",Higher_diversity_guilds)~"Invertebrate",
                                  grepl("halictic bees + orchid bee",Higher_diversity_guilds)~"Invertebrate",
                                  grepl("syrphid flies",Higher_diversity_guilds)~"Invertebrate",
                                  grepl("Native bee",Higher_diversity_guilds)~"Invertebrate",
                                  grepl("meliponina bees",Higher_diversity_guilds)~"Invertebrate",
                                 TRUE ~"NA"))
table(repr.success$`Vert_Invert`)

data$Higher_diversity_guilds<- recode(data$Higher_diversity_guilds,
                                      "all insect visitors"="all visitors",
                                      "honeybees"="Honeybee",
                                      "fly species"="flying pollinators",
                                      "solitary bees, bumblebees, honeybees"="all visitors",
                                      "Bees,flies and ants"="all visitors",
                                      "non-apis bees"="bees",
                                      "bees, hoverflies and butterflies"="all visitors",
                                      "halictic bees + orchid bee"="sp2",
                                      "syrphid flies"="hoverflies",
                                      "Native bee"="wild bees",
                                      "meliponina bees"="wild bees"
                                      
)


##nocturnal or diurnal
repr.success<- repr.success %>%  
  mutate("Diurn_Noctur" = case_when(grepl("Nocturnal", Lower_diversity_guild) ~ "Nocturnal",
                                   grepl("Diurnal",Lower_diversity_guild)~"Diurnal", 
                                   grepl("Lepidoptera excluded", Lower_diversity_guild)~"Diurnal",
                                   TRUE ~"NA"))
table(repr.success$`Diurn_Noctur`)


##flying or no flying
repr.success<- repr.success %>%  
  mutate("Flying_Noflying" = case_when(  grepl("flying/no-flying", Lower_diversity_guild)~"Both",
                                    grepl("no-flying", Lower_diversity_guild)~"No-flying",
                                    grepl("No-flying", Lower_diversity_guild)~"No-flying",
                                    grepl("Flying", Lower_diversity_guild)~"Flying",
                                    grepl("flying", Lower_diversity_guild)~"Flying",
                                    grepl("sp",Higher_diversity_guilds)~"Flying",
                                    grepl("all.sp",Lower_diversity_guild)~"Flying",
                                    if_any(all_of(c("Lower_diversity_guild", "Higher_diversity_guilds")), ~grepl("sp", .))~"Flying", 
                                    TRUE ~"NA"))

table(repr.success$Flying_Noflying)

###crop or no crop observations:
repr.success<- repr.success %>%  
  mutate("Cultivo" = case_when(  grepl("Agricultural", Landscape)~"Crop",
                                         TRUE ~"No_crop"))

table(repr.success$Cultivo)

##grouping all types of temperate and tropical biomes
table(repr.success$Biome)
repr.success<- repr.success %>%  
  mutate("Biomes" = case_when(  grepl("Temperate", Biome)~"Temperate",
                                grepl("Tropical", Biome)~"Tropical",
                                grepl("Mediterranean", Biome)~"Mediterranean",
                                grepl("Deserts", Biome)~"Deserts",
                                 TRUE ~"Others"))
table(repr.success$Biomes)

#######CHANGE SIGN OF THE EFFECT: WHEN DOES THE DECLINE IN BIODIVERSITY MOST AFFECT??
##
repr.success$yi <- -1 * repr.success$yi

##save clean data with effect sizes
write.csv(repr.success,file="data/3.effectsizes_clean_nw.csv")
