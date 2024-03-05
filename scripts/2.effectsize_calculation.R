rm(list = ls(all.names = TRUE)) #Limpiar objetos ocultos
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #Limpiar paquetes ocultos
pacman::p_load(tidyverse,rJava, dplyr,metafor,cowplot,orchaRd,ggbeeswarm,tidyr,ggthemes,sp,broom,lemon,MuMIn,glmulti,PerformanceAnalytics,GGally,gt,geodata,
               ggmap,mapproj,glmulti,MuMIn)

setwd("C:\\Users\\OK\\Desktop\\Meta-analisis") 
datos <- read.csv("datos.limpios1.csv")

unique(datos$Higher_diversity_guilds)
table(datos$Higher_diversity_guilds)
unique(datos$Lower_diversity_guild)
unique(datos$Year)
datos$Country <- recode(datos$Country, "Camada"="Canada",
                        "Neederlands"="Netherlands")

###### EFFECT SIZES#####

##convert to numeric all values
datos$Value<-as.numeric(datos$Value)
datos$N<-as.numeric(datos$N)
datos$valueC<-as.numeric(datos$valueC)
datos$valueT<-as.numeric(datos$valueT)
datos$seC<-as.numeric(datos$seC)
datos$seT<-as.numeric(datos$seT)

##1. solo escogemos datos que comparan trat con contro.
trat <-datos%>%
  filter(!is.na(valueC)& !is.na(valueT))

unique(trat$Title) #142 estudios

## effect size log response ratio
log(trat$valueC)-log(trat$valueT)
##positivo es que es mayor en C que en T.

trat$logRR_manual <- log(trat$valueC)-log(trat$valueT)
trat %>%
  filter(logRR_manual<Inf)%>%
  summarise(mean_imp=mean(logRR_manual))

(exp(0.526)-1)*100 ## en que porcentaje aumenta, aumenta un 69.2%
##inf es que uno de los dos numeros es un 0

##effect size in metafor
#sacamos las sd.
trat$SD_C <- trat$seC*sqrt(trat$N.control)
trat$SD_T <- trat$seT*sqrt(trat$N.treat)

##sacamos cv
trat$cv_c <- trat$SD_C/trat$valueC
trat$cv_t <- trat$SD_T/trat$valueT

###mediana de los cv
trat%>%
  filter(!is.na(cv_c)&!is.na(cv_t))%>%
  summarise(median(cv_c))
##0.383

trat%>%
  filter(!is.na(cv_c)&!is.na(cv_t))%>%
  summarise(median(cv_t))
##0.467

###calculamos sd para los que no tienen datos
newtrat<- trat%>% mutate( 
  SD_C = ifelse(is.na(SD_C), 0.383*valueC, SD_C),
  SD_T =  ifelse(is.na(SD_T), 0.467*valueT,SD_T),
  N=ifelse(is.na(N), N.control+N.treat, N),
  N.treat=ifelse(is.na(N.treat), N/2, N.treat),
  N.control=ifelse(is.na(N.control), N/2, N.control))


trat_es<-newtrat%>%
  filter(logRR_manual<Inf)


######PARA DEMAS MEDIDAS######
##2.los que son t
fruit.t <-datos%>%
  filter(Measure.used=="t")

library(esc)
effect_sizes.t <- esc_t(t = fruit.t$Value, 
                        totaln=fruit.t$N, es.type = "g")
effect_sizes.t2<-as.data.frame(effect_sizes.t)

effect_sizes.t2 <- effect_sizes.t2 %>%
  rename("yi"="es",
         "vi"="var")
effect_sizes.t2 <-cbind(fruit.t, effect_sizes.t2)

effect_sizes.t2<- effect_sizes.t2 %>%
  select(ACC,Author,Title,Year,Biome,Landscape,Lower_diversity_guild,Higher_diversity_guilds,
         Location,Plant.species.family,Plant.species, Reproductive.succes.measure,
         Pollinator.variable.measure,Obs.Exp, Escala.N, yi,vi, Country, Measure.used,N)



##cogemos los que son X2
fruit.x2 <-datos%>%
  filter( Measure.used=="X2")

effect.size_x2 <-esc_chisq(chisq=fruit.x2$Value,totaln=fruit.x2$N,es.type="g")
effect_sizes.x2<-as.data.frame(effect.size_x2)
effect_sizes.x2 <- effect_sizes.x2 %>%
  rename("yi"="es",
         "vi"="var")
effect_sizes.x2 <-cbind(fruit.x2, effect_sizes.x2)

effect_sizes.x2<- effect_sizes.x2 %>%
  select(ACC,Author,Title,Year,Biome,Landscape,Lower_diversity_guild,Higher_diversity_guilds,
         Location,Plant.species.family,Plant.species, Reproductive.succes.measure,
         Pollinator.variable.measure,Obs.Exp, Escala.N, yi,vi, Country, Measure.used,N)

effect.size.total <- bind_rows(effect_sizes.t2,effect_sizes.x2)

###los que son correlaciones
fruit.r <-datos%>%
  filter( Measure.used=="r")

effect.size_r<- esc_rpb(r = fruit.r$Value, totaln=fruit.r$N, es.type = "g")
effect_sizes.r<-as.data.frame(effect.size_r)
effect_sizes.r <- effect_sizes.r%>%
  rename("yi"="es",
         "vi"="var")
effect_sizes.r <-cbind(fruit.r, effect_sizes.r)

effect_sizes.r<- effect_sizes.r %>%
  select(ACC,Author,Title,Year,Biome,Landscape,Lower_diversity_guild,Higher_diversity_guilds,
         Location,Plant.species.family,Plant.species, Reproductive.succes.measure,
         Pollinator.variable.measure,Obs.Exp, Escala.N, yi,vi, Country, Measure.used,N)

effect.size.total <- bind_rows(effect_sizes.t2,effect_sizes.x2, effect_sizes.r)


##con F 
fruit.f <-datos%>%
  filter(Measure.used=="F")

effect.size_f <-esc_f(f=fruit.f$Value, totaln = fruit.f$N, es.type = "g")
effect_sizes.f<-as.data.frame(effect.size_f)
effect_sizes.f <- effect_sizes.f%>%
  rename("yi"="es",
         "vi"="var")
effect_sizes.f <-cbind(fruit.f, effect_sizes.f)

effect_sizes.f<- effect_sizes.f %>%
  select(ACC,Author,Title,Year,Biome,Landscape,Lower_diversity_guild,Higher_diversity_guilds,
         Location,Plant.species.family,Plant.species, Reproductive.succes.measure,
         Pollinator.variable.measure,Obs.Exp, Escala.N, yi,vi, Country, Measure.used,N)

effect.size.total <- bind_rows(effect_sizes.t2,effect_sizes.x2, effect_sizes.f, effect_sizes.r)


###con z value
fruit.z <-datos%>%
  filter(Measure.used=="z")

fruit.z$yi <-sqrt((fruit.z$Value*sqrt(fruit.z$N))/ (1- sqrt((fruit.z$Value^2)*(1/fruit.z$N))))
effect_sizes.z<-as.data.frame(fruit.z)


effect_sizes.z<- effect_sizes.z %>%
  select(ACC,Author,Title,Year,Biome,Landscape,Lower_diversity_guild,Higher_diversity_guilds,
         Location,Plant.species.family,Plant.species, Reproductive.succes.measure,
         Pollinator.variable.measure,Obs.Exp, Escala.N, yi, Country, Measure.used,N)


effect.size.total <- bind_rows( effect_sizes.t2,effect_sizes.x2, effect_sizes.f, effect_sizes.r, effect_sizes.z)

### Control y Trat
trat_es<-escalc(m1i=valueC,m2i=valueT,sd1i=SD_C,sd2i=SD_T,
                     n1i=N.control,n2i=N.treat,data = trat_es,measure = "SMD",
                     slab=Title)

effect.size.total <- bind_rows(trat_es, effect_sizes.t2,effect_sizes.x2, effect_sizes.f, effect_sizes.r, effect_sizes.z)

repr.success<- effect.size.total
write.csv(repr.success, file="RData/effectsizes.csv")

save(repr.success, file="RData/effectsizes.RData")

load("RData/effectsizes.RData")

####DIVIDIR EN COLUMNAS 
repr.success <- read.csv("RData/effectsizes.csv")
unique(repr.success$Lower_diversity_guild)
unique(repr.success$Higher_diversity_guilds)

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

repr.success<- repr.success %>%  
  mutate("Managed_Wild" = case_when(grepl("HB", Lower_diversity_guild) ~ "Managed",
                             grepl("Honey", Lower_diversity_guild) ~"Managed",
                             grepl("Managed", Lower_diversity_guild)~"Managed",
                             grepl("HB", Higher_diversity_guilds)~"Managed",
                             grepl("Bumble bees", Higher_diversity_guilds) ~"Managed",
                             TRUE ~"Wild"))
table(repr.success$`Managed_Wild`)

###sobre que es lo que se ha excluido. hay que hacer subset o filter con exlclusion primero
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
                                  TRUE ~"NA"))
table(repr.success$`Vert_Invert`)


repr.success<- repr.success %>%  
  mutate("Diurn_Noctur" = case_when(grepl("Nocturnal", Lower_diversity_guild) ~ "Nocturnal",
                                   grepl("Diurnal",Lower_diversity_guild)~"Diurnal", 
                                   grepl("Lepidoptera excluded", Lower_diversity_guild)~"Diurnal",
                                   TRUE ~"NA"))
table(repr.success$`Diurn_Noctur`)



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

repr.success<- repr.success %>%  
  mutate("Cultivo" = case_when(  grepl("Agricultural", Landscape)~"Crop",
                                         TRUE ~"No_crop"))

table(repr.success$Cultivo)

table(repr.success$Biome)
repr.success<- repr.success %>%  
  mutate("Biomes" = case_when(  grepl("Temperate", Biome)~"Temperate",
                                grepl("Tropical", Biome)~"Tropical",
                                grepl("Mediterranean", Biome)~"Mediterranean",
                                grepl("Deserts", Biome)~"Deserts",
                                 TRUE ~"Others"))
table(repr.success$Biomes)
#######CAMBIAR SIGNO DEL EFECTO: CUANDO AFECTA MAS LA DISMINUCION DE BIODIVERSIDAD??
repr.success$yi <- -1 * repr.success$yi

write.csv(repr.success,file="RData/effectsizes_clean.csv")
repr.success <- read.csv("RData/effectsizes_clean.csv")

