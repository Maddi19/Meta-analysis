######TRY and Lanuza et al plant traits data cleaning#####
##########################################################


#preparar consola
rm(list = ls(all.names = TRUE)) #Limpiar objetos ocultos
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #Limpiar paquetes ocultos
pacman::p_load(tidyverse,rtry,rJava, dplyr,metafor,cowplot,orchaRd,ggbeeswarm,tidyr,ggthemes,sp,broom,lemon,MuMIn,glmulti,PerformanceAnalytics,GGally,gt,geodata,
               ggmap,mapproj,glmulti,MuMIn, RColorBrewer,ggsci)

#cargar nuestros datos
repr.success <- read.csv("data\\3.effectsizes_clean.csv")
##eliminar datos sin effect size
trait.1<-repr.success[!is.na(repr.success$yi), ]
trait.1<-trait.1[!is.na(trait.1$vi), ]


trait.1<-trait.1%>%
  filter(!str_detect(Pollinator.variable.measure, "abundance")) %>%
  filter(!str_detect(Pollinator.variable.measure, "visitation"))

##751obs
unique(trait.1$Plant.species)

###crear columna genero
library("stringr")
trait.1<- trait.1%>%
  mutate(Genus= word(Plant.species, 1))
unique(trait.1$Genus)


############################################################333
#############################################################
##############TRY DATA BASE######
library(rtry)
trydata <- rtry_import("traits/29811.txt")
unique(trydata$AccSpeciesName)
unique(trydata$TraitName)


sortu <- function(x) {x %>% unique %>% sort}
# Check for typos in TRY species names ...
sortu(trydata$AccSpeciesName)

# In TRY files one trait (defined by 'TraitName') can comprise info from different datasets (the so-called DataName).
# The 'DataName' provides minimum information about how the trait has been measured, but also the so-called ancillary data,
# which is particularly useful for checking the suitability/quality of data
colnames(trydata)
trydata<-trydata%>%
  drop_na(TraitID)

# Some basic summaries
dim(trydata)  # number of total records and cols
rtry_explore(trydata, AccSpeciesID, AccSpeciesName, TraitID, TraitName)  # records by species and trait
rtry_explore(trydata, DataID, DataName, TraitID, TraitName, sortBy = TraitID)  # records by DataName


# 3.Clean data ------------------------------------------------------------

# 1.Select target cols (wd = workdata)
wd <- rtry_select_col(trydata, ObsDataID, ObservationID, AccSpeciesID,
                      AccSpeciesName, ValueKindName, TraitID, TraitName, 
                      DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, 
                      StdValue, UnitName, OrigObsDataID, ErrorRisk, Comment)

# 2.Select rows (all trait records + relevant ancillary data)
wd_explore_anc <- rtry_explore(wd, DataID, DataName, TraitID, TraitName, sortBy = TraitID)
wd_explore_anc %>%
  filter(is.na(TraitID)) %>%
  arrange(-Count) %>%
  View()  # check the table and select most relevant ancillary data

# Extract most relevant ancillary data indentified in the previous step, for istance:
anc_relevant <- c(59:61,  # lat, long, alt
                  113, 114, 1412,  # location, reference, country
                  1863,  # habitat
                  413,  # plant development
                  6557,#reference self incompatibility
                  6556,#reference flower sex
                  6554)  # status in the plant list

# Select all records + relevant ancilliary data
wd <- rtry_select_row(wd, TraitID > 0 | DataID %in% anc_relevant)
View(wd)
# Each ObservationID has >=1 ObDataID corresponding to the trait measurement (e.g. TraitID = 3120), 
# and the others to ancillary data (TraitID = NA)

# Summarise data
rtry_explore(wd, DataID, DataName, TraitID, TraitName, sortBy = TraitID)

# 3.Remove data (duplicates, non-representative, non-meaningful, outliers ...)

# 3.7.Remove duplicates ---------------------------------------------------
wd <- rtry_remove_dup(wd)

# Explore trait coverage
trait_mean %>% group_by(AccSpeciesName) %>% count(name = "n_traits") %>% View()
trait_mean %>% group_by(TraitName) %>% count(name = "n_taxa") %>% arrange(-n_taxa) %>% View()

# Identify traits with VERY low coverage (if any), or alternatively by 
trait_no_coverage <- c("", "")

# or alternativaly remove taxa with too many gaps in trait data
taxa_no_data <- c("", "")

# Apply filter
wd <- rtry_exclude(wd, (TraitName %in% trait_no_coverage), baseOn = ObservationID)
wd <- rtry_exclude(wd, (AccSpeciesName %in% taxa_no_data), baseOn = ObservationID)
unique(wd$AccSpeciesName)


traits<- wd %>%
  pivot_wider(names_from = TraitName,
              values_from = OrigValueStr)

# Columns to rename
columns_to_rename <- colnames(traits)[16:29]

# Function to replace spaces with underscores
replace_spaces <- function(x) gsub(" ", "_", x)

# Apply the function to selected column names
new_column_names <- replace_spaces(columns_to_rename)

# Update the column names
colnames(traits)[16:29] <- new_column_names

# Print the result
print(traits)

##Select columns of interes
traits<- traits%>%
  select(AccSpeciesName,Flower_color,Flower_sex,`Flower_sexual_self-incompatibility_mechanism`,
         Plant_mating_system, Flower_insemination_autogamous_or_xenogamous, Species_reproduction_type, Flower_corolla_type, Flower_stylus_length,
         Flower_stamen_length, Flower_nectar_tube_depth, Flower_number_per_plant,Flower_morphology_type)

unique(traits$Flower_color)


####recode and clean color names
traits$Flower_color<- recode(traits$Flower_color, "gelb"= "Yellow",
                             "yellow"="Yellow",
                             "sattgelb"="Yellow",
                             "schwefelgelb"="Yellow",
                             "goldgelb"="Yellow",
                             "gelb und bräunlich"="Yellow or Brown",
                             "goldgelb, grün"="Yellow and Green",
                             "gelblichgrün, später rötlich"="Yellow-Green",
                             "Green"="Green",
                             "purpurrot"="Purple",
                             "purple"="Purple",
                             "rosarot, blauviolett oder blau, und gelb"="Pink,Purple/Blue or Yellow",
                             "red"="Red",
                             "blutrot"="Red",
                             "karminrot"="Red",
                             "hellkarminrot bis fleischrot"="Red",
                             "weißlich rötlich"="White",
                             "rosa"="Pink",
                             "pink"="Pink",
                             "rosa bis weißlich"="White",
                             "hellrosa oder weiß"="White",
                             "rosa bis purpurviolett"="Purple",
                             "violett oder weiß, dunkel geadert"="Purple or White",
                             "violet"="Purple",
                             "weißlich"="White",
                             "white"="White",
                             "grünlichweiß"="White",
                             "weiß oder rötlich"="White or Red",
                             "weiß"="White",
                             "weiß oder rosarot"="White",
                             "weiß oder rosa"="White",
                             "weiß bis rosa"="Pink/White",
                             "gelblichweiß"="Yellow/White",
                             "weiß oder hellrosa"="White",
                             "orange"="Orange",
                             "blue"="Blue",
                             "pink, purple"="Pink/Purple",
                             "pink, white"="Pink/White",
                             "pink, purple, red"="Pink",
                             "blanco"="White",
                             "amarillo"="Yellow",
                             "verde"="Green",
                             "green"="Green")


##hay mutiples valores para un trait y una misma especie. Vamos a juntarlos
df_collapsed <- traits %>%
  group_by(AccSpeciesName) %>%
  summarise(
    Flower_color = paste(na.omit(Flower_color), collapse = ", "),
    Flower_sex = paste(na.omit(Flower_sex), collapse = ", "),
    `Flower_sexual_self-incompatibility_mechanism` = paste(na.omit(`Flower_sexual_self-incompatibility_mechanism`), collapse = ", "),
    Plant_mating_system= paste(na.omit(Plant_mating_system), collapse = ", "),
    Flower_insemination_autogamous_or_xenogamous= paste(na.omit(Flower_insemination_autogamous_or_xenogamous), collapse = ", "),
    Species_reproduction_type= paste(na.omit(Species_reproduction_type), collapse = ", "),
    Flower_corolla_type= paste(na.omit(Flower_corolla_type), collapse = ", "))
##hay info para 85 especies


#delete data of plant community, more than one plant species
trait.1<- trait.1 %>% 
  filter(!str_detect(Plant.species, ","))

unique(trait.1$Plant.species.family)


##probar cuantos coinciden de nuestros datos trait.1:
traits.1<- trait.1 %>% 
  left_join(df_collapsed, by=c(Plant.species="AccSpeciesName"))%>%
  group_by(Flower_color,Plant.species)%>%
  summarise(n=n())


df_collapsed<-df_collapsed%>%
  mutate(Flower_color = case_when(
    Flower_color=="Pink, White" & AccSpeciesName == "Coffea canephora" ~ "White",
    Flower_color=="Pink/White, White" & AccSpeciesName=="Malus domestica"~"White",
    Flower_color=="Red, Red, Red, Pink/Purple/Red" & AccSpeciesName=="Trifolium pratense" ~ "Pink",
    Flower_color=="Red, Red, Red, Pink" & AccSpeciesName=="Trifolium pratense" ~"Pink",
    Flower_color=="White or Pink, White, White, Pink/White" & AccSpeciesName== "Trifolium repens"~"White",
    Flower_color=="White, White, White, Pink/White" & AccSpeciesName== "Trifolium repens"~"White",
    Flower_color =="Red, Red" & AccSpeciesName=="Trifolium medium" ~ "Pink",
    Flower_color =="Red, Red, Red" & AccSpeciesName=="Trifolium incarnatum" ~ "Red",
    Flower_color =="Purple, Pink" & AccSpeciesName=="Trifolium resupinatum" ~ "Pink",
    Flower_color=="White, White, Yellow" & AccSpeciesName=="Trifolium hybridum" ~ "White/Pink",
    Flower_color=="Yellow/White, White, Yellow" & AccSpeciesName=="Trifolium alexandrinum" ~ "White",
    Flower_color=="White, Pink, Pink, White" & AccSpeciesName=="Prunus persica"~ "Pink/White",
    Flower_color=="Yellow-Green, Green" & AccSpeciesName=="Euphorbia cyparissias"~ "Green",
    Flower_color=="White or Pink, White, White" & AccSpeciesName=="Fagopyrum esculentum"~ "White",
    Flower_color=="Yellow or Brown, Yellow, Yellow, Orange, Yellow"& AccSpeciesName=="Helianthus annuus" ~"Yellow",
    Flower_color=="Green, Yellow"& AccSpeciesName=="Jatropha curcas" ~"Green",
    Flower_color=="Orange, Red, White, Yellow" & AccSpeciesName== "Kniphofia caulescens"~"Orange,Yellow",
    Flower_color=="Green, Orange, Pink, Red, Yellow" & AccSpeciesName== "Kniphofia laxiflora"~"Orange,Yellow",
    Flower_color=="Green, Orange, Pink, Red, Yellow" & AccSpeciesName== "Kniphofia linearifolia"~"Orange,Yellow",
    Flower_color=="Orange, Red, Yellow" & AccSpeciesName=="Kumara plicatilis" ~"Orange",
    Flower_color=="White or Pink, White" & AccSpeciesName=="Lobularia maritima" ~ "White, Pink",
    Flower_color=="Pink, Red" & AccSpeciesName=="Parkia biglobosa" ~ "Red",
    Flower_color=="Purple, White" & AccSpeciesName=="Passiflora edulis" ~ "Purple, White",
    Flower_color=="Pink, White, Pink, White, White" & AccSpeciesName=="Protea caffra" ~ "Pink, White",
    Flower_color=="Pink, Red, White" & AccSpeciesName=="Protea simplex" ~ "Pink, White",
    Flower_color=="Purple,Pink" & AccSpeciesName=="Trifolium resupinatum" ~ "Pink",
    TRUE~ Flower_color))

df_collapsed$Flower_color<- recode(df_collapsed$Flower_color,
                                   "White, White"="White",
                                   "White, Red, White"="White, Red",
                                   "White, White, White"="White",
                                   "White, White, White, White, White"= "White",
                                   "Yellow, Yellow, Yellow, Yellow"="Yellow",
                                   "Yellow, Yellow"="Yellow",
                                   "Yellow, Yellow, Yellow"="Yellow",
                                   "Yellow, Yellow, White, Yellow"="Yellow",
                                   "Orange, Yellow"="Orange",
                                   "Orange, Red"="Orange",
                                   "White or Red, White"="White",
                                   "Purple, Purple, Pink/Purple"="Purple",
                                   "Pink, Red, White, Pink, Red, White, Pink, Red, White"="Pink, Red, White",
                                   "Yellow, Red, Yellow, Yellow"="Yellow",
                                   "Green, Yellow, Green" ="Green/Yellow",
                                   "Purple or White, Purple"="Purple, White",
                                   "White, Pink, Pink/White"= "Pink, White",
                                   "Yellow and Green, Yellow"="Yellow",
                                   "Pink, Pink"="Pink")

##agrupar por gamas de colores: amarillos, verdes, rosamorados,blancos, azules, rojos, muchos fenotipos.
df_collapsed$Flower_color<- recode(df_collapsed$Flower_color,
                                   "White/Pink"="w_wp",
                                   "Pink/White"="w_wp",
                                   "White"="w_wp",
                                   "White, Red"="w_wp",
                                   "Orange"="o_y",
                                   "Orange,Yellow"="o_y",
                                   "Yellow"="o_y",
                                   "Pink"="pw_puw",
                                   "Pink, White"="pw_puw",
                                   "Purple"="pw_puw",
                                   "Purple, White"="pw_puw",
                                   "Green"="g_y",
                                   "Green/Yellow"="g_y",
                                   "Green, White, Yellow"="g_y",
                                   "Blue, White"="bl",
                                   "Red"="rd",
                                   "Green, Pink"="o_y",
                                   "Green, Pink, Red, Yellow"="o_y",
                                   "Green, White, Yellow"="varied_fenotype",
                                   "Pink, Red, White"="varied_fenotype",
                                   "Pink,Purple/Blue or Yellow, Red"="varied_fenotype")

traits.1<- trait.1 %>% 
  left_join(df_collapsed, by=c(Plant.species="AccSpeciesName"))

unique(df_collapsed$Flower_color)

##outliers
traits.1$upperci <- traits.1$yi + 1.96 * sqrt(traits.1$vi)
traits.1$lowerci <- traits.1$yi - 1.96 * sqrt(traits.1$vi)
# Create filter variable
m0<- rma.mv(yi,vi,mods=~Reproductive.succes.measure-1,random= ~1|Title, data=traits.1)
summary(m0)
traits.1$outlier <- traits.1$upperci < m0$ci.lb | traits.1$lowerci > m0$ci.ub
# Count number of outliers:
sum(traits.1$outlier)
traits.1[traits.1$outlier, c("yi", "upperci", "lowerci")]
##eliminamos 5 datos de outliers
traits.1<-traits.1%>%
  arrange(yi)%>%
  slice(-(1:4))
tail(traits.1$yi)



##añadir info de Lanuza:
##cargar dataset traits de Lanuza
library(readxl)
data.trait <- readxl::read_excel("traits\\Lanuza_data.xlsx")
data.trait<-data.trait[!is.na(data.trait$Species_all), ]
data.trait<- data.trait%>%
  select(c(10,12:17,Flower_colour, Flower_morphology, Flowers_per_plant))

##hay mutiples valores para un trait y una misma especie. Vamos a juntarlos
dt_collapsed <- data.trait %>%
  group_by(Species_all) %>%
  summarise(
    Flower_colour = paste(na.omit(Flower_colour), collapse = ", "),
    Breeding_system = paste(na.omit(Breeding_system), collapse = ", "),
    Sex_or_flower_type = paste(na.omit(Sex_or_flower_type), collapse = ", "),
    IMPUTED_Compatibility = paste(na.omit(IMPUTED_Compatibility), collapse = ", "),
    Flower_morphology = paste(na.omit(Flower_morphology), collapse = ", "),
    Flowers_per_plant = paste(na.omit(Flowers_per_plant), collapse = ", "),
    Autonomous_selfing_level= paste(na.omit(Autonomous_selfing_level), collapse = ", "),)



traits_total<- traits.1 %>% 
  left_join(dt_collapsed, by=c(Plant.species="Species_all"))



##rellenar color de la flor con lso datos de lanuza
traits_total <- traits_total %>%
  mutate(Flower_color= coalesce(Flower_color, Flower_colour))
traits_total$Flower_color<- recode(traits_total$Flower_color,
                                   "white"="w_wp",
                                   "yellow"="o_y",
                                   "orange"="o_y",
                                   "White"="w_wp",
                                   "Orange"="o_y",
                                   "Yellow"="o_y",
                                   "white_yellow"="w_wp"
)

##rellenar con try los datos de breeding sistem en columna de lanuza:
traits_total <- traits_total %>%
  mutate(Breeding_system= coalesce(Breeding_system, word(Flower_sex, 1, sep = ", ")))
#quedarnos con la primera palabra antes de la coma, porque se repite info 
traits_total$Breeding_system <- word(traits_total$Breeding_system , 1, sep = ", ")
unique(traits_total$Breeding_system)
traits_total$Breeding_system <- recode(traits_total$Breeding_system ,
                                       "hermaphroditic"="hermaphrodite",
                                       "dioecy"="dioecious",
                                       "andromonoecy"="andromonoecious",
                                       "yes"="",)



##añadir info de try a columna lanuza de compatibilidad:
traits_total <- traits_total %>%
  mutate(IMPUTED_Compatibility= coalesce(IMPUTED_Compatibility, word(`Flower_sexual_self-incompatibility_mechanism`, 1, sep = ", ")))

unique(traits_total$IMPUTED_Compatibility)   
traits_total$IMPUTED_Compatibility <- recode(traits_total$IMPUTED_Compatibility,
                                             
                                             "self-compatibel species"="self_compatible",
                                             "self compatible"= "self_compatible",
                                             "self-incompatibel species"="self_incompatible",
                                             "self incompatible"="self_incompatible",
                                             "± self-incompatibel species"="self_incompatible",
                                             "± self-compatibel species"="partially_self_compatible",
)

traits_total$IMPUTED_Compatibility[traits_total$IMPUTED_Compatibility == "dioecious"] <- NA
traits_total$IMPUTED_Compatibility[traits_total$IMPUTED_Compatibility == "monoecious, monoecious"] <- NA
traits_total$Sex_or_flower_type[traits_total$Sex_or_flower_type == "NA"] <- NA
#nos quedamos primera palabra, info repetida
traits_total$IMPUTED_Compatibility <- word(traits_total$IMPUTED_Compatibility , 1, sep = ", ")
traits_total$Flower_morphology <- word(traits_total$Flower_morphology , 1, sep = ", ")
traits_total$Flowers_per_plant <- word(traits_total$Flowers_per_plant  , 1, sep = ", ")



unique(traits_total$IMPUTED_Compatibility) 
info_comp <- traits_total%>%
  group_by(Plant.species.family,Genus,Plant.species,IMPUTED_Compatibility)%>%
  summarise(n=n())

traits_total<-traits_total%>%
  mutate(IMPUTED_Compatibility = case_when(
    IMPUTED_Compatibility=="self-compatibel family" & Plant.species.family == "Apiaceae" ~ "self_compatible",
    IMPUTED_Compatibility=="self-compatibel genus" & Genus == "Vaccinium" ~ "self_compatible",
    IMPUTED_Compatibility=="± self-compatibel genus" & Genus == "Vaccinium" ~ "self_compatible",
    Genus== "Vaccinium" ~ "self_compatible",
    TRUE~IMPUTED_Compatibility))

write.csv(traits_total, "data\\4.complete_table.csv")


unique(traits_total$Flower_color)
traits_sub.col_breed<- traits_total%>%
  filter(!is.na(Flower_color) & Flower_color != "" & !is.na(Breeding_system) & Breeding_system != "" )


traits_sub.col_breed<- traits_sub.col_breed%>%
  select(Flower_color, Breeding_system, IMPUTED_Compatibility, Flower_morphology, Flowers_per_plant,Plant.species, Plant.species.family, Genus, yi, Title)


##si queremos convertir los colores en binario
##al final nada-
unique(traits_sub.col_breed$Flower_color)
col_binary <- traits_sub.col_breed %>%
  mutate(yesno = 1) %>%
  distinct%>%
  spread(Flower_color, yesno, fill = 0)

col_select <- col_binary%>%
  select(Plant.species,Genus,5:24)


##para hacer analisis de correspondencia
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)


# Realizar el MCA
variables_mca<- traits_sub.col_breed%>%
  select(Genus, yi, Flower_color, Breeding_system, IMPUTED_Compatibility, Flowers_per_plant, Flower_morphology, Flowers_per_plant)
mca_result <- MCA(variables_mca)

fig<-fviz_mca_var(mca_result,repel=TRUE, choice = "mca.cor",
                  col.var="steelblue", arrow = c(FALSE, TRUE),
                  title="Multiple Correspondence Analysis: Variables´correlations",
                  ggtheme = theme_minimal())

ggsave(fig, filename="RData/figures/MCA.png",
       width = 6, height = 5)

fviz_mca_biplot(mca_result, repel=T, choice="mca.cor")

fviz_mca_var(mca_result, col.var = "contrib",repel=TRUE, habillage="Genus",
             palette.colors=(values=P30),  
             ggtheme = theme_minimal())



pal_uchicago()
ggplot(traits_sub.col_breed, aes(x = yi, y = Genus, color = Flower_color)) +
  scale_color_d3()+
  geom_point() +
  labs(title = "Scatter Plot of yi vs. Genus")

ggplot(traits_sub.col_breed, aes(x = yi, y = Genus, color = Breeding_system)) +
  scale_color_d3()+
  geom_point() +
  labs(title = "Scatter Plot of yi vs. Genus")


fig1<-ggplot(traits_sub.col_breed, aes(x=Flower_color, y=Plant.species.family, fill=yi)) +
  geom_tile() +
  labs(title="Heatmap of effect sizes per plant families and flower color")

ggsave(fig1, filename="RData/figures/heatmap.png",
       width = 8, height = 5)

fig2<-ggplot(traits_sub.col_breed, aes(x=IMPUTED_Compatibility, y=Plant.species.family, fill=yi)) +
  geom_tile() +
  labs(title="Heatmap of effect sizes per plant families and compatibility")

ggsave(fig2, filename="RData/figures/heatmap.png",
       width = 8, height = 5)


library(plotly)
# Crear un gráfico interactivo de dispersión en 3D
##nada solo perder tiempo viebdo q se pueden hacer estas cosas

unique(traits_sub.col_breed$Flower_color)
unique()
P30 <- c("#E64B35B2","#4DBBD5B2", "#6A6599FF", "#FFB84D","#3A6589","#9B5672","#DB735C")
fig <- plot_ly(traits_sub.col_breed, x = ~Flower_color, y = ~Plant.species.family, z = ~yi,
               color = ~Flower_color, colors = P30,
               text = ~paste("Genero: ", Genus, "<br>Especie: ", Plant.species,
                             "<br>Breeding system:", Breeding_system, "<br>Compatibility:", IMPUTED_Compatibility,
                             "<br>Flower morphology:", Flower_morphology),
               marker = list(symbol = 'circle', sizemode = 'diameter'),
               type = "scatter3d", mode = "markers") 

# Agregar etiquetas y título
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Color de Flor'),
                                   yaxis = list(title = 'Género'),
                                   zaxis = list(title = 'Tamaño del Efecto')),
                      title = 'Relación Interactiva entre Variables')

# Mostrar el gráfico
fig


##creandp paletas de colores
library(Polychrome)
##para crear paletas de colores
set.seed(935234)
P30 <- createPalette(30, c("#E64B35B2","#4DBBD5B2", "#6A6599FF", "#FFB84D","#3A6589","#9B5672","#DB735C"), range = c(30, 180), M=1000)
swatch(P30)
names(P30) <- NULL

library(RColorBrewer)
##otras formas de crear paleta de colores
n<-35
qual_cols<-brewer.pal.info[brewer.pal.info$category=="qual",]
col_vector<- unlist(mapply(brewer.pal, qual_cols$maxcolors, rownames(qual_cols)))
pie(rep(1,n), col=sample(col_vector,n))
colse<-sample(col_vector,n)

color<- grDevices::colors()[grep("gr(a|e)y", grDevices::colors(),invert=TRUE)]
set.seed(26934)
cols<- sample(color,n)
pie(rep(1,n),col=sample(color,n))

p1<-ggplot(traits_breed, aes(x=IMPUTED_Compatibility, y=yi)) +
  geom_boxplot()+
  geom_jitter(aes(color=Plant.species.family))+
  geom_tile() +
  scale_colour_manual(values=cols)  +
  labs(title="Effect sizes by breeding systems colored as plant family")

traits_sub.col_breed$Flowers_per_plant<-as.numeric(traits_sub.col_breed$Flowers_per_plant)
traits_fl_count<-traits_sub.col_breed%>%
  arrange(desc(Flowers_per_plant))%>%
  slice(-(1:2))

ggplot(traits_fl_count, aes(x=Flowers_per_plant, y=yi)) +
  geom_point()+
  geom_smooth(stat="")


#############################GLMM-s 
library(lme4)
# Create a subset of your data without missing values
traits_sub_no_na <- na.omit(traits_sub.col_breed[, c("yi", "Title","Flower_color", "Flowers_per_plant", "Flower_morphology", "Breeding_system", "IMPUTED_Compatibility", "Genus", "Plant.species.family")])

full <- lmer(yi~ Flower_color+ Flowers_per_plant+ Flower_morphology + Breeding_system + IMPUTED_Compatibility+ 
               Genus + Plant.species.family+ (1|Title), 
             data=traits_sub_no_na)
library(MuMIn)
res <- MuMIn::dredge(full, trace=4)
subset(res, delta <= 2, recalc.weights=FALSE)

traits_sub$Flowers_per_plant <- as.numeric(traits_sub$Flowers_per_plant)

model<- lmer(yi~ Breeding_system + IMPUTED_Compatibility+ 
               IMPUTED_Compatibility_sp_level+ (1|Title), 
             data=traits_sub, na.action = na.pass)
##primero aparece flower color con mucha colinearidad
##despues de quitarlo: flowers per plant tambien la tiene muy alta
##nos quedamops con esas tres variables.

summary(model)
hist(residuals(model))
library(performance)
check_model(model)
library(easystats)
model_dashboard(model)
##residuales no normalidad

library(effects)
plot(allEffects(model.1))
library(sjPlot)
##plotea valores predictivos del modelo
plot_model(model, type = "eff")

##ploteo de resultado preliminares del modelo
library(modelbased)
estimate_means(model)
library(visreg)
visreg(model)

model.1<- lmer(yi~ Flower_colour + Flowers_per_plant + (1|Title), 
               data=traits_sub)
visreg(model.1)

###ORCHARD PLOT FOR TRAITS:



