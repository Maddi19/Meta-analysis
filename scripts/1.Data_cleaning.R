#########DATA CLEANING#####
###########################

rm(list = ls(all.names = TRUE)) #Limpiar objetos ocultos
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #Limpiar paquetes ocultos
pacman::p_load(dplyr, tidyverse, tidyr, readxl,readr, metafor) 

datos <- read_excel("data/data_table.xlsx")

unique(datos$ACC)# 262 estudios, 896 observaciones
unique(datos$Country) #58paises
unique(datos$Year)
table(datos$Year) ## cada pais cuantas obs tiene
#Mexico 111, USA 97, S.Africa 66
#Canada 51, China 58, Brazil 57
#spain 51 (21 de canarias), Australia 32

##cada pais cuantos estudios tiene
estudios.pais <- datos%>%
  group_by(datos$Country)%>%
  summarise(estudios.pais=n_distinct(ACC))

##brazil 21, Aus 16, mexico 15, China 15, S.Africa 26 , usa 33, spain 18 

unique(datos$`Plant species`) 
unique(datos$`Plant species family`) ## 75 familias y 269 especies

familias <- datos %>%
  group_by(`Plant species family`)%>%
  summarise(family=n_distinct(ACC))
##brassicaceae 17 estudios, cactaceae 20, ericaceae 16, fabaceae 14,
# proteaceae 17, rosaceae 28, cucurbitaceae y rubiaceae 11

unique(datos$`Pollinator guilds`)

unique(datos$`Insects vs OtherTaxa`)

unique(datos$Landscape)
datos$Landscape <- recode(datos$Landscape, "beech forest"= "Forest",
                          "beech forest, pasture"="Forest",
                          "dry forest"= "Forest",
                          "pastured forest"="Forest",
                          "orchard"="Agricultural",
                          "desert scrub"="Shrubland",
                          "research plot"="Experimental",
                          "mixed agroforestry system"="Agricultural",
                          "plantation"="Agricultural",
                          "shrubland"="Shrubland",
                          "field"="Agricultural",
                          "Shrubland caatinga"="Shrubland",
                          "Coastal grassland and sandy area"="Dune",
                          "farm field"="Agricultural",
                          "Orchard"="Agricultural",
                          "Bushland"="Shrubland",
                          "Mediterranean forest"="Forest",
                          "wet forest"= "Forest",
                          "Shruland"="Shrubland",
                          "Pastures and fields"= "Agricultural",
                          "savanna"="Savanna",
                          "city yards"="Urban",
                          "fragmentation"="Forest",
                          "homegarden"="Urban",
                          "open site"= "Forest",
                          "botanical cultive"="Urban",
                          "Shrubland and sclerophyllous forets"= "Forest",
                          "Botanical garden"="Urban",
                          "broadleaf & mixed forest"= "Forest",
                          "Subtropical rainforest"="Forest",
                          "Subtropical deciduous forest"="Forest",
                          "Broadleaf forest"="Forest",
                          "Subtropical evergeen forest"="Forest",
                          "tropical forest"="Forest",
                          "coastal woodland"="Shrubland",
                          "Perennial grassland"="Grassland",
                          "Experimental plots"="Experimental",
                          "Fynbos shrus"="Shrubland",
                          "crop"="Agricultural",
                          "rocky areas and shrubland gaps"="Shrubland",
                          "deciduous forest"= "Forest",
                          "wet grassland"="Grassland",
                          "forest"="Forest",
                          "permanent orchard"="Agricultural",
                          "farm"="Agricultural",
                          "woodland"="Forest",
                          "wet coastal heathland"="Shrubland",
                          "Temperate forest-crop edges"= "Agricultural",
                          "Temperate forest"="Forest",
                          "Atlantic forest"="Forest",
                          "Shrubs"="Shrubland",
                          "Apple orchards"="Agricultural",
                          "Tropical floodplain forest and farmlands"= "Agricultural",
                          "marshes"="Marsh",
                          "Obs"="Shrubland",
                          "Agricultural orchards"="Agricultural",
                          "pine woodland"="Forest",
                          "shrubby"="Shrubland",
                          "Crop"="Agricultural",
                          "Experimental farm"="Agricultural",
                          "sawgrass marshes  and marl prairies"="Marsh",
                          "Farm"="Agricultural",
                          "Rainforest"="Forest",
                          "Coastal broadleaf forest"="Forest",
                          "almond fields"="Agricultural",
                          "experimental garden"="Experimental",
                          "meadow, agricultural landscape"="Agricultural",
                          "Sand dune"="Dune",
                          "agricultural landscape"="Agricultural",
                          "Agricultural landscape"="Agricultural",
                          "Exp plots"="Experimental",
                          "Chaparral woodland"="Shrubland",
                          "Fynbos shrubs" ="Shrubland",
                          "Subtropical evergreen forest"="Forest",
                          "Shrubland and sclerophyllous forest"="Forest",
                          "Experimental Cage"="Experimental"
                    
                          )


unique(datos$Landscape)
table(datos$Landscape)
##360 obs agricultural, 195 shrubland, 157 forest

##cuantos tipos de habitat por estudio
habitat.pais <- datos%>%
  group_by(Landscape)%>%
  summarise(habitat.pais=n_distinct(ACC))
##101 estudios de agricultura, 56 de shrubland, 48 forest

unique(datos$Biome)
datos$Biome <- recode(datos$Biome, "Temperate broadleaf & mixed forest"="Temperate Broadleaf & Mixed Forests",
                      "Temperate Grasslands, Savannas &"="Temperate Grasslands, Savannas & Shrublands",
                      "Temperate Broadleaf & Mixed Fore"="Temperate Broadleaf & Mixed Forests",
                      "Temperate Broadleaf & Mixed forest"="Temperate Broadleaf & Mixed Forests",
                      "Temperate"="Temperate Broadleaf & Mixed Forests",
                      "Temperate Broadleaf & Mixed Forest"="Temperate Broadleaf & Mixed Forests")

                     
                
unique(datos$Biome)
table(datos$Biome)
##287 temperate broadleaf mixed forest
## 107 desert
##128tropical & subtropical moist broadleaf forest
#101 Mediterranean

##cantidad de biomas por estudio
Biomes<- datos%>%
  group_by(Biome)%>%
  summarise(pais.biome=n_distinct(ACC))
#81 estudios de Temperate Forest, 38 de Tropical Forest, 35 Mediterranean

Biome<- datos%>%
  group_by(Biome,Country)%>%
  summarise(pais.biome=n_distinct(ACC))


pais.habitat<- datos%>%
  group_by(Landscape,Country)%>%
  summarise(pais.habitat=n_distinct(ACC))

## de agricultura, 10 estudios son de la india, 16 de USA, 7 Indonesia
#de forest, brazil 10, aus 7,
##grassland s.africa 9,
##shrubland aus 7, mexico 9, s africa 12, 

##Categorizar cuando son estudios de guild diversity.
datos$Lower_diversity_guild<- recode(datos$Lower_diversity_guild,
                                      "birds excluded"="Vertebrate Diurnal flying exclusion",
                                     "Nocturnal visitors"="Diurnal exclusion",
                                     "Diurnal visitors"="Nocturnal exclusion",
                                      "Nocturnal vertebrates excluded"= "Vertebrate Nocturnal flying/no-flying exclusion",
                                      "diurnal visitors"  ="Nocturnal exclusion",
                                      "nocturnal visitors"="Diurnal exclusion",
                                      "Crepuscular pollinators"="Diurnal exclusion",
                                      "Hummingbirds excluded"="Vertebrate Diurnal flying exclusion",
                                      "moths"="Invertebrate Diurnal flying exclusion",
                                      "diurnal insects (birds excluded)"= "Vertebrate Diurnal flying exclusion",
                                      "insects (birds excluded)"= "Vertebrate Diurnal flying exclusion",
                                      "Bats excluded"="Vertebrate Nocturnal flying exclusion",
                                      "hummingbird excluded"="Vertebrate Diurnal flying exclusion",
                                      "Hummingbird excluded"="Vertebrate Diurnal flying exclusion",
                                      "Lizards excluded (only birds)" = "Vertebrate No-Flying exclusion",
                                      "Lizards excluded" ="Vertebrate No-flying exclusion",
                                      "Mammals excluded" ="Vertebrate  No-flying exclusion",
                                      "birds and rodents excluded"= "Vertebrate Diurnal flying/no-flying exclusion",
                                      "Humminbirds excluded" = "Vertebrate Diurnal flying exclusion",
                                      "Birds excluded" ="Vertebrate Diurnal flying exclusion",
                                      "Birds and small mammals excluded"  ="Vertebrate Diurnal flying/no-flying exclusion", 
                                      "large vertebrates (rats, monkeys, birds) excluded" ="Vertebrate flying/no-flying exclusion" ,
                                      "Nocturnal moths"  = "Invertebrate Diurnal flying exclusion",
                                      "walking insects" ="Invertebrate  flying exclusion",
                                      "ants"="Invertebrate flying exclusion",
                                      "Podarcis lilfordi (lizards) excluded" ="Vertebrate No-flying exclusion",   
                                      "Non-flying mammals excluded"  ="Vertebrate No-flying exclusion",
                                      "non-flying mammal pollinators excluded"="Vertebrate No-flying exclusion",
                                      "flower dwellers (ants, small beetles)"="Invertebrate flying exclusion",
                                     "Vertebrate diurnal flying excluded"="Vertebrate Diurnal flying exclusion",
                                      "Bats  excluded"="Vertebrate Nocturnal flying exclusion",
                                      "Bats excluded"="Vertebrate Nocturnal flying exclusion",
                                      "Bats excluded" = "Vertebrate Nocturnal flying exclusion",
                                      "bats excluded"="Vertebrate Nocturnal flying exclusion",
                                      "only winged" ="Invertebrate No-flying exclusion",
                                      "Sunbird exclusion" ="Vertebrate Diurnal flying exclusion",
                                      "flying insects and birds (rats excluded)" ="Vertebrate No-flying exclusion",
                                      "Crepuscular visitors"  = "Diurnal flying exclusion",
                                      "ants excluded (honeybees)"="Invertebrate No-flying exclusion",
                                      "honey possums" ="Vert/Invert Diurnal flying exclusion",
                                     "vertebrate diurnal flying excluded"="Vertebrate Diurnal flying exclusion",
                                     "Vertebrate Diurnal flying exclusion"="Vertebrate Diurnal flying exclusion",
                                      "Papilio spp and Macroglossum pyrrhostica" ="Invertebrate Wild Flying exclusion",
                                      "nocturnal and hummingbird exclusion"  = "Invertebrate Diurnal Wild Flying",
                                      "birds and diurnal insects excluded"= "Diurnal flying exclusion",
                                      "diurnal and bats exclusion"  = "Invertebrate Nocturnal Wild Flying",
                                      "nocturnal"="Diurnal exclusion",
                                      "Bird & bat exclusion"  ="Vertebrate exclusion",
                                      "ants excluded" ="Invertebrate No-flying exclusion",
                                      "Thrips"= "Invertebrate flying exclusion",
                                      "beetles and thrips" ="Invertebrate flying exclusion",
                                      "butterflies excluded" ="Invertebrate flying exclusion",
                                      "Papilio spp. Excluded" = "Invertebrate flying exclusion",
                                      "5mm mesh" ="Vertebrate Flying exclusion", 
                                      "1mm mesh" = "Invertebrate  Wild Flying",
                                      "12mm mesh" ="Vertebrate Flying exclusion",
                                      "flying insects excluded"="Invertebrate flying exclusion",
                                      "humming birds"="Invertebrate flying exclusion",
                                     "Ants excluded"="Invertebrate No-flying exclusion",
                                     "ants"="Invertebrate Flying exclusion",
                                     "vertebrate diurnal no-flying excluded"= "Vertebrate Diurnal No-flying excluded",
                                     "vertebrate diurnal flying/no-flying excluded"= "Vertebrate diurnal flying/no-flying excluded",
                                     "Invertebrate diurnal flying exclusion"= "Invertebrate Diurnal flying exclusion",
                                     "vertebrate nocturnal no-flying excluded"  = "Vertebrate nocturnal no-flying excluded",
                                     "Vertebrate diurnal flying/no-flying excluded"= "Vertebrate Diurnal flying/no-flying excluded"
                                      
                            
                       )

unique(datos$Lower_diversity_guild)
table(datos$Lower_diversity_guild)

##Categorizar medidas de exito reproductivo.
unique(datos$`Reproductive succes measure`)
table(datos$`Reproductive succes measure`)

datos$`Reproductive succes measure` <- recode(datos$`Reproductive succes measure`,
                                              "frui set"="fruit set",
                                              "fruit mass"="yield",
                                              "fruit set (%)"="fruit set",
                                              "fruit size (mm)"="fruit size",
                                              "fruit diameter"="fruit size",
                                              "fruit counts"="fruit number",
                                              "fruit biomass"="fruit weight",
                                              "fruit weight (g)"="fruit weight",
                                              "fruits per plant"="fruit number",
                                              "fruit set plant"="fruit number",
                                              "fruits/tree"="fruit number",
                                              "fruit set (fruits/raceme)"="fruit number",
                                              "fruit set (inflo)"="fruit number",
                                              "fruit set (pods/raceme)"="fruit number",
                                              "fruiting success"="fruit set",
                                              "fruits per inflorescence"="fruit set",
                                              "fruits/plant"="fruit number",
                                              "Yield"="yield",
                                              "Yield (fruit g)"="yield",
                                              "Yield (kg)"="yield",
                                              "Yield/ha"="yield",
                                              "yield/ha"="yield",
                                              "Yield (total seed mass)"="Yield",
                                              "seed number (flower)"= "seed set",
                                              "seed per inflorescence"= "seed set",
                                              "seed per plant"="seed set",
                                              "seed set (plants %)"="seed set %",
                                              "seed set (seeds/raceme)"="seed set",
                                              "seed set (totalseed/nflowers)"="seed set",
                                              "sees set /fruit"="seed set",
                                              "seed set flower"="seed set",
                                              "seed set per fruit"="seed set",
                                              "seed set per ovule"="seed set %",
                                              "seed set plant"="seed set",
                                              "seed set/plant"="seed set",
                                              "seed weight (mg)"="seed weight",
                                              "seed weight per plant"="seed weight",
                                              "seed weight/branch"="seed weight",
                                              "seet set mass"="seed weight",
                                              "seed mass plant"="seed weight",
                                              "1000 grain weight (g)"="seed weight",
                                              "seed number"="seed set",
                                              "seeds per flower"="seed set",
                                              "seeds per fruit"="seed set",
                                              "seeds per plant"="seed set",
                                              "seeds/flower"="seed set",
                                              "seeds/fruit"="seed set",
                                              "seeds/inflorescence"="seed set",
                                              "seeds/plant"="seed set",
                                              "seed set mass"="seeds weight",
                                              "seed set plant"="seed set",
                                              "seet set plant"="seed set",
                                              "seed set (inflo)"= "seed set",
                                              "seed set /fruit"="seed set",
                                              "total seed production"="seed set",
                                              "total seed/plant"="seed set",
                                              "total seeds"="seed set",
                                              "viable seeds"="seed set",
                                              "plant community seed set"="seed set",
                                              "plant community fruit set"="fruit set",
                                              "percent seed set"="seed set %",
                                              "percent viable seed set"="seed set %",
                                              "nut yield"="seed yield",
                                              "nut yield (g)"="seed yield",
                                              "nut set (seed set/flower)"="seed set",
                                              "pollen load per stigma"="Pollen load",
                                              "Pollen tubes per stigma (logx+1)"="Pollen load",
                                              )

unique(datos$`Reproductive succes measure`)
table(datos$`Reproductive succes measure`)


unique(datos$Higher_diversity_guilds)
table(datos$Higher_diversity_guilds)
datos$Higher_diversity_guilds<- recode(datos$Higher_diversity_guilds,
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

unique(datos$Year)
table(datos$Year)
datos<-datos%>%
  filter(!is.na(Year))

##clean Plant.species column in our database
datos$`Plant species`<- recode(datos$`Plant species`, 
                               "Mango"="Mangifera indica",
                               "Amygdalus persica"="Prunus persica",
                               "Aloe peglerae"="Aloe plegerae",
                               "Carnegieae gigantea"="Carnegiea gigantea",
                               "Pseudopanax arboreus"="Neopanax arboreus",
                               "Putoria calabrica"="Plocama calabrica",
                               "Vaccinium ashei"="Vaccinium virgatum",
                               "Aloe plicatilis"="Kumara plicatilis",
                               "Actinidia sp."="Actinidia arguta",
                               "Rhabdothanus solandri"="Rhabdothamnus solandri",
                               "Brassica oleracea var.capita"="Brassica oleracea var.capitata",
                               "Perse americana"="Persea americana",
                               "Marginatocereus marginatus"="Lophocereus marginatus",
                               "Oroxylum"="Oroxylum indicum",
                               "Pilosocereus leucocephalu"="Pilosocereus leucocephalus",
                               "Leucostele terscheckii"="Echinopsis terscheckii",
                               "Poeonia broteroi"="Poeonia broteri",
                               "Miconia tococa"="Tococa guianensis",
                               "Fragaria x annanasa"="Fragaria ananassa",
                               "Fragaria x ananassa"="Fragaria ananassa",
                               "Sophora mucrophylla"="Sophora microphylla",
                               "Shepherdia canadiensis"="Shepherdia canadensis",
                               "Solanum quitoens"="Solanum quitoense",
                               "Helianthus annulus"="Helianthus annuus",
                               "Hylocereus undatus"="Selenicereus undatus",
                               "Stenocereus thuberi"="Stenocereus thurberi",
                               "Ipomoea aff.marcellia"="Ipomoeae marcellia",
                               "Litchi chinenis"="Litchi chinensis",
                               "Lophocereus schotti"="Pachycereus schottii")

datos[grep("Rhabdothanus", datos$Plant.species), ]

##or create new column genus now:
datos<- datos%>%
  mutate(Genus= word(`Plant species`, 1))
#fill genus column with new names
#datos<- datos%>%
 # mutate(Genus=case_when(
  #  is.na(Genus) ~ word(`Plant species`, 1),
   # TRUE~Genus))

#fill plant species family column
datos<- datos%>%
  mutate(`Plant species family` = case_when(
    is.na(`Plant species family`) & `Plant species` == "Mangifera indica" ~ "Anacardiaceae",
    is.na(`Plant species family`) & `Plant species` == "Clerodendrum molle" ~ "Lamiaceae",
    is.na(`Plant species family`) & `Plant species` == "Oroxylum indicum" ~ "Bignoniaceae",
    is.na(`Plant species family`) &`Plant species` == "Geniostoma ligustrifolium" ~ "Loganiaceae",
    is.na(`Plant species family`) & `Plant species` == "Neopanax arboreus" ~ "Araliaceae",
    is.na(`Plant species family`) & `Plant species` == "Pittosporum crassifolium" ~ "Pittosporaceae",
    is.na(`Plant species family`) & `Plant species` == "Pyrus sinkiangensis" ~ "Rosaceae",
    is.na(`Plant species family`) & `Plant species` == "Sophora microphylla" ~ "Fabaceae",
    is.na(`Plant species family`) & `Plant species`== "Metrosideros excelsa" ~ "Myrtaceae",
    TRUE~`Plant species family`))

unique(datos$`Plant species`)
#276sp
unique(datos$`Plant species family`)
#78familias
write.csv(datos,"C:\\Users\\OK\\Desktop\\Meta-analisis\\datos.limpios1.csv" )

##info de familias, genero y especie
info_sp <- datos %>%
  group_by(`Plant species family`,Genus,`Plant species`,)%>%
  summarise(n=n())

write.csv(info_sp, "RData/tabla.sp.csv")
 