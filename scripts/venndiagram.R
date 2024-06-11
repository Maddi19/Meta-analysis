###van diagram
install.packages("VennDiagram")
library(VennDiagram)
library(eulerr)
repr.success <- read.csv("data/3.effectsizes_clean_nw.csv")

##delete data without effect size (there is one without country, this is removed from the map)
###remove also observations on abundance and visitation rates
effect.size.total<-repr.success[!is.na(repr.success$yi), ]
unique(effect.size.total$Pollinator.variable.measure)
effect.size.total<-effect.size.total%>%
  filter(!str_detect(Pollinator.variable.measure, "abundance")) %>%
  filter(!str_detect(Pollinator.variable.measure, "visitation"))
effect.size.total<-effect.size.total[!is.na(effect.size.total$vi), ]

# Crear una función para contar las combinaciones de categorías
combination_table <- effect.size.total %>%
  group_by(Diurn_Noctur, Vert_Invert, Managed_Wild) %>%
  summarise(count = n()) %>%
  ungroup()

combination_table[] <- lapply(combination_table, as.character)
combination_table$combination <- paste(combination_table$Diurn_Noctur,
                                       combination_table$Vert_Invert,
                                       combination_table$Managed_Wild,
                                       sep = "_")

combination_table$combination<-as.character(combination_table$combination)



combinations <- c("Diurnal", "Nocturnal", "Vertebrate", "Invertebrate", "Managed", "Wild")

# Inicializar listas vacías para cada combinación
listas_diurnal <- list()
listas_nocturnal <- list()
listas_vertebrate <- list()
listas_invertebrate <- list()
listas_managed <- list()
listas_wild <- list()

# Recorrer todas las listas y asignarlas a la lista correspondiente según la combinación que contienen
for (lst in filtered_combinations) {
  if (any(grepl("Diurnal", lst))) {
    listas_diurnal <- c(listas_diurnal, list(lst))
  }
  if (any(grepl("Nocturnal", lst))) {
    listas_nocturnal <- c(listas_nocturnal, list(lst))
  }
  if (any(grepl("Vertebrate", lst))) {
    listas_vertebrate <- c(listas_vertebrate, list(lst))
  }
  if (any(grepl("Invertebrate", lst))) {
    listas_invertebrate <- c(listas_invertebrate, list(lst))
  }
  if (any(grepl("Managed", lst))) {
    listas_managed <- c(listas_managed, list(lst))
  }
  if (any(grepl("Wild", lst))) {
    listas_wild <- c(listas_wild, list(lst))
  }
}


lista_plana.d <- unlist(listas_diurnal, recursive = FALSE)
lista_plana.n <- unlist(listas_nocturnal, recursive = FALSE)
lista_plana.v <- unlist(listas_vertebrate, recursive = FALSE)
lista_plana.i<- unlist(listas_invertebrate, recursive = FALSE)
lista_plana.m <- unlist(listas_managed, recursive = FALSE)
lista_plana.w <- unlist(listas_wild, recursive = FALSE)


combined_list.d <- unlist(lista_plana.d)
combined_list.n <- unlist(lista_plana.n)
combined_list.v <- unlist(lista_plana.v)
combined_list.i <- unlist(lista_plana.i)
combined_list.m <- unlist(lista_plana.m)
combined_list.w <- unlist(lista_plana.w)

x <- list(
  Diurnal = combined_list.d,
  Nocturnal = combined_list.n,
  Vertebrate = combined_list.v,
  Invertebrate = combined_list.i,
  Managed = combined_list.m,
  Wild = combined_list.w
)

library(VennDiagram)
euler(x = x, filename = NULL)

library(VennDiagram)
venn.diagram(x, filename = NULL, ellipse = TRUE, ggplot = TRUE)

