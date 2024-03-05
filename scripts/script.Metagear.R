##META ANALISIS, PAQUETE METAGEAR

install.packages("BiocManager")
BiocManager::install("EBImage")
library("BiocManager")
library(EBImage)
library(metagear)
install.packages("gWidgets")
library(gWidgets)

#cargar datos
setwd("C:\\Users\\OK\\Desktop\\Meta-analisis") 
theReferences <- read.csv("WOS1.csv", header = TRUE, sep=";") 
names(theReferences)
str(theReferences)
dim(theReferences)

#Crear nuevo csv con una columna nueva, Maddi reviewer
theRef2<-effort_distribute(theReferences, initialize = TRUE, reviewers = "Maddi", save_split = TRUE) 
#se llama effort_Maddi
effort_Maddi <- read.csv("effort_Maddi4.csv", header = TRUE, sep=",") 
str(effort_Maddi)
names(effort_Maddi)
dim(effort_Maddi)


#Abrir nueva ventana para escanear los titulos y los resumenes
abstract_screener(
  file = file.choose("effort_Maddi4.csv"),
  aReviewer = "Maddi",
  reviewerColumnName = "REVIEWERS",
  unscreenedColumnName = "INCLUDE",
  unscreenedValue = "not vetted",
  abstractColumnName = "Abstract.Note",
  titleColumnName = "Title",
  browserSearch = "https://www.google.com/search?q=",
  fontSize = 13,
  windowWidth = 70,
  windowHeight = 16,
  theButtons = c("YES", "maybe", "NO"),
  keyBindingToButtons = c("y", "m", "n"),
  buttonSize = 10,
  highlightColor = "powderblue",
  highlightKeywords = NA
)

#Para conocer cuantos articulos llevas incluidos y excluidos
effort_Maddi
effort_summary(effort_Maddi, column_reviewers = "REVIEWERS", column_effort = "INCLUDE", dual = FALSE, quiet = FALSE)

#Esquema del protocolo del meta-analisis
phases <- c("START_PHASE: 6747 Database searching. Keywords search in title, abstract, keywords", 
            "4962 Studies after duplicates removed", 
            "Examine title and abstract", 
            "EXCLUDE_PHASE: 3298 Obviously irrelevant studies excluded", 
            "1664 Filtered reference library", 
            "1664 Examine full text",
            "EXCLUDE_PHASE: 1457 full-text excluded, not fitting eligibility criteria or incomplete data reported",
            "final 207 Studies included in quantitative synthesis (meta-analysis). Accepted reference library") 
p1<-plot_PRISMA(phases,  design ="sunSplash" )
help("plot_PRISMA")
ggsave(p1, filename="RData/figures/prisma.png")

install.packages("PRISMAstatement")
library(PRISMAstatement)
prisma(found = 6747,
       found_other = 0,
       no_dupes = 4962, 
       screened = 4962, 
       screen_exclusions = 3298, 
       full_text = 1664,
       full_text_exclusions = 1457, 
       qualitative=207,
       quantitative = 207,
       width = 200, height = 200,
       design= "sunSplash")

cinnamonMint, sunSplash, pomegranate, vintage, grey, and greyMono
###
setwd("C:\\Users\\OK\\Desktop\\Meta-analisis\\wos2")
wos2 <- read.csv("WOS.2.csv", header = TRUE, sep=" ") 
names(wos2)
str(wos2)
dim(wos2)

#Crear nuevo csv con una columna nueva, Maddi reviewer
theRef2<-effort_distribute(wos2, initialize = TRUE, reviewers = "Maddi", save_split = TRUE) 
#se llama effort_Maddi
effort_Maddi <- read.csv("effort_Maddi.csv", header = TRUE, sep=",") 
str(effort_Maddi)
names(effort_Maddi)
dim(effort_Maddi)

#Abrir nueva ventana para escanear los titulos y los resumenes
abstract_screener(
  file = file.choose("effort_Maddi.csv"),
  aReviewer = "Maddi",
  reviewerColumnName = "REVIEWERS",
  unscreenedColumnName = "INCLUDE",
  unscreenedValue = "not vetted",
  abstractColumnName = "Abstract.Note",
  titleColumnName = "Title",
  browserSearch = "https://www.google.com/search?q=",
  fontSize = 13,
  windowWidth = 70,
  windowHeight = 16,
  theButtons = c("YES", "maybe", "NO"),
  keyBindingToButtons = c("y", "m", "n"),
  buttonSize = 10,
  highlightColor = "powderblue",
  highlightKeywords = NA
)
