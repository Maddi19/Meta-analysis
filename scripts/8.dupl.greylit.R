#script to check whether benchmark studies are found in scoping searches

install.packages("remotes")
library(remotes)
install_github("elizagrames/litsearchr", ref="main")
library(litsearchr)
library(synthesisr)
library(dplyr)

#locate bibliographic files
bibfiles <- list.files("greylit/",
                       full.names = TRUE,recursive = FALSE,
                       pattern="*ris")


#read in ref
imported_files<- read_refs(
  filename = bibfiles,
  return_df = TRUE)

# first remove articles that have identical titles
df<- deduplicate(
  imported_files,
  match_by = "title",
  method = "exact"
)

# then find likely duplicates that have similar titles
duplicates_string<- find_duplicates(
  df$title,
  method = "string_osa",
  to_lower = TRUE, rm_punctuation= TRUE,
  threshold = 7)

manual_checks <- review_duplicates(df$title, duplicates_string)
results <- extract_unique_references(df, duplicates_string)

##there are not duplicates.

greylit <-write.csv(results,file = "greylit/greylit.csv")
