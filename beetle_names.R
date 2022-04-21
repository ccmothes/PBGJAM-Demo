# Pull species common names from gbif database


library(taxize)
library(tidyverse)


# Read in species dataset

spec_names <- readRDS("PBGJAM-ShinyDemo/data/scientific_name.rds") %>% 
  as_tibble() %>% 
  filter(!(is.na(code6)))



#try spreadsheet of insect common names from Entomological society of america
entsoc <- readxl::read_excel("PBGJAM-ShinyDemo/data/Common names list 4-18-22.xlsx",
                             skip = 3) %>% 
  mutate(sciName = paste(Genus, species, sep = " "))

#see how many pbgjam species found in this spreadsheet
spec_names %>% filter(species %in% entsoc$sciName)
# only 1 species found...Calosoma calidum. Common name is fiery hunter


# try taxize 


#see how many are even found in  database

## try itis database, seemed to have the most found 
test2 <- sci2comm(sci = spec_names$species, db = "itis")
# see how many actually returned a name

itis_commonName <- compact(test2) # only 8 of 216 returned common names


# try other databases
eol_test <- sci2comm(sci = spec_names$species, db = "eol")
eol_commonName <- compact(eol_test)

length(compact(eol_test)) # 15/ 216 returned, 4 of them found in itis db
match <- subset(names(itis_commonName), names(itis_commonName) %in% names(compact(eol_test)))

subset(itis_commonName, names(itis_commonName) %in% match)
subset(eol_commonName, names(eol_commonName) %in% match)
# one of them is different



