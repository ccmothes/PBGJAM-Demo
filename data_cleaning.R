# data cleaning for shiny app
library(tidyverse)
library(sf)

# read in one of the first neon datasets

dat1_bf <- st_read("data/betPosVis/brachiFumans.shp") %>% filter(!is.na(s1_temp)) 

dat1_pf <- st_read("data/betPosVis/pterosFemora.shp")
# 649 sites

#check that env vars are the same at all sites
dat1_bf %>% filter(s1_temp != dat1_pf$s1_temp) # all the same
dat1_bf %>% filter(s1_temp == dat1_pf$s1_temp) # returns 628 sites....are there NAs?

dat1_bf %>% filter(is.na(s1_temp)) #yep 21 sites that are NA for all vars. remove these



dat2 <- readRDS("PBGJAM-ShinyDemo/data/data4Caitlinv2.rds")
dat2_sub <- dat2$agonolConjun %>% as_tibble() %>% rename(site = plot.ID)
#628 sites

#check that gap fraction is the same (only overlapping var)
dat2_sub %>% filter(gap.frac.10 != dat1_bf$gp_f_10)


#make site file to join with species files
site_dat <- st_read("data/betPosVis/brachiFumans.shp") %>% filter(!is.na(s1_temp)) %>% 
  st_drop_geometry() %>% 
  #just keep temp/def and coord columns to combine with other species files
  dplyr::select(site, lat, lon, s1_temp, s2_temp, s1_def, s2_def)

#save this to read into shinyapp
saveRDS(site_dat, "PBGJAM-ShinyDemo/data/site_dat.RDS")


#join species data with site data
joined <- left_join(dat2_sub, site_dat, by = "site")

#restructure for scenario plots
scen <- joined %>%
  pivot_longer(cols = contains(c("s1","s2"))) %>%
  mutate(scenario = if_else(str_detect(name, "s1"), "s1", "s2")) %>%
  mutate(
    name = case_when(
      name %in% c("s1", "s2") ~ "Abundance_change",
      name %in% c("s1.fullT", "s2.fullT") ~ "Full_temp_effects",
      name %in% c("s1.mainT", "s2.mainT") ~ "Main_temp_effects",
      name %in% c("s1_temp", "s2_temp") ~ "Temperature_change",
      name %in% c("s1_def", "s2_def") ~ "Deficit_change"
    )
  ) %>%
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(scenario = factor(scenario, levels = c("s1", "s2"))) %>%
  arrange(site) %>% 
  mutate(paired = rep(1:(n() / 2), each = 2))




# ABUNDANCE AT EACH TIME/SCEN ----------------------------------------
abundance <- readRDS("PBGJAM-ShinyDemo/data/data4Caitlinv4.rds")

abundance_sub <- time_data$agonolConjun %>% as_tibble()



# HABITAT/CLIM VARS FOR MODEL --------------------------------------------------

covaritates <- readRDS("PBGJAM-ShinyDemo/data/data4CaitlinV3.rds") %>% as_tibble()


# TRAIT/NAME DATA -----------------------------------------

traits <- read_csv("PBGJAM-ShinyDemo/data/traits.csv")
#filter out beetles

beetles <- traits %>% filter(code6 %in% names(abundance))
# I think there are duplicates...
beetles %>% group_by(code6) %>% filter(n() > 1)
# 8 of them duplicated code names, differing species-level names

# how many species is "UNKN"
beetles %>% filter(str_detect(code6, "UNKN"))
# 4 unique, one of them a dup (differing genus name)

#save file with duplicates and unknowns

dups <- beetles %>% group_by(code6) %>% filter(n() > 1)

unkn <- beetles %>% filter(str_detect(code6, "UNKN"))

bind_rows(dups, unkn) %>% write.csv("PBGJAM-ShinyDemo/data/dups_unkn_species.csv")


# UPDATED NAME DATA ------------------------------------

spec_names <- readRDS("PBGJAM-ShinyDemo/data/scientific_name.rds") %>% as_tibble() %>% 
  filter(code6 %in% names(spec_dat))%>% 
  mutate(tribe = case_when(tribe == "" ~ "Unknown", 
                           tribe == "Carabini " ~ "Carabini",
                           tribe == "Harpalini " ~ "Harpalini",
                           TRUE ~ tribe))
# 19 species don't have tribe name. 3 of these have UNKN species
# 7 total have UNKN for species level, but for these the genus name is a tribe name...

#also, 216 in this list, but only 136 with abundance data


# FUTURE CLIM VARS -------------------------------------
## three time frames/ 2 climate scenarios

fut_clim <- readRDS("PBGJAM-ShinyDemo/data/data4Caitlinv5.rds")



# UPDATE DATA CLEAN ------------------------------------------------------


## site level env vars -------------------

#habitat vars for model
v3 <- readRDS("PBGJAM-ShinyDemo/data/data4CaitlinV3.rds") %>% as_tibble()

## add _history to all var names for use in 3d plot

#future climate vars
# NOTE: there are some extra sites in here that do not have abundance or habitat data,
# for not filter those out
v5 <- readRDS("PBGJAM-ShinyDemo/data/data4Caitlinv5.rds") %>% 
  filter(plot_ID %in% v3$plot.ID) %>% 
  rename(plot.ID = plot_ID)


#spread to join with v3
v5_spread <- v5 %>% 
  pivot_wider(names_from = c(scenario, interval), values_from = c(tmean.JJA, def.JJA))


site_dat <- left_join(v3, v5_spread, by = "plot.ID")

# get coords to join with site info
coords <- st_read("data/betPosVis/brachiFumans.shp") %>% 
  st_drop_geometry() %>% 
  filter(site %in% site_dat$plot.ID) %>% 
  select(plot.ID = site, lat, lon)


site_dat <- left_join(site_dat, coords, by = "plot.ID")

#create new columns for def and temp dif
site_dat <- site_dat %>% 
  mutate(dif_def_ssp245 = def.JJA_ssp245_2081.21 - def.JJA,
         dif_def_ssp585 = def.JJA_ssp585_2081.21 - def.JJA,
         dif_tmean_ssp245 = tmean.JJA_ssp245_2081.21 - tmean.JJA,
         dif_tmean_ssp585 = tmean.JJA_ssp585_2081.21 - tmean.JJA)

#SAVE UPDATED VERSION
saveRDS(site_dat, "PBGJAM-ShinyDemo/data/site_dat_update.RDS")

## species abundance ----------------------------------

# get total abundance change from v2
v2 <- readRDS("PBGJAM-ShinyDemo/data/data4Caitlinv2.rds")


# get abundance over time
v4 <- readRDS("PBGJAM-ShinyDemo/data/data4Caitlinv4.rds")

#add _abundance tag to all column names to join with site level data
newnames <- c("plot.ID", paste0(names(v4[[1]][,-1]), "_abundance"))
v4 <- lapply(v4, setNames, newnames)


## rename v2 columns and combine to create new spec_dat file
newnames2 <- c("plot.ID", "dif_abundance_ssp245", "dif_abundance_ssp585",
               "fullT_ssp245", "fullT_ssp585",
               "mainT_ssp245", "mainT_ssp585")
v2_update <- lapply(v2, dplyr::select, -c(gap.frac.10, tmean.JJA)) %>% 
  lapply(setNames, newnames2)

# check that all the names/order of list match
names(v4) == names(v2_update)

spec_dat <- purrr::map2(v4, v2_update, full_join, by = "plot.ID")

#save as updated file
saveRDS(spec_dat, "PBGJAM-ShinyDemo/data/spec_dat_update.RDS")


## create file format for scenario plot ---------------------

site_dat <- readRDS("PBGJAM-ShinyDemo/data/site_dat_update.RDS")
spec_dat <- readRDS("PBGJAM-ShinyDemo/data/spec_dat_update.RDS")

dat_test <- spec_dat[[1]] %>% 
  as_tibble() %>% 
  left_join(site_dat, by = "plot.ID")


scen <- dat_test %>% 
  #remove difference/change columns
  select(-contains(c("dif", "mainT", "fullT"))) %>% 
  rename(def_history = def.JJA, tmean_history = tmean.JJA) %>% 
  pivot_longer(cols = contains(c("ssp", "history"))) %>% 
  mutate(scenario = case_when(str_detect(name, "ssp245") ~ "ssp245",
                              str_detect(name, "ssp585") ~ "ssp585",
                              str_detect(name, "history") ~ "history"),
         time = case_when(str_detect(name, "history") ~ "history",
                          str_detect(name, "2021.204") ~ "2021.204",
                          str_detect(name, "2061.208") ~ "2061.208",
                          str_detect(name, "2081.21") ~ "2081.21"),
         var = case_when(str_detect(name, "abundance") ~ "abundance",
                         str_detect(name, "tmean") ~ "tmean",
                         str_detect(name, "def") ~ "def")) %>% 
  pivot_wider(names_from = var, values_from = value) %>% 
  group_by(plot.ID, scenario, time) %>% 
  summarise(across(c("abundance", "tmean", "def"), ~sum(.x, na.rm = TRUE)), across()) %>% 
  mutate(time = factor(time, levels = c("history", "2021.204", "2061.208", "2081.21")))


# Update Species lists for Mapper

beetles <- read_csv("PBGJAM-ShinyDemoFull/data/filesAll/mapList_NEON_Beetles.csv") %>% 
  mutate(scientificName = str_replace(Name.Spaces, " ", ".")) %>% 
  dplyr::select(scientificName, nameShort = Name.short)
  
write_csv(beetles, "PBGJAM-ShinyDemoFull/data/filesAll/mapList_NEON_Beetles.csv")

