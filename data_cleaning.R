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

