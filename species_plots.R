#Species comparison plots

#read in file with all species abundance 
abundance <- readRDS("PBGJAM-ShinyDemo/data/data4Caitlinv4.rds")


#create single df that averages values/site and bind rows for all species

abundance_sub <- abundance$agonolConjun


# create site level column
asub <- abundance_sub %>% 
  as_tibble() %>% 
  rename(site = plot.ID) %>% 
  mutate(site_level = case_when(str_detect(site, "_") ~ substr(site, start = 1, stop = 4),
                                                        str_detect(site, "-") ~ word(site, 1, sep = "\\-")))

#summarize all vars
asub %>% 
  group_by(site_level) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))


#now do that for each species and combine
abun_all <- 
map2_df(abundance, names(abundance), ~mutate(.x, species = .y)) %>% 
  as_tibble() %>%
  rename(site = plot.ID) %>% 
  mutate(site_level = case_when(str_detect(site, "_") ~ substr(site, start = 1, stop = 4),
                                str_detect(site, "-") ~ word(site, 1, sep = "\\-"))) %>% 
  group_by(site_level, species) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), site = site) %>% 
  ungroup()


#save to use in app
saveRDS(abun_all, "PBGJAM-ShinyDemo/data/abun_all.RDS")

trace.x <- "scaphiAngust"
trace.x <- as.numeric(select(filter(abun_all, site_level == "ABBY" & species == "scaphiAngust"), history))
trace.y <- as.numeric(select(filter(abun_all, site_level == "ABBY" & species == "scaphiAngust"), ssp245_2021.204_normal))

#now make barplots
abun_all %>% 
  filter(site_level == "ABBY") %>% 
  distinct(site_level, species, .keep_all = TRUE) %>% 
  plot_ly(x = ~species, y = ~ssp245_2021.204_normal, type = "bar", name = "All Species") %>% 
  add_trace(x = trace.x, y = trace.y, type = "bar", name = trace.x, marker = list(color = "orange")) %>% 
  layout(barmode = "overlay",
         xaxis = list(categoryorder = "total descending", showticklabels = F))


#what about scatter with two vars
abun_all %>% 
  filter(site_level == "ABBY") %>% 
  plot_ly(x = ~history, y = ~ssp245_2021.204_normal, type = "scatter", name = 'All species') %>% 
  add_trace(x = trace.x, y = trace.y, type = "scatter", name = trace.x, marker = list(color = "orange")) %>% 
  layout(barmode = "overlay",
         xaxis = list(categoryorder = "total descending", showticklabels = F))


