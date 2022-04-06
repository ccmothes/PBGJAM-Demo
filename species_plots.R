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

spec <- "scaphiAngust"
trace.x <- as.numeric(select(filter(abun_all, site == "ABBY_002" & species == "scaphiAngust"), history))
trace.y <- as.numeric(select(filter(abun_all, site == "ABBY_002" & species == "scaphiAngust"), ssp245_2021.204_normal))

#now make barplots
abun_all %>% 
  filter(site_level == "ABBY") %>% 
  distinct(site_level, species, .keep_all = TRUE) %>% 
  mutate(current_color = if_else(species == spec, "red", "blue"),
         test = if_else(species == spec, paste(spec), "All species")) %>% 
  plot_ly(x = ~species, y = ~ssp245_2021.204_normal, type = "bar", 
          color = ~current_color, name = ~test) %>% 
  #add_trace(x = trace.x, y = trace.y, type = "bar", name = trace.x, marker = list(color = "orange")) %>% 
  layout(barmode = "overlay",
         xaxis = list(categoryorder = "total descending", showticklabels = F))


#what about scatter with two vars
abun_all %>% 
  filter(site_level == "ABBY") %>%
  distinct(site_level, species, .keep_all = TRUE) %>% 
  mutate(current_color = if_else(species == spec, "red", "blue"),
         name = if_else(species == spec, paste(spec), "All species")) %>% 
  plot_ly(x = ~history, y = ~ssp245_2021.204_normal,
          color = ~current_color,
          name = ~name) #%>% 
  #add_trace(x = trace.x, y = trace.y, name = "test", marker = list(color = "orange")) %>% 
  # layout(scattermode = "overlay",
  #        xaxis = list(categoryorder = "total descending", showticklabels = F))


