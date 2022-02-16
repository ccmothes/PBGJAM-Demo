# test out NEON maps and plots

library(sf)
library(leaflet)
library(plotly)
library(viridis)
library(dplyr)

bf <- st_read("data/betPosVis/brachiFumans.shp") 

pf <- st_read("data/betPosVis/pterosFemora.shp")

bf_df <- st_drop_geometry(bf) %>% as_tibble() %>% 
  mutate(species = "Brachi_fumans")


pf_df <- st_drop_geometry(pf) %>% as_tibble() %>% 
  mutate(species = "Pteros_femora")

combined <- bind_rows(bf_df, pf_df)

bins <- c(-Inf, )

pal <- colorBin(palette = "RdBu", domain = c(min(bf$s1, na.rm = TRUE), abs(min(bf$s1, na.rm = TRUE))),
                 reverse = FALSE, bins = 13)

bf_jitter <- st_jitter(bf, factor = 0.009) %>% filter(s1 != 0)

#add topographic/ecoregion base layers
#color/size markers by variable
# color by: s1/s2
# size by _temp, _def, gp .....but what about negatives...

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = bf_jitter, color = "black",
                   fillColor = ~pal(s1), weight = 1, stroke = TRUE,
                   radius = 5, fillOpacity = 1,
                   popup = paste("s1:", bf_jitter$s1, "<br>",
                                 "s2:", bf_jitter$s2, "<br>",
                                 "Site:", bf_jitter$site))

# scatter plot (choose variables)
plot_ly(data = bf_df, x = ~s1_temp, y = ~s1_def, color = ~log(s1+0.00001))

# box plot (highlight species, better with multiple species)
plot_ly(data = combined, y = ~ s2, color = ~species, type = "box")

# histogram of site abundance change (highlight site on click)
plot_ly(bf_df, type = "histogram", x = ~log(s1), nbinsx = 40)


#case study with ABBY sites (near Portland)

# habitat tifs
tif_test <- raster("data/ABBY_terrain/ABBY_002.png")


#basic shiny

ui <-
  fluidPage(sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(leafletOutput("map"))
  ))

 
 server <- function(input, output, session) {
   output$map <- renderLeaflet(
     leaflet() %>% 
       addTiles()
   )
 }
 
 
 shinyApp(ui, server)
 
 
 
 # Get NEON data ------------------------------------------------
 library(neonUtilities)
   
 
 # get soil temop over the past year for ABBY site
 abby_soil <- loadByProduct(dpID = "DP1.00041.001",
                            site = "ABBY",
                            startdate = "2022-01",
                            enddate = "2022-02",
                            savepath = "data/")
 

 
# pull 30min data
st_30 <- as_tibble(abby_soil$ST_30_minute) %>% 
  arrange(startDateTime) %>% 
  #get daily average
  group_by(startDateTime) %>% 
  summarise(soilTempMean = mean(soilTempMean, na.rm = TRUE))




# make plotly

plot_ly(st_30) %>%
  add_trace(x = st_30$startDateTime, y = st_30$soilTempMean,
            mode = "lines+markers")
 

 