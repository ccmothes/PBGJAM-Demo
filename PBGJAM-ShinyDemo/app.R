

library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(shinyWidgets)
library(ggplot2)
library(Metrics)
library(colorspace)
library(raster)
library(bslib)
library(dplyr)
library(sf)
library(plotly)
library(tidyr)
library(stringr)


# set up -----------------------------------------------

#read in soil temp csv
#st_30 <- read.csv("data/st_30.csv")

# read in site env data w/ coords
site_dat <- readRDS("data/site_dat_update.RDS")

#read in species files
spec_dat <- readRDS("data/spec_dat_update.RDS")

#read in species names and spec_group info
spec_names <- readRDS("data/scientific_name.rds") %>% as_tibble() %>% 
  filter(code6 %in% names(spec_dat)) %>% 
  mutate(tribe = case_when(tribe == "" ~ "Unknown", 
                           tribe == "Carabini " ~ "Carabini",
                           tribe == "Harpalini " ~ "Harpalini",
                           TRUE ~ tribe))

#get spec_group names for select input
spec_group <- spec_names %>% 
  arrange(tribe, decending = TRUE) %>% 
  pull(tribe) %>% unique()

#read in combined sp abundance file
abun_all <- readRDS("data/abun_all.RDS") %>% 
  mutate(plot.ID = site) %>% 
  left_join(select(spec_names, species = code6, species_full = species), by = "species")

#dataframe to pull time range values from sliderTextInput
sliderVals <- data.frame("time_label" = c("Current", "2021-2040", "2061-2080", "2081-2100"), 
                         "time_value" = c("history", "2021.204", "2061.208", "2081.21"))

#function to read in USGS basemaps
GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

#attribution for usgs maps
att <- paste0("<a href='https://www.usgs.gov/'>",
              "U.S. Geological Survey</a> | ",
              "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
              "Policies</a>")


#pbgjam logo
corner_element = HTML(paste0('<a href=',shQuote("https://pbgjam.env.duke.edu/"), '>', 
                             '<img src=', 'logo.png', '/></a>'))


ui <- 
  navbarPage(id = "nav",
                 tags$a(
                   href="https://pbgjam.env.duke.edu/",
                   tags$img(src="pbgjam_logo.png",
                            title="PBGJAM Website"),
                            width="20%",
                            height="20%"
                 ),
                 theme = bslib::bs_theme(
                   bootswatch = "sandstone",
                   #bg = "#FFFFFF",
                   #fg = "#000",
                   #primary = "#082466",
                   secondary = "#fafaf5"
                   #success = "#f28e35",
                   #base_font = font_google("Cairo")
                 ) %>% 
               bslib::bs_add_rules(sass::sass_file("www/style.scss")),
             
             # UI --------------------------------------------------------------------
             tabPanel("",
                      fluidPage(
                        fluidRow(
                          column(6,
                                 fluidRow(
                                   column(4,
                                          pickerInput("spec_1", 
                                                      label = HTML("<b>Choose Species</b>"),
                                                      choices = list(
                                                        "Amblycheilini" = list(pull(select(filter(spec_names, tribe == spec_group[1]), species))),
                                                        "Bembidiini" = list(pull(select(filter(spec_names, tribe == spec_group[2]), species))),
                                                        "Brachinini" = pull(select(filter(spec_names, tribe == spec_group[3]), species)),
                                                        "Broscini" = list(pull(select(filter(spec_names, tribe == spec_group[4]), species))),
                                                        "Carabini" = pull(select(filter(spec_names, tribe == spec_group[5]), species)),
                                                        "Chlaeniini" = pull(select(filter(spec_names, tribe == spec_group[6]), species)),
                                                        "Cicindelini" = pull(select(filter(spec_names, tribe == spec_group[7]), species)),
                                                        "Cychrini" = pull(select(filter(spec_names, tribe == spec_group[8]), species)),
                                                        "Cyclosomini" = list(pull(select(filter(spec_names, tribe == spec_group[9]), species))),
                                                        "Dyschiriini" = list(pull(select(filter(spec_names, tribe == spec_group[10]), species))),
                                                        "Galeritini" = list(pull(select(filter(spec_names, tribe == spec_group[11]), species))),
                                                        "Harpalini" = pull(select(filter(spec_names, tribe == spec_group[12]), species)),
                                                        "Lebiini" = pull(select(filter(spec_names, tribe == spec_group[13]), species)),
                                                        "Licinini" = pull(select(filter(spec_names, tribe == spec_group[14]), species)),
                                                        "Megacephalini" = list(pull(select(filter(spec_names, tribe == spec_group[15]), species))),
                                                        "Pasimachini" = pull(select(filter(spec_names, tribe == spec_group[16]), species)),
                                                        "Platynini" = pull(select(filter(spec_names, tribe == spec_group[17]), species)),
                                                        "Pterostichini" = pull(select(filter(spec_names, tribe == spec_group[18]), species)),
                                                        "Scaritini" = pull(select(filter(spec_names, tribe == spec_group[19]), species)),
                                                        "Sphodrini" = pull(select(filter(spec_names, tribe == spec_group[20]), species)),
                                                        "Unknown" = pull(select(filter(spec_names, tribe == spec_group[21]), species)),
                                                        "Zabrini" = pull(select(filter(spec_names, tribe == spec_group[22]), species))

                                                      ), selected = "Agonoleptus conjunctus")),
                                   column(4,
                                          pickerInput("map_var", label = HTML("<b>Color Map By:</b>"),
                                                       choices = list(
                                                         "Model Covariates (Climate + Habitat)" = c("Gap Fraction" = "gap.frac.10",
                                                                       "Surface Roughness" = "s.roughness",
                                                                       "Canopy Nitrogen" = "Nitrogen.mean",
                                                                       "Cation Exchange Capacity" = "cec30",
                                                                       "Understory Density" = "nrd.15cm.2m",
                                                                       "Coarse Woody Debris" = "CwdVolume.sqrt",
                                                                       "Summer Moisture Deficit" = "def.JJA",
                                                                       "Summer Temperature" = "tmean.JJA"
                                                                       ),
                                                         "Total Predicted Change (Current - 2100)" = c("Abundance" = "abundance",
                                                                       "Temperature" = "tmean",
                                                                       "Moisture Deficit" = "def"),
                                                         "Temperature Effects on Abundance Change" = c("Main Effects" = "mainT",
                                                                                                       "FUll effects" = "fullT")
                                                       ), selected = "abundance")),
                                   column(4, prettyRadioButtons("scen_map", label = HTML("<b>Choose scenario for predicted values:</b>"),
                                                                choices = c("SSP 245" = "ssp245", 
                                                                            "SSP 585" = "ssp585"),
                                                                inline = TRUE, fill = TRUE)
                                          )
                                 ),
                                 
                                 leaflet::leafletOutput("NEONmap2", height = 500),
                                 h5(em("Click a site on the map to view its habitat conditions. Zoom in to filter sites shown in plots.")),
                                 h5(strong(textOutput("siteText1"))),
                                 fluidRow(
                                 column(4,
                                        p(strong("Terrain")),
                                        plotOutput("terrainPNG", height = 200)
                                        ),
                                 column(2),
                                 column(4,
                                        p(strong("Gap Fraction")),
                                        plotOutput("gapPNG", height = 200)
                                        )),
                                 hr()
                          ),
                            column(6,
                                   tabsetPanel(
                                     tabPanel(
                                       "Compare Across Sites",
                                       h3(strong("Explore all variables in 3D space")),
                                       fluidRow(
                                        column(6, prettyRadioButtons("scen_3d", label = HTML("<b>Choose Climate Scenario:</b>"),
                                                          choices = c("SSP 245" = "ssp245", 
                                                                      "SSP 585" = "ssp585"),
                                                          inline = TRUE, fill = TRUE)),
                                      ),
                                       fluidRow(
                                         column(4,
                                                pickerInput("choose_x", "Choose X:",
                                                            choices = list(
                                                              "Model Covariates (Climate + Habitat)" = c("Gap Fraction" = "gap.frac.10",
                                                                                                         "Surface Roughness" = "s.roughness",
                                                                                                         "Canopy Nitrogen" = "Nitrogen.mean",
                                                                                                         "Cation Exchange Capacity" = "cec30",
                                                                                                         "Understory Density" = "nrd.15cm.2m",
                                                                                                         "Coarse Woody Debris" = "CwdVolume.sqrt",
                                                                                                         "Summer Moisture Deficit" = "def.JJA",
                                                                                                         "Summer Temperature" = "tmean.JJA"
                                                              ),
                                                              "Total Predicted Change (Current - 2100)" = c("Abundance" = "abundance",
                                                                                                            "Temperature" = "tmean",
                                                                                                            "Moisture Deficit" = "def"),
                                                              "Temperature Effects on Abundance Change" = c("Main Effects" = "mainT",
                                                                                                            "FUll effects" = "fullT")
                                                            ), selected = "tmean")),
                                         column(4,
                                                
                                                pickerInput("choose_y", "Choose Y:",
                                                            choices = list(
                                                              "Model Covariates (Climate + Habitat)" = c("Gap Fraction" = "gap.frac.10",
                                                                                                         "Surface Roughness" = "s.roughness",
                                                                                                         "Canopy Nitrogen" = "Nitrogen.mean",
                                                                                                         "Cation Exchange Capacity" = "cec30",
                                                                                                         "Understory Density" = "nrd.15cm.2m",
                                                                                                         "Coarse Woody Debris" = "CwdVolume.sqrt",
                                                                                                         "Summer Moisture Deficit" = "def.JJA",
                                                                                                         "Summer Temperature" = "tmean.JJA"
                                                              ),
                                                              "Total Predicted Change (Current - 2100)" = c("Abundance" = "abundance",
                                                                                                            "Temperature" = "tmean",
                                                                                                            "Moisture Deficit" = "def"),
                                                              "Temperature Effects on Abundance Change" = c("Main Effects" = "mainT",
                                                                                                            "FUll effects" = "fullT")
                                                            ),
                                                            selected = "abundance")),
                                         column(4,
                                                pickerInput("choose_color", "Color By: ",
                                                            choices = list(
                                                              "Model Covariates (Climate + Habitat)" = c("Gap Fraction" = "gap.frac.10",
                                                                                                         "Surface Roughness" = "s.roughness",
                                                                                                         "Canopy Nitrogen" = "Nitrogen.mean",
                                                                                                         "Cation Exchange Capacity" = "cec30",
                                                                                                         "Understory Density" = "nrd.15cm.2m",
                                                                                                         "Coarse Woody Debris" = "CwdVolume.sqrt",
                                                                                                         "Summer Moisture Deficit" = "def.JJA",
                                                                                                         "Summer Temperature" = "tmean.JJA"
                                                              ),
                                                              "Total Predicted Change (Current - 2100)" = c("Abundance" = "abundance",
                                                                                                            "Temperature" = "tmean",
                                                                                                            "Moisture Deficit" = "def"),
                                                              "Temperature Effects on Abundance Change" = c("Main Effects" = "mainT",
                                                                                                            "FUll effects" = "fullT")
                                                            ),
                                                            selected = "gap.frac.10"))),
                                       plotlyOutput("choose_scatter"),
                                       hr(),
                                       h3(strong("Explore Change Over Time")),
                                        fluidRow(
                                          column(6, prettyRadioButtons("scen_time", label = HTML("<b>Choose Climate Scenario:</b>"),
                                                                       choices = c("SSP 245" = "ssp245", 
                                                                                   "SSP 585" = "ssp585"),
                                                                       inline = TRUE, fill = TRUE)),

                                      ),
                                       #h5(strong("Animation")),
                                       fluidRow(
                                         column(4,
                                                pickerInput("choose_x2", "Choose X:",
                                                            choices = c("Abundance" = "abundance",
                                                                        "Temperature" = "tmean",
                                                                        "Moisture Deficit" = "def"),
                                                            selected = "tmean")),
                                         column(4,
                                                pickerInput("choose_y2", "Choose Y:",
                                                            choices = c("Abundance" = "abundance",
                                                                        "Temperature" = "tmean",
                                                                        "Moisture Deficit" = "def"),
                                                            selected = "abundance")),
                                         column(4,
                                                pickerInput("choose_size", "Size Points By: ",
                                                            choices = c("Gap Fraction" = "gap.frac.10",
                                                                        "Surface Roughness" = "s.roughness",
                                                                        "Canopy Nitrogen" = "Nitrogen.mean",
                                                                        "Cation Exchange Capacity" = "cec30",
                                                                        "Understory Density" = "nrd.15cm.2m",
                                                                        "Coarse Woody Debris" = "CwdVolume.sqrt"
                                                            ),
                                                            selected = "gap.frac.10"))
                                       ),
                                       plotlyOutput("scenario_anim"),
                                       hr(),
                                       #h5(strong("Vector Plots")),
                                      fluidRow(
                                        column(6, prettyRadioButtons("vect_scen", label = HTML("<b>Choose Climate Scenario:</b>"),
                                                                     choices = c("SSP 245" = "ssp245", 
                                                                                 "SSP 585" = "ssp585"),
                                                                     inline = TRUE, fill = TRUE)),
                                        column(6,
                                               sliderTextInput("vect_time", label = HTML("<b>Choose Time Range:</b>"),
                                                               choices = c("Current",
                                                                           "2021-2040",
                                                                           "2061-2080",
                                                                           "2081-2100"),
                                                               grid = TRUE,
                                                               selected = c("Current", "2081-2100"))
                                        ),
                                        
                                      ),
                                      
                                       fluidRow(
                                         column(4,
                                                pickerInput("choose_x3", "Choose X:",
                                                            choices = c("Abundance" = "abundance",
                                                                                  "Temperature" = "tmean",
                                                                                  "Moisture Deficit" = "def"),
                                                            selected = "tmean")),
                                         column(4,
                                                pickerInput("choose_y3", "Choose Y:",
                                                            choices =  c("Abundance" = "abundance",
                                                                                  "Temperature" = "tmean",
                                                                                  "Moisture Deficit" = "def"),
                                                            selected = "abundance")),
                                       ),
                                       plotlyOutput("vector_plot")
                                       
                                     ),
                                     tabPanel(
                                       "Compare Across Species",
                                       h5(strong("Site-level Species Comparisons"),em(" (click a site on the map to view plots)")),
                                       HTML("<p style = color:#538b01> Note: Abundance units are counts per trap night"),
                                       pickerInput("choose_y_spbar", "Choose Y:",
                                                   choices = list(
                                                     "Current" = "history",
                                                     "SSP245" = c("2021-2040" = "ssp245_2021.204_normal",
                                                                  "2061-2080" = "ssp245_2061.208_normal",
                                                                  "2081-2100" = "ssp245_2081.21_normal"),
                                                     "SSP585" = c("2021-2040" = "ssp585_2021.204_normal",
                                                                  "2061-2080" = "ssp585_2061.208_normal",
                                                                  "2081-2100" = "ssp585_2081.21_normal")
                                                   )),
                                       plotlyOutput("speciesBar"),
                                       hr(),
                                       fluidRow(
                                         column(4,
                                                pickerInput("choose_x_spscat", "Choose X:",
                                                            choices = list(
                                                              "Current" = "history",
                                                              "SSP245" = c("2021-2040" = "ssp245_2021.204_normal",
                                                                           "2061-2080" = "ssp245_2061.208_normal",
                                                                           "2081-2100" = "ssp245_2081.21_normal"),
                                                              "SSP585" = c("2021-2040" = "ssp585_2021.204_normal",
                                                                           "2061-2080" = "ssp585_2061.208_normal",
                                                                           "2081-2100" = "ssp585_2081.21_normal")
                                                            ))),
                                         column(4,
                                                pickerInput("choose_y_spscat", "Choose Y:",
                                                            choices = list(
                                                              "Current" = "history",
                                                              "SSP245" = c("2021-2040" = "ssp245_2021.204_normal",
                                                                           "2061-2080" = "ssp245_2061.208_normal",
                                                                           "2081-2100" = "ssp245_2081.21_normal"),
                                                              "SSP585" = c("2021-2040" = "ssp585_2021.204_normal",
                                                                           "2061-2080" = "ssp585_2061.208_normal",
                                                                           "2081-2100" = "ssp585_2081.21_normal")
                                                            ),
                                                            selected = "ssp245_2021.204_normal"))
                                       ),
                                       plotlyOutput("speciesScatter")

                                     )
                                   )
                                   
                          )

                        )
                      )
             )
             
             # DEACTIVATE THIS TAB FOR NOW ##
             # tabPanel("NEON Sites",
             #          fluidPage(
             #            fluidRow(
             #              column(7,
             #                     
             #                       
             #                       h3(strong("Change in Species Abundance")),
             #                       br(),
             #                       
             #                       fluidRow(  
             #                         column(4,
             #                                selectInput("neon_spec", label = "Choose Species",
             #                                            choices = c("Brachi fumans" = "brachiFumans",
             #                                                        "Pteros femora" = "pterosFemora"))),
             #                         column(4,
             #                                radioButtons("neon_scenario1", label = "Choose Scenario",
             #                                             choices = c("SSP 245" = "s1",
             #                                                         "SSP 585" = "s2")))),
             #                       
             #                       
             #                       leaflet::leafletOutput("NEONmap1", height = 500)
             #                       
             #                       
             #                     ,
             #                     br(),
             #                     br(),
             #                     br(),
             #                     br(),
             #                     br(),
             #                     br(),
             #                     hr(),
             #                     
             #                     
             #              ),
             #              column(5,
             #                     br(),
             #                     br(),
             #                     h5(strong("Click a site on the map to view its habitat conditions")),
             #                     p(em("Note: Only have data for ABBY sites currently")),
             #                     h5(strong(textOutput("siteText"))),
             #                     selectInput("habitatVar", "Choose Habitat Variable",
             #                                 choices = c(
             #                                   "Terrain" = "terrain",
             #                                   "Gap Fraction" = "gf"
             #                                 )),
             #                     plotOutput("habitatPNG", height = 200),
             #                     p(strong("NEON Time Series")),
             #                     selectInput("neon_timeVar", "Choose variable",
             #                                 choices = "Soil Moisture"),
             #                     #plotly time series output here
             #                     plotlyOutput("neon_timeseries", height = 250),
             #                     br(),
             #                     hr(),
             #                     
             #                     
             #              )
             #            )
             #          )
             # )         

)
    


server <- function(input, output, session) {
# 
#     
# NEON SITES ----------------------------------------------------------------------------
    
# DEACTIVATE THIS TAB FOR NOW #

    #read in species data
    # neon_spec_data <- reactive({
    #   st_read(paste0(pathin, "/betPosVis/", input$neon_spec, ".shp")) %>% 
    #   
    #     #create constant column name for scenario
    #     rename(scenario = input$neon_scenario1) %>% 
    #     #filter out zeros, depending on scenario
    #     filter(scenario != 0)
    # })
    
    # #make jittered points for zoomed out view
    # neon_spec_jitter <- reactive({
    #   st_read(paste0(pathin, "/betPosVis/", input$neon_spec, ".shp")) %>% 
    #     #jitter points
    #     st_jitter(factor = 0.009) %>% 
    #     #create constant column name for scenario
    #     rename(scenario = input$neon_scenario1) %>% 
    #     #filter out zeros, depending on scenario
    #     filter(scenario != 0)
    #   
    # })
    
    
    # first species map
    
    
    #palette based on scenario
    # pal1 <- reactive({
    #   colorBin(palette = "RdBu", domain = c(min(neon_spec_data()$scenario, na.rm = TRUE),
    #                                         abs(min(neon_spec_data()$scenario, na.rm = TRUE))),
    #            reverse = TRUE)
    #   
    # })
    # 
    # grp <- c("USGS Topo", "USGS Imagery",
    #          "USGS Shaded Relief")
    # 
    # output$NEONmap1 <- renderLeaflet({
    #   leaflet() %>% 
    #    
    #     addWMSTiles(GetURL("USGSTopo"),
    #                 group = grp[1], attribution = att, layers = "0") %>%
    #     addWMSTiles(GetURL("USGSImageryOnly"),
    #                 group = grp[2], attribution = att, layers = "0") %>%
    #     addWMSTiles('https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png',
    #                 attribution = '&copy; <a href="https://stadiamaps.com/">Stadia Maps</a>, &copy; <a href="https://openmaptiles.org/">OpenMapTiles</a> &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
    #                 group = "Dark Theme", layers = "0") %>%
    #     addWMSTiles(GetURL("USGSShadedReliefOnly"),
    #                 group = grp[3], attribution = att, layers = "0") %>%
    #     
    #     
    #     addCircleMarkers(
    #       data = neon_spec_data(),
    #       layerId = ~ site,
    #       group = "original",
    #       color = "black",
    #       fillColor = ~ pal1()(scenario),
    #       weight = 1,
    #       stroke = TRUE,
    #       radius = 7,
    #       fillOpacity = 1,
    #       popup = paste(
    #         "Site:",
    #         neon_spec_data()$site,
    #         "<br>",
    #         paste0(input$neon_scenario1, ":"),
    #         neon_spec_data()$scenario
    #       )
    #     ) %>%
    #     addCircleMarkers(data = st_jitter(neon_spec_data(), factor = 0.009),
    #                      #layerId = ~site,
    #                      group = "jitter", color = "black",
    #                      fillColor = ~pal1()(scenario), 
    #                      weight = 1, stroke = TRUE,
    #                      radius = 6, fillOpacity = 1,
    #                      popup = paste("Site:", neon_spec_data()$site, "<br>",
    #                                    paste0(input$neon_scenario1, ":"), neon_spec_data()$scenario)
    #     ) %>%
    #       addLayersControl(baseGroups = c("Dark Theme", "USGS Topo", 
    #                                       "USGS Imagery", "USGS Shaded Relief"),
    #                        position = "bottomleft",
    #                        options = layersControlOptions(collapsed = FALSE)
    #                        ) %>% 
    #     groupOptions("jitter", zoomLevels = 1:6) %>% 
    #     groupOptions("original", zoomLevels = 7:20)
    # })
    

    
    #Add selected site name as header
    # output$siteText <- renderText(
    #   if(is.null(input$NEONmap1_marker_click)){
    #     return(NULL)
    #   } else {
    #     input$NEONmap1_marker_click$id
    #   })
    
    
    #paste habitat variable png
    # observeEvent(input$NEONmap1_marker_click, {
    #   
    #   output$habitatPNG <- renderImage({
    #     if (input$habitatVar == "terrain") {
    #       list(
    #         src = paste0(
    #           "data/ABBY_terrain/",
    #           input$NEONmap1_marker_click$id,
    #           ".png"
    #         ),
    #         width = 200,
    #         height = 200
    #       )
    #     }
    #     
    #     else {
    #       list(
    #         src = paste0(
    #           "data/ABBY_gf/",
    #           input$NEONmap1_marker_click$id,
    #           "_bet_norm_chm.png"
    #         ),
    #         width = 200,
    #         height = 200
    #       )
    #     }
    #     
    #     
    #     
    #   }, deleteFile = FALSE)
    #   
    # })
    
    
    #NEON environment plotly
    
    
    # observeEvent(input$NEONmap1_marker_click, {
    #  
    #    output$neon_timeseries <- renderPlotly({
    #     plot_ly(st_30) %>%
    #       add_trace(x = st_30$startDateTime, y = st_30$soilTempMean,type = 'scatter',
    #                 mode = "lines", connectgaps = TRUE) %>% 
    #       plotly::layout(yaxis = list(title = "Mean Soil Temperature"),
    #                      xaxis = list(type = "date", tickformate = "%d %B <br> %Y"))
    #     
    #     
    #     
    #   })
    #   
    #   
    #   
    # })
    
    
    # SPEC-ENV INT --------------------------------------------------------------------------------

    
    ## data prep ------------------
   
    sei_plot <- reactive({
      spec_dat[[spec_names$code6[spec_names$species == input$spec_1]]] %>% 
        as_tibble() %>% 
        #rename(site = plot.ID) %>% 
        left_join(site_dat, by = "plot.ID")
      
    })
    
    #reactive map_var to pull column name
    map_var_name <- reactive({
      if (input$map_var %in% c("abundance", "tmean", "def")) {
        x <-
          sei_plot() %>% select(contains(input$scen_map) &
                                  contains("dif") & contains(input$map_var)) %>% names()
        
      } else if (input$map_var %in% c("mainT", "fullT")) {
        x <-
          sei_plot() %>% select(contains(input$scen_map) &
                                  contains(input$map_var)) %>% names()
      } else {
        x <- input$map_var
      }
      
      return(x)
      
    })
    
    sei_map <- reactive({
      sei_plot() %>% 
        rename(variable = map_var_name()) %>% 
        st_as_sf(coords = c("lon", "lat"), crs = 4326)
      
      
    })
    
    sei_map_jitter <- reactive({
      sei_plot() %>% 
        rename(variable = map_var_name()) %>% 
        mutate(site_level = case_when(str_detect(plot.ID, "_") ~ substr(plot.ID, start = 1, stop = 4),
                                      str_detect(plot.ID, "-") ~ word(plot.ID, 1, sep = "\\-")),
               ID = 1:nrow(.)) %>% 
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
        st_jitter(factor = 0.009) 
    })
    
    #restructure data for scenario plots
    sei_scen <- reactive({
      
      sei_plot()  %>% 
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
        mutate(time = factor(time, levels = c("history", "2021.204", "2061.208", "2081.21")),
               time_label = case_when(time == "history" ~ "Current",
                                      time == "2021.204" ~ "2021-2040",
                                      time == "2061.208" ~ "2061-2080",
                                      time == "2081.21" ~ "2081-2100"),
               time_label = factor(time_label, c("Current", "2021-2040", "2061-2080", "2081-2100"))) #%>%
        #arrange(plot.ID) %>% 
        #mutate(paired = rep(1:(n() / 2), each = 2))
      
      
    })
    

    
    #bounds object
    bounds <- reactive({
      input$NEONmap2_bounds
    })
    
    #filter plot data to map bounds
    sei_plot_bounds <- reactive({
      if(is.null(input$NEONmap2_bounds)){
        sei_plot()
      } else {
      sei_plot() %>% 
        dplyr::filter(lat > bounds()$south &
                        lat < bounds()$north &
                        lon < bounds()$east &
                        lon > bounds()$west)
      }
      
    })
    
    sei_scen_bounds <- reactive({
      if(is.null(input$NEONmap2_bounds)){
        sei_scen()
      } else {
      sei_scen() %>% 
        dplyr::filter(lat > bounds()$south &
                        lat < bounds()$north &
                        lon < bounds()$east &
                        lon > bounds()$west)
      }
      
      
    })
    
    # create choice name vectors to get names for legends/axes
    choiceVal1 <- c("Gap Fraction" = "gap.frac.10",
                    "Surface Roughness" = "s.roughness",
                    "Canopy Nitrogen" = "Nitrogen.mean",
                    "Cation Exchange Capacity" = "cec30",
                    "Understory Density" = "nrd.15cm.2m",
                    "Coarse Woody Debris" = "CwdVolume.sqrt",
                    "Summer Moisture Deficit" = "def.JJA",
                    "Summer Temperature" = "tmean.JJA",
                    "Abundance Change" = "abundance",
                    "Temperature Change" = "tmean",
                    "Moisture Deficit Change" = "def",
                    "Main Effects on Change in Abundance" = "mainT",
                    "Full effects on Change in Abundance" = "fullT")
    
    
    choiceVal2 <- c("Abundance" = "abundance",
                    "Temperature" = "tmean",
                    "Moisture Deficit" = "def")
    
    
    choiceSpec <- c("Current Abundance" = "history",
                    "2021-2040 SSP 245 Abundance" = "ssp245_2021.204_normal",
                    "2061-2080 SSP 245 Abundance" = "ssp245_2061.208_normal",
                    "2081-2100 SSP 245 Abundance" = "ssp245_2081.21_normal",
                    "2021-2040 SSP 585 Abundance" = "ssp585_2021.204_normal",
                    "2061-2080 SSP 585 Abundance" = "ssp585_2061.208_normal",
                    "2081-2100 SSP 585 Abundance" = "ssp585_2081.21_normal")
                    
    

    
    ## Column 1 -----------------------------------------
    
    
    # get species-specific value range for color palette
    range <- reactive({
      max(max(sei_map()$variable, na.rm = TRUE), abs(min(sei_map()$variable, na.rm = TRUE)))
      
    })
    
    pal2 <- reactive({
      
      if(input$map_var == "gap.frac.10"){
        colorNumeric(palette = "Greens", domain = sei_map()$variable, reverse = TRUE)
      } 
      
      else if(input$map_var == "abundance"){
        colorBin(palette = "RdBu", domain = c(-range(),
                                              range()),
                 reverse = TRUE)
      } else if(input$map_var == "def"){
        colorBin(palette = "RdBu", domain = c(-max(sei_map()$variable, na.rm = TRUE),
                                              max(sei_map()$variable, na.rm = TRUE)),
                 reverse = TRUE)
        
      } else {
      
      colorNumeric(palette = "Reds", domain = sei_map()$variable)
      }
      
    })
    
    grp <- c("USGS Topo", "USGS Imagery",
             "USGS Shaded Relief")

    
    output$NEONmap2 <- renderLeaflet({
      leaflet() %>% 
        addWMSTiles(GetURL("USGSTopo"),
                    group = grp[1], attribution = att, layers = "0") %>% 
        addWMSTiles(GetURL("USGSImageryOnly"),
                    group = grp[2], attribution = att, layers = "0") %>%
        addWMSTiles('https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png', 
                    attribution = '&copy; <a href="https://stadiamaps.com/">Stadia Maps</a>, &copy; <a href="https://openmaptiles.org/">OpenMapTiles</a> &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
                    group = "Dark Theme", layers = "0") %>% 
        addWMSTiles(GetURL("USGSShadedReliefOnly"),
                    group = grp[3], attribution = att, layers = "0") %>% 
        
        
        addCircleMarkers(data = sei_map(), 
                         #lng = ~lon,
                         #lat = ~lat,
                         layerId = ~plot.ID,
                         group = "original",
                         color = "black",
                         fillColor = ~pal2()(variable), weight = 1, stroke = TRUE,
                         radius = 7, fillOpacity = 1,
                         popup = paste("Site:", sei_map()$plot.ID, "<br>",
                                       paste0(names(choiceVal1)[choiceVal1 == input$map_var], ":"), sei_map()$variable)
        ) %>% 
        addCircleMarkers(data = sei_map_jitter(), layerId = ~ID,
                         group = "jitter",
                         color = "black",
                         fillColor = ~pal2()(variable), weight = 1, stroke = TRUE,
                         radius = 6, fillOpacity = 1,
                         popup = paste("Site:", sei_map_jitter()$plot.ID, "<br>",
                                       paste0(names(choiceVal1)[choiceVal1 == input$map_var], ":"), sei_map_jitter()$variable)
        ) %>% 
        addLegend("bottomright", data = sei_map(), values = ~variable, pal = pal2(),
                  title = names(choiceVal1)[choiceVal1 == input$map_var]) %>% 
        addLayersControl(baseGroups = c("Dark Theme", "USGS Topo", 
                                          "USGS Imagery", "USGS Shaded Relief"),
                           position = "bottomleft",
                         options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        groupOptions("jitter", zoomLevels = 1:6) %>% 
        groupOptions("original", zoomLevels = 7:20)
      
    })
    
    
    #Add selected site name as header
    output$siteText1 <- renderText(
      if(is.null(input$NEONmap2_marker_click)){
        return(NULL)
      } else {
        #input$NEONmap2_marker_click$id
        plot_click()[1] #need to do this because sei_map_jitter is spatial object
      })
    
    
    # Add site habitat PNGs and species comparison maps
    
    ## get site name from mapclick
    site_click <- reactive({
      if(is.numeric(input$NEONmap2_marker_click$id)){
        sei_map_jitter() %>% 
          filter(ID == input$NEONmap2_marker_click$id) %>% 
          select(site_level) %>% 
          as.character()
      
        } else{
        abun_all %>% 
          filter(species_full == input$spec_1) %>% 
          filter(plot.ID == input$NEONmap2_marker_click$id) %>% 
          select(site_level) %>% 
          as.character()
        
      }
    })
    
    ## get plot (higher order) name from mapclick
    plot_click <- reactive({
      if(is.numeric(input$NEONmap2_marker_click$id)){
        sei_map_jitter() %>% 
          filter(ID == input$NEONmap2_marker_click$id) %>% 
          select(plot.ID) %>% 
          as.character()
        
      } else{
        abun_all %>% 
          filter(species_full == input$spec_1) %>% 
          filter(plot.ID == input$NEONmap2_marker_click$id) %>% 
          select(plot.ID) %>% 
          as.character()
        
      }
    })
    
    
    #get reactive x and y for highlighted species
    trace.x1 <- reactive({
      input$spec_1
    })
    
    trace.y1 <- reactive({
      abun_all %>% 
        filter(plot.ID == plot_click()[1] & species == trace.x1()) %>%
        select(input$choose_y_spbar) %>% 
        as.numeric()
    })
    
    
    #reactive dataframe for species plots
    sp_plot_dat <- reactive({
      
      abun_all %>% 
        mutate(#current_color = if_else(species == trace.x1(), "red", "lightblue"),
               name = if_else(species_full == trace.x1(), paste(trace.x1()), "All species"),
               #size = if_else(species == trace.x1(), 12, 7)
               ) %>% 
        filter(site_level == site_click()) %>% 
        distinct(site_level, species, .keep_all = TRUE)
      
    })
    
    #add all site level info upon click
    observeEvent(input$NEONmap2_marker_click, {
      
      output$terrainPNG <- renderImage({
          list(
            src = paste0(
              "data/habitat_visualization/terrainVis/",
              substr(plot_click()[1], start = 1, stop = 4),
              "/", plot_click()[1],
              ".png"
            ),
            width = 200,
            height = 200
          )
        }, deleteFile = FALSE)
      
      output$gapPNG <- renderImage({
        list(
          src = paste0(
            "data/habitat_visualization/gapVis/",
            substr(plot_click()[1], start = 1, stop = 4),
            "/", plot_click()[1],
            "_bet_norm_chm.png"
          ),
          width = 200,
          height = 200
        )
      }, deleteFile = FALSE)
      
      output$speciesBar <- renderPlotly({
        
        sp_plot_dat() %>% 
          plot_ly(x = ~species_full, y = ~get(input$choose_y_spbar), type = "bar", 
                  color = ~species_full == trace.x1(), colors = c("lightblue", "red"), name = ~name,
                  hovertemplate =  paste("%{x},%{y}<br>","<extra></extra>")
          ) %>% 
          layout(barmode = "overlay",
                 xaxis = list(categoryorder = "total descending", showticklabels = F, title = "Species"),
                 yaxis = list(title = names(choiceSpec)[choiceSpec == input$choose_y_spbar]),
                 legend = list(orientation = "h", x = 0.7, y = 1)) #%>%
        # add_trace(x = trace.x1(), y = trace.y1(), type = "bar", name = trace.x1(), marker = list(color = "orange")) 
        
      })
      
      
      
      output$speciesScatter <- renderPlotly({
        
        sp_plot_dat() %>% 
          plot_ly(x = ~get(input$choose_x_spscat), y = ~get(input$choose_y_spscat), type = "scatter", mode = "markers",
                  color = ~species_full == trace.x1(), colors = c("lightblue", "red"),
                  size = ifelse(.$species_full == trace.x1(), 12, 7),
                  line = list(color = NA, width = 0), name = ~name,
                  hovertemplate =  paste("%{x},%{y}<br>","Species:", .$species_full, "<extra></extra>")) %>% 
           layout(
                  xaxis = list(title = names(choiceSpec)[choiceSpec == input$choose_x_spscat]),
                  yaxis = list(title = names(choiceSpec)[choiceSpec == input$choose_y_spscat]),
                  legend = list(orientation = "h", x = 0.7, y = 1)) #%>%
        # add_trace(x = trace.x1(), y = trace.y1(), type = "bar", name = trace.x1(), marker = list(color = "orange")) 
        
      })
      
        
    })
    
    
   
    
    
    
    
    ## Column 2 --------------------------------
    
    # get reactive column names depending on var and scenario selected
    x_3d <- reactive({
      if (input$choose_x %in% c("abundance", "tmean", "def")) {
        x <-
          sei_plot() %>% select(contains(input$scen_3d) &
                                  contains("dif") & contains(input$choose_x)) %>% names()
        
      } else if (input$choose_x %in% c("mainT", "fullT")) {
        x <-
          sei_plot() %>% select(contains(input$scen_3d) &
                                  contains(input$choose_x)) %>% names()
      } else {
        x <- input$choose_x
      }
      
      return(x)
      
    })
    
    y_3d <- reactive({
      if (input$choose_y %in% c("abundance", "tmean", "def")) {
        x <-
          sei_plot() %>% select(contains(input$scen_3d) &
                                  contains("dif") & contains(input$choose_y)) %>% names()
        
      } else if (input$choose_y %in% c("mainT", "fullT")) {
        x <-
          sei_plot() %>% select(contains(input$scen_3d) &
                                  contains(input$choose_y)) %>% names()
      } else {
        x <- input$choose_y
      }
      
      return(x)
      
    })
    
    z_3d <- reactive({
      if (input$choose_color %in% c("abundance", "tmean", "def")) {
        x <-
          sei_plot() %>% select(contains(input$scen_3d) &
                                  contains("dif") & contains(input$choose_color)) %>% names()
        
      } else if (input$choose_color %in% c("mainT", "fullT")) {
        x <-
          sei_plot() %>% select(contains(input$scen_3d) &
                                  contains(input$choose_color)) %>% names()
      } else {
        x <- input$choose_color
      }
      
      return(x)
      
    })
    
    
    output$choose_scatter <- renderPlotly({
      
      sei_plot_bounds() %>% 
        plot_ly(x = as.formula(paste0("~", x_3d())), y = as.formula(paste0("~", y_3d())), 
                color = as.formula(paste0("~", z_3d())),
                size = 4,
                #marker = list(line = list(width = 0)),
                hovertemplate =  paste("%{x},%{y}<br>","Site:", .$plot.ID, "<extra></extra>"),
                type = "scatter", mode = "markers") %>%
        plotly::layout(yaxis = list(title = names(choiceVal1)[choiceVal1 == input$choose_y], 
                                    range = c(min(sei_plot()[,y_3d()]), 
                                              max(sei_plot()[,y_3d()]))),
                       xaxis = list(title = names(choiceVal1)[choiceVal1 == input$choose_x], 
                                    range = c(min(sei_plot()[,x_3d()]), 
                                              max(sei_plot()[,x_3d()])))
        ) %>%
        colorbar(title=names(choiceVal1)[choiceVal1 == input$choose_color], limits = c(min(sei_plot()[,z_3d()]), max(sei_plot()[,z_3d()])))
      
      
      
    })
    

    
    output$scenario_anim <- renderPlotly({
      
      #if(input$year_time[1] == "Current"){
        
        sei_scen_bounds() %>% 
         #filter(time %in% c(input$year_time[1], input$year_time[2])) %>%
          filter(scenario %in% c("history", input$scen_time)) %>% 
          plot_ly(x = ~get(input$choose_x2), y = ~get(input$choose_y2), size = ~get(input$choose_size),
                  frame = ~time_label,
                  hovertemplate =  paste("%{x},%{y}<br>","Site:", .$plot.ID, "<extra></extra>"),
                  type = "scatter", mode = "markers") %>%
          plotly::layout(yaxis = list(title = names(choiceVal2)[choiceVal2 == input$choose_y2],
                                      range = c(min(sei_scen()[,input$choose_y2]), 
                                                max(sei_scen()[,input$choose_y2]))),
                         xaxis = list(title = names(choiceVal2)[choiceVal2 == input$choose_x2],
                                      range = c(min(sei_scen()[,input$choose_x2]), 
                                                max(sei_scen()[,input$choose_x2])))) %>% 
        animation_slider(currentvalue = list(prefix = "Timeframe:"))
        
        
      
    })
    
    
    #get reactive column names based on var, time and scenario input
    x_vect1 <- reactive({
      if (input$vect_time[1] == "Current") {
        x <- sei_plot() %>%
          rename(def_history = def.JJA, tmean_history = tmean.JJA) %>%
          select(contains("history") &
                   contains(input$choose_x3)) %>%
          names()
      } else {
        x <-
          sei_plot() %>% select(
            contains(paste(select(filter(sliderVals, time_label == input$vect_time[1]), time_value))) &
              contains(input$vect_scen) &
              contains(input$choose_x3)
          ) %>% names()
      }
      return(x)
    })
    
    y_vect1 <- reactive({
      if (input$vect_time[1] == "Current") {
        x <- sei_plot() %>%
          rename(def_history = def.JJA, tmean_history = tmean.JJA) %>%
          select(contains("history") &
                   contains(input$choose_y3)) %>%
          names()
      } else {
        x <-
          sei_plot() %>% select(
            contains(paste(select(filter(sliderVals, time_label == input$vect_time[1]), time_value))) &
              contains(input$vect_scen) &
              contains(input$choose_y3)
          ) %>% names()
      }
      return(x)
    })
    
    
    x_vect2 <- reactive({
      
        x <-
          sei_plot() %>% select(
            contains(paste(select(filter(sliderVals, time_label == input$vect_time[2]), time_value))) &
              contains(input$vect_scen) &
              contains(input$choose_x3)
          ) %>% names()

              return(x)
    })
    
    y_vect2 <- reactive({
      
      x <-
        sei_plot() %>% select(
          contains(paste(select(filter(sliderVals, time_label == input$vect_time[2]), time_value))) &
            contains(input$vect_scen) &
            contains(input$choose_y3)
        ) %>% names()
      
      return(x)
    })
    
    
    output$vector_plot <- renderPlotly({

      # ## INTERACTIVE ## THIS WORKS BUT MAKES MAP FILTERING SLOW
      sei_plot_bounds() %>%
        rename(def_history = def.JJA, tmean_history = tmean.JJA) %>%
        plot_ly(hovertemplate =  paste("%{x},%{y}<br>","Site:", .$plot.ID, "<extra></extra>")) %>%
        add_markers(x = as.formula(paste0("~", x_vect1())),
                    y = as.formula(paste0("~", y_vect1())),
                    marker = list(color = "#ff0000", size = 10, sizemode = "diameter"), opacity = 0.8,  name = input$vect_time[1]) %>%
        # add_markers(x = as.formula(paste0("~", x_vect2())),
        #             y = as.formula(paste0("~", y_vect2())),
        #             marker = list(color = "#850101", size = 10, sizemode = "diameter"), name = input$vect_time[2]) %>%
        add_annotations(x = as.formula(paste0("~", x_vect2())),
                        y = as.formula(paste0("~", y_vect2())),
                        xref = "x", yref = "y",
                        axref = "x", ayref = "y",
                        ax = as.formula(paste0("~", x_vect1())), ay = as.formula(paste0("~", y_vect1())),
                        text = "", showarrow = T, arrowcolor = "black",
                        arrowwidth = 1,
                        opacity = 0.5, arrowsize = 2, arrowhead = 5) %>%
        plotly::layout(yaxis = list(title = names(choiceVal2)[choiceVal2 == input$choose_y3],
                                    range = c(min(sei_scen()[,input$choose_y3]),
                                              max(sei_scen()[,input$choose_y3]))),
                       xaxis = list(title = names(choiceVal2)[choiceVal2 == input$choose_x3],
                                    range = c(min(sei_scen()[,input$choose_x3]),
                                              max(sei_scen()[,input$choose_x3])))) 
      ## Static ggplot ##
      # sei_scen_bounds() %>%
      #   ggplot(aes(x = get(input$choose_x3), y = get(input$choose_y3)))+
      #   geom_path(aes(group = site), color = "black",
      #             arrow = arrow(length = unit(4, "mm"), ends = "last"), size = 1, alpha = 0.5)+
      #   geom_point(aes(color = scenario), alpha = 0.6, size = 5) +
      #     scale_color_manual(values = c("#ff0000", "#850101"))+
      #   theme_minimal()+
      #   xlab(input$choose_x3)+
      #   ylab(input$choose_y3)+
      #   theme(
      #     axis.title.x = element_text(size = 16),
      #     axis.title.y = element_text(size = 16),
      #     legend.title = element_text(size = 16),
      #     legend.text = element_text(size = 16)
      #   )
      
    })
    


}

# Run the application 
shinyApp(ui = ui, server = server)
