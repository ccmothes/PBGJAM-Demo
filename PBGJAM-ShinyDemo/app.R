

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

# source("addRasterImage2.R")
# 
pathin <- "data"


#read in soil temp csv
st_30 <- read.csv("data/st_30.csv")

# read in site env data w/ coords
site_dat <- readRDS("data/site_dat.RDS")

#read in species files
spec_dat <- readRDS("data/data4Caitlinv2.rds")

#read in combined sp abundance file
abun_all <- readRDS("data/abun_all.RDS")

#function to read in USGS basemaps
GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

#attribution for usgs maps
att <- paste0("<a href='https://www.usgs.gov/'>",
              "U.S. Geological Survey</a> | ",
              "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
              "Policies</a>")



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
                   secondary = "#fafaf5",
                   #success = "#f28e35",
                   #base_font = font_google("Cairo")
                 ),
             
             # NEON UI --------------------------------------------------------------------
             tabPanel("",
                      fluidPage(
                        fluidRow(
                          column(6,
                                 #h3(strong("Environmental Characteristics")),
                                 #br(),
                                 fluidRow(
                                   column(4,
                                          selectInput("spec_1", 
                                                      label = HTML("<b>Choose Species</b>"),
                                                      choice = names(spec_dat))),
                                   column(4,
                                          pickerInput("map_var", label = "Color Map By:",
                                                       choices = list(
                                                         "Abundance change" = c("SSP245" = "s1",
                                                                                "SSP585" = "s2"),
                                                         "Habitat" = c("Gap Fraction" = "gap.frac.10"),
                                                         "Climate" = c("Summer Temp" = "tmean.JJA",
                                                                       "SSP245 Temp Change" = "s1_temp",
                                                                       "SSP585 Temp Change" = "s2_temp",
                                                                       "SSP245 Temp Full Effects" = "s1.fullT",
                                                                       "SSP585 Temp Full Effects" = "s2.fullT",
                                                                       "SSP245 Temp Main Effects" = "s1.mainT",
                                                                       "SSP585 Temp Main Effects" = "s2.mainT",
                                                                       "SSP245 Deficit Change" = "s1_def",
                                                                       "SSP585 Deficit Change" = "s2_def")
                                                       )))
                                 ),
                                 
                                 leaflet::leafletOutput("NEONmap2", height = 500),
                                 h5(em("Click a site on the map to view its habitat conditions")),
                                 h5(strong(textOutput("siteText1"))),
                                 fluidRow(
                                 column(4,
                                        p(strong("Terrain")),
                                        plotOutput("terrainPNG", height = 200)
                                        ),
                                 column(2),
                                 column(4,
                                        p(strong("Gap Fraction")),
                                        plotOutput("gapPNG", height = 200),
                                        )),
                                 hr()
                          ),
                            column(6,
                                   tabsetPanel(
                                     tabPanel(
                                       "Compare Across Sites",
                                       h3(strong("Explore all variables in 3D space")),
                                       fluidRow(
                                        column(6,
                                                sliderTextInput("year_3d", label = HTML("<b>Choose a Timeframe:</b>"),
                                                                                        choices = c("history" = "Current",
                                                                                                    "2021.204" = "2021-2040",
                                                                                                    "2061.208" = "2061-2080",
                                                                                                    "2081.21" = "2081-2100"),
                                                                                        grid = TRUE)
                                         ),
                                        column(4, prettyRadioButtons("scen_3d", label = HTML("<b>Choose Climate Scenario:</b>"),
                                                          choices = c("SSP 245" = "ssp245", 
                                                                      "SSP 585" = "ssp585"),
                                                          inline = TRUE, fill = TRUE)),
                                      ),
                                       fluidRow(
                                         column(4,
                                                pickerInput("choose_x", "Choose X:",
                                                            choices = list(
                                                              "Abundance Change" = list("s1", "s2"),
                                                              "Habitat" =c("Gap Fraction" = "gap.frac.10"),
                                                              "Climate" = c("tmean.JJA", "s1_temp", "s2_temp",
                                                                            "s1_def", "s2_def", "s1.fullT",
                                                                            "s2.fullT", "s1.mainT", "s2.mainT")
                                                              
                                                            ), selected = "s1_temp")),
                                         column(4,
                                                
                                                pickerInput("choose_y", "Choose Y:",
                                                            choices = list(
                                                              "Abundance Change" = c("s1", "s2"),
                                                              "Habitat" = c("Gap Fraction" = "gap.frac.10"),
                                                              "Climate" = c("tmean.JJA", "s1_temp", "s2_temp",
                                                                            "s1_def", "s2_def", "s1.fullT",
                                                                            "s2.fullT", "s1.mainT", "s2.mainT")),
                                                            selected = "s1_def")),
                                         column(4,
                                                pickerInput("choose_color", "Color By: ",
                                                            choices = list(
                                                              "Abundance Change" = c("s1", "s2"),
                                                              "Habitat" =c("Gap Fraction" = "gap.frac.10"),
                                                              "Climate" = c("tmean.JJA", "s1_temp", "s2_temp",
                                                                            "s1_def", "s2_def", "s1.fullT",
                                                                            "s2.fullT", "s1.mainT", "s2.mainT")),
                                                            selected = "gap.frac.10"))),
                                       plotlyOutput("choose_scatter"),
                                       hr(),
                                       h3(strong("Explore Change Over Time")),
                                        fluidRow(
                                          column(4, prettyRadioButtons("scen_time", label = HTML("<b>Choose Climate Scenario:</b>"),
                                                                       choices = c("SSP 245" = "ssp245", 
                                                                                   "SSP 585" = "ssp585"),
                                                                       inline = TRUE, fill = TRUE)),
                                        column(6,
                                               sliderTextInput("year_time", label = HTML("<b>Choose Time Range:</b>"),
                                                               choices = c("history" = "Current",
                                                                           "2021.204" = "2021-2040",
                                                                           "2061.208" = "2061-2080",
                                                                           "2081.21" = "2081-2100"),
                                                               grid = TRUE,
                                                               selected = c("Current", "2081-2100"))
                                        ),
                              
                                      ),
                                       h5(strong("Animation")),
                                       fluidRow(
                                         column(4,
                                                pickerInput("choose_x2", "Choose X:",
                                                            choices = c("Abundance_change", "Gap Fraction" = "gap.frac.10", 
                                                                        "Temperature_change",
                                                                        "Deficit_change", "Full_temp_effects", "Main_temp_effects"),
                                                            selected = "Full_temp_effects")),
                                         column(4,
                                                pickerInput("choose_y2", "Choose Y:",
                                                            choices = c("Abundance_change", "Gap Fraction" = "gap.frac.10", 
                                                                        "Temperature_change",
                                                                        "Deficit_change", "Full_temp_effects", "Main_temp_effects"),
                                                            selected = "Abundance_change")),
                                         column(4,
                                                pickerInput("choose_size", "Size Points By: ",
                                                            choices = c("Abundance_change", "Gap Fraction" = "gap.frac.10", 
                                                                        "Temperature_change",
                                                                        "Deficit_change", "Full_temp_effects", "Main_temp_effects"),
                                                            selected = "gap.frac.10") )
                                       ),
                                       plotlyOutput("scenario_anim"),
                                       hr(),
                                       h5(strong("Vector Plots")),
                                       fluidRow(
                                         column(4,
                                                pickerInput("choose_x3", "Choose X:",
                                                            choices = c("Abundance_change",
                                                                        "Temperature_change",
                                                                        "Deficit_change", "Full_temp_effects", "Main_temp_effects"),
                                                            selected = "Full_temp_effects")),
                                         column(4,
                                                pickerInput("choose_y3", "Choose Y:",
                                                            choices = c("Abundance_change",
                                                                        "Temperature_change",
                                                                        "Deficit_change", "Full_temp_effects", "Main_temp_effects"),
                                                            selected = "Abundance_change")),
                                       ),
                                       plotlyOutput("vector_plot")
                                       
                                     ),
                                     tabPanel(
                                       "Compare Across Species",
                                       h5(strong("Site-level Species Comparisons"),em(" (click a site on the map to view plots)")),
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
    

    #read in species data
    neon_spec_data <- reactive({
      st_read(paste0(pathin, "/betPosVis/", input$neon_spec, ".shp")) %>% 
      
        #create constant column name for scenario
        rename(scenario = input$neon_scenario1) %>% 
        #filter out zeros, depending on scenario
        filter(scenario != 0)
    })
    
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
    pal1 <- reactive({
      colorBin(palette = "RdBu", domain = c(min(neon_spec_data()$scenario, na.rm = TRUE),
                                            abs(min(neon_spec_data()$scenario, na.rm = TRUE))),
               reverse = TRUE)
      
    })
    
    grp <- c("USGS Topo", "USGS Imagery",
             "USGS Shaded Relief")
    
    output$NEONmap1 <- renderLeaflet({
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
        
        
        addCircleMarkers(
          data = neon_spec_data(),
          layerId = ~ site,
          group = "original",
          color = "black",
          fillColor = ~ pal1()(scenario),
          weight = 1,
          stroke = TRUE,
          radius = 7,
          fillOpacity = 1,
          popup = paste(
            "Site:",
            neon_spec_data()$site,
            "<br>",
            paste0(input$neon_scenario1, ":"),
            neon_spec_data()$scenario
          )
        ) %>%
        addCircleMarkers(data = st_jitter(neon_spec_data(), factor = 0.009),
                         #layerId = ~site,
                         group = "jitter", color = "black",
                         fillColor = ~pal1()(scenario), 
                         weight = 1, stroke = TRUE,
                         radius = 6, fillOpacity = 1,
                         popup = paste("Site:", neon_spec_data()$site, "<br>",
                                       paste0(input$neon_scenario1, ":"), neon_spec_data()$scenario)
        ) %>%
          addLayersControl(baseGroups = c("Dark Theme", "USGS Topo", 
                                          "USGS Imagery", "USGS Shaded Relief"),
                           position = "bottomleft",
                           options = layersControlOptions(collapsed = FALSE)
                           ) %>% 
        groupOptions("jitter", zoomLevels = 1:6) %>% 
        groupOptions("original", zoomLevels = 7:20)
    })
    

    
    #Add selected site name as header
    output$siteText <- renderText(
      if(is.null(input$NEONmap1_marker_click)){
        return(NULL)
      } else {
        input$NEONmap1_marker_click$id
      })
    
    
    #paste habitat variable png
    observeEvent(input$NEONmap1_marker_click, {
      
      output$habitatPNG <- renderImage({
        if (input$habitatVar == "terrain") {
          list(
            src = paste0(
              "data/ABBY_terrain/",
              input$NEONmap1_marker_click$id,
              ".png"
            ),
            width = 200,
            height = 200
          )
        }
        
        else {
          list(
            src = paste0(
              "data/ABBY_gf/",
              input$NEONmap1_marker_click$id,
              "_bet_norm_chm.png"
            ),
            width = 200,
            height = 200
          )
        }
        
        
        
      }, deleteFile = FALSE)
      
    })
    
    
    #NEON environment plotly
    
    
    observeEvent(input$NEONmap1_marker_click, {
     
       output$neon_timeseries <- renderPlotly({
        plot_ly(st_30) %>%
          add_trace(x = st_30$startDateTime, y = st_30$soilTempMean,type = 'scatter',
                    mode = "lines", connectgaps = TRUE) %>% 
          plotly::layout(yaxis = list(title = "Mean Soil Temperature"),
                         xaxis = list(type = "date", tickformate = "%d %B <br> %Y"))
        
        
        
      })
      
      
      
    })
    
    
    # SPEC-ENV INT --------------------------------------------------------------------------------

    # #read in environmental data (keep same species to use for scatter plot)
    # env_data <-  reactive({
    #   st_read(paste0(pathin, "/betPosVis/", input$neon_spec, ".shp")) #%>% 
    #     #jitter points for plot
    #     #st_jitter(factor = 0.009)
    #   
    # })
    #  
    # 
    # neon_env_data <- reactive({
    #     
    #   if(input$neon_env == "gp_f_10"){
    #     neon_env_data <- env_data() %>% 
    #       rename(variable = gp_f_10)
    #   } else {
    #     neon_env_data <- env_data() %>% 
    #       rename(variable = paste0(input$neon_scenario2, "_", input$neon_env))
    #   }
    #   
    # })
    
    # filter species data and join with site data
   
    sei_plot <- reactive({
      spec_dat[[input$spec_1]] %>% 
        as_tibble() %>% 
        rename(site = plot.ID) %>% 
        left_join(site_dat, by = "site")
      
    })
    
    sei_map <- reactive({
      sei_plot() %>% 
        rename(variable = input$map_var) %>% 
        st_as_sf(coords = c("lon", "lat"), crs = 4326)
      
      
    })
    
    sei_map_jitter <- reactive({
      sei_plot() %>% 
        rename(variable = input$map_var) %>% 
        mutate(site_level = case_when(str_detect(site, "_") ~ substr(site, start = 1, stop = 4),
                                      str_detect(site, "-") ~ word(site, 1, sep = "\\-")),
               ID = 1:nrow(.)) %>% 
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
        st_jitter(factor = 0.009) 
    })
    
    #restructure data for scenario plots
    sei_scen <- reactive({
      
      sei_plot() %>% 
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
    

    
    ## Column 1 -----------------------------------------
    
    pal2 <- reactive({
      
      if(input$map_var == "gap.frac.10"){
        colorNumeric(palette = "Greens", domain = sei_map()$variable, reverse = TRUE)
      } 
      
      else if(input$map_var %in% c("s1", "s2")){
        colorBin(palette = "RdBu", domain = c(min(sei_map()$variable, na.rm = TRUE),
                                              max(sei_map()$variable, na.rm = TRUE)),
                 reverse = TRUE)
      } else {
      
      colorNumeric(palette = "Reds", domain = sei_map()$variable)
      }
      
    })
    
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
                         layerId = ~site,
                         group = "original",
                         color = "black",
                         fillColor = ~pal2()(variable), weight = 1, stroke = TRUE,
                         radius = 7, fillOpacity = 1,
                         popup = paste("Site:", sei_map()$site, "<br>",
                                       paste0(input$map_var, ":"), sei_map()$variable)
        ) %>% 
        addCircleMarkers(data = sei_map_jitter(), layerId = ~ID,
                         group = "jitter",
                         color = "black",
                         fillColor = ~pal2()(variable), weight = 1, stroke = TRUE,
                         radius = 6, fillOpacity = 1,
                         popup = paste("Site:", sei_map_jitter()$site, "<br>",
                                       paste0(input$map_var, ":"), sei_map_jitter()$variable)
        ) %>% 
        addLegend("bottomright", data = sei_map(), values = ~variable, pal = pal2(),
                  title = input$map_var) %>% 
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
          filter(species == input$spec_1) %>% 
          filter(site == input$NEONmap2_marker_click$id) %>% 
          select(site_level) %>% 
          as.character()
        
      }
    })
    
    ## get plot (higher order) name from mapclick
    plot_click <- reactive({
      if(is.numeric(input$NEONmap2_marker_click$id)){
        sei_map_jitter() %>% 
          filter(ID == input$NEONmap2_marker_click$id) %>% 
          select(site) %>% 
          as.character()
        
      } else{
        abun_all %>% 
          filter(species == input$spec_1) %>% 
          filter(site == input$NEONmap2_marker_click$id) %>% 
          select(site) %>% 
          as.character()
        
      }
    })
    
    
    #get reactive x and y for highlighted species
    trace.x1 <- reactive({
      input$spec_1
    })
    
    trace.y1 <- reactive({
      abun_all %>% 
        filter(site == plot_click()[1] & species == trace.x1()) %>%
        select(input$choose_y_spbar) %>% 
        as.numeric()
    })
    
    
    #reactive dataframe for species plots
    sp_plot_dat <- reactive({
      
      abun_all %>% 
        mutate(#current_color = if_else(species == trace.x1(), "red", "lightblue"),
               name = if_else(species == trace.x1(), paste(trace.x1()), "All species"),
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
          plot_ly(x = ~species, y = ~get(input$choose_y_spbar), type = "bar", 
                  color = ~species == trace.x1(), colors = c("lightblue", "red"), name = ~name,
                  hovertemplate =  paste("%{x},%{y}<br>","<extra></extra>")
          ) %>% 
          layout(barmode = "overlay",
                 xaxis = list(categoryorder = "total descending", showticklabels = F, title = "Species"),
                 yaxis = list(title = input$choose_y_spbar)) #%>%
        # add_trace(x = trace.x1(), y = trace.y1(), type = "bar", name = trace.x1(), marker = list(color = "orange")) 
        
      })
      
      
      
      output$speciesScatter <- renderPlotly({
        
        sp_plot_dat() %>% 
          plot_ly(x = ~get(input$choose_x_spscat), y = ~get(input$choose_y_spscat), type = "scatter", mode = "markers",
                  color = ~species == trace.x1(), colors = c("lightblue", "red"),
                  size = ifelse(.$species == trace.x1(), 12, 7),
                  line = list(color = NA, width = 0), name = ~name,
                  hovertemplate =  paste("%{x},%{y}<br>","Species:", .$species, "<extra></extra>")) %>% 
           layout(
                  xaxis = list(title = input$choose_x_spscat),
                  yaxis = list(title = input$choose_y_spscat)) #%>%
        # add_trace(x = trace.x1(), y = trace.y1(), type = "bar", name = trace.x1(), marker = list(color = "orange")) 
        
      })
      
        
    })
    
    
   
    
    
    
    
    ## Column 2 --------------------------------
    output$choose_scatter <- renderPlotly({
      
      sei_plot_bounds() %>% 
      
      plot_ly(x = ~get(input$choose_x), y = ~get(input$choose_y), 
                color = ~get(input$choose_color),
                size = 4,
                #marker = list(line = list(width = 0)),
              hovertemplate =  paste("%{x},%{y}<br>","Site:", .$site, "<extra></extra>"),
              type = "scatter", mode = "markers") %>%
        plotly::layout(yaxis = list(title = input$choose_y, 
                                    range = c(min(sei_plot()[,input$choose_y]), 
                                              max(sei_plot()[,input$choose_y]))),
                       xaxis = list(title = input$choose_x, 
                                    range = c(min(sei_plot()[,input$choose_x]), 
                                              max(sei_plot()[,input$choose_x])))
                       ) %>%
        colorbar(title=input$choose_color, limits = c(min(sei_plot()[,input$choose_color]), max(sei_plot()[,input$choose_color])))
      
    })
    
    
    output$scenario_anim <- renderPlotly({
      
      sei_scen_bounds() %>% 
        
        plot_ly(x = ~get(input$choose_x2), y = ~get(input$choose_y2), size = ~get(input$choose_size),
                frame = ~scenario,
                hovertemplate =  paste("%{x},%{y}<br>","Site:", .$site, "<extra></extra>"),
                type = "scatter", mode = "markers") %>%
        plotly::layout(yaxis = list(title = input$choose_y2,
                                    range = c(min(sei_scen()[,input$choose_y2]), 
                                              max(sei_scen()[,input$choose_y2]))),
                       xaxis = list(title = input$choose_x2,
                                    range = c(min(sei_scen()[,input$choose_x2]), 
                                              max(sei_scen()[,input$choose_x2])))) 
      
    })
    
    output$vector_plot <- renderPlotly({

      # ## INTERACTIVE ## THIS WORKS BUT MAKES MAP FILTERING SLOW
      sei_scen_bounds() %>%
        pivot_wider(names_from = scenario, values_from = c("Abundance_change", "Full_temp_effects",
                                                           "Temperature_change", "Deficit_change")) %>%
        group_by(site) %>%
        summarize(across(contains(c("s1","s2")), ~sum(.x, na.rm = TRUE))) %>%

        plot_ly(hovertemplate =  paste("%{x},%{y}<br>","Site:", .$site, "<extra></extra>")) %>%
        add_markers(x = as.formula(paste0("~", as.name(input$choose_x3), "_s1")),
                    y = as.formula(paste0("~", as.name(input$choose_y3), "_s1")),
                    marker = list(color = "#ff0000", size = 10, sizemode = "diameter"), opacity = 0.8,  name = "SSP245") %>%
        add_markers(x = as.formula(paste0("~", as.name(input$choose_x3), "_s2")),
                    y = as.formula(paste0("~", as.name(input$choose_y3), "_s2")),
                    marker = list(color = "#850101", size = 10, sizemode = "diameter"), name = "SSP585") %>%
        add_annotations(x = as.formula(paste0("~", as.name(input$choose_x3), "_s2")),
                        y = as.formula(paste0("~", as.name(input$choose_y3), "_s2")),
                        xref = "x", yref = "y",
                        axref = "x", ayref = "y",
                        ax = as.formula(paste0("~", as.name(input$choose_x3), "_s1")), ay = as.formula(paste0("~", as.name(input$choose_y3), "_s1")),
                        text = "", showarrow = T, arrowcolor = "#a19e9d",
                        arrowwidth = 1,
                        opacity = 0.8, arrowsize = 2, arrowhead = 5) %>%
        plotly::layout(yaxis = list(title = input$choose_y3,
                                    range = c(min(sei_scen()[,input$choose_y3]),
                                              max(sei_scen()[,input$choose_y3]))),
                       xaxis = list(title = input$choose_x3,
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
