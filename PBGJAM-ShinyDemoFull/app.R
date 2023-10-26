
# SET UP -----------------------------------------------------------

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
library(rgdal)
library(arcpullr)



## neon tab --------------------------------------------

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

# habitat varialbe description table
var_table <- readxl::read_xlsx("data/var_description.xlsx") %>% 
  mutate(Reference = if_else(is.na(Reference), "", Reference))

## maps and models tab -------------------------------------------------------------------

source("addRasterImage2.R")

pathin <- "data"

SelectTaxa <- "NEON_Beetles"
SelectCovars <- "CL"

covars <- c("climate + topography + soils" = "CL",
            "remotely sensed imagery + topography + soils" = "RS")

varsTime <- c("2018",
              "Future Projections")

varsTimeScen <- c("RCP 4.5" = "rcp45",
                  "RCP 8.5" = "rcp85")

varsTimeScenMap <- c("RCP 4.5" = "RCP45",
                     "RCP 8.5" = "RCP85")

varsTimePer <- c("2040-2069" = "2040_2069",
                 "2070-2099" = "2070_2099")

varsTaxa  <- c("Trees" = "FIA_trees",
               "Small Mammals" = "NEON_Small-Mammals",
               "Breeding Birds" = "BBS-NEON_Breeding-Birds",
               "Beetles" = "NEON_Beetles")

varsGOacc <- c("Species-specific abundance" = "SSA")

varsGOacc2 <- c("in-sample" = "in",
                "Cross-validation: out-of-sample" = "out")

varsGOacc3 <- c("sp1" = "s1",
                "sp2" = "s2")

varsGObetas1 <- c("beta 1" = "beta1", 
                  "beta 2" = "beta2")

varsGObetas2 <- c("trait 1" = "trait1", 
                  "trait 2" = "trait2")

varsGObetas3 <- c("Common name" = "vernacularName",
                  "Scientific name" = "scientificName")

varsGObetas3b <- c("Scientific name" = "scientificName")

#load("data/comm_layers.RData")
load("data/comm_layers_simple.RData")
layers <- layers_simplify

#get community layers
names45 <- layers[[3]]$Name # need to use 3rd layers, one of communities is lost in first and second when simplifying
names85 <- layers[[6]]$Name




#pbgjam logo
corner_element = HTML(paste0('<a href=',shQuote("https://pbgjam.env.duke.edu/"), '>', 
                             '<img src=', 'logo.png', '/></a>'))


# UI ---------------------------------------------------------------------------

ui <- 
  navbarPage(id = "nav",
             tags$a(
               href="https://pbgjam.org",
               tags$img(src="pbgjam_logo.png",
                        title="PBGJAM Website"),
               width="20%",
               height="20%"
             ),
             #stop map elements from covering drop down menus
             tags$head(tags$style('.dropdown-menu {z-index: 10000 !important}'),
                       #embed google analytics script
                       tags$head(includeHTML("google-analytics.html"))
             ),
             theme = bslib::bs_theme(
               version = 4,
               bootswatch = "sandstone",
               #bg = "#FFFFFF",
               #fg = "#000",
               #primary = "#082466",
               secondary = "#fafaf5"
               #success = "#f28e35",
               #base_font = font_google("Cairo")
             ) %>% 
               bslib::bs_add_rules(sass::sass_file("www/style.scss")),
             
             ## Homepage -------------------------------------------------------
             tabPanel("Home", 
                      tags$div(class = "container-fluid1",
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "homepage.css")
                      ),
                      
                      fluidRow(class = 'header',
                               column(6,
                               h1("Biodiversity habitats in transition: big data offer insights
                                  for species and communities"),
                               p("We apply the latest advancements in technology and statistics to forecast the
                                 effects of a changing climate on the abundance and distribution of America's wildlife")),
                               tags$img(src='wave-bottom.svg', align= 'bottom', width='100%', padding= "0px")
                               
                                       
                      ),
                      fluidRow(class = "logo",
                               
                               column(3, tags$img(src= 'nasa.jpeg',
                                                  title='NASA logo',
                                                  height='65px'), align = 'center'),
                               column(3, tags$img(src= 'duke.png',
                                                  title='Duke logo',
                                                  height='65px'), align = 'center'),
                               column(3, tags$img(src= 'nsf.png',
                                                  title='NSF logo',
                                                  height='65px'), align = 'center'),
                               column(3, tags$img(src= 'neon.png',
                                                  title='NEON logo',
                                                  height='65px'), align = 'center')),
                      fluidRow(class = "title",
                               column(8, 
                               p(strong("PREDICTING BIODIVERSITY WITH GENERALIZED JOINT ATTRIBUTE MODELS")),
                               h3("With the support of NASA, we use satellites to monitor our changing planet,
                                  and through the National Ecological Observatory Network (NEON), we
                                  track how these changes will impact North America's species and wildlife communities"),
                               p(em("NASA-AIST 16 & 18 grants to", a(href = 'https://nicholas.duke.edu/people/faculty/swenson', 'Jennifer J. Swenson'),
                                    "&", a(href = 'https://nicholas.duke.edu/people/faculty/clark', 'James Clark'))),
                                    #a(href = 'https://nicholas.duke.edu/', 'Nicholas School of Environment'), ', Duke University')),
                               p(em(a(href = 'https://nicholas.duke.edu/', 'Nicholas School of Environment'), ', Duke University')),
                               br(),
                               h3("We offer online tools and maps to explore these changes, and the option
                                  to download this information for your own use"))),
                      fluidRow(class = "research",
                               column(10,
                               fluidRow(class = "split",
                                       
                               p(strong("MAPS AND MODELS")),
                               h2("Explore Our Research")),
                               p("Our maps are based on advances in Bayesian modeling, and unique in
                                 their focus on critical habitat. Below, you can dive under the hood of
                                 the maps and explore the model specifics or explore the maps by
                                 species taxa."),
                               br(),
                               fluidRow(class = "speciesBlock",
                                      
                               column(3, class = "specs", tags$img(src= 'beetles.png',
                                                  title='Beetle photo'), align = 'center',
                                      h5("Beetles"),
                                      actionButton("beetleMaps", "Maps"),
                                      actionButton("beetleModels", "Models")),
                               column(3, class = "specs", tags$img(src= 'birds.jpg',
                                                  title='Bird photo'), align = 'center',
                                      h5("Birds"),
                                      actionButton("birdMaps", "Maps"),
                                      actionButton("birdModels", "Models")),
                               column(3, class = "specs", tags$img(src= 'mammals.jpg',
                                                  title='Mammal photo'), align = 'center',
                                      h5("Mammals"),
                                      actionButton("mammalMaps", "Maps"),
                                      actionButton("mammalModels", "Models")),
                               column(3, class = "specs", tags$img(src= 'trees.png',
                                                  title='Tree photo'), align = 'center',
                                      h5("Trees"),
                                      actionButton("treeMaps", "Maps"),
                                      actionButton("treeModels", "Models"))))),
                      fluidRow(class = "data",
                               column(10,
                               fluidRow(class = "split2",
                               p(strong("SUSTAINABLE SCIENCE")),
                               h2("Our Data Streams")),
                               p("Long-term studies are drivers of scientific innovation. Combining
                                 the resources of NASA with those of the National Ecological Observatory
                                 Network and other open source data sets and citizen science projects,
                                 we can better predict the impacts of climate change on our country and
                                 world."),
                               fluidRow(class = "datatabs",
                               tabsetPanel(type = "tabs",
                                           tabPanel("Remote Sensing Data", class = "RS",
                                                    fluidRow(
                                                    column(5, 
                                                           tags$img(src= 'river.jpeg',
                                                                    title='River photo'), align = 'center',
                                                           h2("NEON Aerial Observation Platform"),
                                                           p("NEON provides hyperspectral and LiDAR data collected
                                                             annually by an aerial observation platform. We use
                                                             this data to better understand the local traits of
                                                             ecosystems."),
                                                           tags$a(href = "https://www.neonscience.org/data-collection/airborne-remote-sensing", 
                                                                        target = "_blank", "Learn More")),
                                                    column(2),
                                                    column(5,
                                                           tags$img(src= 'satellite.png',
                                                                    title='Satellite photo'), align = 'center',
                                                           h2("NASA Satellite Remote Sensing"),
                                                           p("We use NASA satellites to understand how temperature,
                                                             soil moisture, and the productivity of trees and other 
                                                             planets drives the structure and composition of communities"),
                                                           tags$a(href = 'https://www.earthdata.nasa.gov/learn/backgrounders/remote-sensing', 
                                                                        target = "_blank", "Learn More")))),
                                           tabPanel("Biodiversity Data", class = "biodiversity",
                                                    fluidRow(
                                                      column(5,
                                                             tags$img(src= 'Landscape.jpg',
                                                                      title='Landscape photo'), align = 'center'),
                                                      column(2),
                                                      column(5,
                                                             p(strong("The wonder of life")),
                                                             h2("Biodiversity drives the natural world and ecosystem
                                                                services"),
                                                             p("We use data collected by field assistants and
                                                               research technicians from the Nationlal Ecological
                                                               Observatory Network to develop our joint
                                                               attribute models, and estimate where and when species
                                                               will occur in the future under climate change"),
                                                             tags$a(href = "https://www.neonscience.org/theme/organisms-populations-and-communities", 
                                                                    target = "_blank", "Learn More"))
                                                    )),
                                           tabPanel("Climate Data", class = "climatedata",
                                                    fluidRow(
                                                      column(5,
                                                             p(strong("Observations and interpolations")),
                                                             h2("Our Changing Climate"),
                                                             p("We use climate information that is taken and interpolated
                                                               from many sources, including local weather stations, satellites,
                                                               and remote observations to model how climate shapes the
                                                               distribution and abundance of species."),
                                                             tags$a(href = "https://climate.nasa.gov/", 
                                                                    target = "_blank", "Learn More")),
                                                      column(2),
                                                      column(5,
                                                              tags$img(src= 'climate.gif',
                                                                       title='Climate gif'), align = 'center'))
                                                    ))))),
                      fluidRow(class = "community",
                               fluidRow(
                                 p(strong("A community of science"))
                               ),
                               fluidRow(class = "links",
                                 #column(5, class = "coms",
                                        tags$a(href = "https://sites.nicholas.duke.edu/clarklab/people/", 
                                               target = "_blank", "Our Research Team"),
                                        #),
                                 #column(2),
                                 #column(5, class = "coms",
                                        tags$a(href = "https://sites.nicholas.duke.edu/clarklab/news/", 
                                               target = "_blank", "News and Updates")
                                 #)
                               ))
                    

                      
                      )
                      ),
             
             ## neon --------------------------------------------------------------------
             tabPanel("NEON Biodiversity",
                      #Stop leaflet elements from covering select inputs
                      
                      fluidPage(
                        fluidRow(
                          h2(strong("Ground beetle species abundance and habitat variable exploration"))
                        ),
                        hr(),
                        
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
                                                          selected = "abundance"))),
                                       fluidRow(
                                         column(12,
                                              actionButton("plotVec", "Click to make vector plot!", width = "100%")
                                       )
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
                                     
                                   ),
                                   tabPanel(
                                     "Description of Habitat Variables",
                                     tableOutput('var_description')
                                   )
                                 )
                                 
                          )
                          
                        )
                      )
             ),
             
             ## maps --------------------------------------------------------
             tabPanel("Species Maps",
                      # div(class = "outer"),
                      #sidebarLayout(
                      #mainPanel(
                      #fillPage(
                      # shinyUI(bootstrapPage(
                      #
                      # leaflet::leafletOutput("map", height = "85vh"),
                      #
                      # #sidebarPanel(
                      #   ## code for absolute panel:
                      #   fixedPanel(
                      #   #absolutePanel(
                      #   id = "controls",
                      #   class = "panel panel-default",
                      #   #fixed = TRUE,
                      #   #draggable = TRUE,
                      #   top = 100,
                      #   left = "auto",
                      #   right = 20,
                      #   bottom = "auto",
                      #   width = 330,
                      #   height = "auto",
                      #   style = "opacity: 0.9; background-color: white; padding: 0 20px 20px 20px",
                      #   HTML('<button class ="btn btn-primary" data-toggle="collapse" data-target="#collapse-panel">Collapse Controls</button>'),
                      #
                      #   tags$div(id = "collapse-panel", class = "collapse show",
                      tabsetPanel(
                        type = "tabs",
                        
                        tabPanel(
                          "Species Maps",
                          sidebarLayout(
                            position = "right",
                            mainPanel(leaflet::leafletOutput("map", height = "85vh")),
                            sidebarPanel(
                              #br(),
                              radioGroupButtons(
                                "taxa1",
                                "Choose Taxa Group",
                                varsTaxa,
                                #choices = c("Small Mammals", "Birds", "Beetles", "Trees"),
                                
                                individual = TRUE,
                                selected = "NEON_Small-Mammals"
                              ),
                              #update list of species based on taxa selection)
                              selectInput("specs1", "Choose Species",
                                          varsGOacc3),
                              hr(),
                              
                              #paste species name as header
                              h3(em(textOutput("spText"))),
                              
                              
                              p(textOutput("abunText")),
                              
                              hr(),
                              
                              # If split map view was implemented:
                              # switchInput(inputId = "splitView", label = "Split Map View",
                              #             labelWidth = "100px", onStatus = "success",
                              #             offStatus = "danger",
                              #             value = FALSE, inline = TRUE),
                              
                              #predictors (Not applicable for trees/beetles/new models)
                              # radioGroupButtons("preds1","",
                              #                   choices = c(
                              #                     HTML("<b>Use Elevation, Soils & Climate</b><br>More text here"),
                              #                     HTML("<b>Use Remote Sensing Inputs</b><br>More text here")
                              #                   )),
                              
                              #map variable
                              radioGroupButtons(
                                "mapVar",
                                "Map Variable",
                                choiceNames = c(
                                  HTML("<b>Mean Abundance</b>"),
                                  HTML("<b>Uncertainty (RSE)</b>")
                                ),
                                choiceValues = c("Mean", "RSE")
                              ),
                              
                              #year
                              radioGroupButtons(
                                "year1",
                                "Set Year",
                                choices = c(
                                  "2018" = "Hist",
                                  "2040-2069" = "2040",
                                  "2070-2099" = "2070"
                                )
                              ),
                              
                              #RCP
                              radioGroupButtons("rcp1", "Set Climate Scenario",
                                                choices = varsTimeScenMap),
                              
                              
                            )
                          )
                        ),
                        tabPanel(
                          "Community Maps",
                          #br(),
                          sidebarLayout(
                            position = "right",
                            mainPanel(leaflet::leafletOutput("map2", height = "85vh")),
                            sidebarPanel(
                              HTML("<p>Communities are identified using Gaussian Mixture Modeling with the <a href = 'https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html'> mclust package in R </a>. Clustering is done on relative abundance-weighted habitat suitability. Communities with similar relative abundance-weighted habitat suitability across maps share the same color</p>"),
                              #jump to more detail on communities models
                              actionButton("jumpToComm", "More detail on Communities"),
                              hr(),
                              #year
                              radioGroupButtons(
                                "year2",
                                "Set Year",
                                choices = c(
                                  "2018" = "hist",
                                  "2040-2069" = "2040",
                                  "2070-2099" = "2070"
                                ),
                                selected = "hist"
                              ),
                              
                              #RCP
                              radioGroupButtons("rcp2", "Set Climate Scenario",
                                                choices = varsTimeScen,
                                                selected = "rcp45"),
                              #p("Select which communities are shown on map"),
                              #actionButton("commLayers", "Update Map!"),
                              
                              #select which communities shown on map
                              conditionalPanel("input.rcp2 == 'rcp45'",
                                               checkboxGroupButtons(
                                                 "commBlocks45_2",
                                                 "Turn Communities On/Off:",
                                                 choiceNames =
                                                   list(
                                                     list(icon("square", "C1"), "Cascades and Sierra Nevada Forest"),
                                                     #light gray 12
                                                     list(icon("square", "C2"), "Central Basin and Range"),
                                                     #dark gray 6
                                                     list(icon("square", "C3"), "Central Forest/Grassland Transition"),
                                                     #light teal 16
                                                     list(icon("square", "C4"), "Colorado Rockies Forests"),
                                                     #dark teal 5
                                                     list(icon("square", "C5"), "Desert"),
                                                     #light lavendar 9
                                                     list(icon("square", "C6"), "Eastern Temperate Forests"),
                                                     #dark lavendar 11
                                                     list(
                                                       icon("square", "C7"),
                                                       "Great Basin Shrub Steppe/Colordo Plateau Shrublands"
                                                     ),
                                                     #light brown 13
                                                     list(icon("square", "C8"), "Marine and Mediterranean Forests"),
                                                     #dark brown 19
                                                     list(icon("square", "C9"), "NE Coastal Zone"),
                                                     #light blue 18
                                                     list(icon("square", "C10"), "New England-Acadian/Great Lakes Forest"),
                                                     #dark blue 4
                                                     list(icon("square", "C11"), "Northern Tallgrass"),
                                                     #light green 7
                                                     list(icon("square", "C12"), "NW Great Plains"),
                                                     #dark green 14
                                                     list(icon("square", "C13"), "NW Great/Glaciated Plains"),
                                                     #light red 3
                                                     list(icon("square", "C14"), "SE Conifer Forest"),
                                                     #dark red 2
                                                     list(icon("square", "C15"), "SE Conifer/Mixed Forest"),
                                                     #light orange 20
                                                     list(icon("square", "C16"), "SE Mixed Forest/Piney Woods"),
                                                     #dark orange 1
                                                     list(icon("square", "C17"), "SE Plains"),
                                                     #light purple 17
                                                     list(icon("square", "C18"), "Southern Tallgrass Prarie"),
                                                     #dark purple 15
                                                     list(icon("square", "C19"), "Tallgrass and Prarie Peninsula"),
                                                     #light yellow 8
                                                     list(icon("square", "C20"), "Western Short Grasslands")
                                                   ),
                                                 #dark yellow 10
                                                 choiceValues = names45,
                                                 direction = "vertical",
                                                 individual = TRUE,
                                                 selected = names45
                                               )),
                              conditionalPanel("input.rcp2 == 'rcp85'",
                                               checkboxGroupButtons(
                                                 "commBlocks85_2",
                                                 "Choose a community:",
                                                 choiceNames =
                                                   list(
                                                     list(icon("square", "D1"), "Appalachian and Cascade Forests"),
                                                     #light gray 12
                                                     list(icon("square", "D2"), "Cascades, Sierra Nevada, and Northern Rockies"),
                                                     #dark gray 6
                                                     list(icon("square", "D3"), "Central Basin and Range"),
                                                     #light teal 16
                                                     list(icon("square", "D4"), "Central Forest/Grassland Transition"),
                                                     #dark teal 5
                                                     list(icon("square", "D5"), "Cold Deserts"),
                                                     #light lavendar 9
                                                     list(icon("square", "D6"), "Deserts"),
                                                     #dark lavendar 11
                                                     list(
                                                       icon("square", "D7"),
                                                       "Eastern Temperate Forests"
                                                     ),
                                                     #light brown 13
                                                     list(icon("square", "D8"), "Great Basin Shrub Steppe/Colorado Plateau Shrublands"),
                                                     #dark brown 19
                                                     list(icon("square", "D9"), "NE Coastal Zone"),
                                                     #light blue 18
                                                     list(icon("square", "D10"), "Nebraska Sand Hills"),
                                                     #dark blue 4
                                                     list(icon("square", "D11"), "New England-Acadian Forest"),
                                                     #light green 7
                                                     list(icon("square", "D12"), "Northern Lakes and Forests"),
                                                     #dark green 14
                                                     list(icon("square", "D13"), "NW Great/Glaciated Plains"),
                                                     #light red 3
                                                     list(icon("square", "D14"), "SE Conifer/Mixed Forest"),
                                                     #dark red 2
                                                     list(icon("square", "D15"), "SE Mixed Forest/Piney Woods"),
                                                     #light orange 20
                                                     list(icon("square", "D16"), "SE Plains"),
                                                     #dark orange 1
                                                     list(icon("square", "D17"), "South Central Semi-arid Prairies"),
                                                     #light purple 17
                                                     list(icon("square", "D18"), "Southern California Mountains and Coastal Plain"),
                                                     #dark purple 15
                                                     list(icon("square", "D19"), "Southern Tallgrass Prairie"),
                                                     #light yellow 8
                                                     list(icon("square", "D20"), "Western Short Grasslands")
                                                   ),
                                                 #dark yellow 10
                                                 choiceValues = names85,
                                                 direction = "vertical",
                                                 individual = TRUE,
                                                 selected = names85
                                               )),

                              tags$style(".C1 {color:#cccccc"),
                              tags$style(".C2 {color:#686868"),
                              tags$style(".C3 {color:#9ed7c2"),
                              tags$style(".C4 {color:#00a884"),
                              tags$style(".C5 {color:#e8beff"),
                              tags$style(".C6 {color:#c500ff"),
                              tags$style(".C7 {color:#d7c29e"),
                              tags$style(".C8 {color:#895a44"),
                              tags$style(".C9 {color:#a6cee3"),
                              tags$style(".C10 {color:#1f79b5"),
                              tags$style(".C11 {color:#b1de8a"),
                              tags$style(".C12 {color:#33a12b"),
                              tags$style(".C13 {color:#fa9a98"),
                              tags$style(".C14 {color:#e3191c"),
                              tags$style(".C15 {color:#fcbf6f"),
                              tags$style(".C16 {color:#ff8000"),
                              tags$style(".C17 {color:#cab2d6"),
                              tags$style(".C18 {color:#693d99"),
                              tags$style(".C19 {color:#ffff99"),
                              tags$style(".C20 {color:#a8a800"),
                              tags$style(".D1 {color:#cccccc"),
                              tags$style(".D2 {color:#686868"),
                              tags$style(".D3 {color:#9ed7c2"),
                              tags$style(".D4 {color:#00a884"),
                              tags$style(".D5 {color:#e8beff"),
                              tags$style(".D6 {color:#c500ff"),
                              tags$style(".D7 {color:#d7c29e"),
                              tags$style(".D8 {color:#895a44"),
                              tags$style(".D9 {color:#a6cee3"),
                              tags$style(".D10 {color:#1f79b5"),
                              tags$style(".D11 {color:#b1de8a"),
                              tags$style(".D12 {color:#33a12b"),
                              tags$style(".D13 {color:#fa9a98"),
                              tags$style(".D14 {color:#e3191c"),
                              tags$style(".D15 {color:#fcbf6f"),
                              tags$style(".D16 {color:#ff8000"),
                              tags$style(".D17 {color:#cab2d6"),
                              tags$style(".D18 {color:#693d99"),
                              tags$style(".D19 {color:#ffff99"),
                              tags$style(".D20 {color:#a8a800")
                              
                            )
                            
                          )
                        )
                        
                        
                      )
                      
                      ),
             
             ## models ---------------------------------------------------------------------------
             navbarMenu(title = "Species Models",
                        tabPanel("Communities",
                                 sidebarLayout(position = "left",
                                               sidebarPanel(width = 6,
                                                            #selectInput("taxaComm", "Input: Biodiversity data options", varsTaxa),
                                                            #selectInput("covarsComm", "Input: Environmental data options", covars),
                                                            tags$h3("Communities of small mammals, beetles, and vascular plants"),
                                                            #selectInput("optTimeComm", "Time-period", varsTime),
                                                            selectInput("optTimeScenComm", "Future climate scenario (CMIP5)", varsTimeScen),
                                                            selectInput("optTimePerComm", "Future time period", varsTimePer),
                                                            tags$a(href="https://www.ipcc-data.org/docs/factsheets/TGICA_Fact_Sheet_CMIP5_data_provided_at_the_IPCC_DDC_Ver_1_2016.pdf", 
                                                                   "Learn more about the CMIP5 Representative Concentration Pathways (RCPs)")
                                                            
                                               ),
                                               
                                               mainPanel(width = 6,
                                                         helpText("Communities are identified using Gaussian Mixture Modeling with the mclust package in R.
                                             Clustering is done on relative abundance-weighted habitat suitability through time. 
                                             The top map shows 2018 community distributions 
                                             and the bottom map shows projected future shifts. 
                                             Communities with similar relative abundance-weighted habitat suitability across the two maps share the same color."),#Gaussian Mixture Modeling with the mclust package in R
                                             helpText("The buttons on the left correspond to each community mapped on the right.
                                            Select a button to learn how the abundance-weighted habitat suitability for each species 
                                            is predicted to change across time within the selected community. 
                                            The figure on the bottom left will then show the mean change in abundance-weighted habitat suitability
                                            for species predicted to see the largest change in
                                            abundance-weighted habitat suitability within the selected community.")                                   
                                               )),
                                 fluidRow(
                                   column(6,
                                          tags$h3("Compositional Changes in Communities"),
                                          helpText("To see how the species composition is changing select a community below."),
                                          conditionalPanel("input.optTimeScenComm == 'rcp45'",
                                                           radioButtons("CommBlocks45", "Choose a community:",
                                                                        choiceNames =
                                                                          list(icon("square","C1"),#light gray 12
                                                                               icon("square","C2"),#dark gray 6
                                                                               icon("square","C3"),#light teal 16
                                                                               icon("square","C4"),#dark teal 5
                                                                               icon("square","C5"),#light lavendar 9
                                                                               icon("square","C6"),#dark lavendar 11
                                                                               icon("square","C7"),#light brown 13
                                                                               icon("square","C8"),#dark brown 19
                                                                               icon("square","C9"),#light blue 18
                                                                               icon("square","C10"),#dark blue 4
                                                                               icon("square","C11"),#light green 7
                                                                               icon("square","C12"),#dark green 14
                                                                               icon("square","C13"),#light red 3
                                                                               icon("square","C14"),#dark red 2
                                                                               icon("square","C15"),#light orange 20
                                                                               icon("square","C16"),#dark orange 1
                                                                               icon("square","C17"),#light purple 17
                                                                               icon("square","C18"),#dark purple 15
                                                                               icon("square","C19"),#light yellow 8
                                                                               icon("square","C20")),#dark yellow 10
                                                                        choiceValues =
                                                                          list(12,6,16,5,9,11,13,19,18,4,7,14,3,2,20,1,17,15,8,10),#12,6,16,5,9,11,13,19,18,4,7,14,3,2,20,1,17,15,8,10
                                                                        inline = T,
                                                                        selected = 1)),
                                          conditionalPanel("input.optTimeScenComm == 'rcp85'",
                                                           radioButtons("CommBlocks85", "Choose a community:",
                                                                        choiceNames =
                                                                          list(icon("square","D1"),#light gray 13
                                                                               icon("square","D2"),#dark gray 12
                                                                               icon("square","D3"),#light teal 16
                                                                               icon("square","D4"),#dark teal 6
                                                                               icon("square","D5"),#light lavendar 9
                                                                               icon("square","D6"),#dark lavendar 7
                                                                               icon("square","D7"),#light brown 14
                                                                               icon("square","D8"),#dark brown 15
                                                                               icon("square","D9"),#light blue 20
                                                                               icon("square","D10"),#dark blue 5
                                                                               icon("square","D11"),#light green 8
                                                                               icon("square","D12"),#dark green 2
                                                                               icon("square","D13"),#light red 10
                                                                               icon("square","D14"),#dark red 18
                                                                               icon("square","D15"),#light orange 1
                                                                               icon("square","D16"),#dark orange 19
                                                                               icon("square","D17"),#light purple 4
                                                                               icon("square","D18"),#dark purple 11
                                                                               icon("square","D19"),#light yellow 17
                                                                               icon("square","D20")),#dark yellow 3
                                                                        choiceValues =
                                                                          list(13,12,16,6,9,7,14,15,20,5,8,2,10,18,1,19,4,11,17,3),
                                                                        inline = T,
                                                                        selected = 1)),
                                          
                                          tags$style(".C1 {color:#cccccc"),
                                          tags$style(".C2 {color:#686868"),
                                          tags$style(".C3 {color:#9ed7c2"),
                                          tags$style(".C4 {color:#00a884"),
                                          tags$style(".C5 {color:#e8beff"),
                                          tags$style(".C6 {color:#c500ff"),
                                          tags$style(".C7 {color:#d7c29e"),
                                          tags$style(".C8 {color:#895a44"),
                                          tags$style(".C9 {color:#a6cee3"),
                                          tags$style(".C10 {color:#1f79b5"),
                                          tags$style(".C11 {color:#b1de8a"),
                                          tags$style(".C12 {color:#33a12b"),
                                          tags$style(".C13 {color:#fa9a98"),
                                          tags$style(".C14 {color:#e3191c"),
                                          tags$style(".C15 {color:#fcbf6f"),
                                          tags$style(".C16 {color:#ff8000"),
                                          tags$style(".C17 {color:#cab2d6"),
                                          tags$style(".C18 {color:#693d99"),
                                          tags$style(".C19 {color:#ffff99"),
                                          tags$style(".C20 {color:#a8a800"),
                                          tags$style(".D1 {color:#cccccc"),
                                          tags$style(".D2 {color:#686868"),
                                          tags$style(".D3 {color:#9ed7c2"),
                                          tags$style(".D4 {color:#00a884"),
                                          tags$style(".D5 {color:#e8beff"),
                                          tags$style(".D6 {color:#c500ff"),
                                          tags$style(".D7 {color:#d7c29e"),
                                          tags$style(".D8 {color:#895a44"),
                                          tags$style(".D9 {color:#a6cee3"),
                                          tags$style(".D10 {color:#1f79b5"),
                                          tags$style(".D11 {color:#b1de8a"),
                                          tags$style(".D12 {color:#33a12b"),
                                          tags$style(".D13 {color:#fa9a98"),
                                          tags$style(".D14 {color:#e3191c"),
                                          tags$style(".D15 {color:#fcbf6f"),
                                          tags$style(".D16 {color:#ff8000"),
                                          tags$style(".D17 {color:#cab2d6"),
                                          tags$style(".D18 {color:#693d99"),
                                          tags$style(".D19 {color:#ffff99"),
                                          tags$style(".D20 {color:#a8a800"),
                                          
                                          selectInput("VarsCommonSci", "Species name options", varsGObetas3),
                                          tags$h5(htmlOutput("textCommComp")),
                                          tags$h5("The abundance-weighted habitat suitability of species within each community is also expected to change. For example, certain species might become more
                                                  abundant, whereas other species may become rarer, even though overall the community composition indicates that this future community is similar enough to
                                                  the current community to be considered the same. 
                                                  The figure below shows mean change in abundance-weighted habitat suitability from present for species within the selected
                                                  community. Only species with the highest predicted change in abundance-weighted habitat suitability are shown. Units depend on the
                                                  taxa: estimates of abundance per plot (counts, basal area, or cover)"),
                                          plotOutput("plotCommComp")
                                   ),
                                   column(6,
                                          tags$h3("Spatial Shifts in Communities"),
                                          tags$h5("2018"),
                                          plotOutput("mapCommunitiesHist"),
                                          #leafletOutput("mapCommunitiesHist",height = 300, width = 600),
                                          tags$h5("Future"),
                                          plotOutput("mapCommunitiesFut"),
                                          tags$style(type="text/css",
                                                     ".shiny-plot-output { text-align: center; height: auto }"
                                          )
                                          #leafletOutput("mapCommunitiesFut",height = 300, width = 600)
                                   )
                                 )
                        ),
                        tabPanel("Species",
                                 sidebarLayout(position = "left",
                                               sidebarPanel(
                                                 selectInput("taxa", "Input: Biodiversity data options", varsTaxa),
                                                 selectInput("covars", "Input: Environmental data options", covars),
                                                 #actionButton("buttonMaps", "View habitat suitability maps",
                                                 #icon = icon("th"), 
                                                 # onclick ="window.open('https://mapspbgjam.env.duke.edu/mapper/')"),
                                                 tags$h3(textOutput("bioTextPlot")),
                                                 
                                                 #selectInput("optG2", "Graphical model output", varsGO),
                                                 conditionalPanel("input.conditionedPanels == 'Accuracy'",
                                                                  helpText("Model Accuracy in- and out-of-sample. Plots may take a few seconds to load. "),
                                                                  tags$h5(textOutput("unitTextPlot")),
                                                                  selectInput("GOacc", "Accuracy Metric", varsGOacc),
                                                                  selectInput("GOacc2", "Options", varsGOacc2),
                                                                  conditionalPanel("input.GOacc == 'SSA' & input.taxa != 'NEON_Beetles'",
                                                                                   selectInput("GOaccNames", "Species name options", varsGObetas3)),
                                                                  conditionalPanel("input.GOacc == 'SSA' & input.taxa == 'NEON_Beetles'",
                                                                                   selectInput("GOaccNames", "Species name options", varsGObetas3b)),
                                                                  conditionalPanel("input.GOacc == 'SSA'",
                                                                                   selectInput("GOacc3", "Species", varsGOacc3))
                                                 ),
                                                 conditionalPanel("input.conditionedPanels == 'Regression coefficients'",
                                                                  helpText("Mean posterior estimates: Fitted coefficients having 95% intervals that exclude zero. Use the dropdown menus below 
                                                                 (1) to display regression coefficients for an explanatory variable in the model, (2) to color-code each species by a trait, or (3) to switch between common and scientific name.
                                                                 Environmental variables explain variation in species abundance across the map. Positive values ['Mean posterior estimate'] mean that higher values of a variable (e.g. high elevations) are associated with higher abundance, and vice versa."),
                                                                 selectInput("GObetas1", "Environmental Variable", varsGObetas1),
                                                                 selectInput("GObetas2", "Color-coded by trait", varsGObetas2),
                                                                 conditionalPanel("input.taxa != 'FIA_trees'",
                                                                                  uiOutput("traitsText")),
                                                                 p(),
                                                                 conditionalPanel("input.taxa != 'NEON_Beetles'",
                                                                                  selectInput("GObetas3", "Species name options", varsGObetas3)),
                                                                 conditionalPanel("input.taxa == 'NEON_Beetles'",
                                                                                  selectInput("GObetas3b", "Species name options", varsGObetas3b))),
                                                 conditionalPanel("input.conditionedPanels == 'Sensitivity'",
                                                                  helpText("Variable Importance: Sensitivity coefficients integrate the effects of predictors across all responses (species) in the model.
                                                                 Variables with high sensitivity account for much of the variation in the community of species.")),
                                                 conditionalPanel("input.conditionedPanels == 'Communities'",
                                                                  helpText(HTML("Commonalities in responses across species help to define communities on the basis of response 
                                                                 rather than distribution or abundance. The clustering of responses here can help us rethink 
                                                                 community relationships across these diverse species groups. Species are clustered using the 
                                                                 <b><i>E</i></b> matrix, which is the correlation among species in terms of their responses to the environment.
                                                                 The figure to the right combines 
                                                                 the effects of individual species responses with covariance in the environment.
                                                                 Species are displayed as both rows and columns. Warmer colors indicate species that have similar responses to environmental variables. 
                                                                 Individual species' responses to environmental variables are shown in the far right column section as the fitted coefficient matrix <b><i>B</i></b>."))),
                                                 conditionalPanel("input.conditionedPanels == 'Sensitivity & Regression coef'",
                                                                  helpText("Sensitivity of predictors compared to mean posterior estimates: In this figure to the right the warmer colors along 
                                                                 the diagonal of F indicate covariates with higher sensitivities.  Sensitivity coefficients integrate the effects of 
                                                                 predictors across all responses (species) in the model."))),
                                               mainPanel(
                                                 tags$style(type="text/css",
                                                            ".shiny-output-error { visibility: hidden; }",
                                                            ".shiny-output-error:before { visibility: hidden; }"
                                                 ),
                                                 tabsetPanel(type = "tabs",
                                                             #tabPanel("Maps"),
                                                             tabPanel("Regression coefficients", uiOutput('ui_plot')),
                                                             tabPanel("Sensitivity", plotOutput("plot2sens")),
                                                             tabPanel("Communities", plotOutput("plot2comm")),
                                                             tabPanel("Sensitivity & Regression coef", plotOutput("plot2FbyB")),
                                                             tabPanel("Accuracy", 
                                                                      plotOutput("plot2Acc")),
                                                             id = "conditionedPanels")
                                               ))))
             
             
  )


# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  # neon -----------------------------------------------------------------------
  
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
  
  
  
  
  ## Column 1/species plots -----------------------------------------
  
  
  # get species-specific value range for map color palette
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
      addWMSTiles('https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png', 
                  attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
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
  
  
  
  
  
  
  
  ## Column 2/site plots --------------------------------
  
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
  
  
  # get species-specific value range for abundance color palette
  z_range <- reactive({
    max(max(sei_plot()[,z_3d()], na.rm = TRUE), abs(min(sei_plot()[,z_3d()], na.rm = TRUE)))
    
  })
  
  #color by palette based on variable (to match map color scale)
  z_color <- reactive({
    
    if(input$choose_color == "gap.frac.10"){
      return(list(
        color=as.formula(paste0("~", z_3d())),
        colorscale = "Greens",
        cmin = 0,
        cmax = 1,
        reversescale = F,
        line=list(width=0),
        colorbar=list(title=names(choiceVal1)[choiceVal1 == input$choose_color], 
                               limits = c(min(sei_plot()[,z_3d()]), max(sei_plot()[,z_3d()])))
      ))
      #colorNumeric(palette = "Greens", domain = sei_plot()[,z_3d()], reverse = TRUE)
    } 
    
    else if(input$choose_color == "abundance"){
      return(list(
        color=as.formula(paste0("~", z_3d())),
        colorscale = "RdBu",
        cmin = -z_range(),
        cmax = z_range(),
        reversescale = F,
        line=list(width=0),
        colorbar=list(title=names(choiceVal1)[choiceVal1 == input$choose_color], 
                      limits = c(min(sei_plot()[,z_3d()]), max(sei_plot()[,z_3d()])))
      ))
      
      # colorBin(palette = "RdBu", domain = c(-z_range(),
      #                                       z_range()),
      #          reverse = TRUE)
      
    } else if(input$choose_color == "def"){
      return(list(
        color=as.formula(paste0("~", z_3d())),
        colorscale = "RdBu",
        cmin = -max(sei_plot()[,z_3d()], na.rm = TRUE),
        cmax = max(sei_plot()[,z_3d()], na.rm = TRUE),
        reversescale = F,
        line=list(width=0),
        colorbar=list(title=names(choiceVal1)[choiceVal1 == input$choose_color], 
                      limits = c(min(sei_plot()[,z_3d()]), max(sei_plot()[,z_3d()])))
      ))
      
      # colorBin(palette = "RdBu", domain = c(-max(sei_plot()[,z_3d()], na.rm = TRUE),
      #                                       max(sei_plot()[,z_3d()], na.rm = TRUE)),
      #          reverse = TRUE)
      
    } else {
      
      return(list(
        color=as.formula(paste0("~", z_3d())),
        colorscale = "Reds",
        cmin = min(sei_plot()[,z_3d()], na.rm = TRUE),
        cmax = max(sei_plot()[,z_3d()], na.rm = TRUE),
        reversescale = F,
        line=list(width=0),
        colorbar=list(title=names(choiceVal1)[choiceVal1 == input$choose_color], 
                      limits = c(min(sei_plot()[,z_3d()]), max(sei_plot()[,z_3d()])))
      ))
      #colorNumeric(palette = "Reds", domain = sei_plot()[,z_3d()])
    }
    
  })
  
  
  output$choose_scatter <- renderPlotly({
    
    sei_plot_bounds() %>% 
      plot_ly(x = as.formula(paste0("~", x_3d())), y = as.formula(paste0("~", y_3d())), 
              #color = as.formula(paste0("~", z_3d())),
              size = 4,
              #colors = z_color(),
              marker = z_color(),
              hovertemplate =  paste("%{x},%{y}<br>","Site:", .$plot.ID, "<extra></extra>"),
              type = "scatter", mode = "markers") %>%
      plotly::layout(yaxis = list(title = names(choiceVal1)[choiceVal1 == input$choose_y], 
                                  range = c(min(sei_plot()[,y_3d()]), 
                                            max(sei_plot()[,y_3d()]))),
                     xaxis = list(title = names(choiceVal1)[choiceVal1 == input$choose_x], 
                                  range = c(min(sei_plot()[,x_3d()]), 
                                            max(sei_plot()[,x_3d()])))
      )# %>%
      #colorbar(title=names(choiceVal1)[choiceVal1 == input$choose_color], limits = c(min(sei_plot()[,z_3d()]), max(sei_plot()[,z_3d()])))
    
    
    
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
  
  
  
  observeEvent(input$plotVec, {
    
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
    })

    
  })
  
  
  # Variable description table
 output$var_description <- renderTable(var_table)
  
  
  
  # maps ------------------------------------------------------------------------------
  
 ## species map --------------------------------------------------------------
  
 # Jump to maps tab from homepage buttons
 
 observeEvent(input$beetleMaps, {
   
   updateTabsetPanel(session, "nav",
                     selected = "Species Maps")
   
   updateRadioGroupButtons(session, "taxa1",
                     selected = "NEON_Beetles")
 })
 
 observeEvent(input$birdMaps, {
   
   updateTabsetPanel(session, "nav",
                     selected = "Species Maps")
   
   updateRadioGroupButtons(session, "taxa1",
                           selected = "BBS-NEON_Breeding-Birds")
 })
 
 observeEvent(input$mammalMaps, {
   
   updateTabsetPanel(session, "nav",
                     selected = "Species Maps")
   
   updateRadioGroupButtons(session, "taxa1",
                           selected = "NEON_Small-Mammals")
 })
 
 observeEvent(input$treeMaps, {
   
   updateTabsetPanel(session, "nav",
                     selected = "Species Maps")
   
   updateRadioGroupButtons(session, "taxa1",
                           selected = "FIA_trees")
 })
 
 
  #update species list based on selected taxa
  observe({
    
    if (input$taxa1 %in% c("NEON_Beetles", "FIA_trees")) {
      
      specUpdate <-
        read.csv(paste0("data/filesAll/mapList_", input$taxa1, ".csv"),
                 stringsAsFactors = F)
      
      charNames <- 
        gsub("[.]", " ", specUpdate[, "scientificName"])
      
    } else {
      specUpdate <-
        read.csv(paste0("data/filesAll/speciesList_", input$taxa1, ".csv"),
                 stringsAsFactors = F)
      
      charNames <- 
        gsub("[.]", " ", specUpdate[, "scientificName"])

    }
    
    updateSelectInput(session, "specs1",
                      choices = c("", charNames), # default no species selected
                      #choices = charNames, # default first species selected
                      
                      #selected = charNames[1]
                      )
    #selected = gsub("[.]"," ",specUpdate[order(specUpdate$colSums, decreasing = T),"scientificName"][1])) # remove selection
  })
  
  # Add selected species name as header
  output$spText <- renderText(
    if(input$specs1 == "s1"){
      return(NULL)
    } else {
      input$specs1
    })
  
  # Add taxa-specific habitat suitability definition
  output$abunText <- renderText(
    if(input$specs1 == "s1"){
      return(NULL)
    } else if(input$taxa1 == "FIA_trees"){
      return("Abundance = basal area (m^2) per hectare")
    } else if(input$taxa1 == "NEON_Small-Mammals"){
      return("Abundance = Small Mammals per 100 trap nights")
    } else if(input$taxa1 == "BBS-NEON_Breeding-Birds"){
      return("Abundance = Breeding Birds per point count of 150 minutes")
    } else {
      return("Abundance = Beetles per trap night over a year (i.e., summation of abundance divided by the total number of trap-nights over a year")
    }
  )
  
  #read in raster map (OLD)
  
  # folder <- reactive({
  #   
  #   if(input$year1 == "historical"){
  #     return("historical/")
  #   } else {
  #     return(paste0(
  #       input$rcp1,
  #       "_",
  #       input$year1,
  #       "/"))
  #   }
  #   
  #   
  # })
  # 
  # reactiveRas <- reactive({
  #   
  #   if (input$specs1 == "s1") {
  #     return(NULL)
  #   } else {
  #     
  #     raster(paste0(
  #       "data/",
  #       folder(),
  #       "R_SPQR_",
  #       gsub(" ", ".", input$specs1),
  #       ".tif"))
  #   }
  #   
  # })
  
  #get tile map URL
  tileMap <- reactive({
    
    url <- paste0("https://tiles.arcgis.com/tiles/swlKRWoduvVuwMcX/arcgis/rest/services/", input$mapVar, "_")
    
    if (input$specs1 == "s1") {
      return(NULL)
    
      } else if (input$year1 == 'Hist') {
      
      return(paste0(url, 'Hist_', gsub(" ", "_", input$specs1), "/MapServer/tile/{z}/{y}/{x}"))
      
        } else {
          
          return(paste0(url, input$rcp1, "_", input$year1, "_", gsub(" ", "_", input$specs1), "/MapServer/tile/{z}/{y}/{x}"))
          
    }
    
    
  })
  
  
  #set color palette for mapping based on PBGJAM colors/categories
  # pal <- reactive({
  #   
  #   if (is.null(reactiveRas())) {
  #     return(NULL)
  #   } else {
  #     colorBin(palette = c("#6246A5","#4C87B3", "#77C7A4", "#BEE7A5", "#EDFEAE",
  #                          "#FBF1A9", "#FDC473", "#F67D52", "#D44853", "#990745"), 
  #              values(reactiveRas()), 
  #              bins = c(0,1,2,3,4,5,6,7,8,9,Inf), na.color = "transparent")
  #   }
  # }) 

  #this updates the map only when species is changed
  #observeEvent(input$specs1, {
  
  #reactive color palette for legend
  colors <- reactive({
    if(input$mapVar == "Mean"){
      return(c("#6246A5","#4C87B3", "#77C7A4", "#BEE7A5", "#EDFEAE",
               "#FBF1A9", "#FDC473", "#F67D52", "#D44853", "#990745"))
    } else {
      return(c("#ffffe5", "#fff8c2", "#ffe89b", "#ffcf67", "#ffad3a",
               "#f78821", "#e3670b", "#c24700", "#94310b", "#67260b"))
    }
  })
  
  labels <- reactive({
    if(input$mapVar == "Mean"){
      
      if(input$taxa1 == "FIA_trees"){
        return(c("< = 0.005",
                 "0.005 - 0.011",
                 "0.011 - 0.025",
                 "0.025 - 0.056",
                 "0.056 - 0.126",
                 "0.126 - 0.282",
                 "0.282 - 0.631",
                 "0.631 - 1.413",
                 "1.413 - 3.162",
                 "> 3.162"
        ))
        
      } else if(input$taxa1 == "BBS-NEON_Breeding-Birds"){
        return(c("0-5",
                 "5-10",
                 "10-15",
                 "15-20",
                 "20-25",
                 "25-30",
                 "30-35",
                 "35-40",
                 "40-45",
                 ">45"
        ))
      } else if(input$taxa1 == "NEON_Small-Mammals") {
        return(c("0-0.25",
                 "0.25-0.5",
                 "0.5-0.75",
                 "0.75-1",
                 "1-1.25",
                 "1.25-1.5",
                 "1.5-1.75",
                 "1.75-2",
                 "2-2.25",
                 ">2.25"
        ))
        
      } else{
        return(c("< = 0.017",
                 "0.017 - 0.031",
                 "0.031 - 0.054",
                 "0.054 - 0.095",
                 "0.095 - 0.167",
                 "0.167 - 0.293",
                 "0.293 - 0.514",
                 "0.514 - 0.903",
                 "0.903 - 1.585",
                 "> 1.585"
        ))
        
      }
      
     
    } else {
      return(c("<=1",
        "1-2",
        "2-3",
        "3-4",
        "4-5",
        "5-6",
        "6-7",
        "7-8",
        "8-9",
        ">9"
      ))
    }
  })
  
  
  # set up base map
  output$map <- renderLeaflet({
    leaflet() %>% 
      # these options allow for switch panel:
      addMapPane("left", zIndex = 0) %>%
      addMapPane("right", zIndex = 0) %>%
      addTiles(options = pathOptions(pane = "left")) %>% 
      setView(lat = 39, lng = -94, zoom = 3.5) 
  })
  
  
  
  observe({  
    
    if (is.null(tileMap())) {
      return(NULL)
    } else {
      leafletProxy("map") %>%
        removeImage("A") %>%
        clearControls() %>%
        #addRasterImage2(reactiveRas(), colors = pal(), layerId = "A", project = TRUE, options = tileOptions(pane = "left")) %>%
        addTiles(tileMap(), layerId = "A", options=tileOptions(maxNativeZoom = 6)) %>% 
        addLegend(
          #pal = pal(),
          #values = values(reactiveRas()),
          # labFormat = function(type, cuts, p) {
          #   paste0(c(
          #     "0-1",
          #     "1-2",
          #     "2-3",
          #     "3-4",
          #     "4-5",
          #     "5-6",
          #     "6-7",
          #     "7-8",
          #     "8-9",
          #     ">9"
          #   ))
          # }
           colors = colors(),
           labels = labels(),
            title = ifelse(input$mapVar == "Mean",
                           HTML("Abundance-weighted<br>Habitat Suitability<br>"),
                           HTML("Uncertainty (RSE)<br>")),
                           
            position = "bottomleft",
            opacity = 1
        )
    }
  })
  
  #this updates when year is changed and adds split view, but since folder/reactiveRas() is the same the two maps are the same
  #observeEvent(input$year1, {
  # observe({
  #   
  #   if(input$splitView == TRUE)
  #     
  #     # if (is.null(reactiveRas())) {
  #     #   return(NULL)
  #     # } else {
  #     leafletProxy("map") %>%
  #     removeImage("B") %>%
  #     #clearControls() %>%
  #     addRasterImage2(reactiveRas(), colors = pal(), layerId = "B", project = TRUE, options = pathOptions(pane = "right")) %>%
  #     #     addLegend(
  #     #       pal = pal(),
  #     #       values = values(reactiveRas()),
  #     #       labFormat = function(type, cuts, p) {
  #     #         paste0(c(
  #     #           "0-1",
  #     #           "1-2",
  #     #           "2-3",
  #     #           "3-4",
  #     #           "4-5",
  #     #           "5-6",
  #   #           "6-7",
  #   #           "7-8",
  #   #           "8-9",
  #   #           ">9"
  #   #         ))
  #   #       }
  #   #       ,
  #   #       title = "Abundance-weighted Habitat Suitability",
  #   #       position = "bottomleft",
  #   #       opacity = 1
  #   #     )
  #   # }
  #   addSidebyside(layerId = "sidecontrols",
  #                 rightId = "baseid",
  #                 leftId = "cartoid")
  #   
  #   if(input$splitView == FALSE)
  #     if (is.null(reactiveRas())) {
  #       return(NULL)
  #     } else {
  #       leafletProxy("map") %>%
  #         removeImage("A") %>%
  #         clearControls() %>%
  #         addRasterImage2(reactiveRas(), colors = pal(), layerId = "A", project = TRUE, options = tileOptions(pane = "left")) %>%
  #         addLegend(
  #           pal = pal(),
  #           values = values(reactiveRas()),
  #           labFormat = function(type, cuts, p) {
  #             paste0(c(
  #               "0-1",
  #               "1-2",
  #               "2-3",
  #               "3-4",
  #               "4-5",
  #               "5-6",
  #               "6-7",
  #               "7-8",
  #               "8-9",
  #               ">9"
  #             ))
  #           }
  #           ,
  #           title = paste0(HTML("Abundance-weighted<br>Habitat Suitability<br>"), input$specs1),
  #           position = "bottomleft",
  #           opacity = 1
  #         )
  #     }
  #   
  #   
  # })
  # 
  
  ## community map --------
  
  #jump to communities models
  observeEvent(input$jumpToComm, {
    updateTabsetPanel(session, "nav",
                      selected = "Communities")
  })
  
  #get communities layer
  
  commMap <- reactive({

     if(input$rcp2 == "rcp45"){
    
       if(input$year2 == "hist"){
         m <- layers[[1]]
       } else if(input$year2 == "2040"){
         m <- layers[[2]]
       } else if(input$year2 == "2070"){
         m <- layers[[3]]
       }
       
       m_filter <- m %>% 
         filter(Name %in% input$commBlocks45_2)
       
     } else if(input$rcp2 == "rcp85"){
       if(input$year2 == "hist"){
         m <- layers[[4]]
       } else if(input$year2 == "2040"){
         m <- layers[[5]]
       } else if(input$year2 == "2070"){
         m <- layers[[6]]
       }
       
       m_filter <- m %>% 
         filter(Name %in% input$commBlocks85_2)
    
     }
    
    
    return(m_filter)

  })

  commPal <- reactive({
    
    if(input$rcp2 == 'rcp45'){
      
      colorFactor(c("#cccccc",
                    "#686868",
                    "#9ed7c2",
                    "#00a884",
                    "#e8beff",
                    "#c500ff",
                    "#d7c29e",
                    "#895a44",
                    "#a6cee3",
                    "#1f79b5",
                    "#b1de8a",
                    "#33a12b",
                    "#fa9a98",
                    "#e3191c",
                    "#fcbf6f",
                    "#ff8000",
                    "#cab2d6",
                    "#693d99",
                    "#ffff99",
                    "#a8a800"), 
                  names45)

    } else {
      
      colorFactor(c("#cccccc",
                    "#686868",
                    "#9ed7c2",
                    "#00a884",
                    "#e8beff",
                    "#c500ff",
                    "#d7c29e",
                    "#895a44",
                    "#a6cee3",
                    "#1f79b5",
                    "#b1de8a",
                    "#33a12b",
                    "#fa9a98",
                    "#e3191c",
                    "#fcbf6f",
                    "#ff8000",
                    "#cab2d6",
                    "#693d99",
                    "#ffff99",
                    "#a8a800"), 
                  names85)
      
    }
    
  })
    
  
  # set up base map
  output$map2 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lat = 39, lng = -94, zoom = 3.5) %>% 
      addPolygons(data = commMap(), group = "communities", color = ~commPal()(Name),
                  stroke = FALSE, fillOpacity = 1)
  })
  
  #  observeEvent(input$commLayers, {
  #    
  #    leafletProxy("map2") %>%
  #        clearGroup("communities")
  #             #clearControls() %>%
  #             #addRasterImage2(reactiveRas(), colors = pal(), layerId = "A", project = TRUE, options = tileOptions(pane = "left")) %>%
  #            addPolygons(data = filter(commMap(), Name %in% input$CommBlocks2),  color = ~commPal(Name),
  #                        stroke = FALSE, fillOpacity = 1) 
  # #  
  #  })
  
  # observe({  
  #   
  #   if (is.null(commMap())) {
  #     return(NULL)
  #   } else {
  #     leafletProxy("map2") %>%
  #       removeImage("A") %>%
  #       #clearControls() %>%
  #       #addRasterImage2(reactiveRas(), colors = pal(), layerId = "A", project = TRUE, options = tileOptions(pane = "left")) %>%
  #       addPolygons(data = commMap(), layerId = "A",  color = ~commPal(Name),
  #                   stroke = FALSE, fillOpacity = 1) 
        #addLegend(
          #pal = pal(),
          #values = values(reactiveRas()),
          # labFormat = function(type, cuts, p) {
          #   paste0(c(
          #     "0-1",
          #     "1-2",
          #     "2-3",
          #     "3-4",
          #     "4-5",
          #     "5-6",
          #     "6-7",
          #     "7-8",
          #     "8-9",
          #     ">9"
          #   ))
          # }
        #   colors = colors(),
        #   labels = labels(),
        #   title = ifelse(input$mapVar == "Mean",
        #                  HTML("Abundance-weighted<br>Habitat Suitability<br>"),
        #                  HTML("Uncertainty (RSE)<br>")),
        #   
        #   position = "bottomleft",
        #   opacity = 1
        # )
  #   }
  # })
  
  
  # models -------------------------------------------------------------------------------
  
  ## go to models page from homepage buttons
  
  observeEvent(input$beetleModels, {
    
    updateTabsetPanel(session, "nav",
                      selected = "Species")
    
    updateSelectInput(session, "taxa",
                      selected = "NEON_Beetles")
  })
  
  observeEvent(input$birdModels, {
    
    updateTabsetPanel(session, "nav",
                      selected = "Species")
    
    updateSelectInput(session, "taxa",
                      selected = "BBS-NEON_Breeding-Birds")
  })
  
  observeEvent(input$mammalModels, {
    
    updateTabsetPanel(session, "nav",
                      selected = "Species")
    
    updateSelectInput(session, "taxa",
                      selected = "NEON_Small-Mammals")
  })
  
  observeEvent(input$treeModels, {
    
    updateTabsetPanel(session, "nav",
                      selected = "Species")
    
    updateSelectInput(session, "taxa",
                      selected = "FIA_trees")
  })
  
  # pulled from Amanda's code
  
  observe({
    
    updateSelectInput(session, "taxa",
                      selected = SelectTaxa)
    updateSelectInput(session, "covars",
                      selected = SelectCovars)
    
    # if (SelectComunities == T){
    #   updateNavbarPage(session, "nav", 
    #                    selected = "Model Outputs: Communities")
    # }
    
  })
  
  observe(output$bioTextPlot <- renderText(paste0("Model outputs for  ",gsub("-"," ",unlist(strsplit(input$taxa,"_"))[2]))))
  
  observe({
    unitDict  <- c("FIA_trees" = ": basal area (m2 per hectare)",
                   "NEON_Small-Mammals" = " per 100 trap-nights",
                   "BBS-NEON_Breeding-Birds" = " per point count of 150 minutes",
                   "NEON_Beetles" = " per 4 pitfall traps over a 14-day collection period")
    output$unitTextPlot <- renderText({paste0("Units: abundance-weighted habitat 
                                             suitability of ",
                                             gsub("-"," ",unlist(strsplit(input$taxa,"_"))[2]),
                                             unitDict[input$taxa])})
  })
  
  observe({
    traitsURLDict  <- c("FIA_trees" = "",
                        "NEON_Small-Mammals" = "http://esapubs.org/archive/ecol/E090/184/metadata.htm",
                        "BBS-NEON_Breeding-Birds" = "https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/13-1917.1",
                        "NEON_Beetles" = "https://data.neonscience.org/documents/-/document_library_display/JEygRkSpUBoq/view_file/2449843?_110_INSTANCE_JEygRkSpUBoq_redirect=https%3A%2F%2Fdata.neonscience.org%2Fdocuments%3Fp_p_id%3D110_INSTANCE_JEygRkSpUBoq%26p_p_lifecycle%3D0%26p_p_state%3Dnormal%26p_p_mode%3Dview%26p_p_col_id%3Dcolumn-1%26p_p_col_count%3D1%26_110_INSTANCE_JEygRkSpUBoq_redirect%3Dhttps%253A%252F%252Fdata.neonscience.org%252Fdocuments%253Fp_p_id%253D110_INSTANCE_JEygRkSpUBoq%2526p_p_lifecycle%253D0%2526p_p_state%253Dnormal%2526p_p_mode%253Dview%2526p_p_col_id%253Dcolumn-1%2526p_p_col_count%253D1%26_110_INSTANCE_JEygRkSpUBoq_searchFolderId%3D0%26_110_INSTANCE_JEygRkSpUBoq_breadcrumbsFolderId%3D11098%26_110_INSTANCE_JEygRkSpUBoq_keywords%3Dground%2Bbeetle%2Bsampling%26_110_INSTANCE_JEygRkSpUBoq_formDate%3D1578344192154%26_110_INSTANCE_JEygRkSpUBoq_repositoryId%3D10179%26_110_INSTANCE_JEygRkSpUBoq_struts_action%3D%252Fdocument_library_display%252Fsearch%26_110_INSTANCE_JEygRkSpUBoq_searchFolderIds%3D11098%26_110_INSTANCE_JEygRkSpUBoq_folderId%3D11098")
    url <- a("To learn more about each trait click here", href=traitsURLDict[input$taxa])
    output$traitsText <- renderUI({tagList(url)})
  })
  
  
  observe({
    
    betaTab <- read.csv(paste0(pathin,"/filesAll/betas_",input$taxa,"_",input$covars,".csv"),stringsAsFactors = F)
    
    characterCols <- unique(betaTab$varLong)
    #characterCols <- characterCols[!characterCols %in% c("intercept")]#c("sin(slope) * sin(aspect)","sin(slope) * cos(aspect)")
    updateSelectInput(session, "GObetas1",
                      choices = characterCols, # update choices
                      selected = characterCols[2]) # remove selection
  })
  
  observe({
    
    ydataTest <- read.csv(paste0(pathin,"/filesAll/speciesList_",input$taxa,".csv"),stringsAsFactors = F)
    
    characterCols2 <- gsub("[.]"," ",ydataTest[,c(input$GOaccNames)])
    updateSelectInput(session, "GOacc3",
                      choices = characterCols2, # update choices
                      selected = gsub("[.]"," ",ydataTest[order(ydataTest$colSums, decreasing = T),c(input$GOaccNames)][1])) # remove selection
  })
  
  observe({
    traitsTab <- read.csv(paste0(pathin,"/filesAll/traits_",input$taxa,"_",input$covars,".csv"),stringsAsFactors = F)
    characterCols3 <- gsub("[.]"," ",names(traitsTab)[4:ncol(traitsTab)])
    updateSelectInput(session, "GObetas2",
                      choices = characterCols3, # update choices
                      selected = characterCols3[1]) # remove selection
    
  })
  
  observe({
    
    if (input$optTimeScenComm == "rcp45"){
      selectedC <- paste0(input$CommBlocks45)
    } else if (input$optTimeScenComm == "rcp85"){
      selectedC <- paste0(input$CommBlocks85)
    }
    
    
    output$textCommComp <- renderText({
      
      tabHFC <- read.csv(paste0(pathin,"/filesAll/A_NEON_Small-Mammals-Beetles-and-Plants_",input$optTimeScenComm,"_M1.csv"),stringsAsFactors = F)
      CChist <- tabHFC[tabHFC$timeID == "2018_2018_hist" & 
                         tabHFC$cluster == selectedC,2:(ncol(tabHFC)-2)]
      CCFut <- tabHFC[tabHFC$timeID == paste0(input$optTimePerComm,"_",input$optTimeScenComm) & 
                        tabHFC$cluster == selectedC,2:(ncol(tabHFC)-2)]
      
      SpH <- gsub("[.]"," ",names(CChist[order(CChist, decreasing = T)][1:5]))
      SpF <- gsub("[.]"," ",names(CCFut[order(CCFut, decreasing = T)][1:5]))
      
      if (input$VarsCommonSci == "vernacularName") {
        SpNames <- read.csv(paste0(pathin,"/filesAll/speciesList_NEON_Combo.csv"),stringsAsFactors = F)[,c("vernacularName","scientificName")]
        
        betaTab3 <- t(SpNames$vernacularName)
        names(betaTab3) <- SpNames$scientificName
        betaTab4 <- as.list(betaTab3)
        SpH <- c(betaTab4[[SpH[1]]],betaTab4[[SpH[2]]],betaTab4[[SpH[3]]],betaTab4[[SpH[4]]],betaTab4[[SpH[5]]])
        SpF <- c(betaTab4[[SpF[1]]],betaTab4[[SpF[2]]],betaTab4[[SpF[3]]],betaTab4[[SpF[4]]],betaTab4[[SpF[5]]])
      }
      
      paste0("For the community selected above, dominant species are currently (2018) ",
             "<font color=\"#000099\"><b>",
             paste(SpH,collapse = ", "),
             "</b></font>",
             ", whereas future dominant species are projected to be ",
             "<font color=\"#000099\"><b>",
             paste(SpF,collapse = ", "),
             "</b></font>")
      
    })
    
  })
  
  observe({
    
    if (input$optTimeScenComm == "rcp45"){
      selectedC2 <- paste0(input$CommBlocks45)
    } else if (input$optTimeScenComm == "rcp85"){
      selectedC2 <- paste0(input$CommBlocks85)
    }
    
    output$plotCommComp <- renderPlot({
      
      tabHFC <- read.csv(paste0(pathin,"/filesAll/A_NEON_Small-Mammals-Beetles-and-Plants_",input$optTimeScenComm,"_M1.csv"),stringsAsFactors = F)
      CChist <- tabHFC[tabHFC$timeID == "2018_2018_hist" & 
                         tabHFC$cluster == selectedC2,2:(ncol(tabHFC)-2)]
      CCFut <- tabHFC[tabHFC$timeID == paste0(input$optTimePerComm,"_",input$optTimeScenComm) & 
                        tabHFC$cluster == selectedC2,2:(ncol(tabHFC)-2)]
      
      if (input$VarsCommonSci == "vernacularName") {
        SpNames <- read.csv(paste0(pathin,"/filesAll/speciesList_NEON_Combo.csv"),stringsAsFactors = F)[,c("vernacularName","scientificName")]
        
        existingH <- match(names(CChist),gsub(" ",".",SpNames$scientificName))
        names(CChist) <- SpNames$vernacularName[existingH]
        
        existingF <- match(names(CCFut),gsub(" ",".",SpNames$scientificName))
        names(CCFut) <- SpNames$vernacularName[existingF]
        
      }
      
      
      if (all(names(CCFut) == names(CChist))){
        
        df <- as.data.frame(t((CCFut-CChist)))
        names(df) <- "change"
        df$name <- rownames(df)
        df <- df[df$change > 0.1 | df$change < -0.1,]
        dfMax <- max(df$change)
        df$placement <- ifelse(df$change < 0, 0.4*(max(c(df$change,10))),0.4*(min(c(df$change,-10))))
        p <- ggplot(data=df, aes(x=reorder(name,change), y=change,label = reorder(name,change))) +
          geom_bar(stat="identity", fill = "#9BCFC2",width=.5) +
          geom_text(data=df, aes(x=reorder(name,change), y=placement,label = gsub("[.]"," ",reorder(name,change)))) +
          ylim(min(c(df$change,-10)), max(c(10,df$change))) +
          coord_flip() +
          labs(y = "Composition shifts: mean change in abundance-weighted habitat suitability from present",
               x = "") +
          theme(panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                panel.background = element_blank(),
                plot.background = element_blank(),
                axis.text = element_text(colour = "black"),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
        p
        
      }
      
    },
    #height = 450,
    bg="transparent")
    
  })
  
  output$plot2Acc <- renderPlot({
    
    qcut = function(x, n) {
      quantiles = seq(0, 1, length.out = n+1)
      cutpoints = unique(unname(quantile(x, quantiles, na.rm = TRUE)))
      as.character(cut(x, cutpoints, include.lowest = TRUE))
    }
    
    if (input$GOacc == 'TOA') {
      
      tabAll2 <- read.csv(paste0(pathin,"/filesAll/TotalAll_",input$GOacc2,"_",input$taxa,"_",input$covars,".csv"),stringsAsFactors = F)
      maxAll <- max(tabAll2$ypredAll,tabAll2$yobsAll)
      par(bty = 'n', cex=1.3, family='serif', pty = "s")
      valMAE <- rmse(tabAll2$yobsAll,tabAll2$ypredAll)
      bp <- ggplot(tabAll2, aes(x = ypredAll, y = yobsAll)) +
        geom_boxplot(fill = "springgreen3", aes(group = ypredAllQ2),outlier.shape = NA) +
        #geom_point(alpha = 0.5, size = 3, color = "blue4", shape = 16) + 
        theme_classic(base_size = 18,base_family = "serif") +
        geom_abline(intercept = 0, slope= 1, linetype = "dashed", color = "black", size = 1.5) +
        scale_y_sqrt(name = "Observed Total Abundance-weighted habitat suitability\n", limits = c(0,maxAll)) + 
        scale_x_sqrt(name = "\nPredicted Total Abundance-weighted habitat suitability", limit = c(0,maxAll)) +
        coord_fixed() +
        theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),axis.text = element_text(colour = "black"))
      bp + annotate("text",x = maxAll*0.2, y = maxAll,label = paste0("RMSE = ",round(valMAE,2)), size = 6)
      
      #plot(ypredAll,yobsAll, xlab = "Predicted Total Abundance", ylab = "Observed Total Abundance", xlim = c(0,maxAll), ylim = c(0,maxAll), pch = 16, cex = 1.3,col=rgb(red=0, green=0, blue=0, alpha=0.25))
      #mtext(paste0("Mean absolute Error = ",round(valMAE,2)),side = 3,cex = 1.3)
      
    } else if (input$GOacc == 'SSA') {
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      progress$set(message = "Making plot", value = 0)
      
      progress$inc(1/3)
      
      ydataTest <- read.csv(paste0(pathin,"/filesAll/speciesList_",input$taxa,".csv"),stringsAsFactors = F)
      
      tabAll1 <- read.csv(paste0(pathin,"/filesAll/SpeciesAll_",input$GOacc2,"_",input$taxa,"_",input$covars,".csv"),stringsAsFactors = F)
      
      spNameReal <- input$GOacc3
      if (input$taxa != "NEON_Beetles"){
        spName <- ifelse(input$GOaccNames == "scientificName",
                         input$GOacc3,
                         ydataTest$scientificName[ydataTest$vernacularName == input$GOacc3])
      } else {
        spName <- input$GOacc3
      }
      progress$inc(2/3)
      nameQ <- paste0("ypredAllQ2_",gsub(" ", ".",spName))
      nameObs <- paste0("yobsAll_",gsub(" ", ".",spName))
      namePred <- paste0("ypredAll_",gsub(" ", ".",spName))
      
      tabAll2 <- tabAll1[,c(nameQ,nameObs,namePred)]
      
      yobs <- tabAll1[,substr(names(tabAll1),1,9) == "ypredAll_"]
      
      yobsTOTALPER <- round((sum(tabAll2[,nameObs])/sum(rowSums(yobs)))*100,1)
      
      maxAll <- max(tabAll2[,2:3])
      valMAE <- rmse(tabAll2[,nameObs],tabAll2[,namePred])
      
      names(tabAll2) <- c("ypredAllQ2","yobsAll","ypredAll")
      progress$inc(3/3)
      Sys.sleep(0.1)
      bp <- ggplot(tabAll2, aes(x = ypredAll, y = yobsAll)) +
        #geom_point(alpha = 0.5, size = 3, color = "blue4", shape = 16) + 
        geom_boxplot(fill = "springgreen3", aes(group = ypredAllQ2),outlier.shape = NA) +
        theme_classic(base_size = 18,base_family = "serif") +
        geom_abline(intercept = 0, slope= 1, linetype = "dashed", color = "black", size = 1.5) +
        scale_y_sqrt(name = "Observed Total Abundance-weighted habitat suitability\n", limits = c(0,maxAll)) +
        scale_x_sqrt(name = "\nPredicted Total Abundance-weighted habitat suitability", limit = c(0,maxAll)) +
        coord_fixed() +
        theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),axis.text = element_text(colour = "black"),
              plot.title = element_text(size=18))
      bp + annotate("text",x = maxAll*0.2, y = maxAll,label = paste0("RMSE = ",round(valMAE,2)), size = 6) + 
        ggtitle(paste0(spNameReal, " represents ",yobsTOTALPER, "% of the total observations"))
      #plot(ypredAll,yobsAll, xlab = "Predicted Abundance", ylab = "Observed Abundance", xlim = c(0,maxAll), ylim = c(0,maxAll), pch = 16, cex = 1.3,col=rgb(red=0, green=0, blue=0, alpha=0.25))
      #mtext(paste0("Mean absolute Error = ",round(valMAE,2)),side = 3,cex = 1.3)
      
      
    } else {
      par(bty = 'n', cex=1, family='serif', pty = "s")
      plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
    }
    
  },height = 600)
  
  output$plot2betas <- renderPlot({
    
    traitsTab <- read.csv(paste0(pathin,"/filesAll/traits_",input$taxa,"_",input$covars,".csv"),stringsAsFactors = F)
    betaTab <- read.csv(paste0(pathin,"/filesAll/betas_",input$taxa,"_",input$covars,".csv"),stringsAsFactors = F)
    
    var9 <- gsub(" ",".",input$GObetas2)
    
    habitats <- unique(traitsTab[,var9])[order(unique(traitsTab[,var9]))]
    coolH <- rainbow_hcl(length(habitats), start = 30)
    
    betaTab$col <- "black"
    
    for (k in 1:length(habitats)){
      betaTab$col[betaTab$scientificName %in% traitsTab$scientificName[traitsTab[,var9] == habitats[k]]] <- coolH[k]
    }
    
    spNames <- ifelse(input$taxa == "NEON_Beetles",input$GObetas3b,input$GObetas3)
    
    mat_var <- betaTab[betaTab$varLong == input$GObetas1,]
    
    mat_var2 <- mat_var[which(mat_var$sig95 == "*"),]
    mat_var2 <- mat_var2[order(mat_var2$Estimate),]
    indexP <- mat_var2$Estimate > 0
    
    par(bty = 'n', cex=1.3, family='serif')
    plot(mat_var2$Estimate,c(1:length(mat_var2$Estimate)), pch = 16, xlab = "Mean posterior estimate", ylab = "",col = mat_var2$col,yaxt = 'n', ylim = c(-1,length(mat_var2$Estimate)+3),xlim = c(min(min(mat_var2$CI_025)*1.4,max(mat_var2$CI_975)*-1),max(max(mat_var2$CI_975)*1.4,min(mat_var2$CI_025)*-1)), cex = 1.3, cex.lab = 1.3, cex.axis = 1.3)
    abline(v = 0)
    arrows(mat_var2$CI_025,c(1:length(mat_var2$Estimate)), mat_var2$CI_975,c(1:length(mat_var2$Estimate)), col = mat_var2$col,length=0.05, angle=90, code=3, cex = 1.3)
    
    if (length(mat_var2$CI_975[!indexP]) != 0){
      text(mat_var2$CI_975[!indexP],c(1:length(mat_var2$Estimate))[!indexP], labels = gsub("[.]"," ",mat_var2[!indexP,spNames]),pos = 4, srt = 0, adj = c(0,0), offset = 4, xpd = T, col = mat_var2$col[!indexP], cex = 1.3)
    }
    if (length(mat_var2$CI_025[indexP]) != 0){
      text(mat_var2$CI_025[indexP],c(1:length(mat_var2$Estimate))[indexP], labels = gsub("[.]"," ",mat_var2[indexP,spNames]),pos = 2, srt = 0, adj = c(0,0), offset = 4, xpd = T, col = mat_var2$col[indexP], cex = 1.3)
    }
    
    ##text(mat_var2$CI_975,c(1:length(mat_var2$Estimate)), labels = mat_var2[,input$GObetas3],pos = 4, srt = 0, adj = c(0,0), offset = 6, xpd = T, col = mat_var2$col, cex = 1.3)
    legend("topleft",
           legend = habitats,
           text.col=coolH, bty = "n", horiz = T, cex = 1.2)
    
  })
  
  output$ui_plot <- renderUI({
    betaTab <- read.csv(paste0(pathin,"/filesAll/betas_",input$taxa,"_",input$covars,".csv"),stringsAsFactors = F)
    plotOutput("plot2betas",20*length(unique(betaTab$name)), width = "100%")
  })
  
  output$plot2sens <- renderImage({
    
    list(src = normalizePath(file.path(paste0(pathin,"/filesAll/"),paste0("sens_",input$taxa,"_",input$covars,".png"))),
         width = 600,
         height = 600)
  }, deleteFile = FALSE)
  
  output$plot2comm <- renderImage({
    
    list(src = normalizePath(file.path(paste0(pathin,"/filesAll/"),paste0("cluster_",input$taxa,"_",input$covars,".PNG"))),
         width = 600,
         height = 600)
  }, deleteFile = FALSE)
  
  output$plot2FbyB <- renderImage({
    list(src = normalizePath(file.path(paste0(pathin,"/filesAll/"),paste0("FbyB_",input$taxa,"_",input$covars,".PNG"))),
         width = 600,
         height = 600)
  }, deleteFile = FALSE)
  
  output$mapCommunitiesHist <- renderImage({
    
    list(src = normalizePath(file.path(paste0(pathin,"/filesAll/"),paste0("comm_",input$optTimeScenComm,"_2018.PNG"))),
         width = 600)
  }, deleteFile = FALSE)
  
  output$mapCommunitiesFut <- renderImage({
    
    list(src = normalizePath(file.path(paste0(pathin,"/filesAll/"),paste0("comm_",input$optTimeScenComm,"_",substr(input$optTimePerComm,1,4),".PNG"))),
         width = 600)
  }, deleteFile = FALSE)
  
  
}

             
# Run the application 
shinyApp(ui = ui, server = server)



