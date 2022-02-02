library(shiny)
library(leaflet)
library(colorspace)
library(leaflet.esri)
library(shinydashboard)
library(htmlwidgets)
library(RColorBrewer)
library(shinythemes)
library(htmltools)
library(ggplot2)
library(Metrics)

#Specify path to server not local files, when uploading to server
pathin <- "C:/Users/aschw/Documents/GitHub/NEON-GJAM/shiny_app"
#pathin <- "/srv/shiny-server"

#When uploading, create a new copy of each app, with the right taxa and covariates selected
#specific to each folder. Basically, uncomment the right selection for each of the 8 model runs.

#For the communities app, set SelectComunities <- T

#then copy paste the whole file structure onto the AWS server

SelectComunities <- F

# SelectTaxa <- "FIA_trees"
# SelectCovars <- "RS"

# SelectTaxa <- "FIA_trees"
# SelectCovars <- "CL"

# SelectTaxa <- "NEON_Small-Mammals"
# SelectCovars <- "RS"

# SelectTaxa <- "NEON_Small-Mammals"
# SelectCovars <- "CL"

# SelectTaxa <- "BBS-NEON_Breeding-Birds"
# SelectCovars <- "RS"

# SelectTaxa <- "BBS-NEON_Breeding-Birds"
# SelectCovars <- "CL"

# SelectTaxa <- "NEON_Beetles"
# SelectCovars <- "RS"

SelectTaxa <- "NEON_Beetles"
SelectCovars <- "CL"

esriPlugin <- htmlDependency("leaflet.esri", "2.24",
                             src = c(href = "https://unpkg.com/esri-leaflet@2.2.4/dist/"),
                             script = "esri-leaflet.js")

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

varsTaxa  <- c("Trees" = "FIA_trees",
               "Small Mammals" = "NEON_Small-Mammals",
               "Breeding Birds" = "BBS-NEON_Breeding-Birds",
              "Beetles" = "NEON_Beetles")

varsSpecies <- c("sp1" = "s1",
                 "sp2" = "s2")

# varsGOacc <- c("Species-specific abundance" = "SSA",
#                "Total abundance" = "TOA")

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

covars <- c("climate + topography + soils" = "CL",
            "remotely sensed imagery + topography + soils" = "RS")

varsTime <- c("2018",
              "Future Projections")

varsTimeScen <- c("RCP 4.5" = "rcp45",
                  "RCP 8.5" = "rcp85")

varsTimePer <- c("2040-2069" = "2040_2069",
                  "2070-2099" = "2070_2099")

ui <- navbarPage("PBGJAM", id="nav",
          theme = shinytheme("sandstone"),
          
          tabPanel("Model Outputs: Plots",
                       #titlePanel(tags$b("Model outputs")),
                       
                       sidebarLayout(position = "left",
                                     sidebarPanel(
                                       selectInput("taxa", "Input: Biodiversity data options", varsTaxa),
                                       selectInput("covars", "Input: Environmental data options", covars),
                                       actionButton("buttonMaps", "View habitat suitability maps",
                                                  icon = icon("th"), 
                                                  onclick ="window.open('https://mapspbgjam.env.duke.edu/mapper/')"),
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
                                                                 (1) to display regression coefficients for an explanatory variable in the model, (2) to color-code each species by a trait, or (3) to switch between common and scientific name."),
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
                                                        helpText("Variable Importance: Sensitivity coefficients integrate the effects of predictors across all responses (species) in the model.")),
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
                                                   tabPanel("Regression coefficients", uiOutput('ui_plot')),
                                                   tabPanel("Sensitivity", plotOutput("plot2sens")),
                                                   tabPanel("Communities", plotOutput("plot2comm")),
                                                   tabPanel("Sensitivity & Regression coef", plotOutput("plot2FbyB")),
                                                   tabPanel("Accuracy", 
                                                            plotOutput("plot2Acc")),
                                                            id = "conditionedPanels")
                                       ))),
          tabPanel("Model Outputs: Communities",
                   
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
                                   helpText("Communities are identified using kmeans.
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
                                          leafletOutput("mapCommunitiesHist",height = 300, width = 600),
                                          tags$h5("Future"),
                                          leafletOutput("mapCommunitiesFut",height = 300, width = 600)
                                   )
                                   )
                                 )
          # tabPanel("More Info",
          #          titlePanel(tags$b("Welcome to Predicting Biodiversity with Generalized Joint Attribute Models (PBGJAM)")),
          #          
          #          sidebarLayout(position = "left",
          #                        sidebarPanel(
          #                          p("PBGJAM is an interactive web-based tool that provides 
          #                            forecasts of species and community responses to climate change for
          #                            birds, plants, small mammals, and insects."),
          #                          p("Select the options to the left to explore model accuracy and other outputs 
          #                            for your taxa of interest."),
          #                          p("To return to viewing species distribution/community maps click here")),
          #                        mainPanel(
          #                          
          #                          br(),
          #                          br(),
          #                          br(),
          #                          br(),
          #                          br(),
          #                          br(),
          #                          br(),
          #                          br()
          #                        )))
          
)

server <- function(input, output, session) {
  
  observe({
    
    updateSelectInput(session, "taxa",
                      selected = SelectTaxa)
    updateSelectInput(session, "covars",
                      selected = SelectCovars)
    
    if (SelectComunities == T){
      updateNavbarPage(session, "nav", 
                       selected = "Model Outputs: Communities")
    }
    
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
    
  },height = 450,bg="transparent")
  
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
  
  output$mapCommunitiesHist <- renderLeaflet({
    
    AWStab <- read.csv(paste0(pathin,"/filesAll/AWSlink3_NEON_Small-Mammals-Beetles-and-Plants_",input$optTimeScenComm,"_M1.csv"),stringsAsFactors = F)

    layerNum <- AWStab$LayerNum[(AWStab$speciesName == "cluster") &
                                    (AWStab$ModelScen == "current")]
    
    leaflet(options = leafletOptions(zoomSnap = 0.1, 
                                        zoomDelta = 0.1)) %>%
      setView(lng = -95, lat = 38, zoom = 3.6) %>%
      addEsriBasemapLayer(esriBasemapLayers$Topographic, autoLabels = T) %>%
      
      registerPlugin(esriPlugin) %>%
      #onRender(paste0("function(el, x) {L.esri.dynamicMapLayer({url: 'https://mapspbgjam.env.duke.edu/arcgis/rest/services/pbgjam/",gsub("-","",unlist(strsplit(input$taxaComm,"_"))[2]),input$covarsComm,"b/MapServer',layers: [",layerNum,"],opacity: 0.8}).addTo(this);}")) %>%
      onRender(paste0("function(el, x) {L.esri.dynamicMapLayer({url: 'https://mapspbgjam1.env.duke.edu/arcgis/rest/services/pbgjam/comm_layers5_split/MapServer',layers: [",layerNum,"],opacity: 0.8}).addTo(this);}")) %>%
      
      
      addScaleBar(position = "bottomleft")
  })
  
  output$mapCommunitiesFut <- renderLeaflet({
    
    AWStab <- read.csv(paste0(pathin,"/filesAll/AWSlink3_NEON_Small-Mammals-Beetles-and-Plants_",input$optTimeScenComm,"_M1.csv"),stringsAsFactors = F)
    
    layerNum <- AWStab$LayerNum[(AWStab$speciesName == "cluster") &
                                    (AWStab$ModelScen == input$optTimeScenComm) & 
                                    (AWStab$ModelTime == substr(input$optTimePerComm,1,4))]
    
    leaflet(options = leafletOptions(zoomSnap = 0.1, 
                                     zoomDelta = 0.1,zoomControl = FALSE,
                                     dragging = FALSE)) %>%
      setView(lng = -95, lat = 38, zoom = 3.6) %>%
      addEsriBasemapLayer(esriBasemapLayers$Topographic, autoLabels = T) %>%
      
      registerPlugin(esriPlugin) %>%
      #onRender(paste0("function(el, x) {L.esri.dynamicMapLayer({url: 'https://mapspbgjam.env.duke.edu/arcgis/rest/services/pbgjam/",gsub("-","",unlist(strsplit(input$taxaComm,"_"))[2]),input$covarsComm,"b/MapServer',layers: [",layerNum,"],opacity: 0.8}).addTo(this);}")) %>%
      onRender(paste0("function(el, x) {L.esri.dynamicMapLayer({url: 'https://mapspbgjam1.env.duke.edu/arcgis/rest/services/pbgjam/comm_layers5_split/MapServer',layers: [",layerNum,"],opacity: 0.8}).addTo(this);}")) %>%
      
      
      addScaleBar(position = "bottomleft")
  })
  
  observe({ # Observer to respond to zoom / pan of map1 and apply to map2
    coords <- input$mapCommunitiesHist_bounds
    
    if (!is.null(coords)) {
      tproxy <- leafletProxy("mapCommunitiesFut") %>% 
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
}

shinyApp(ui, server)
