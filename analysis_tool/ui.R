#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# 
.lib<- c("shiny","lubridate","jsonlite","dplyr", "stringr", "tidyr","ggplot2", "plotly", "DT","reshape2","visNetwork","ggalt","knitr",
         "grid","gridExtra", "igraph","shinyjs", "shinyBS", "shinythemes", "shinycssloaders", "shinyWidgets", "shinyWidgets", "shinythemes",
         "Hmisc","openair", "visNetwork")

.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
lapply(.lib, require, character.only=TRUE)






polen_meteo_all <- readRDS("polen_meteo_all_current_norm.rds")
polen_meteo_all_orig <- readRDS("polen_meteo_all_current.rds")

polenS <- colnames(polen_meteo_all)[2:41]
vlc <- c("Populus", "Morus", "Quercus", "Fraxinus", "Ligustrum", "Ulmus", "Platanus", "Pinus", 
         "Olea", "Cupressaceae.Taxaceae", "Casuarina")



pollution <- readRDS("pollution_orig.rds")
pollution_norm <- readRDS("pollution_norm.rds")
polls <- colnames(pollution)[2:7]




pm <- readRDS("pm_orig.rds")
pm_norm <- readRDS("pm_norm.rds")
pms <- colnames(pm)[2:4]

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  #useShinyjs(),
  
  theme=shinytheme("spacelab"),
  useShinyjs(),
  
  # Application title
  # titlePanel("ParticleAI"),
  headerPanel(
    fluidRow(
      #column(8, h1("SALER Analytics",  style = "font-size: 1.5em; font-weight: 500; line-height: 1.1;")), 
      
      #column(8, img(height = 110, src = "tenor.gif")),
      column(9),
      column(3, img(height = 110, src = "ParticleAIlogo.png"))
      
      
    ),"ParticleAI"
  ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
       sliderTextInput("DataRange",
                       label = "Data Range",
                       choices = c("1 year ago", "6 months ago", "3 months ago", "1 month ago", "Currently", 
                                   "Next week", "Next month", "Next 3 months"),
                       selected = "1 year ago", 
                       grid = T
       ),
       hr(),
       
       selectizeInput(inputId = "Location", 
                      label = "Location",
                      multiple = FALSE,
                      options = list(
                        placeholder = 'Campos',
                        'plugins' = list("drag_drop","remove_button","restore_on_backspace"),
                        'create' = TRUE,
                        'persist' = FALSE),
                      choices = "Valencia",
                      selected = "Valencia"),
       
       selectizeInput(inputId = "TypeParticle", 
                      label = "Suspended Particulate Type",
                      multiple = FALSE,
                      options = list(
                        placeholder = 'Campos',
                        'plugins' = list("drag_drop","remove_button","restore_on_backspace"),
                        'create' = TRUE,
                        'persist' = FALSE),
                      choices = c("Pollen","Air Pollution", "Particulate air pollution"),
                      selected = "Pollen"),
       
       conditionalPanel(condition= paste0("input.TypeParticle == 'Pollen'"),
                        
              selectizeInput(inputId = "Pollens", 
                              label = "Particle",
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Campos',
                                'plugins' = list("drag_drop","remove_button","restore_on_backspace"),
                                'create' = TRUE,
                                'persist' = FALSE),
                              choices = list("From Valencia"= c(vlc), Others = c(polenS[!polenS%in%vlc])),
                              selected = "Platanus")
              ),
    
       
       conditionalPanel(condition= paste0("input.TypeParticle == 'Air Pollution'"),
                        
                        selectizeInput(inputId = "Pollutions", 
                                       label = "Particle",
                                       multiple = TRUE,
                                       options = list(
                                         placeholder = 'Campos',
                                         'plugins' = list("drag_drop","remove_button","restore_on_backspace"),
                                         'create' = TRUE,
                                         'persist' = FALSE),
                                       choices = c(polls),
                                       selected = "SO2")
       ),
       
       conditionalPanel(condition= paste0("input.TypeParticle == 'Particulate air pollution'"),
                        
                        selectizeInput(inputId = "Others", 
                                       label = "Particle",
                                       multiple = TRUE,
                                       options = list(
                                         placeholder = 'Campos',
                                         'plugins' = list("drag_drop","remove_button","restore_on_backspace"),
                                         'create' = TRUE,
                                         'persist' = FALSE),
                                       choices = c(pms),
                                       selected = "PM1")
       ),
       
    # div(p("Download Data"), style = "text-align:left; font-weight: 900; font-size:14px;margin-bottom:10px;margin-top:20px"),                       
    # div(downloadButton('ParticleData.csv', 'CSV', class = "btn btn-primary"), style = "text-align:left;margin-bottom:20px;"),
    hr(),
    div(splitLayout(cellWidths = c("66%","33%"), 
                    div(p("Download Data"), style = "text-align:left; font-weight: 600; font-size:14px;" ),
                    div(downloadButton('ParticleData.csv', 'CSV', class = "btn btn-primary"), style = "text-align:right;margin-bottom:20px;")
                    )),
    hr(),
    helpText("ParticleAI allows you to analyse, escrutinise and visualise historical data about different sort of 
             particles (allergens, pollution, and other suspended particulates) as well as predict their levels in the 
             short term future.")
       
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
       tabsetPanel(
        
        tabPanel("Analysis", 
                 div(HTML("<h1 style=\"color: #428bca;\"> Historical and future evolution</h1>"),style = "font-size:100%;margin-bottom:4%;"),
                 div(withSpinner(plotlyOutput("Aller.Temp",height = 500)),
                     style = "margin-bottom:1%;"),
                 
                 # column(12,
                 #        column(6,
                 #            div(splitLayout(cellWidths = c("50%","50%"), 
                 #                              plotlyOutput("Aller.Prec",height = 250),
                 #                              plotlyOutput("Aller.Wind",height = 250),
                 #                 style = "margin-bottom:5%;")),
                 #            div(splitLayout(cellWidths = c("50%","50%"), 
                 #                            plotlyOutput("Aller.Sun",height = 250),
                 #                            plotlyOutput("Aller.Press",height = 250)),
                 #                style = "margin-bottom:5%;")
                 #        ),
                 #        column(6,
                 #              visNetworkOutput("Flan",height = 500)
                 #        )
                 #  ),
                 
                 column(12, 
                        column(4,plotlyOutput("Aller.Prec",height = 300)),
                        column(4,plotlyOutput("Aller.Sun",height = 300)),
                        column(4,plotlyOutput("Aller.Press",height = 300))
                 ),
                 
                 # column(12,
                 #        column(6,
                 #               visNetworkOutput("Flan",height = 500)),
                 #        column(6,
                 #               visNetworkOutput("Tabla",height = 500))
                 # ),
                 
                 
                 # div(splitLayout(cellWidths = c("50%","50%"), 
                 #                 withSpinner(plotlyOutput("Aller.Prec",height = 250)),
                 #                             withSpinner(plotlyOutput("Aller.Humi",height = 250))),
                 #     style = "margin-bottom:5%;"),
                 # 
                 # div(splitLayout(cellWidths = c("50%","50%"), 
                 #                 withSpinner(plotlyOutput("Aller.Sun",height = 250)),
                 #                 withSpinner(plotlyOutput("Aller.Press",height = 250))),
                 #     style = "margin-bottom:5%;"),
                 
                 div(HTML("<h1 style=\"color: #428bca;\"> Dispersion</h1>"),style = "font-size:100%;margin-bottom:4%;"),
                 
                 column(12,
                    div(splitLayout(cellWidths = c("50%","50%"), 
                                 (plotOutput("Aller.windRose",height = 400)),
                                 (plotOutput("Aller.pollRose",height = 400))),
                     style = "margin-bottom:5%;")
                 
                 ),
                 div(HTML("<h1 style=\"color: #428bca;\"> Particle Levels</h1>"),style = "font-size:100%;margin-bottom:4%;"),
                 
                 column(12,
                        div(DT::dataTableOutput("detailDT"),style = "margin-bottom:50px;")
                        )
                 ),
        
         
        tabPanel("Info",
                tags$head(tags$script(src="leaflet.js")),
                conditionalPanel(condition= paste0("input.TypeParticle == 'Pollen'"),
                          div(uiOutput("titleInfo"),style = "font-size:100%;margin-bottom:4%;"),
                          column(12,
                                 column(6,div(uiOutput("infoPollen"),style="margin-bottom:5%;")),
                                 column(6,div(uiOutput("infoPollen2"),style="margin-bottom:5%;"))
                                 
                          ),
                          div(HTML("<h1 style=\"color: #428bca;\"> Geolocation</h1>"),style = "font-size:100%;margin-bottom:4%;"),
                          column(12,div(uiOutput("mapPollen"),style="margin-bottom:5%;")),
                          #includeHTML("./pollen_info/casuarina.html"),
                          #includeHTML("./www/info/platanus_v2.html"),
                          
                          
                          div(HTML("<h1 style=\"color: #428bca;\">Blossom period and overlaps</h1>"),style = "font-size:100%;margin-bottom:4%;"),
                          column(12,
                                 # div(splitLayout(cellWidths = c("40%","60%"), 
                                 #          visNetworkOutput("Flan"),
                                 #          div(plotOutput("Aller.Calendar"),
                                 #        style = "margin-bottom:5%"))
                                     
                                            column(5, visNetworkOutput("Flan")),
                                            column(7, div(plotOutput("Aller.Calendar", height = 500),
                                                          style = "margin-bottom:5%"))
                                   
                          )

                 ),
                conditionalPanel(condition= paste0("input.TypeParticle == 'Air Pollution'"),
                                 
                                 column(12,div(img(height = 500, src = "tenor.gif"),style="margin-top:5%;margin-left:25%;"))
                
                ),
                conditionalPanel(condition= paste0("input.TypeParticle == 'Particulate air pollution'"),
                                 
                                 column(12,div(img(height = 500, src = "tenor.gif"),style="margin-top:5%;margin-left:25%;"))
                )
                
                
          ),
        #
         tabPanel("About",
                  #includeHTML("about.html")#,
                  #includeHTML("mapa.html")
                  uiOutput("about")
                  )

      )#tabpanels
      
    )
  )
))
