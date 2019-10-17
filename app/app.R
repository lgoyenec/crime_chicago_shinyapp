# Laura Goyeneche
# October 17, 2018
# Project 2 R Shiny 
# -------------------------------------------------------------------

# Libraries
# -------------------------------------------------------------------

# Define libraries
libs = c("shiny", "shinythemes", "shinydashboard",             # dashboard
         "leaflet", "leaflet.extras", "leaflet.minicharts",    # leaflet maps
         "readxl",                                             # read csv 
         "DT",                                                 # render data frames
         "plotly")                                             # interactive viz      

# Attach libraries
invisible(suppressMessages(lapply(libs, library, character.only = T)))

# Data 
# -------------------------------------------------------------------

# City of Chicago
    # Import data API
    url  = 'https://data.cityofchicago.org/resource/w98m-zvie.json'
    req  = GET(URLencode(url))
    cont = content(req, 'text')
    json = gsub('NaN', 'NA', cont, perl = T)
    df   = data.frame(jsonlite::fromJSON(json))
    
    # Data manipulation
    df = 
        df %>%
        
        # Choosing variables of interest 
        select(date,
               primary_type,
               location_description,
               arrest, domestic,
               beat, district,
               latitude, longitude) %>%
        
        # District as numeric
        mutate(district = as.numeric(district)) %>%
        
        # Select primary type of crimes for maps
        filter(
            primary_type %in%
                c('THEFT',
                  'ASSAULT',
                  'NARCOTICS',
                  'CRIMINAL TRESPASS',
                  'WEAPONS VIOLATION',
                  'PUBLIC PEACE VIOLATION')) %>%
        
        # Generate categorical variable for chosen types
        mutate(
            crime_type = 
                ifelse(primary_type == "THEFT", 1, 
                ifelse(primary_type == "ASSAULT", 2, 
                ifelse(primary_type == "NARCOTICS", 3, 
                ifelse(primary_type == "CRIMINAL TRESPASS", 4, 
                ifelse(primary_type == "WEAPONS VIOLATION", 5, 6)))))
        )

# Import Shapefiles
    # Police beats
    sh_beats = readOGR(dsn     = './data_shp', 
                       layer   = 'police_beats',
                       verbose = F)
    
    # Police districts
    sh_distr = readOGR(dsn     = './data_shp', 
                       layer   = 'police_district',
                       verbose = F)
    
    # Data manipulation
    # Variables `district` as numeric
    sh_beats@data$district = as.numeric(sh_beats@data$district)
    sh_distr@data$DIST_NUM = as.numeric(sh_distr@data$DIST_NUM)
    
    # Change name to `district` in sh_distr shapefile
    colnames(sh_distr@data)[2] = 'district'

# Other
# -------------------------------------------------------------------

# Changes in absolutePanel:
absPanel = '#panelOpts {background-color: rgba(255,255,255, 0.7); padding: 0 20px 20px 20px;}'

# User interface 
# -------------------------------------------------------------------
ui = navbarPage(
    "Crime in Chicago", 
    id = "nav",
    theme = shinytheme("flatly"),
    tabPanel(
        "Interactive Map",
        leafletOutput("map", width = "100%", height = 790),
        absolutePanel(
            id        = "panelOpts",
            class     = "panel panel-default",
            draggable = T,
            top       = 100, 
            bottom    = "auto",
            left      = "auto", 
            right     = 30,
            width     = 310, height = "80%",
            
            h4("Interactive Map"),
            selectInput("input1", "Color", c(1,2,3)),
            selectInput("input2", "Size", c(1,2,3)),
            plotlyOutput("plot1", height = 250),
            plotlyOutput("plot2", height = 250),
            
            fluidPage(tags$head(tags$style(HTML(absPanel))))
        )
    ),
    tabPanel(
        "Statistics"
    ),
    tabPanel(
        "Data explorer",
        DT::dataTableOutput("table1")
    )
)

# Server function 
# -------------------------------------------------------------------

server = function(input, output) {
    
}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# -------------------------------------------------------------------