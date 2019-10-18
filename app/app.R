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
        # `lon` and `lat` as numeric
        mutate(district  = as.numeric(district),
               latitude  = as.numeric(latitude),
               longitude = as.numeric(longitude)) %>%
        
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
    sh_distr@data$dist_num = as.numeric(sh_distr@data$dist_num)

# Other
# -------------------------------------------------------------------

# Changes in absolutePanel:
absPanel = '#panelOpts {background-color: rgba(255,255,255, 0.7); padding: 0 20px 20px 20px;}'

# User interface 
# -------------------------------------------------------------------
ui = navbarPage(
    "Crimes in Chicago 2019", 
    id = "nav",
    theme = shinytheme("flatly"),
    tabPanel(
        # Interactive Map Options
        "Interactive Map",
        leafletOutput("map", width = "100%", height = 790),
        
        # Draggable Panel with input options and interactive plots
        absolutePanel(
            
            # Define options of absolute panel
            id        = "panelOpts",
            class     = "panel panel-default",
            draggable = T,
            top       = 100, 
            bottom    = "auto",
            left      = "auto", 
            right     = 30,
            width     = 310, height = "80%",
            
            # Title for absolute panel
            h3(strong("Interactive Map")),
            br(),
            
            # Input options
            # Crime type
            radioButtons("input1", 
                         h4(strong("Crime type")), 
                         choices = 
                             c('Theft' = 1,
                               'Assault' = 2,
                               'Narcotics' = 3,
                               'Criminal trespassing' = 4,
                               'Weapons violation' = 5,
                               'Public peace violation' = 6), 
                         selected = 1),
            
            br(),
            
            # Show only crimes with an arrest
            checkboxInput("input2", 
                          strong("Show only crimes with an arrest"), 
                          value = T),
            
            # Show only domestic crimes
            checkboxInput("input3",
                         strong("Show only domestic crimes"),
                         value = T),
            
            # Interactive plots
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
        h5(strong(paste("Recent crimes in Chicago 2019"))),
        hr(),
        DT::dataTableOutput("table1"),
        hr(),
        h5(strong("Download data for selected variable:")),
        downloadButton('downloadData',"Download data"), 
        hr()
    )
)

# Server function 
# -------------------------------------------------------------------

server = function(input, output) {
    
    # Tab - Interactive Map
    # ---------------------------------------------------------------
    
    # Base map
    output$map = renderLeaflet({
        leaflet() %>% 
            
            setView(lng = -87.70, lat = 41.82, zoom = 11) %>%
            
            addProviderTiles("CartoDB.DarkMatterNoLabels"     , group = "World Dark") %>% 
            addProviderTiles(provider = "Esri.WorldGrayCanvas", group = "World Gray") %>%
            
            addPolygons(data   = sh_distr, 
                        color  = "#878B8E",
                        fill   = F, 
                        weight = 1.5) %>%
            
            addLayersControl(
                baseGroups = c("World Dark","World Gray"),
                position   = "bottomleft")
    })
    
    # Data filter
    crimeInput = reactive({
        crime = 
            df %>%
            filter(!is.na(latitude),
                   crime_type == input$input1)
        return(crime)
    })
    
    # Leaflet maps 
    observe({
        leafletProxy("map", data = crimeInput()) %>%
            clearMarkers() %>% 
            addCircleMarkers(lng         =~ longitude, 
                             lat         =~ latitude, 
                             fillOpacity = 1, 
                             color       = "#00A6A6", 
                             radius      = 3, 
                             stroke      = T)
    })
    
}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# -------------------------------------------------------------------