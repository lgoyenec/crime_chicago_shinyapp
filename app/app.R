# Laura Goyeneche
# October 17, 2018
# Project 2 R Shiny 
# -------------------------------------------------------------------

# Libraries
# -------------------------------------------------------------------

# Install library directly 
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)
library(httr)
library(jsonlite)
library(readxl)
library(rgdal)
library(lubridate)
library(stringr)
library(DT)
library(plotly)
library(plyr)

# Data 
# -------------------------------------------------------------------

# City of Chicago
    # Import data API
    # The API allows us to query maximum 1,000 crimes
    # The following dataset have the 1,000 most recent crimes in Chicago
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
        # `date` as datetime
        # Create month, day and hour of the day 
        mutate(district  = as.numeric(district),
               latitude  = as.numeric(latitude),
               longitude = as.numeric(longitude),
               date      = as_datetime(date),
               month     = month(date),
               day       = day(date),
               hour      = hour(date)) %>%
        
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
        # This makes easier the manipulation reactive inputs in shiny
        mutate(
            crime_type = 
                ifelse(primary_type == "THEFT",             1, 
                ifelse(primary_type == "ASSAULT",           2, 
                ifelse(primary_type == "NARCOTICS",         3, 
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
    # For both absolute panel the default color of the background is white
    # With this code, the background is between white and transparent
    absPanel1 = '#panelOpts  {background-color: rgba(255,255,255, 0.7); padding: 0 20px 20px 20px;}'
    absPanel2 = '#graphsOpts {background-color: rgba(255,255,255, 0.7); padding: 0 20px 20px 20px;}'
    
    # Color Palette
    # Generate personlize palette for the choropleth map
    cPal = c('#005B5B','#007979','#009797','#17AEAE','#2EB6B6','#45BEBE','#5CC6C6',
             '#73CECE','#8BD6D6','#A2DEDE','#B9E6E6','#D0EEEE','#E7F6F6')

# User interface 
# -------------------------------------------------------------------
ui = navbarPage(
    # Name of the app
    "Crimes in Chicago 2019", 
    id = "nav",
    
    # Theme
    theme = shinytheme("flatly"),
    
    # Create different tabs
    tabPanel(
        
        # Interactive Map & Graphs
        #------------------------------------------------------------
        "Interactive Map",
        
        # Leaflet map 
        
        leafletOutput("map", width = "100%", height = 800),
        
        # Interactive graphs
        # Draggable Panel with the graphs
        absolutePanel(
            
            # Define options of absolute panel
            id        = "graphsOpts",
            class     = "panel panel-default",
            draggable = T,
            top       = 100, 
            bottom    = "auto",
            left      = "auto", 
            right     = 350,
            width     = 310, height = "85%",
            
            # Title for absolute panel
            h3(strong("Interactive Graphs")),
            h6(strong("Data:"),"1,000 most recent crimes in Chicago"),
            h6(strong("Source"), "City of Chicago - Data Portal"),
            
            br(),
            
            # Interactive plots 
            # Distribution of crimes across the day
            h5(strong("Distribution of crimes across day hours")),
            h6(strong("Comparison against total crimes")),
            plotlyOutput("plot1", height = 170),
            
            br(),
            
            # Number of arrests
            h5(strong("Number of arrests")),
            plotlyOutput("plot2", height = 120),
            
            # Number of domestic crimes
            h5(strong("Number of domestic crimes")),
            plotlyOutput("plot3", height = 120),
            
            # With the following code we're applying the changes in absolutePanel 
            # The options are in line 103
            fluidPage(tags$head(tags$style(HTML(absPanel2))))
        ),
        
        # Input options
        # Draggable Panel with input options
        absolutePanel(
            
            # Define options of absolute panel
            id        = "panelOpts",
            class     = "panel panel-default",
            draggable = T,
            top       = 100, 
            bottom    = "auto",
            left      = "auto", 
            right     = 30,
            width     = 310, height = "85%",
            
            # Title for absolute panel
            h3(strong("Interactive Options")),
            
            # Input options
            # Analysis type
            radioButtons("input0",
                         h5(strong("Choose type of analysis:")),
                         choices  = c("Markers","Polygons"),
                         selected = "Markers"),
            
            # Crime type among the ones selected previously
            radioButtons("input1", 
                         h5(strong("Crime type")), 
                         choices = 
                             c('Theft'                  = 1,
                               'Assault'                = 2,
                               'Narcotics'              = 3,
                               'Criminal trespassing'   = 4,
                               'Weapons violation'      = 5,
                               'Public peace violation' = 6), 
                         selected = 1),
            br(),
            
            # Checkbox: Show only crimes with an arrest
            checkboxInput("input2", 
                          strong("Show only crimes with an arrest"), 
                          value = F),
            
            # Checkbox: Show only domestic crimes
            checkboxInput("input3",
                         strong("Show only domestic crimes"),
                         value = F),
            
            br(),
            
            # Interactive value boxes
            # Number of crimes per selected crime
            fluidRow(useShinydashboard(),
                     tags$head(tags$style(HTML(".small-box {height: 90px}"))),
                     valueBoxOutput("vb1", width = 12)),
            
            # Number of total crimes in Chicago
            fluidRow(useShinydashboard(),
                     tags$head(tags$style(HTML(".small-box {height: 90px}"))),
                     valueBoxOutput("vb2", width = 12)),
            
            # With the following code we're applying the changes in absolutePanel 
            # The options are in line 102
            fluidPage(tags$head(tags$style(HTML(absPanel1))))
        )
    ),
    tabPanel(
        
        # Data explorer
        #------------------------------------------------------------
        "Data explorer",
        
        # Title
        h5(strong(paste("Recent crimes in Chicago 2019"))),
        hr(),
        
        # Data table output
        DT::dataTableOutput("table1"),
        hr(),
        
        # Download Handler
        h5(strong("Download data:")),
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
    # ---------------------------------------------------------------
    output$map = renderLeaflet({
        leaflet() %>% 
            
            # Set view in area of interest
            setView(lng  = -87.47, 
                    lat  = 41.83, 
                    zoom = 11) %>%
            
            # Add base maps 
            addProviderTiles("CartoDB.DarkMatterNoLabels"     , group = "World Dark") %>% 
            addProviderTiles(provider = "Esri.WorldGrayCanvas", group = "World Gray") %>%
            
            # Add polygons for districts in Chicago
            addPolygons(data   = sh_distr, 
                        color  = "#878B8E",
                        fill   = F, 
                        weight = 1.5) %>%
            
            # Add layers control
            addLayersControl(
                baseGroups = c("World Dark","World Gray"),
                position   = "bottomleft")
    })
    
    # Data filter base on inputs
    # ---------------------------------------------------------------
    crimeInput = reactive({
        # Subset data as `crime`
        crime = 
            df %>%
            
            # Filter crime of interest
            filter(!is.na(latitude),
                   crime_type == input$input1) %>%
            
            # Transform crime name to sentence case
            mutate(primary_type = str_to_sentence(primary_type))
        
        # Subset again base on two additional filters
        if (input$input2 == T){
            
            # Subset those crimes that ended up in an arrest
            crime = 
                crime %>% 
                filter(arrest == T)
        }
        
        if (input$input3 == T){
            
            # Subset those crimes classified as domestic crimes
            crime = 
                crime %>% 
                filter(domestic == T)
        }
        
        return(crime)
    })
    
    # Leaflet maps 
    # ---------------------------------------------------------------
    # Based on crimeInput() we're generating two types of maps
    # 1. Markers: points of crimes across the city
    # 2. Choropleth: number of crimes per district
    
    observe({
        
        # Markers map
        # -----------------------------------------------------------
        if (input$input0 == "Markers"){
            
            leafletProxy("map", data = crimeInput()) %>%
                
                clearGroup(group = "marker") %>%
                clearGroup(group = "chloropleth") %>%
                clearControls() %>%
                
                addCircleMarkers(lng         =~ longitude,
                                 lat         =~ latitude,
                                 fillOpacity = 1,
                                 color       = "#00A6A6",
                                 radius      = 4,
                                 stroke      = T,
                                 group       = "marker")
        # Choropleth map
        # -----------------------------------------------------------
        } else {
            if (nrow(crimeInput()) > 0) {
                
                # Temporal data with number of crimes per district
                temp = 
                    crimeInput() %>%
                    group_by(district) %>%
                    dplyr::summarise(n = n()) %>%
                    dplyr::rename(dist_num = district)
                
                # Merge `temp` data with shapefile district
                map      = sh_distr
                map@data = plyr::join(map@data, temp, by = "dist_num")
                map@data
                
                # Create bins for choropleth map
                dom  = map$n
                bin  = sort(unique(dom))
                
                    # For one of the selections, the number of bins is 1
                    # bin length has to be at least 2
                    if (length(bin) == 1) {
                        bin = c(0, bin)
                    }
                
                # Create and match color palette with bins
                pal  = colorBin(rev(cPal), domain = dom, bins = bin, na.color = 'transparent')
                
                # Choropleth map
                leafletProxy("map", data = map) %>%
                    
                    clearGroup(group = "marker") %>%
                    clearGroup(group = "chloropleth") %>%
                    clearControls() %>%
                    
                    addPolygons(stroke      = T,
                                fillOpacity = 0.7,
                                fillColor   =~ pal(dom),
                                color       = "white",
                                weight      = 0.3,
                                popup       =~ paste("Number of crimes:",n),
                                group       = "chloropleth") %>%
                    
                    addLegend(title    = '',
                              pal      = pal,
                              values   =~ n, 
                              na.label = '',
                              position = 'topleft')
                
            # Default map
            # When there's no onservations in the data
                
            } else {
                leafletProxy("map", data = crimeInput()) %>%
                    
                    clearGroup(group = "marker") %>%
                    clearGroup(group = "chloropleth") %>%
                    clearControls()
            }
        }
    })
    
    # Value box 
    # ---------------------------------------------------------------
    
    # Number of crimes for selected crime in input
    # ---------------------------------------------------------------
    output$vb1 = renderValueBox({
        value = 
            crimeInput() %>% 
            dplyr::summarise(n = n()) %>% 
            as.numeric()
        
        text  = 
            df %>% 
            filter(crime_type == input$input1) %>% 
            select(primary_type) %>% 
            unique() %>% 
            str_to_sentence()
        
        valueBox(paste(value), 
                 subtitle = paste(text, "crimes"), 
                 color    = "blue")
    })
    
    # Total number of crimes
    # ---------------------------------------------------------------
    output$vb2 = renderValueBox({
        value = 
            df %>% 
            dplyr::summarise(n = n()) %>% 
            as.numeric()
        
        valueBox(paste(value), 
                 subtitle = "Total crimes", 
                 color    = "yellow")
    })
    
    # Tab - Statistics
    # ---------------------------------------------------------------
    
    # Plot 1: Distribution of crimes across day hours
    # ---------------------------------------------------------------
    output$plot1 = renderPlotly({
        plot_ly(alpha = 0.6) %>%
        
        add_histogram(data = df,
                      x    =~ hour,
                      name = 'Total') %>%
            
        add_histogram(data  = crimeInput(),
                      x     =~ hour, 
                      color = I('#4DFFF3'),
                      name  = unique(crimeInput()['primary_type'])) %>%
            
        layout(barmode       = 'overlay',
               legend        = list(orientation = 'h'),
               xaxis         = list(title = ''),
               plot_bgcolor  = 'transparent',
               paper_bgcolor = 'transparent',
               margin        = list(l = 0, r = 0, b = 0, t = 0, pad = 0))
    })
    
    # Plot 2: Bar plot of arrests
    # Number of crimes of selected crime that ended up in a arrest
    # ---------------------------------------------------------------
    output$plot2 = renderPlotly({
        df %>%
            filter(!is.na(latitude),
                   crime_type == input$input1) %>%
            group_by(arrest) %>%
            dplyr::summarise(n = n()) %>%
            mutate(arrest = ifelse(arrest == T, "Yes", "No")) %>%
            
            plot_ly(x    =~ n,
                    y    =~ arrest,
                    type = 'bar',
                    color = I('#E4572E'),
                    alpha = 0.7, 
                    orientation = 'h') %>%
            
            layout(xaxis         = list(title = ''),
                   yaxis         = list(title = ''),
                   plot_bgcolor  = 'transparent',
                   paper_bgcolor = 'transparent',
                   margin        = list(l = 0, r = 0, b = 0, t = 0, pad = 0))
    })
    
    # Plot 3: Bar plot of domestic crimes
    # number of crimes of selected crime that were classified as domestic
    # ---------------------------------------------------------------
    output$plot3 = renderPlotly({
        df %>%
            filter(!is.na(latitude),
                   crime_type == input$input1) %>%
            group_by(domestic) %>%
            dplyr::summarise(n = n()) %>%
            mutate(domestic = ifelse(domestic == T, "Yes", "No")) %>%
            
            plot_ly(x    =~ n,
                    y    =~ domestic,
                    type = 'bar',
                    color = I('#76B041'),
                    alpha = 0.7,
                    orientation = 'h') %>%
            
            layout(xaxis         = list(title = ''),
                   yaxis         = list(title = ''),
                   plot_bgcolor  = 'transparent',
                   paper_bgcolor = 'transparent',
                   margin        = list(l = 0, r = 0, b = 0, t = 0, pad = 0))
    })
    
    # Tab - Data Explorer 
    # ---------------------------------------------------------------
    
    # Data table
    # ---------------------------------------------------------------
    output$table1 = DT::renderDataTable({
        DT::datatable(df,
                      options = list(pageLength = 10), 
                      rownames = F)
    })
    
    # Download
    # ---------------------------------------------------------------
    output$downloadData = downloadHandler(
        filename = function() {paste("data-", Sys.Date(), ".csv", sep = "")},
        content  = function(file) {write.csv(df,file)}
    )
}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# -------------------------------------------------------------------