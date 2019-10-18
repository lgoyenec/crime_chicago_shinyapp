# Laura Goyeneche
# October 17, 2018
# Project 2 R Shiny 
# -------------------------------------------------------------------

# Libraries
# -------------------------------------------------------------------

# Define libraries
libs = c("shiny", "shinythemes", "shinydashboard", "shinyWidgets",  # dashboard
         "leaflet", "leaflet.extras", "leaflet.minicharts",         # leaflet maps
         "httr", "jsonlite",                                        # read json
         "readxl",                                                  # read csv 
         "rgdal",                                                   # read shapefiles
         "lubridate",                                               # transform dates
         "stringr",                                                 # string case: upper, lower, title
         "DT",                                                      # render data frames
         "plotly")                                                  # interactive viz 

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
absPanel1 = '#panelOpts  {background-color: rgba(255,255,255, 0.7); padding: 0 20px 20px 20px;}'
absPanel2 = '#graphsOpts {background-color: rgba(255,255,255, 0.7); padding: 0 20px 20px 20px;}'

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
        
        # Draggable Panel with interactive graphs
        absolutePanel(
            id        = "graphsOpts",
            class     = "panel panel-default",
            draggable = T,
            top       = 100, 
            bottom    = "auto",
            left      = "auto", 
            right     = 350,
            width     = 310, height = "80%",
            
            # Title for absolute panel
            h3(strong("Interactive Graphs")),
            br(),
            
            # Interactive plots 
            h5(strong("Density distribution of hour of crime")),
            h6(strong("Comparison against total crimes")),
            plotlyOutput("plot1", height = 170),
            
            br(),
            
            h5(strong("Proportion of arrests")),
            plotlyOutput("plot2", height = 120),
            
            h5(strong("Proportion of domestic crimes")),
            plotlyOutput("plot3", height = 120),
            
            fluidPage(tags$head(tags$style(HTML(absPanel2))))
        ),
        
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
            h3(strong("Interactive Options")),
            br(),
            
            # Input options
            # Crime type
            radioButtons("input1", 
                         h4(strong("Crime type")), 
                         choices = 
                             c('Theft'                  = 1,
                               'Assault'                = 2,
                               'Narcotics'              = 3,
                               'Criminal trespassing'   = 4,
                               'Weapons violation'      = 5,
                               'Public peace violation' = 6), 
                         selected = 1),
            
            br(),
            
            # Show only crimes with an arrest
            checkboxInput("input2", 
                          strong("Show only crimes with an arrest"), 
                          value = F),
            
            # Show only domestic crimes
            checkboxInput("input3",
                         strong("Show only domestic crimes"),
                         value = F),
            
            # Interactive value box
            fluidRow(useShinydashboard(),
                     tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                     valueBoxOutput("vb1", width = 12)),
            
            fluidRow(useShinydashboard(),
                     tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                     valueBoxOutput("vb2", width = 12)),
            
            fluidPage(tags$head(tags$style(HTML(absPanel1))))
        )
    ),
    tabPanel(
        "Data explorer",
        h5(strong(paste("Recent crimes in Chicago 2019"))),
        hr(),
        DT::dataTableOutput("table1"),
        hr(),
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
    output$map = renderLeaflet({
        leaflet() %>% 
        
            setView(lng  = -87.47, 
                    lat  = 41.83, 
                    zoom = 11) %>%
            
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
                   crime_type == input$input1) %>%
            mutate(primary_type = str_to_sentence(primary_type))
        
        if (input$input2 == T){
            crime = 
                crime %>% 
                filter(arrest == T)
        }
        
        if (input$input3 == T){
            crime = 
                crime %>% 
                filter(domestic == T)
        }
        
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
                             radius      = 4, 
                             stroke      = T)
    })
    
    # Value box 
    output$vb1 = renderValueBox({
        value = 
            crimeInput() %>% 
            summarise(n = n()) %>% 
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
    
    output$vb2 = renderValueBox({
        value = 
            df %>% 
            summarise(n = n()) %>% 
            as.numeric()
        
        valueBox(paste(value), 
                 subtitle = "Total crimes", 
                 color    = "yellow")
    })
    
    # Tab - Statistics
    # ---------------------------------------------------------------
    
    # Plot 1
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
    
    # Plot 2
    output$plot2 = renderPlotly({
        df %>%
            filter(!is.na(latitude),
                   crime_type == input$input1) %>%
            group_by(arrest) %>%
            summarise(n = n()) %>%
            mutate(arrest = ifelse(arrest == T, "Yes", "No")) %>%
            
            plot_ly(x    =~ n,
                    y    =~ arrest,
                    type = 'bar', 
                    orientation = 'h') %>%
            
            layout(yaxis         = list(title = ''),
                   plot_bgcolor  = 'transparent',
                   paper_bgcolor = 'transparent',
                   margin        = list(l = 0, r = 0, b = 0, t = 0, pad = 0))
    })
    
    # Plot 3
    output$plot3 = renderPlotly({
        df %>%
            filter(!is.na(latitude),
                   crime_type == input$input1) %>%
            group_by(domestic) %>%
            summarise(n = n()) %>%
            mutate(domestic = ifelse(domestic == T, "Yes", "No")) %>%
            
            plot_ly(x    =~ n,
                    y    =~ domestic,
                    type = 'bar',
                    orientation = 'h') %>%
            
            layout(yaxis         = list(title = ''),
                   plot_bgcolor  = 'transparent',
                   paper_bgcolor = 'transparent',
                   margin        = list(l = 0, r = 0, b = 0, t = 0, pad = 0))
    })
    
    # Tab - Data Explorer 
    # ---------------------------------------------------------------
    
    # Data table
    output$table1 = DT::renderDataTable({
        DT::datatable(df,
                      options = list(pageLength = 10), 
                      rownames = F)
    })
    
    # Download
    output$downloadData = downloadHandler(
        filename = function() {paste("data-", Sys.Date(), ".csv", sep = "")},
        content  = function(file) {write.csv(df,file)}
    )
}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# -------------------------------------------------------------------