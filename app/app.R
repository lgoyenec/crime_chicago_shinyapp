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
    
    output$map = renderLeaflet(
        leaflet() %>%
            addTiles() %>%
            setView(lng = -87.70, lat = 41.82, zoom = 11)
    )
}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# -------------------------------------------------------------------