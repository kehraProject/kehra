library(leaflet)

# Choices for drop-downs
locations <- c(
  "Kazahkstan" = "KAZ",
  "United Kingdom" = "GBR"
)

enviroVar <- c(
  "Precipitation" = "P",
  "Temperature" = "T"
)

healthVar <- c(
  "Mortality" = "1",
  "Morbidity" = "2"
)

shinyUI(
  navbarPage("Kazahkstan's Environment-Health Risk Analysis (KEHRA) project", 
             id="nav",

  tabPanel("Map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", 
                    fixed = TRUE, draggable = TRUE, 
                    top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",

        h2("Select a study area"),

        selectInput("location", "", locations, selected = "KAZ")
      )
    )
  ),
  
#   tabPanel("Data explorer",
#            
#            plotOutput('plot1')
#            
#   ),
  
  # tabPanel("Dashboard"),
  
#   tabPanel("Project Time",
#            
#            htmlOutput('timeline')
#            
#   ),

  conditionalPanel("false", icon("crosshair"))
))
