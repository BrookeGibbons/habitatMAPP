tagList(
  useShinyjs(),
  dashboardPage(
    dashboardHeader(title = "Map Geographe Bay"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Interactive map", tabName = "map", icon = icon("map"))
      )
    ),
    dashboardBody(
      tabItems(
        # Upload data ----
        tabItem(tabName = "map",
                
                tags$head(
                  # Include our custom CSS
                  includeCSS("styles.css")#,
                  # includeScript("gomap.js")
                ),
                
                
                fluidRow(box(width=12,leafletOutput("map", height = 625)
                             # ),
                         #    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, alpha=0.5,draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",width = 300, height = "auto",
                         #  # sliderInput("depth", "Depth", min(forwards.2014$Depth), max(forwards.2014$Depth),value = range(forwards.2014$Depth), step = 1 ),
                         # checkboxGroupInput("select.markers", "Select imagery to display:", 
                         #                    selected=c("stereo-bruv.image","stereo-bruv.video","auv.video"),
                         #                    c("stereo-BRUV images"="stereo-bruv.image",
                         #                      "stereo-BRUV videos"="stereo-bruv.video",
                         #                      "AUV photogrammetry"="auv.video")),
                         # selectInput("show.mp", "Display marine parks:",
                         #             c("All" = "all",
                         #               "Commonwealth only" = "commonwealth",
                         #               "State only" = "state",
                         #               "None"="none"))
                         
                         ),
                )
        )
        
      )
    )
  )
)

