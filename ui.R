tagList(
  useShinyjs(),
  dashboardPage(
    dbHeader,
    # dashboardHeader(title = "habitatMAPPer"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Interactive imagery", tabName = "imagery", icon = icon("map")),
        menuItem("Pie charts", tabName = "pie", icon = icon("pie-chart")),
        menuItem("Bubble plots", tabName = "bubble", icon = icon("circle")),
        menuItem("Acknowledgements", tabName = "acknowledgements", icon = icon("hands-helping", lib="font-awesome"))
      )
    ),
    dashboardBody(
      tabItems(
        # Upload data ----
        tabItem(tabName = "imagery",
                #tags$head(
                #  includeCSS("styles.css") # Include custom CSS
                #),
                fluidRow(box(width = 3, title = "Select a Marine Park to explore", status = "primary", solidHeader = TRUE, 
                             selectInput("leaflet.marine.park", "", c("Geographe Marine Park" = "Geographe Bay",
                                                                     "Ningaloo Marine Park" = "Ningaloo",
                                                                     "South-west Corner" = "South-west Corner"))),
                         
                         # box(width = 3, title = "Map display options", status = "primary", solidHeader = TRUE,
                             # checkboxInput("leaflet.cluster", "Cluster fish highlights", TRUE)), # ,checkboxInput("leaflet.zoom", "Animated zoom", TRUE)
                         
                         box(width = 12, leafletOutput("imagery.leaflet", height = 625))
                )
                ), # End tab item
        
        tabItem(tabName = "pie",
                fluidRow(box(width = 3, title = "Select a Marine Park to plot", status = "primary", solidHeader = TRUE, 
                             selectInput("pie.marine.park", "", c("Geographe Marine Park" = "Geographe Bay",
                                                                     "Ningaloo Marine Park" = "Ningaloo",
                                                                     "South-west Corner" = "South-west Corner"))),
                         box(width = 3, title = "Select a method to plot", status= "primary", solidHeader = TRUE, 
                             selectInput("pie.method", "",c("All methods"="all",
                                                                  "AUV" = "AUV",
                                                                  "stereo-BRUV" = "stereo-BRUV",
                                                                  "Towed video" = "Towed"))),
                         box(width=12,leafletOutput("pie.leaflet", height = 625))
                )
                ), # End tab item
        
        tabItem(tabName = "bubble",
                fluidRow(box(width = 3, title = "Select a Marine Park to plot", status = "primary", solidHeader = TRUE, 
                             selectInput("bubble.marine.park", "", c("Geographe Marine Park" = "Geographe Bay",
                                                                 "Ningaloo Marine Park" = "Ningaloo",
                                                                 "South-west Corner" = "South-west Corner"))),
                         box(width = 3, title = "Select a method to plot", status = "primary", solidHeader = TRUE, 
                             selectInput("bubble.method", "", c("All"="all",
                                                            "AUV" = "AUV",
                                                            "stereo-BRUV" = "stereo-BRUV",
                                                            "Towed video" = "Towed"))),
                         box(width = 3, title = "Select habitat type to plot", status = "primary", solidHeader = TRUE, 
                             selectInput("bubble.habitat", "", c("Consolidated", "Macroalgae", "Seagrasses", "Sponges", "Stony.corals", "Turf.algae", "Unconsolidated", "Other"), multiple = FALSE)),
                         box(width = 12, leafletOutput("bubble.leaflet", height = 625))
                )
        ), # End tab item
        
        tabItem(tabName = "acknowledgements",
                fluidRow(box(width = 12, status = "primary",
                  "Acknowledgments here", br(), "More box content"
                  
                )
                         
                )
        )
        
      )
    )
  ),
  tags$footer("Developed by Brooke Gibbons and Tim Langlois, 2020", align = "center", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:30px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: black;
              z-index: 1000;")
  
)#end tagList
# )

