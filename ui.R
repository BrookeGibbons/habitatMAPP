tagList(
  useShinyjs(),
  dashboardPage(
    dashboardHeader(title = "habitatMAPPer"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Interactive imagery", tabName = "imagery", icon = icon("map")),
        menuItem("Pie charts", tabName = "pie", icon = icon("pie-chart")),
        menuItem("Bubble plots", tabName = "bubble", icon = icon("circle"))
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
        )
      )
    )
  )
)

