tagList(
  useShinyjs(),
  dashboardPage(
    dashboardHeader(title = "habitatMAPP"),
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
                
                
                fluidRow(box(width=3,title = "Select a Marine Park to explore",status="primary",solidHeader = TRUE, 
                             selectInput("leaflet.marine.park", "",c("Geographe Marine Park" = "Geographe Bay",
                                                                     "Ningaloo Marine Park" = "Ningaloo",
                                                                     "South-west Corner" = "South-west Corner"))),
                         box(width=12,leafletOutput("leaflet.map", height = 625)
                             
                         )
                )
        )
        
      )
    )
  )
)

