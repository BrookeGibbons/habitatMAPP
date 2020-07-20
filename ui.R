tagList(
  useShinyjs(),
  dashboardPage(
    dashboardHeader(title = "HabitAPP"),
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
                
                
                fluidRow(box(width=3,title = "Select a Marine Park",status="primary",solidHeader = TRUE, 
                             selectInput("leaflet.select.marine.park", "",c("Geographe Bay" = "geographe",
                                                                            "Ningaloo" = "ningaloo",
                                                                            "South-west corner" = "southwest"))),
                         box(width=12,leafletOutput("map", height = 625)
                         
                         ),
                )
        )
        
      )
    )
  )
)

