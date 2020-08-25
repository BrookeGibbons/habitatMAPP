tagList(
  useShinyjs(),
  dashboardPage(
    dbHeader,
    # dashboardHeader(title = "habitatMAPPer"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Interactive imagery", tabName = "imagery", icon = icon("map")),
        menuItem("Fish plots", tabName = "fish", icon = icon("fish", lib="font-awesome")),
        menuItem("Habitat pie charts", tabName = "pie", icon = icon("pie-chart")),
        menuItem("Habitat bubble plots", tabName = "bubble", icon = icon("circle")),
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
                fluidRow(box(width = 12, title = "Select an area to explore", status = "primary", solidHeader = TRUE, 
                             add_busy_spinner(spin = "fading-circle"),
                             # add_busy_gif(src = "https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/Blocks-1s-200px.gif", height = 70, width = 70),
                             selectInput("leaflet.marine.park", "", c("Geographe Marine Park" = "Geographe Bay",
                                                                     "Ningaloo Marine Park" = "Ningaloo",
                                                                     "South-west Corner Marine Park" = "South-west Corner",
                                                                     "Dongara lobster fishery"="Dongara"))),
                         
                         # box(width = 3, title = "Map display options", status = "primary", solidHeader = TRUE,
                             # checkboxInput("leaflet.cluster", "Cluster fish highlights", TRUE)), # ,checkboxInput("leaflet.zoom", "Animated zoom", TRUE)
                         
                         box(width = 12, leafletOutput("imagery.leaflet", height = 625))
                )
                ), # End tab item
        
        tabItem(tabName = "fish",
                fluidRow(box(width = 4, title = "Select an area to plot", status = "primary", solidHeader = TRUE, 
                             selectInput("fish.marine.park", "", c("Geographe Marine Park" = "Geographe Bay"#,
                                                                  # "Ningaloo Marine Park" = "Ningaloo",
                                                                  # "South-west Corner" = "South-west Corner"
                                                                  ))),
                         box(width = 4, title = "Select a metric to plot", status= "primary", solidHeader = TRUE, 
                             selectInput("fish.metric", "",c("Abundance"="maxn",
                                                            "Length" = "length",
                                                            "Biomass" = "biomass"))),
                         
                         box(width=4,title = "Number of species to plot",status="primary",solidHeader = TRUE,numericInput("species.limit", "Number:", 15, min = 5, max = 20)),
                         # add_busy_bar(color = "#FF0000"),
                         box(width=12,height = 500,
                             title = "Plot of most abundant species", status = "primary",
                             plotOutput("top.species")),
                         
                         box(width=12,title = "Choose a species to plot below:", status = "primary", solidHeader = TRUE,
                             htmlOutput("fish.species.dropdown",multiple=FALSE)),
                         

                         box(width=12,title = "Marine Park Zones", status = "primary", plotOutput("fish.zones", height = 250)),
                         box(width=12,title = "Fished vs. No-take", status = "primary", plotOutput("fish.status", height = 250))
                         # box(width=12,leafletOutput("pie.leaflet", height = 625))
                )
        ), # End tab item
        
        
        tabItem(tabName = "pie",
                fluidRow(box(width = 3, title = "Select a Marine Park to plot", status = "primary", solidHeader = TRUE, 
                             selectInput("pie.marine.park", "", c("Geographe Marine Park" = "Geographe Bay"#,
                                                                     # "Ningaloo Marine Park" = "Ningaloo",
                                                                     # "South-west Corner" = "South-west Corner"
                                                                  ))),
                         box(width = 3, title = "Select a method to plot", status= "primary", solidHeader = TRUE, 
                             selectInput("pie.method", "",c(#"All methods"="all",
                               "stereo-BRUV" = "stereo-BRUV",
                               "AUV" = "AUV",
                                                                  
                                                                  "Towed video" = "Towed"))),
                         # add_busy_bar(color = "#FF0000"),
                         box(width=12,leafletOutput("pie.leaflet", height = 625))
                )
                ), # End tab item
        
        tabItem(tabName = "bubble",
                fluidRow(box(width = 3, title = "Select a Marine Park to plot", status = "primary", solidHeader = TRUE, 
                             selectInput("bubble.marine.park", "", c("Geographe Marine Park" = "Geographe Bay",
                                                                     "Dongara" ="Dongara"
                                                                 #"Ningaloo Marine Park" = "Ningaloo",
                                                                 #"South-west Corner" = "South-west Corner"
                                                                 ))),
                         box(width = 3, title = "Select a method to plot", status = "primary", solidHeader = TRUE, br(),
                             # selectInput("bubble.method", "", c(#"All"="all",
                             #   "stereo-BRUV" = "stereo-BRUV",
                             #   "AUV" = "AUV",
                             #   "Towed video" = "Towed"))),
                         
                         htmlOutput("bubble.method.dropdown", multiple=FALSE)),
                         
                         # box(width = 3, title = "Select habitat type to plot", status = "primary", solidHeader = TRUE, 
                         #     selectInput("bubble.habitat", "", c("Consolidated", "Macroalgae", "Seagrasses", "Sponges", "Stony corals", "Turf algae", "Unconsolidated", "Other"), multiple = FALSE)),
                         box(width = 3, title = "Select habitat type to plot", status = "primary", solidHeader = TRUE, br(),
                             # selectInput("bubble.method", "", c(#"All"="all",
                             #   "stereo-BRUV" = "stereo-BRUV",
                             #   "AUV" = "AUV",
                             #   "Towed video" = "Towed"))),
                             
                             htmlOutput("bubble.habitat.dropdown", multiple=FALSE)),
                         box(width = 12, leafletOutput("bubble.leaflet", height = 625))
                )
        ), # End tab item
        
        tabItem(tabName = "acknowledgements",
                fluidRow(box(width = 8, status = "primary", height = 800, title = "Acknowledgments",
                             "The Marine Biodiversity Hub is funded by the Australian Government's National Environmental Science Program", br(), br(),
                             "Ningaloo video footage from the baseline survey of deepwater fish in the Ningaloo Marine Park, Commonwealth waters. Funded by the Marine Biodiversity Hub and Parks Australia.", br(),br(),
                             "Geographe Bay video footage from the National Envrionmental Research Programme, UWA and Curtin",br(),br(),
                             "South-west corner video footage from the baseline survey of deeperwater fish and habitats in the South-west Corner Marine Park, Commonwealth waters. Funded by the Marine Biodiversity Hub, Parks Australia and the University of Western Australia.
"),
                         box(width = 4, status = "primary", height = 800,
                             imageOutput("logos")
                
                )
                )
        )
        
      )
    )
  ),
  tags$footer("Developed by Brooke Gibbons and Tim Langlois, 2020", align = "center"#, style = "
              # position:absolute;
              # bottom:0;
              # width:100%;
              # height:30px;   /* Height of the footer */
              # color: white;
              # padding: 10px;
              # background-color: black;
              # z-index: 1000;"
              )
  
)#end tagList
# )

