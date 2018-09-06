#########################################################################################
#load necessary packages
#########################################################################################

require("shiny")
require("shinydashboard")
require("leaflet")
require("dplyr")
require("plotly")
require("ggplot2")
require("tidyverse")


dados <- read_csv("2018_09_04_dados.csv")


#########################################################################################
#lCreating a page layout
#########################################################################################

sidebar <- dashboardSidebar(
    
    sidebarMenu(
        menuItem("Home", tabName = "Home", icon = icon("home")),
        menuItem("Sites", tabName = "Sites", icon = icon("globe")),
        menuItem("Descripitive", tabName = "desc", icon = icon("signal")),
        menuItem("Richness", tabName = "Sr", icon = icon("bug"))

    )
)


body <- dashboardBody(
    tabItems(
        tabItem(tabName = "Home",
                
                #Create boxing for general information
                tags$img(src = "Carrapato.png",width = "640px", height = "480px"),
                tags$h1("Some nice text about ticks"),
                tags$blockquote("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
Curabitur dictum justo ac dignissim ornare. Ut porttitor purus ac eros varius, sit amet venenatis augue imperdiet. 
Nullam rutrum vel quam vitae laoreet. Phasellus ut lacus nec nibh facilisis condimentum a in diam. Duis vitae lectus 
vitae nisi faucibus ullamcorper. Nam tincidunt magna ex. Pellentesque tempor lectus vel lectus condimentum convallis 
non vitae ex. Phasellus sagittis elementum laoreet. Vivamus arcu mi, feugiat sollicitudin tortor vel, consectetur 
vehicula leo. Quisque eros mi, dictum quis lectus nec, pretium ultrices nulla. Donec vitae libero aliquam, 
porta odio ac, tincidunt dui. Praesent in tempor eros."),
                
                tags$blockquote("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
Curabitur dictum justo ac dignissim ornare. Ut porttitor purus ac eros varius, sit amet venenatis augue imperdiet. 
Nullam rutrum vel quam vitae laoreet. Phasellus ut lacus nec nibh facilisis condimentum a in diam. Duis vitae lectus 
vitae nisi faucibus ullamcorper. Nam tincidunt magna ex. Pellentesque tempor lectus vel lectus condimentum convallis 
non vitae ex. Phasellus sagittis elementum laoreet. Vivamus arcu mi, feugiat sollicitudin tortor vel, consectetur 
vehicula leo. Quisque eros mi, dictum quis lectus nec, pretium ultrices nulla. Donec vitae libero aliquam, 
porta odio ac, tincidunt dui. Praesent in tempor eros.")
                
        ),
        
        
        tabItem(tabName = "Sites",
                fluidRow(
    
                    #Ploting map inside this box
                    box(
                        solidHeader = TRUE, background = "green", width = 13,
                        height = 900,
                        leafletOutput(outputId = "mapa", height = 875))
                    
                )
        ),
        tabItem(tabName = "desc",
                fluidRow(
                    #Create whitaker plot
                    box(
                        solidHeader = TRUE, width = 6,
                        height = 450, background = "green",
                        plotlyOutput("wihtakker", height = 435)
                    ),
                    box(
                        solidHeader = TRUE, width = 6,
                        height = 450, background = "green",
                        plotlyOutput("prop_plot", height = 435)
                    ),
                    box(
                        solidHeader = TRUE, width = 2,
                        height = 450, background = "green",
                        selectInput(inputId = "sel_species", label = "Select a specie",
                                    choices = c("All", unique(dados$Especie)),
                                    selected = "All"),
                        selectInput(inputId = "sel_stage", label = "Select a stage",
                                    choices = c("Adult", "Nymph", "All"),
                                    selected = "All")
                    ),
                    
                    box(
                        solidHeader = TRUE, width = 10,
                        height = 450, background = "green",
                        plotlyOutput("ts_plot", height = 435)
                        
                    )
                    
                )),
        tabItem(tabName = "Sr",
                fluidRow(
                    #Create hill numbers plot
                    box(
                        solidHeader = TRUE, width = 6,
                        height = 920, background = "green",
                        selectInput(inputId = "sel_hill", label = "Select a indicator",
                                    choices = c("Sample coverage", "Species richness", 
                                                "Shannon diversity","Simpson diversity"),
                                    selected = "Sample coverage"),
                        plotOutput("hill_plot", height = 460),
                        br(),
                        br(),
                        br(),
                        tableOutput("table1")

                        
                    ),
                    box(
                        solidHeader = TRUE, width = 6,
                        height = 300, background = "green",
                        selectInput(inputId = "sel_com", label = "Select a stage",
                                    choices = c("All", "Adult","Nymph"),
                                    selected = "All"),
                        tableOutput("table2")
                        
                    ),
                    box(
                        solidHeader = TRUE, width = 6,
                        height = 600, background = "green",
                        imageOutput("image")
                    )
                   
                    
                ))
        
    ),
    
    
    
    
    tags$head(
        tags$style(HTML("
                        .content-wrapper {
                        background-color: linen !important;
                        }
                        .main-sidebar {
                        background-color: #2ca25f !important;
                        }
                        ")))
        ) 


dashboardPage(
    skin = "green",
    dashboardHeader(title = "Ticks surveillance"),
    sidebar,
    body
)