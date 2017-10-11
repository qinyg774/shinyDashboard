library(DT)
library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  title = "2015 New York City Tree Census",
  titleWidth = 350)

sidebar <- dashboardSidebar(
  width = 350,
  
  sidebarUserPanel("Yongguang Qin", image = "YongGuang2.jpg"),
  
  sidebarMenu(
    menuItem("Tree Size", tabName = "treesize", icon = icon("expand")),
    menuItem("Species", tabName = "species", icon = icon("tree")),
    menuItem("Condition", tabName = "condition", icon = icon("stethoscope")),
    menuItem("Data", tabName = "data", icon = icon("database"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "treesize",
      fluidRow(
        plotOutput("size1")
      ),
      fluidRow(
        box(selectInput("species", "Choose species", top_species))
      )
    ),
    
   
    tabItem(
      tabName = "species",
      fluidRow(
        box(checkboxInput("alive_only", "Alive Trees Only", value = TRUE), width = 4, height = 120),
        box(selectizeInput("boro", "Select Borough", boros), width = 4, height = 120),
        box(sliderInput("top", "Select Top N", min = 3, max = 20, value = 5, step = 1), width = 4, height = 120)
      ),
      br(),
      fluidRow(
        htmlOutput("species2")
      )
    ),
    
    
    tabItem(
      tabName = "condition",
      fluidRow(
        box(selectInput("site", "Choose curb location", choices = sites)),
        box(selectInput("sort_hi", "Choose sort-by", choices = health_indicators))
      ),
      fluidRow(
        box(plotOutput("health_heatmap"), width = 8),
        box(selectInput("species_multi", "Choose up to 10 species", 
                        choices = top_species[-1], 
                        selected = top_species[2:11],
                        multiple = T), width = 4
        )
      )
    ),
    
    tabItem(
      tabName = "data",
      fluidRow(
        DT::dataTableOutput("table")
      )
    )
  )
)

dashboardPage(skin='green', header, sidebar, body)