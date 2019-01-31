# app.R
#
# R Shiny web app for tracking individual I-CONECT participants
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)

# _ Heights & widths ----
SIDEBAR_WIDTH <- 250
TABLE_WIDTH <- 5
PLOT_WIDTH <- 12 - TABLE_WIDTH

# _ Data Table (DT) options ----
DT_OPTIONS <- list(paging = TRUE,
                   searching = TRUE,
                   ordering = TRUE,
                   info = TRUE,
                   lengthMenu = list(c(10, 25, 50, -1), 
                                     c("10", "25", "50", "All")),
                   pageLength = 25)



# **************************************** ----
# DEFINE UI ----
ui <- dashboardPage(
  
  # _ Header ----
  dashboardHeader(title = "I-CONECT Tracker", titleWidth = SIDEBAR_WIDTH),
  
  # _ Sidebar ----
  dashboardSidebar(
    
    # _ _ Sidebar menu ----
    sidebarMenu(
      id = "tabs",
      menuItem(text = "Study Status", tabName = "study_status",
               icon = icon("bullhorn")),
      menuItem(text = "Blah 1", tabName = "blah_1",
               icon = icon("filter"),
               menuSubItem(text = "Blah Sub 1",
                           tabName = "blah_sub_1"),
               menuSubItem("Blah Sub 2",
                           tabName = "blah_sub_2"))
    ),
    width = SIDEBAR_WIDTH
  ),
  
  # _ Body ----
  dashboardBody(
    # Layout tabItems to correspond to sidebarMenu
    tabItems(
      tabItem(
        # _ _ Recruitment tab ----
        tabName = "study_status",
        h1("Study Status"),
        fluidRow(
          box(
            dataTableOutput(outputId = "data_summ"),
            width = 12
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # _ _ Load recruitment status summary data ----
  study_status_summ <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = "./rds/data_summ.Rds",
    session = NULL,
    readFunc = readRDS
  )
  
  output$data_summ <- renderDataTable({
    datatable(study_status_summ(), options = DT_OPTIONS)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

