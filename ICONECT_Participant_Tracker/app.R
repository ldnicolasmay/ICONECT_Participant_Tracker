# app.R

#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@                                                                       @###
###                                   APP                                   ###
###@                                                                       @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#


# LOAD LIBRARIES ----

suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))

suppressMessages( library(DT) )
suppressMessages( library(dplyr) )
suppressMessages( library(tidyr) )
suppressMessages( library(purrr) )
suppressMessages( library(rlang) )
suppressMessages( library(stringr) )
suppressMessages( library(lubridate) )


# USEFUL GLOBALS ----

DEPLOYED <- FALSE
# DEPLOYED <- TRUE

DT_OPTIONS <- list(
  paging = FALSE,
  searching = TRUE,
  searchHighlight = TRUE,
  search = list(regex = TRUE, caseInsensitive = TRUE),
  ordering = TRUE,
  info = FALSE
)


out <-
  c(
    'tabPanel(title = "Telephone Screen", dataTableOutput("scrn_tel_arm_1"))',
    'tabPanel(title = "Screen Visit", dataTableOutput("scrn_v_arm_1"))',
    'tabPanel(title = "Baseline CDx", dataTableOutput("bl_cdx_arm_1"))',
    'tabPanel(title = "Baseline MRI", dataTableOutput("bl_mri_arm_1"))'
  )

# Define UI for application that draws a histogram
ui <- dashboardPage(

  # Header ----
  dashboardHeader(
    title = "I-CONECT Pt Tracker"
  ),

  # Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Summary",
        tabName = "summary",
        icon = icon("table")
      )
    ),
    collapsed = TRUE),

  # Body ----
  dashboardBody(# Tab container
    tabItems(
      # tabItem for "summary"
      tabItem(
        tabName = "summary",
        h2("Participant Status Summaries"),
        fluidRow(
          tabBox(
            width = 12,
            id = "tabset_summ_tbls",
            ###
            tabPanel(title = "Telephone Screen",
                     dataTableOutput("scrn_tel_arm_1")),
            tabPanel(title = "Screen Visit",
                     dataTableOutput("scrn_v_arm_1")),
            tabPanel(title = "Baseline CDx",
                     dataTableOutput("bl_cdx_arm_1")),
            tabPanel(title = "Baseline MRI",
                     dataTableOutput("bl_mri_arm_1")),
            tabPanel(title = "Baseline Visit",
                     dataTableOutput("bl_v_arm_1")),
            tabPanel(title = "Month 6 CDx",
                     dataTableOutput("06_cdx_arm_1")),
            tabPanel(title = "Month 6 MRI",
                     dataTableOutput("06_mri_arm_1")),
            tabPanel(title = "Month 6 Visit",
                     dataTableOutput("06_v_arm_1")),
            tabPanel(title = "Month 12 CDx",
                     dataTableOutput("12_cdx_arm_1")),
            tabPanel(title = "Month 12 Visit",
                     dataTableOutput("12_v_arm_1")),
            tabPanel(title = "Telephone Follow Up",
                     dataTableOutput("fup_tel_arm_1"))
            ###
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Read Data ----

  invalidation_time <- 1000 * 60 * 60 * 6 # 6-hour refresh
  #                    ^      ^    ^    ^
  #                    |      |    |    |> 6 hr
  #                    |      |    |> 60 min / hr
  #                    |      |> 60 sec / min
  #                    |> 1000 ms / sec
  # invalidation_time <- 1000 * 60 * 5 # 5-minute refresh (debug)

  dfs_rens_rdc_aug_nst_mfs <-
    reactiveFileReader(
      intervalMillis = invalidation_time,
      filePath = "rds/dfs_rens_rdc_aug_nst_mfs.Rds",
      readFunc = readRDS,
      session = NULL
    )

  ren_strs <- reactive({ names(dfs_rens_rdc_aug_nst_mfs()) })

  observe({
    map(.x = ren_strs(),
        .f = function(ren_str) {
          output[[ren_str]] <-
            renderDataTable({
              datatable(
                dfs_rens_rdc_aug_nst_mfs()[[ren_str]] %>%
                  select(`Participant ID` = ts_sub_id,
                         `Missing Forms` = missing_forms) %>%
                  # Only keep UM IDs for now
                  filter(str_detect(`Participant ID`, "^C2\\d{3}$")),
                options = DT_OPTIONS,
                escape = FALSE)
            })
        }
    )
  })

  # # Example of single data table renders w/o observe-lapply
  # output$scrn_tel_arm_1 <-
  #   renderDataTable({
  #     datatable(dfs_rens_rdc_aug_nst_mfs()[["scrn_tel_arm_1"]]
  #               , options = DT_OPTIONS)
  #   })

}

# Run the application
shinyApp(ui = ui, server = server)


###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
