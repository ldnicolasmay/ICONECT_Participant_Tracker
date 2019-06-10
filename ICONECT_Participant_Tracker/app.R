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
        text = "Statuses",
        tabName = "statuses",
        icon = icon("table")
      ),
      menuItem(
        text = "Missing Forms",
        tabName = "missing_forms",
        icon = icon("table")
      )
    ),
    collapsed = FALSE),

  # Body ----
  dashboardBody(# Tab container
    tabItems(
      # tabItem for "summary"
      tabItem(
        tabName = "statuses",
        h2("Participant Statuses"),
        fluidRow(
          box(
            checkboxGroupInput(
              inputId = "status_values",
              label = "Status Values",
              choices = c("In Screening",
                          "Screen Failure",
                          "Enrolled",
                          "Active",
                          "In Follow-Up",
                          "Discontinued"),
              selected = c("Active"),
              inline = TRUE
            ),
            width = 12
          )
        ),
        fluidRow(
          box(
            dataTableOutput("statuses"),
            width = 12
          )
        )
      ),
      # tabItem for "missing_forms"
      tabItem(
        tabName = "missing_forms",
        h2("Participant Missing Forms"),
        fluidRow(
          tabBox(
            width = 12,
            id = "tabset_missforms_tbls",
            ###
            tabPanel(title = "Telephone Screen",
                     dataTableOutput("mfs_scrn_tel_arm_1")),
            tabPanel(title = "Screen Visit",
                     dataTableOutput("mfs_scrn_v_arm_1")),
            tabPanel(title = "Baseline CDx",
                     dataTableOutput("mfs_bl_cdx_arm_1")),
            tabPanel(title = "Baseline MRI",
                     dataTableOutput("mfs_bl_mri_arm_1")),
            tabPanel(title = "Baseline Visit",
                     dataTableOutput("mfs_bl_v_arm_1")),
            tabPanel(title = "Month 6 CDx",
                     dataTableOutput("mfs_06_cdx_arm_1")),
            tabPanel(title = "Month 6 MRI",
                     dataTableOutput("mfs_06_mri_arm_1")),
            tabPanel(title = "Month 6 Visit",
                     dataTableOutput("mfs_06_v_arm_1")),
            tabPanel(title = "Month 12 CDx",
                     dataTableOutput("mfs_12_cdx_arm_1")),
            tabPanel(title = "Month 12 Visit",
                     dataTableOutput("mfs_12_v_arm_1")),
            tabPanel(title = "Telephone Follow Up",
                     dataTableOutput("mfs_fup_tel_arm_1"))
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

  dfs_rens_rds_aug_nst_cmp <-
    reactiveFileReader(
      intervalMillis = invalidation_time,
      filePath = "rds/dfs_rens_rdc_aug_nst_cmp.Rds",
      readFunc = readRDS,
      session = NULL
    )

  dfs_rens_rdc_aug_nst_mfs <-
    reactiveFileReader(
      intervalMillis = invalidation_time,
      filePath = "rds/dfs_rens_rdc_aug_nst_mfs.Rds",
      readFunc = readRDS,
      session = NULL
    )

  ren_strs <- reactive({ names(dfs_rens_rdc_aug_nst_mfs()) })

  output$statuses <-
    renderDataTable({
      datatable(
        imap_dfc(.x = dfs_rens_rds_aug_nst_cmp(),
                 ~ {
                   if (.y == "scrn_tel_arm_1") {
                     .x %>%
                       select(ts_sub_id,
                              !!sym(paste0(.y, "\ncomplete")) := complete)
                   } else {
                     .x %>%
                       select(!!sym(paste0(.y, "\ncomplete")) := complete)
                   }
                 }) %>%
          filter(str_detect(ts_sub_id, "^C2\\d{3}$")) %>%
          mutate(Treatment = case_when(
            `admin_arm_1\ncomplete` == "Active" ~ "Yes",
            TRUE ~ "No"
          )) %>%
          select(
            `ID` = `ts_sub_id`,
            `Status` = `admin_arm_1\ncomplete`,
            `Tel Screen` = `scrn_tel_arm_1\ncomplete`,
            `Screen Visit` = `scrn_v_arm_1\ncomplete`,
            `BL Visit` = `bl_v_arm_1\ncomplete`,
            `BL MRI` = `bl_mri_arm_1\ncomplete`,
            `BL CDx` = `bl_cdx_arm_1\ncomplete`,
            `Treatment`,
            `M 06 Visit` = `06_v_arm_1\ncomplete`,
            `M 06 MRI` = `06_mri_arm_1\ncomplete`,
            `M 06 CDx` = `06_cdx_arm_1\ncomplete`,
            `M 12 Visit` = `12_v_arm_1\ncomplete`,
            `M 12 CDx` = `12_cdx_arm_1\ncomplete`,
            `Tel Follow Up` = `fup_tel_arm_1\ncomplete`
          ) %>%
          filter(`Status` %in% input$status_values),
        options = DT_OPTIONS,
        escape = FALSE
      ) %>%
        formatStyle(.,
                    c(
                      "Tel Screen",
                      "Screen Visit",
                      "BL CDx",
                      "BL MRI",
                      "BL Visit",
                      "M 06 CDx",
                      "M 06 MRI",
                      "M 06 Visit",
                      "M 12 CDx",
                      "M 12 Visit",
                      "Tel Follow Up"
                    ),
                    backgroundColor =
                      styleEqual(c("No", "Pending", "Yes"),
                                 c("#ff8080", "#ffcc99", "lightgreen"))) %>%
        formatStyle(.,
                    c("Treatment"),
                    backgroundColor =
                      styleEqual(c("Yes"),
                                 c("#99bbff"))) %>%
        formatStyle(.,
                    c("Status"),
                    backgroundColor =
                      styleEqual(c("Active"),
                                 c("#99bbff")))
    })

  observe({
    map(.x = ren_strs(),
        .f = function(ren_str) {
          output[[paste0("mfs_", ren_str)]] <- # eg, "mfs_scrn_tel_arm_1"
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
