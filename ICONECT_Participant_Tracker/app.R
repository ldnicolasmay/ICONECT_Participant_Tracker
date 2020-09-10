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

suppressMessages( library(shiny)          )
suppressMessages( library(shinydashboard) )
suppressMessages( library(DT)             )

suppressMessages( library(dplyr)     )
suppressMessages( library(tidyr)     )
suppressMessages( library(readr)     )
suppressMessages( library(purrr)     )
suppressMessages( library(rlang)     )
suppressMessages( library(stringr)   )
suppressMessages( library(lubridate) )


# USEFUL GLOBALS ----

# source("../get_data_helpers.R")
derive_date_range <- function(origin, num_weeks, week_range) {
  range_start  <- as_date(origin) + dweeks(num_weeks - week_range)
  range_finish <- as_date(origin) + dweeks(num_weeks + week_range)
  paste0(range_start, " -- ", range_finish)
}

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
      ),
      menuItem(
        text = "Study Progress",
        menuSubItem(
          text = "Screening",
          tabName = "screening",
          icon = icon("table")
        ),
        menuSubItem(
          text = "Baseline",
          tabName = "baseline",
          icon = icon("table")
        ),
        menuSubItem(
          text = "Activation",
          tabName = "activation",
          icon = icon("table")
        ),
        menuSubItem(
          text = "Complete",
          tabName = "complete",
          icon = icon("table")
        )
      )
    ),
    checkboxGroupInput(
      inputId = "site_values",
      label = "Sites",
      choices = c("OHSU", "UM"),
      selected = c("OHSU", "UM"),
      inline = FALSE
    ),
    fluidRow(box(
      textOutput(outputId = "data_update"),
      width = 12,
      background = "black"
    )),
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
      ), # ... end tabItem for "summary"
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
      ), # ... end tabItem for "missing_forms"
      # tabItem for "screening"
      tabItem(
        tabName = "screening",
        h2("Screening"),
        fluidRow(
          box(
            dataTableOutput("screening"),
            width = 12
          )
        )
      ), # ... end tabItem for "screening"
      # tabItem for "baseline"
      tabItem(
        tabName = "baseline",
        h2("Baseline"),
        fluidRow(
          box(
            dataTableOutput("baseline"),
            width = 12
          )
        )
      ), # ... end tabItem for "baseline"
      # tabItem for "activation"
      tabItem(
        tabName = "activation",
        h2("Activation"),
        fluidRow(
          box(
            dataTableOutput("activation"),
            width = 12
          )
        )
      ), # ... end tabItem for "activation"
      # tabItem for "complete"
      tabItem(
        tabName = "complete",
        h2("Complete"),
        fluidRow(
          box(
            dataTableOutput("complete"),
            width = 12
          )
        )
      ) # ... end tabItem for "screening"
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

  # dfs  -> dataframes
  # sbl  -> screening + baseline (excl. weekly phone calls & daily video chats)
  # rens -> redcap event names
  # rdc  -> reduced
  # aug  -> augmented
  # nst  -> nested
  # cmp  -> complete (empty columns added for consistency)
  # flt  -> filtered
  # mfs  -> missing forms
  # act  -> activation (incl. only weekly phone calls & daily video chats)

  # Load `dfs_sbl_rens_rdc_aug_nst_cmp.Rds`
  dfs_sbl_rens_rdc_aug_nst_cmp <-
    reactiveFileReader(
      intervalMillis = invalidation_time,
      filePath = "rds/dfs_sbl_rens_rdc_aug_nst_cmp.Rds",
      readFunc = readRDS,
      session = NULL
    )

  # Apply app-wide reactive site checkbox filter
  dfs_sbl_rens_rdc_aug_nst_cmp_flt <-
    reactive({
      map(.x = dfs_sbl_rens_rdc_aug_nst_cmp(),
          .f = function(df) {
            df %>%
              mutate(site = case_when(
                str_detect(ts_sub_id, "^C1\\d{3}$") ~ "OHSU",
                str_detect(ts_sub_id, "^C2\\d{3}$") ~ "UM",
                TRUE ~ NA_character_
              )) %>%
              filter(site %in% input$site_values)
          })
    })

  # Load `dfs_sbl_rens_rdc_aug_nst_mfs.Rds`
  dfs_sbl_rens_rdc_aug_nst_mfs <-
    reactiveFileReader(
      intervalMillis = invalidation_time,
      filePath = "rds/dfs_sbl_rens_rdc_aug_nst_mfs.Rds",
      readFunc = readRDS,
      session = NULL
    )

  # Apply app-wide reactive site checkbox filter
  dfs_sbl_rens_rdc_aug_nst_mfs_flt <-
    reactive({
      map(.x = dfs_sbl_rens_rdc_aug_nst_mfs(),
          .f = function(df) {
            df %>%
              mutate(site = case_when(
                str_detect(ts_sub_id, "^C1\\d{3}$") ~ "OHSU",
                str_detect(ts_sub_id, "^C2\\d{3}$") ~ "UM",
                TRUE ~ NA_character_
              )) %>%
              filter(site %in% input$site_values)
          })
    })

  df_cln_act_sel_mut_flt <-
    reactiveFileReader(
      intervalMillis = invalidation_time,
      filePath = "rds/df_cln_act_sel_mut_flt.Rds",
      readFunc = readRDS,
      session = NULL
    )

  df_cln_act_sel_mut_flt_flt <-
    reactive({
      df_cln_act_sel_mut_flt() %>%
        mutate(site = case_when(
          str_detect(ts_sub_id, "^C1\\d{3}$") ~ "OHSU",
          str_detect(ts_sub_id, "^C2\\d{3}$") ~ "UM",
          TRUE ~ NA_character_
        )) %>%
        filter(site %in% input$site_values)
    })

  ren_strs <- reactive({ names(dfs_sbl_rens_rdc_aug_nst_mfs_flt()) })

  # Get timestamp for when data last updated
  timestamp <- reactive({
    as.character(as_date(file.info("./rds/df_cln_act_sel_mut_flt.Rds")$mtime))
  })

  output$data_update <- renderText({ paste0("Data as of: ", timestamp()) })

  # Statuses tab ----

  output$statuses <-
    renderDataTable({
      datatable(
        imap_dfc(.x = dfs_sbl_rens_rdc_aug_nst_cmp_flt(),
                 ~ {
                   if (.y == "scrn_tel_arm_1") {
                     .x %>%
                       select(ts_sub_id,
                              !!sym(paste0(.y, "_complete")) := complete)
                   } else {
                     .x %>%
                       select(!!sym(paste0(.y, "_complete")) := complete)
                   }
                 }) %>%
          select(
            `ID`              = `ts_sub_id`
            , `Status`        = `admin_arm_1_complete`
            , `Tel Screen`    = `scrn_tel_arm_1_complete`
            , `Screen Visit`  = `scrn_v_arm_1_complete`
            , `BL Visit`      = `bl_v_arm_1_complete`
            , `BL MRI`        = `bl_mri_arm_1_complete`
            , `BL CDx`        = `bl_cdx_arm_1_complete`
            , `M 06 Visit`    = `06_v_arm_1_complete`
            , `M 06 MRI`      = `06_mri_arm_1_complete`
            , `M 06 CDx`      = `06_cdx_arm_1_complete`
            , `M 12 Visit`    = `12_v_arm_1_complete`
            , `M 12 CDx`      = `12_cdx_arm_1_complete`
            , `Tel Follow Up` = `fup_tel_arm_1_complete`
          ) %>%
          filter(`Status` %in% input$status_values),
        options = DT_OPTIONS,
        escape = FALSE
      ) %>%
        formatStyle(.,
                    c(
                      "Tel Screen"
                      , "Screen Visit"
                      , "BL CDx"
                      , "BL MRI"
                      , "BL Visit"
                      , "M 06 CDx"
                      , "M 06 MRI"
                      , "M 06 Visit"
                      , "M 12 CDx"
                      , "M 12 Visit"
                      , "Tel Follow Up"
                    ),
                    backgroundColor =
                      styleEqual(c("No", "Pending", "Yes"),
                                 c("#ff8080", "#ffcc99", "lightgreen"))) %>%
        formatStyle(.,
                    c("Status"),
                    backgroundColor =
                      styleEqual(c("Active"),
                                 c("#99bbff")))
    })

  # Missing Forms tab ----

  observe({
    map(.x = ren_strs(),
        .f = function(ren_str) {
          output[[paste0("mfs_", ren_str)]] <- # eg, "mfs_scrn_tel_arm_1"
            renderDataTable({
              datatable(
                dfs_sbl_rens_rdc_aug_nst_mfs_flt()[[ren_str]] %>%
                  select(`Participant ID` = ts_sub_id,
                         `Missing Forms`  = missing_forms),
                options = DT_OPTIONS,
                escape = FALSE)
            })
        }
    )
  })

  # # Example of single data table renders w/o observe-lapply
  # output$scrn_tel_arm_1 <-
  #   renderDataTable({
  #     datatable(dfs_sbl_rens_rdc_aug_nst_mfs_flt()$scrn_tel_arm_1
  #               , options = DT_OPTIONS)
  #   })

  # Screening Tab ----

  df_screening_bl_cdx_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$bl_cdx_arm_1 %>%
        filter(complete != "Yes") %>%
        unnest(data) %>%
        select(ts_sub_id, d1_dat, elg_dat, elg_yn) %>%
        mutate(elg_yn = case_when(
          elg_yn == 0 ~ "No",
          elg_yn == 1 ~ "Yes",
          TRUE ~ NA_character_
        ))
    })

  df_screening_scrn_tel_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$scrn_tel_arm_1 %>%
        unnest(data) %>%
        select(ts_sub_id)
    })

  df_screening_scrn_v_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$scrn_v_arm_1 %>%
        unnest(data) %>%
        select(ts_sub_id, con_ins, con_dtc, mrp_dat, mrp_saf, mrp_yn) %>%
        # mutate(days_since_con_dtc =
        #          as.integer(today() - as_date(con_dtc))) %>%
        mutate(weeks_since_con_dtc =
                 (as.integer(today() - as_date(con_dtc))) %/% 7) %>%
        mutate(mrp_saf = case_when(
          mrp_saf == 0 ~ "No",
          mrp_saf == 1 ~ "Yes",
          TRUE ~ NA_character_
        )) %>%
        mutate(mrp_yn = case_when(
          mrp_yn == 0 ~ "No",
          mrp_yn == 1 ~ "Yes",
          TRUE ~ NA_character_
        ))
    })

  df_screening_admin_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$admin_arm_1 %>%
        unnest(data) %>%
        select(ts_sub_id, ps_stt, ran_dat)
    })

  df_screening <-
    reactive({
      left_join(df_screening_bl_cdx_arm_1(),
                df_screening_scrn_tel_arm_1(),
                by = "ts_sub_id") %>%
        left_join(.,
                  df_screening_scrn_v_arm_1(),
                  by = "ts_sub_id") %>%
        left_join(.,
                  df_screening_admin_arm_1(),
                  by = "ts_sub_id") %>%
        filter(is.na(elg_yn) | elg_yn == "Yes") %>%
        filter(!is.na(con_dtc)) %>%
        filter(is.na(ran_dat)) %>%
        filter(ps_stt != 2) %>%
        select(
          `Participant ID`             = ts_sub_id
          , `Assessor`                 = con_ins
          , `Consent Date`             = con_dtc
          # , `Days Since Consent`       = days_since_con_dtc
          , `Weeks Since Consent`      = weeks_since_con_dtc
          , `CDx Date`                 = d1_dat
          , `Study Elig. Determ. Date` = elg_dat
          , `Study Eligible`           = elg_yn
          , `MRI Safety Date`          = mrp_dat
          , `MRI Safety Admin.`        = mrp_saf
          # , `MRI Eligible`             = mrp_yn
        )
    })

  output$screening <-
    renderDataTable(
      datatable(
        df_screening(),
        options = DT_OPTIONS,
        escape = FALSE
      )
    )

  # Baseline tab ----

  df_baseline_scrn_v_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$scrn_v_arm_1 %>%
        unnest(data) %>%
        select(ts_sub_id, con_ins, con_dtc) %>%
        # mutate(days_since_con_dtc =
        #          as.integer(today() - as_date(con_dtc)))
        mutate(weeks_since_con_dtc =
                 (as.integer(today() - as_date(con_dtc))) %/% 7)
    })

  df_baseline_scrn_tel_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$scrn_tel_arm_1 %>%
        unnest(data) %>%
        select(ts_sub_id)
    })

  df_baseline_admin_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$admin_arm_1 %>%
        unnest(data) %>%
        select(ts_sub_id, ran_dat, ps_stt, ps_dod) %>%
        # mutate(days_since_ran_dat =
        #          as.integer(today() - as_date(ran_dat)))
        mutate(weeks_since_ran_dat =
                 (as.integer(today() - as_date(ran_dat))) %/% 7)
    })

  df_baseline_bl_v_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$bl_v_arm_1 %>%
        unnest(data) %>%
        select(ts_sub_id, stb_dat, redcap_repeat_instance) %>%
        mutate(redcap_repeat_instance = as.integer(redcap_repeat_instance))
    })

  df_baseline_bl_mri_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$bl_mri_arm_1 %>%
        unnest(data) %>%
        select(ts_sub_id, mcf_dat)
    })

  ids_act <-
    reactive({
      df_cln_act_sel_mut_flt() %>%
        distinct(ts_sub_id) %>%
        pull
    })

  df_baseline <-
    reactive({
      left_join(df_baseline_scrn_v_arm_1(),
                df_baseline_scrn_tel_arm_1(),
                by = "ts_sub_id") %>%
        left_join(.,
                  df_baseline_admin_arm_1(),
                  by = "ts_sub_id") %>%
        filter(!(ts_sub_id %in% ids_act())) %>%  # filter out pts in activation
        filter(!is.na(ran_dat)) %>%  # filter out anyone w/o randomization date
        filter(ps_stt != 6) %>%      # filter out anyone who's discontinued
        filter(is.na(ps_dod)) %>%    # filter out anyone who's discontinued
        left_join(.,
                  df_baseline_bl_v_arm_1(),
                  by = "ts_sub_id") %>%
        left_join(.,
                  select(df_screening_scrn_v_arm_1(), ts_sub_id, mrp_yn),
                  by = "ts_sub_id") %>%
        left_join(.,
                  df_baseline_bl_mri_arm_1(),
                  by = "ts_sub_id") %>%
        select(
          `Participant ID`               = ts_sub_id
          , `Assessor`                   = con_ins
          , `Consent Date`               = con_dtc
          # , `Days Since Consent`         = days_since_con_dtc
          , `Weeks Since Consent`        = weeks_since_con_dtc
          , `Randomiz. Date`             = ran_dat
          # , `Days Since Randomiz.`       = days_since_ran_dat
          , `Weeks Since Randomiz.`      = weeks_since_ran_dat
          , `Last Baseline Visit Date`   = stb_dat
          , `Last Baseline Visit Number` = redcap_repeat_instance
          , `MRI Eligible`               = mrp_yn
          , `MRI Date`                   = mcf_dat
        )
    })

  output$baseline <-
    renderDataTable(
      datatable(
        df_baseline(),
        options = DT_OPTIONS,
        escape = FALSE
      )
    )

  # Activation tab ----

  df_activation_base <- reactive({ df_cln_act_sel_mut_flt_flt() })

  df_activation_scrn_tel_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$scrn_tel_arm_1 %>%
        unnest(data) %>%
        select(ts_sub_id)
    })

  df_activation_admin_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$admin_arm_1 %>%
        unnest(data) %>%
        select(ts_sub_id, ps_stt, ps_dod)
    })

  df_activation_scrn_v_arm_1 <-
    reactive({
      df_screening_scrn_v_arm_1() %>%
        select(ts_sub_id, con_ins, mrp_yn)
    })

  df_activation_bl_mri_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$bl_mri_arm_1 %>%
        unnest(data) %>%
        select(ts_sub_id, mcf_3)
    })

  ids_cmp <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$fup_tel_arm_1 %>%
        unnest(data) %>%
        filter(complete == "Yes") %>%
        distinct(ts_sub_id) %>%
        pull()
    })

  df_activation <-
    reactive({
      left_join(df_activation_base(),
                df_activation_scrn_tel_arm_1(),
                by = "ts_sub_id") %>%
        left_join(.,
                  df_activation_admin_arm_1(),
                  by = "ts_sub_id") %>%
        left_join(.,
                  df_activation_scrn_v_arm_1(),
                  by = "ts_sub_id") %>%
        left_join(.,
                  df_activation_bl_mri_arm_1(),
                  by = "ts_sub_id") %>%
        filter(!(ts_sub_id %in% ids_cmp())) %>%
        filter(ps_stt != 6) %>%      # filter out anyone who's discontinued
        filter(is.na(ps_dod)) %>%    # filter out anyone who's discontinued
        select(
          `Participant ID`          = ts_sub_id
          , `Assessor`              = con_ins
          , `Study Week`            = week_max
          , `Week 1 Day 1`          = wkq_dat_monday_min
          , `Baseline MRI`          = mcf_3
          , `Approx 6 Month Visit`  = approx_06_mo_vis
          , `Approx 12 Month Visit` = approx_12_mo_vis
          , `Week 52 Follow Up`     = fllwup_52_wk
          , `MRI Eligible`          = mrp_yn
          # , `Approx 6 Month MRI`    = approx_06_mo_mri
        )
    })


  output$activation <-
    renderDataTable(
      datatable(
        df_activation(),
        options = DT_OPTIONS,
        escape = FALSE
      )
    )

  # Complete tab ----

  df_complete_scrn_v_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$scrn_v_arm_1 %>%
        unnest(data) %>%
        select(ts_sub_id, con_ins)
    })

  df_complete_fup_tel_arm_1 <-
    reactive({
      dfs_sbl_rens_rdc_aug_nst_cmp_flt()$fup_tel_arm_1 %>%
        unnest(data) %>%
        filter(complete == "Yes") %>%
        select(ts_sub_id, complete, lsn_dat)
    })

  df_complete <- reactive({
    right_join(df_complete_scrn_v_arm_1(),
               df_complete_fup_tel_arm_1(),
               by = "ts_sub_id") %>%
      select(
        `Participant ID`    = ts_sub_id
        , `Assessor`        = con_ins
        , `Complete`        = complete
        , `Completion Date` = lsn_dat
      )
  })

  output$complete <-
    renderDataTable(
      datatable(
        df_complete(),
        options = DT_OPTIONS,
        escape = FALSE
      )
    )

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
