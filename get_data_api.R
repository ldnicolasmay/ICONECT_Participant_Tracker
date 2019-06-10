#!/usr/bin/env Rscript

# get_data_csv.R

#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@                                                                       @###
###                        GET, PROCESS, SAVE DATA                          ###
###                   Data Downloaded from REDCap as CSV                    ###
###@                                                                       @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#


# LOAD USEFUL LIBRARIES ----

suppressMessages( library(dplyr) )
suppressMessages( library(tidyr) )
suppressMessages( library(readr) )
suppressMessages( library(rlang) )
suppressMessages( library(purrr) )
suppressMessages( library(crayon) )
suppressMessages( library(stringr) )


# LOAD USEFUL GLOBALS / FUNCTIONS ----
wd_path <- paste0("~/Box Sync/Documents/I-CONECT/",
                  "Documents-USB/ICONECT_Participant_Tracker/")
source(paste0(wd_path, "get_data_helpers.R"))
source("~/Box Sync/Documents/R_helpers/config.R")


# GET RAW DATA (VIA API) ----

# Read proxy fields data from hand-built XLSX
proxy_fields_df <- readxl::read_excel(paste0(wd_path, "proxy_fields.xlsx"))
# proxy_fields_df <- read_csv(paste0(wd_path, "proxy_fields.csv"),
#                             col_types = cols(.default = col_character()))

# Define study data fields to keep
keeper_fields <-
  c(
    "ts_sub_id"
    , "redcap_event_name"
    , "redcap_repeat_instrument"
    , "redcap_repeat_instance"
    # , "ps_stt"
    , unique(proxy_fields_df$Field)
  )

keeper_fields_collapsed <- keeper_fields %>%
  str_replace(
    "redcap_event_name|redcap_repeat_instrument|redcap_repeat_instance", ""
  ) %>%
  stringi::stri_remove_empty() %>%
  paste(collapse = ",")

cat("Downloading JSON...")

json_ic <- RCurl::postForm(
  uri     = OHSU_REDCAP_API_URI,
  token   = OHSU_REDCAP_API_MAIN_TOKEN,
  content = 'record',
  format  = 'json',
  type    = 'flat',
  fields  = keeper_fields_collapsed,
  rawOrLabel             = 'raw',
  rawOrLabelHeaders      = 'raw',
  exportCheckboxLabel    = 'false',
  exportSurveyFields     = 'false',
  exportDataAccessGroups = 'false',
  returnFormat           = 'json'
  # Get only UM site IDs
  # , filterLogic = "([ts_sub_id] >= 'C2000')"
)

df_raw <- jsonlite::fromJSON(json_ic) %>% as_tibble() %>% na_if("")

# Define redcap_event_name values to keep
keeper_RENs <-
  c(
    unique(proxy_fields_df$REN)
    # , "admin_arm_1"
  ) %>%
  str_remove("^w\\d{2}(d\\d)?_(tel|vc)_arm_1$") %>%
  stringi::stri_remove_empty()


# PROCESS DATA ----

cat("Processing data...")

# Get only fields and rows of interest
df_cln <- df_raw %>%
  select(keeper_fields) %>%
  filter(str_detect(ts_sub_id, "^C\\d{4}$")) %>%
  filter(redcap_event_name %in% keeper_RENs) %>%
  type_convert(col_types = cols(
    .default = col_date(),
    ts_sub_id = col_character(),
    redcap_event_name = col_character(),
    redcap_repeat_instrument = col_character(),
    redcap_repeat_instance = col_character(),
    ps_stt = col_integer()
  ))

# Get unique IDs
ids <-
  df_cln %>%
  distinct(ts_sub_id) %>%
  pull() %>%
  sort()

# Get redcap_event_name values as strings in a character vector
ren_strs <-
  proxy_fields_df %>%
  distinct(REN) %>%
  pull() %>%
  # for now, filter out weekly telephone calls and daily video chats
  str_remove("^w\\d{2}(d\\d)?_(tel|vc)_arm_1$") %>%
  stringi::stri_remove_empty()

# For each redcap_event_name, create a df from `df_cln` that contains only
# the fields that are identified as relevant proxy fields from the
# `proxy_fields_df`, then pack all the dfs into a list.
dfs_rens <-
  map(.x = ren_strs,
      .f = reduce_df_by_proxy_field_ren, # sourced above
      df_cln,
      proxy_fields_df)

# Use the redcap_event_name strings to name the dfs in `dfs_rens`
names(dfs_rens) <- ren_strs

# The dfs in `dfs_rens` have values scattered across several rows, so these
# need to be collapsed into one row for each participant. That's what
# `collapse_df_rows_by_ren` does.
dfs_rens_rdc <-
  map(.x = ren_strs,
      .f = collapse_df_rows_by_ren,
      dfs_rens,
      ids)

# Again, use the redcap_event_name strings to name the dfs
# in `dfs_rens_rdc`
names(dfs_rens_rdc) <- ren_strs

# Ensure that each df in dfs_rens_rdc has all possible IDs
dfs_rens_rdc_aug1 <-
  map(dfs_rens_rdc,
      function(df) {
        if ("ts_sub_id" %in% names(df)) {
          left_join(tibble(ts_sub_id = ids),
                    df,
                    by = "ts_sub_id")
        } else {
          left_join(tibble(ts_sub_id = ids),
                    bind_rows(tibble(ts_sub_id = ids),
                              df),
                    by = "ts_sub_id")
        }
      })

# Ensure that each df in dfs_rens_rdc_aug2 has all appropriate fields
dfs_rens_rdc_aug2 <-
  map2(.x = dfs_rens_rdc_aug1,
       .y = ren_strs,
       .f = add_missing_fields,
       proxy_fields_df)

# Nest all the proxy fields for each ID in order to ease identification of
# incomplete instruments/forms
dfs_rens_rdc_aug_nst <-
  map(.x = dfs_rens_rdc_aug2,
      .f = function(df) {
        df %>%
          group_by(ts_sub_id) %>%
          nest()
      })

# cmp = complete
# Add column that identifies which stages are incomplete/pending/complete
# for each participant
dfs_rens_rdc_aug_nst_cmp <-
  map2(.x = dfs_rens_rdc_aug_nst[which(ren_strs != "admin_arm_1")],
       .y = ren_strs[which(ren_strs != "admin_arm_1")],
       .f = add_complete_col,
       proxy_fields_df)

dfs_rens_rdc_aug_nst_stt <-
  map2(.x = dfs_rens_rdc_aug_nst[which(ren_strs == "admin_arm_1")],
       .y = ren_strs[which(ren_strs == "admin_arm_1")],
       .f = add_status_col,
       proxy_fields_df)

dfs_rens_rdc_aug_nst_cmp <-
  c(
    dfs_rens_rdc_aug_nst_cmp
    , dfs_rens_rdc_aug_nst_stt
  )

# mfs = missing forms
# Add columns that help identify which instruments/forms are missing
# for each participant
dfs_rens_rdc_aug_nst_mfs <-
  map2(.x = dfs_rens_rdc_aug_nst,
       .y = ren_strs,
       .f = add_missing_forms_col,
       proxy_fields_df)


# SAVE DATA AS RDS ----

cat("Saving data as RDS files...")

saveRDS(dfs_rens_rdc_aug_nst_cmp,
        paste0(wd_path,
               "/ICONECT_Participant_Tracker/rds/",
               "dfs_rens_rdc_aug_nst_cmp.Rds"))

saveRDS(dfs_rens_rdc_aug_nst_mfs,
        paste0(wd_path,
               "/ICONECT_Participant_Tracker/rds/",
               "dfs_rens_rdc_aug_nst_mfs.Rds"))

cat("Done.")


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
