#!/usr/bin/env Rscript

# get_data_csv.R

#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@                                                                       @###
###                        GET, PROCESS, SAVE DATA                          ###
###                   Data Downloaded from REDCap as JSON                   ###
###@                                                                       @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#


# LOAD USEFUL LIBRARIES ----

suppressMessages( library(crayon) )

cat(green("Loading packages and useful functions and globals...\n"))

suppressMessages( library(dplyr) )
suppressMessages( library(tidyr) )
suppressMessages( library(readr) )
suppressMessages( library(rlang) )
suppressMessages( library(purrr) )
suppressMessages( library(stringr) )
suppressMessages( library(lubridate) )


# LOAD USEFUL GLOBALS / FUNCTIONS ----

DOCKER_DEV <- TRUE    # when developing using docker container
# DOCKER_DEV <- FALSE   # when developing using local machine (not advised)

if (DOCKER_DEV) {
  box_path <- "/Box/"
} else {
  box_path <- "~/Box/"
}

wd_path <- paste0(box_path,
                  "Documents/I-CONECT/",
                  "Documents-USB/ICONECT_Participant_Tracker/")

source(paste0(wd_path, "get_data_helpers.R"))
source(paste0(box_path, "Documents/R_helpers/config.R"))


# GET RAW DATA (VIA API) ----

cat(
  paste0(green("Retrieving control data from "),
         white(bold("proxy_fields.xlsx")),
         green(" ...\n")
  )
)

# Read proxy fields data from hand-built XLSX
proxy_fields_df <-
  readxl::read_excel(path = paste0(wd_path, "proxy_fields.xlsx"),
                     sheet = "Sheet1")

# Define study data fields to keep
keeper_fields <-
  c(
    "ts_sub_id"
    , "redcap_event_name"
    , "redcap_repeat_instrument"
    , "redcap_repeat_instance"
    , unique(proxy_fields_df$Field)
  )

keeper_fields_collapsed <- keeper_fields %>%
  str_replace(
    "redcap_event_name|redcap_repeat_instrument|redcap_repeat_instance", ""
  ) %>%
  str_subset(".+") %>%
  paste(collapse = ",")

keeper_fields_sbl <- keeper_fields %>%
  # filter out weekly telephone calls and daily video chats
  str_remove("wkq_dat|vcd_dat") %>%
  str_subset(".+")

keeper_fields_act <- keeper_fields %>%
  # keep only weekly telephone calls and daily video chats
  str_extract("ts_sub_id|redcap.*|wkq_dat|vcd_dat") %>%
  str_subset(".+")

cat(green("Downloading JSON...\n"))

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
)

df_raw <- jsonlite::fromJSON(json_ic) %>% as_tibble() %>% na_if("")

# Define redcap_event_name values to keep
keeper_RENs_sbl <-
  c(
    unique(proxy_fields_df$REN)
  ) %>%
  # filter out weekly telephone calls and daily video chats
  str_remove("^w\\d{2}(d\\d)?_(tel|vc)_arm_1$") %>%
  str_subset(".+")

keeper_RENs_act <-
  c(
    unique(proxy_fields_df$REN)
  ) %>%
  # keep only weekly telephone calls and daily video chats
  str_extract("^w\\d{2}(d\\d)?_(tel|vc)_arm_1$") %>%
  str_subset(".+")


# PROCESS DATA ----

cat(green("Processing data...\n"))

# Get only fields and rows of interest
df_cln_sbl <- df_raw %>%
  select(keeper_fields_sbl) %>%
  filter(str_detect(ts_sub_id, "^C\\d{4}$")) %>%
  filter(redcap_event_name %in% keeper_RENs_sbl) %>%
  type_convert(col_types = cols(
    .default = col_date(),
    ts_sub_id = col_character(),
    redcap_event_name = col_character(),
    redcap_repeat_instrument = col_character(),
    redcap_repeat_instance = col_character(),
    mrp_saf = col_integer(),
    mrp_yn = col_integer(),
    elg_yn = col_integer(),
    ps_stt = col_integer()
  ))

df_cln_act <- df_raw %>%
  select(keeper_fields_act) %>%
  filter(str_detect(ts_sub_id, "^C\\d{4}$")) %>%
  filter(redcap_event_name %in% keeper_RENs_act) %>%
  type_convert(col_types = cols(
    .default = col_date(),
    ts_sub_id = col_character(),
    redcap_event_name = col_character(),
    redcap_repeat_instrument = col_character(),
    redcap_repeat_instance = col_character()
  ))

df_cln_act_sel <- df_cln_act %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance) %>%
  filter(!is.na(wkq_dat))

df_cln_act_sel_mut <- df_cln_act_sel %>%
  mutate(wkq_dat_monday =
           # date...          reset to prev Saturday...  add 2 to get Monday
           as.Date(wkq_dat) - lubridate::wday(wkq_dat) + 2L)

df_cln_act_sel_mut_fltmin <- df_cln_act_sel_mut %>%
  group_by(ts_sub_id) %>%
  filter(redcap_event_name == min(redcap_event_name)) %>%
  rename(redcap_event_name_min = redcap_event_name,
         wkq_dat_min           = wkq_dat,
         wkq_dat_monday_min    = wkq_dat_monday) %>%
  ungroup()


df_cln_act_sel_mut_fltmax <- df_cln_act_sel_mut %>%
  group_by(ts_sub_id) %>%
  filter(redcap_event_name == max(redcap_event_name)) %>%
  mutate(week_max =
           as.integer(str_extract(redcap_event_name, "(?<=w)\\d{2}"))) %>%
  rename(redcap_event_name_max = redcap_event_name,
         wkq_dat_max           = wkq_dat,
         wkq_dat_monday_max    = wkq_dat_monday) %>%
  ungroup()

df_cln_act_sel_mut_flt <-
  full_join(df_cln_act_sel_mut_fltmin, df_cln_act_sel_mut_fltmax,
            by = "ts_sub_id") %>%
  filter(redcap_event_name_min == "w01_tel_arm_1") %>%
  select(ts_sub_id,
         week_max,                # study week
         wkq_dat_monday_min) %>%  # effective w1d1 date
  mutate(approx_06_mo = as.Date(wkq_dat_monday_min) + dweeks(25L),
         approx_12_mo = as.Date(wkq_dat_monday_min) + dweeks(49L))

# Get unique IDs
ids <-
  df_cln_sbl %>%
  distinct(ts_sub_id) %>%
  pull() %>%
  sort()

# Get redcap_event_name values as strings in a character vector
ren_strs_sbl <-
  proxy_fields_df %>%
  distinct(REN) %>%
  pull() %>%
  # filter out weekly telephone calls and daily video chats
  str_remove("^w\\d{2}(d\\d)?_(tel|vc)_arm_1$") %>%
  str_subset(".+")

ren_strs_act <-
  proxy_fields_df %>%
  distinct(REN) %>%
  pull() %>%
  # filter out weekly telephone calls and daily video chats
  str_extract("^w\\d{2}(d\\d)?_(tel|vc)_arm_1$") %>%
  str_subset(".+")

# For each redcap_event_name, create a df from `df_cln_sbl` that contains only
# the fields that are identified as relevant proxy fields from the
# `proxy_fields_df`, then pack all the dfs into a list.
dfs_sbl_rens <-
  map(.x = ren_strs_sbl,
      .f = reduce_df_by_proxy_field_ren, # sourced above
      df_cln_sbl,
      proxy_fields_df)

# Use the redcap_event_name strings to name the dfs in `dfs_sbl_rens`
names(dfs_sbl_rens) <- ren_strs_sbl

# The dfs in `dfs_sbl_rens` have values scattered across several rows, so these
# need to be collapsed into one row for each participant. That's what
# `collapse_df_rows_by_ren` does.
dfs_sbl_rens_rdc <-
  map(.x = ren_strs_sbl,
      .f = collapse_df_rows_by_ren,
      dfs_sbl_rens,
      ids)

# Again, use the redcap_event_name strings to name the dfs
# in `dfs_sbl_rens_rdc`
names(dfs_sbl_rens_rdc) <- ren_strs_sbl

# Ensure that each df in dfs_sbl_rens_rdc has all possible IDs
dfs_sbl_rens_rdc_aug1 <-
  map(dfs_sbl_rens_rdc,
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

# Ensure that each df in dfs_sbl_rens_rdc_aug2 has all appropriate fields
dfs_sbl_rens_rdc_aug2 <-
  map2(.x = dfs_sbl_rens_rdc_aug1,
       .y = ren_strs_sbl,
       .f = add_missing_fields,
       proxy_fields_df)

# Nest all the proxy fields for each ID in order to ease identification of
# incomplete instruments/forms
dfs_sbl_rens_rdc_aug_nst <-
  map(.x = dfs_sbl_rens_rdc_aug2,
      .f = function(df) {
        df %>%
          group_by(ts_sub_id) %>%
          nest()
      })

# cmp = complete
# Add column that identifies which stages are incomplete/pending/complete
# for each participant (excluding `admin_arm_1` df)
dfs_sbl_rens_rdc_aug_nst_cmp <-
  map2(.x = dfs_sbl_rens_rdc_aug_nst[which(ren_strs_sbl != "admin_arm_1")],
       .y = ren_strs_sbl[which(ren_strs_sbl != "admin_arm_1")],
       .f = add_complete_col,
       proxy_fields_df)

# stt = status
# Use `admin_arm_1` to determine participant status
dfs_sbl_rens_rdc_aug_nst_stt <-
  map2(.x = dfs_sbl_rens_rdc_aug_nst[which(ren_strs_sbl == "admin_arm_1")],
       .y = ren_strs_sbl[which(ren_strs_sbl == "admin_arm_1")],
       .f = add_status_col,
       proxy_fields_df)

# Tack `admin_arm_1` back onto the end of `dfs_sbl_rens_rdc_aug_nst_cmp`
dfs_sbl_rens_rdc_aug_nst_cmp <-
  c(
    dfs_sbl_rens_rdc_aug_nst_cmp
    , dfs_sbl_rens_rdc_aug_nst_stt
  )

# mfs = missing forms
# Add columns that help identify which instruments/forms are missing
# for each participant
dfs_sbl_rens_rdc_aug_nst_mfs <-
  map2(.x = dfs_sbl_rens_rdc_aug_nst,
       .y = ren_strs_sbl,
       .f = add_missing_forms_col,
       proxy_fields_df)


# SAVE DATA AS RDS ----

cat(green("Saving data as RDS files...\n"))

saveRDS(dfs_sbl_rens_rdc_aug_nst_cmp,
        paste0(wd_path,
               "/ICONECT_Participant_Tracker/rds/",
               "dfs_sbl_rens_rdc_aug_nst_cmp.Rds"))

saveRDS(dfs_sbl_rens_rdc_aug_nst_mfs,
        paste0(wd_path,
               "/ICONECT_Participant_Tracker/rds/",
               "dfs_sbl_rens_rdc_aug_nst_mfs.Rds"))

saveRDS(df_cln_act_sel_mut_flt,
        paste0(wd_path,
               "/ICONECT_Participant_Tracker/rds/",
               "df_cln_act_sel_mut_flt.Rds"))

cat(cyan("\nDone.\n\n"))


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
